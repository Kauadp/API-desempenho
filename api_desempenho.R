source("funcoes.R")

# Carrega bibliotecas necessárias
library(plumber)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)

# Configuração global da autenticação
setup_google_auth <- function(credentials_json = NULL) {
  tryCatch({
    # Prioridade: 1. Parâmetro passado, 2. Variável de ambiente, 3. Arquivo local
    if (!is.null(credentials_json)) {
      # Se recebeu as credenciais como JSON string
      if (is.character(credentials_json)) {
        # Escreve temporariamente o JSON em um arquivo
        temp_file <- tempfile(fileext = ".json")
        writeLines(credentials_json, temp_file)
        credentials_path <- temp_file
      } else {
        credentials_path <- credentials_json
      }
    } else if (Sys.getenv("GOOGLE_CREDENTIALS_JSON") != "") {
      # Se tem nas variáveis de ambiente
      temp_file <- tempfile(fileext = ".json")
      writeLines(Sys.getenv("GOOGLE_CREDENTIALS_JSON"), temp_file)
      credentials_path <- temp_file
    } else if (file.exists("credentials.json")) {
      # Se tem o arquivo local
      credentials_path <- "credentials.json"
    } else {
      stop("Nenhuma credencial do Google encontrada")
    }
    
    # Configura a autenticação
    gs4_auth(path = credentials_path)
    drive_auth(path = credentials_path)
    
    # Remove arquivo temporário se foi criado
    if (exists("temp_file") && file.exists(temp_file)) {
      unlink(temp_file)
    }
    
    cat("Autenticação Google configurada com sucesso\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Erro na configuração da autenticação:", e$message, "\n")
    return(FALSE)
  })
}

# Inicializa a autenticação na inicialização da API
init_auth <- setup_google_auth()

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @apiTitle API de Desempenho de Vendas
#* @apiDescription Esta API atualiza e retorna os dados de desempenho de vendas com autenticação Google Sheets.

#* Endpoint para configurar credenciais do Google
#* @param credentials_json JSON string com as credenciais do Google Cloud
#* @post /auth/setup
#* @serializer unboxedJSON
function(req, res) {
  cat("=== CONFIGURANDO AUTENTICAÇÃO ===\n")
  
  # Extrai as credenciais da requisição
  credentials <- tryCatch({
    if ("credentials_json" %in% names(req$args) && !is.null(req$args$credentials_json)) {
      req$args$credentials_json
    } else if (!is.null(req$postBody) && req$postBody != "") {
      parsed_body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
      parsed_body$credentials_json
    } else {
      stop("Credenciais não fornecidas")
    }
  }, error = function(e) {
    cat("Erro ao extrair credenciais:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar credenciais:", e$message)))
  })
  
  if ("erro" %in% names(credentials)) {
    return(credentials)
  }
  
  # Configura a autenticação
  auth_success <- setup_google_auth(credentials)
  
  if (auth_success) {
    return(list(
      status = "sucesso",
      message = "Credenciais configuradas com sucesso",
      timestamp = Sys.time()
    ))
  } else {
    res$status <- 500
    return(list(
      status = "erro",
      message = "Falha ao configurar credenciais"
    ))
  }
}

#* Endpoint para retornar o desempenho do dia
#* @param agendamentos Uma lista nomeada com o número de agendamentos do dia.
#* @param credentials_json Opcional: JSON string com credenciais do Google (se não configurado previamente)
#* @post /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  
  # Debug: imprime informações da requisição
  cat("=== DEBUG REQUISIÇÃO ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody raw:", req$postBody, "\n")
  cat("postBody class:", class(req$postBody), "\n")
  cat("postBody length:", length(req$postBody), "\n")
  
  # Parse dos dados da requisição
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "" && nchar(req$postBody) > 0) {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else if ("agendamentos" %in% names(req$args)) {
      if (is.character(req$args$agendamentos)) {
        list(agendamentos = jsonlite::fromJSON(req$args$agendamentos, simplifyVector = FALSE))
      } else {
        list(agendamentos = req$args$agendamentos)
      }
    } else if (length(req$args) > 0) {
      req$args
    } else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing JSON:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  if ("erro" %in% names(request_data)) {
    return(request_data)
  }
  
  # Configura autenticação se fornecida
  if ("credentials_json" %in% names(request_data) && !is.null(request_data$credentials_json)) {
    auth_success <- setup_google_auth(request_data$credentials_json)
    if (!auth_success) {
      res$status <- 401
      return(list(erro = "Falha na autenticação do Google"))
    }
  }
  
  # Extrai os agendamentos
  agendamentos <- request_data$agendamentos
  if (is.null(agendamentos)) {
    res$status <- 400
    return(list(erro = "Agendamentos não fornecidos"))
  }
  
  cat("Agendamentos parsed:", str(agendamentos), "\n")
  cat("========================\n")
  
  # Continua com o processamento normal
  tryCatch({
    dados_completos <- api_etl(agendamentos)
    
    desempenho_diario <- dados_completos |>
      filter(data == Sys.Date())
    
    return(desempenho_diario)
  }, error = function(e) {
    cat("Erro no processamento:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

#* Endpoint para retornar o desempenho da semana  
#* @param agendamentos Uma lista nomeada com o número de agendamentos do dia.
#* @param credentials_json Opcional: JSON string com credenciais do Google (se não configurado previamente)
#* @post /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  
  # Debug: imprime informações da requisição
  cat("=== DEBUG REQUISIÇÃO SEMANAL ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody raw:", req$postBody, "\n")
  
  # Parse dos dados da requisição
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "" && nchar(req$postBody) > 0) {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else if ("agendamentos" %in% names(req$args)) {
      if (is.character(req$args$agendamentos)) {
        list(agendamentos = jsonlite::fromJSON(req$args$agendamentos, simplifyVector = FALSE))
      } else {
        list(agendamentos = req$args$agendamentos)
      }
    } else if (length(req$args) > 0) {
      req$args
    } else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing JSON:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  if ("erro" %in% names(request_data)) {
    return(request_data)
  }
  
  # Configura autenticação se fornecida
  if ("credentials_json" %in% names(request_data) && !is.null(request_data$credentials_json)) {
    auth_success <- setup_google_auth(request_data$credentials_json)
    if (!auth_success) {
      res$status <- 401
      return(list(erro = "Falha na autenticação do Google"))
    }
  }
  
  # Extrai os agendamentos
  agendamentos <- request_data$agendamentos
  if (is.null(agendamentos)) {
    res$status <- 400
    return(list(erro = "Agendamentos não fornecidos"))
  }
  
  cat("Agendamentos parsed:", str(agendamentos), "\n")
  cat("================================\n")
  
  tryCatch({
    dados_completos <- api_etl(agendamentos)
    
    # Define o início da semana (segunda-feira)
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    
    dados_da_semana <- dados_completos |>
      filter(data >= inicio_da_semana)
    
    desempenho_final <- desempenho_semana(dados_da_semana)
    
    return(desempenho_final)
  }, error = function(e) {
    cat("Erro no processamento:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

#* Endpoint de teste para verificar se a API está funcionando
#* @get /health
function() {
  # Verifica também o status da autenticação
  auth_status <- tryCatch({
    # Tenta fazer uma operação simples para verificar se a auth está ok
    gs4_user()
    "Autenticado"
  }, error = function(e) {
    "Não autenticado"
  })
  
  return(list(
    status = "OK", 
    timestamp = Sys.time(),
    message = "API funcionando corretamente",
    google_auth_status = auth_status
  ))
}

#* Endpoint de teste para POST
#* @param dados Dados de teste
#* @param credentials_json Credenciais do Google (opcional para teste)
#* @post /test-post
function(req, res) {
  cat("=== TESTE POST ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody:", req$postBody, "\n")
  cat("postBody empty?:", is.null(req$postBody) || req$postBody == "", "\n")
  cat("args:", paste(names(req$args), collapse = ", "), "\n")
  
  # Mostra o conteúdo dos args
  for(arg_name in names(req$args)) {
    cat("arg", arg_name, ":", substr(req$args[[arg_name]], 1, 100), "...\n") # Limita output para não logar credenciais completas
  }
  
  # Testa o parsing dos dados
  dados_processados <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else if ("dados" %in% names(req$args) && !is.null(req$args$dados)) {
      if (is.character(req$args$dados)) {
        jsonlite::fromJSON(req$args$dados, simplifyVector = FALSE)
      } else {
        req$args$dados
      }
    } else if (length(req$args) > 0) {
      req$args
    } else {
      list(erro = "Nenhum dado encontrado")
    }
  }, error = function(e) {
    list(erro = e$message)
  })
  
  # Remove credenciais sensíveis do retorno
  dados_limpos <- dados_processados
  if ("credentials_json" %in% names(dados_limpos)) {
    dados_limpos$credentials_json <- "*** CREDENCIAIS PRESENTES ***"
  }
  
  return(list(
    received_body_length = nchar(req$postBody %||% ""),
    content_type = req$HTTP_CONTENT_TYPE,
    args_names = names(req$args),
    dados_processados = dados_limpos,
    timestamp = Sys.time()
  ))
}