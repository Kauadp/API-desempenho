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

#* Endpoint GET para retornar o desempenho do dia (lendo direto do Google Sheets)
#* @get /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  
  cat("=== GET DESEMPENHO DIÁRIO ===\n")
  cat("Lendo dados do dia:", as.character(Sys.Date()), "direto do Google Sheets\n")
  
  tryCatch({
    # Lê os dados diretamente do Google Sheets
    dados_sheets <- googlesheets4::read_sheet(sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Página1")
    
    # Filtra apenas os dados do dia atual
    desempenho_diario <- dados_sheets |>
      filter(data == Sys.Date())
    
    cat("Dados lidos com sucesso do Google Sheets\n")
    cat("Registros encontrados:", nrow(desempenho_diario), "\n")
    cat("============================\n")
    
    # Verifica se não há dados para retornar
    if (nrow(desempenho_diario) == 0) {
      cat("Não houveram ligações hoje\n")
      return(list(
        status = "sucesso",
        message = "Não houveram ligações hoje",
        data = as.character(Sys.Date()),
        dados = list()
      ))
    }
    
    # Se há dados, retorna normalmente
    return(list(
      status = "sucesso",
      message = paste("Encontrados", nrow(desempenho_diario), "registros para hoje"),
      data = as.character(Sys.Date()),
      dados = desempenho_diario
    ))
    
  }, error = function(e) {
    cat("Erro ao ler dados do Google Sheets:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro ao acessar Google Sheets:", e$message)))
  })
}

#* Endpoint GET para retornar o desempenho da semana (lendo direto do Google Sheets)
#* @get /desempenho-semanal  
#* @serializer unboxedJSON
function(req, res) {
  
  cat("=== GET DESEMPENHO SEMANAL ===\n")
  
  # Define o início da semana (segunda-feira)
  inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
  cat("Lendo dados da semana desde:", as.character(inicio_da_semana), "direto do Google Sheets\n")
  
  tryCatch({
    # Lê os dados diretamente do Google Sheets
    dados_sheets <- googlesheets4::read_sheet(sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Página1")
    
    # Filtra os dados da semana atual
    dados_da_semana <- dados_sheets |>
      filter(data >= inicio_da_semana)
    
    cat("Dados da semana lidos com sucesso do Google Sheets\n")
    cat("Registros encontrados:", nrow(dados_da_semana), "\n")
    cat("===============================\n")
    
    # Verifica se não há dados para retornar
    if (nrow(dados_da_semana) == 0) {
      cat("Não houveram ligações esta semana\n")
      return(list(
        status = "sucesso",
        message = "Não houveram ligações esta semana",
        periodo = paste("desde", as.character(inicio_da_semana)),
        dados = list()
      ))
    }
    
    # Se há dados, processa com a função desempenho_semana
    desempenho_final <- desempenho_semana(dados_da_semana)
    
    return(list(
      status = "sucesso",
      message = paste("Encontrados", nrow(dados_da_semana), "registros para esta semana"),
      periodo = paste("desde", as.character(inicio_da_semana)),
      dados = desempenho_final
    ))
    
  }, error = function(e) {
    cat("Erro ao ler dados do Google Sheets:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro ao acessar Google Sheets:", e$message)))
  })
}

#* Endpoint para adicionar agendamentos no quadro da semana
#* @param responsavel Nome do responsável pelo agendamento
#* @param empresa Nome da empresa agendada
#* @param data Data do agendamento (formato YYYY-MM-DD)
#* @post /adicionar-agendamento
#* @serializer unboxedJSON
function(req, res) {
  
  cat("=== ADICIONAR AGENDAMENTO NO QUADRO ===\n")
  
  # Parse dos dados recebidos
  dados_agendamento <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "" && nchar(req$postBody) > 0) {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else if (length(req$args) > 0) {
      req$args
    } else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  if ("erro" %in% names(dados_agendamento)) {
    return(dados_agendamento)
  }
  
  cat("Dados recebidos:", str(dados_agendamento), "\n")
  
  # Validação dos dados obrigatórios
  if (is.null(dados_agendamento$responsavel) || dados_agendamento$responsavel == "") {
    res$status <- 400
    return(list(erro = "Campo 'responsavel' é obrigatório"))
  }
  
  if (is.null(dados_agendamento$empresa) || dados_agendamento$empresa == "") {
    res$status <- 400
    return(list(erro = "Campo 'empresa' é obrigatório"))
  }
  
  if (is.null(dados_agendamento$data) || dados_agendamento$data == "") {
    res$status <- 400
    return(list(erro = "Campo 'data' é obrigatório"))
  }
  
  tryCatch({
    # Ler dados existentes da planilha de agendamentos
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    
    # Tentar ler a aba de agendamentos (criar se não existir)
    dados_existentes <- tryCatch({
      googlesheets4::read_sheet(sheet_id, sheet = "Agendamentos")
    }, error = function(e) {
      cat("Aba 'Agendamentos' não existe, criando estrutura vazia\n")
      data.frame(
        responsavel = character(0),
        empresa = character(0),
        data = as.Date(character(0)),
        data_adicao = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    })
    
    # Verificar duplicatas (mesma empresa na mesma data)
    data_agendamento <- as.Date(dados_agendamento$data)
    duplicata_existe <- FALSE
    
    if (nrow(dados_existentes) > 0) {
      duplicata_existe <- any(
        dados_existentes$empresa == dados_agendamento$empresa & 
          as.Date(dados_existentes$data) == data_agendamento
      )
    }
    
    if (duplicata_existe) {
      return(list(
        erro = paste("Agendamento duplicado: empresa", dados_agendamento$empresa, "já agendada para", data_agendamento),
        status = "duplicata"
      ))
    }
    
    # Criar novo registro (usando a data atual)
    novo_agendamento <- data.frame(
      responsavel = dados_agendamento$responsavel,
      empresa = dados_agendamento$empresa,
      data = Sys.Date(),
      data_adicao = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Combinar dados existentes com novo agendamento
    dados_atualizados <- rbind(dados_existentes, novo_agendamento)
    
    # Escrever de volta na planilha
    googlesheets4::write_sheet(dados_atualizados, sheet_id, sheet = "Agendamentos")
    
    cat("Agendamento adicionado com sucesso\n")
    cat("==================================\n")
    
    return(list(
      status = "sucesso",
      message = "Agendamento adicionado com sucesso",
      agendamento = novo_agendamento
    ))
    
  }, error = function(e) {
    cat("Erro ao processar agendamento:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

#* Endpoint para buscar dados do quadro da semana
#* @get /quadro-semana
#* @serializer unboxedJSON
function(req, res) {
  
  cat("=== QUADRO DA SEMANA ===\n")
  
  # Define o início da semana (segunda-feira)
  inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
  cat("Buscando agendamentos desde:", as.character(inicio_da_semana), "\n")
  
  tryCatch({
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    
    # Ler dados da aba de agendamentos
    dados_agendamentos <- tryCatch({
      googlesheets4::read_sheet(sheet_id, sheet = "Agendamentos")
    }, error = function(e) {
      cat("Aba 'Agendamentos' não encontrada\n")
      data.frame(
        responsavel = character(0),
        empresa = character(0),
        data = as.Date(character(0)),
        data_adicao = as.POSIXct(character(0)),
        stringsAsFactors = FALSE
      )
    })
    
    if (nrow(dados_agendamentos) == 0) {
      return(list(
        status = "sucesso",
        message = "Nenhum agendamento encontrado para esta semana",
        periodo = paste("desde", as.character(inicio_da_semana)),
        dados = list()
      ))
    }
    
    # Filtrar agendamentos da semana atual
    dados_agendamentos$data <- as.Date(dados_agendamentos$data)
    agendamentos_semana <- dados_agendamentos[dados_agendamentos$data >= inicio_da_semana, ]
    
    if (nrow(agendamentos_semana) == 0) {
      return(list(
        status = "sucesso",
        message = "Nenhum agendamento encontrado para esta semana",
        periodo = paste("desde", as.character(inicio_da_semana)),
        dados = list()
      ))
    }
    
    # Agrupar por responsável
    quadro_dados <- agendamentos_semana %>%
      group_by(responsavel) %>%
      summarise(
        agendamentos = n(),
        empresas = list(empresa),
        .groups = 'drop'
      )
    
    # Converter para formato de lista para JSON
    resultado <- list()
    for (i in 1:nrow(quadro_dados)) {
      responsavel <- quadro_dados$responsavel[i]
      resultado[[responsavel]] <- list(
        agendamentos = quadro_dados$agendamentos[i],
        empresas = quadro_dados$empresas[[i]]
      )
    }
    
    cat("Dados do quadro processados com sucesso\n")
    cat("Responsáveis encontrados:", length(resultado), "\n")
    cat("========================\n")
    
    return(list(
      status = "sucesso",
      message = paste("Encontrados agendamentos de", length(resultado), "responsáveis para esta semana"),
      periodo = paste("desde", as.character(inicio_da_semana)),
      dados = resultado
    ))
    
  }, error = function(e) {
    cat("Erro ao buscar dados do quadro:", e$message, "\n")
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}