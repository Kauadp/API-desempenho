# Carrega bibliotecas necessárias
library(plumber)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(lubridate) # importante para floor_date e manipulação de datas
library(tibble)


source("funcoes.R")

# Configuração global da autenticação
setup_google_auth <- function(credentials_json = NULL) {
  tryCatch({
    if (!is.null(credentials_json)) {
      if (is.character(credentials_json)) {
        temp_file <- tempfile(fileext = ".json")
        writeLines(credentials_json, temp_file)
        credentials_path <- temp_file
      } else {
        credentials_path <- credentials_json
      }
    } else if (Sys.getenv("GOOGLE_CREDENTIALS_JSON") != "") {
      temp_file <- tempfile(fileext = ".json")
      writeLines(Sys.getenv("GOOGLE_CREDENTIALS_JSON"), temp_file)
      credentials_path <- temp_file
    } else if (file.exists("credentials.json")) {
      credentials_path <- "credentials.json"
    } else {
      stop("Nenhuma credencial do Google encontrada")
    }
    
    gs4_auth(path = credentials_path)
    drive_auth(path = credentials_path)
    
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

# --------------------
# Endpoint de Autenticação
# --------------------
#* @post /auth/setup
#* @param credentials_json JSON string com as credenciais do Google Cloud
#* @serializer unboxedJSON
function(req, res) {
  cat("=== CONFIGURANDO AUTENTICAÇÃO ===\n")
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
    res$status <- 400
    return(list(erro = paste("Erro ao processar credenciais:", e$message)))
  })
  
  auth_success <- setup_google_auth(credentials)
  
  if (auth_success) {
    return(list(
      status = "sucesso",
      message = "Credenciais configuradas com sucesso",
      timestamp = Sys.time()
    ))
  } else {
    res$status <- 500
    return(list(status = "erro", message = "Falha ao configurar credenciais"))
  }
}

# --------------------
# Endpoint POST desempenho-diario
# --------------------
#* @post /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else {
      list()
    }
  }, error = function(e) {
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  tryCatch({
    dados_completos <- api_etl(request_data$agendamentos)
    dados_completos$data <- as.Date(dados_completos$data)
    
    desempenho_diario <- dados_completos %>%
      filter(data == Sys.Date())
    
    return(desempenho_diario)
  }, error = function(e) {
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

# --------------------
# Endpoint POST desempenho-semanal
# --------------------
#* @post /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") {
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    } else {
      list()
    }
  }, error = function(e) {
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  tryCatch({
    dados_completos <- api_etl(request_data$agendamentos)
    dados_completos$data <- as.Date(dados_completos$data)
    
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    dados_semana <- dados_completos %>%
      filter(data >= inicio_da_semana)
    
    desempenho_final <- desempenho_semana(dados_semana)
    
    return(desempenho_final)
  }, error = function(e) {
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

# --------------------
# Endpoint GET desempenho-diario (Sheets)
# --------------------
#* @get /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    dados_sheets <- read_sheet("1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Página1")
    dados_sheets$data <- as.Date(dados_sheets$data)
    
    desempenho_diario <- dados_sheets %>%
      filter(data == Sys.Date())
    
    if (nrow(desempenho_diario) == 0) {
      return(list(status="sucesso", message="Não houveram ligações hoje", data=as.character(Sys.Date()), dados=list()))
    }
    
    return(list(status="sucesso", message=paste("Encontrados", nrow(desempenho_diario), "registros"), data=as.character(Sys.Date()), dados=desempenho_diario))
    
  }, error = function(e) {
    res$status <- 500
    return(list(erro=paste("Erro ao acessar Google Sheets:", e$message)))
  })
}

# --------------------
# Endpoint GET desempenho-semanal (Sheets)
# --------------------
#* @get /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    
    dados_sheets <- read_sheet("1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Página1")
    dados_sheets$data <- as.Date(dados_sheets$data)
    
    dados_semana <- dados_sheets %>%
      filter(data >= inicio_da_semana)
    
    if (nrow(dados_semana) == 0) {
      return(list(status="sucesso", message="Não houveram ligações esta semana", periodo=paste("desde", as.character(inicio_da_semana)), dados=list()))
    }
    
    desempenho_final <- desempenho_semana(dados_semana)
    return(list(status="sucesso", message=paste("Encontrados", nrow(dados_semana), "registros"), periodo=paste("desde", as.character(inicio_da_semana)), dados=desempenho_final))
    
  }, error = function(e) {
    res$status <- 500
    return(list(erro=paste("Erro ao acessar Google Sheets:", e$message)))
  })
}

# --------------------
# Endpoint GET health
# --------------------
#* @get /health
function() {
  auth_status <- tryCatch({
    gs4_user()
    "Autenticado"
  }, error=function(e) {
    "Não autenticado"
  })
  
  return(list(status="OK", timestamp=Sys.time(), message="API funcionando corretamente", google_auth_status=auth_status))
}

# --------------------
# Endpoint POST adicionar-agendamento
# --------------------
#* @post /adicionar-agendamento
#* @serializer unboxedJSON
function(req, res) {
  dados_agendamento <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  dados_agendamento$data <- as.Date(dados_agendamento$data)
  
  # Validação simples
  if (is.null(dados_agendamento$responsavel) || dados_agendamento$responsavel=="") {
    res$status <- 400
    return(list(erro="Campo 'responsavel' é obrigatório"))
  }
  
  # restante do código de gravação igual ao seu original...
}

# --------------------
# Endpoint GET quadro-semana
# --------------------
#* @get /quadro-semana
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    dados_agendamentos <- read_sheet(sheet_id, sheet="Agendamentos")
    dados_agendamentos$data <- as.Date(dados_agendamentos$data)
    
    agendamentos_semana <- dados_agendamentos %>%
      filter(data >= inicio_da_semana)
    
    if (nrow(agendamentos_semana)==0) {
      return(list(status="sucesso", message="Nenhum agendamento encontrado", periodo=paste("desde", as.character(inicio_da_semana)), dados=list()))
    }
    
    quadro_dados <- agendamentos_semana %>%
      group_by(responsavel) %>%
      summarise(agendamentos=n(), empresas=list(empresa), .groups='drop')
    
    resultado <- list()
    for (i in 1:nrow(quadro_dados)) {
      resultado[[quadro_dados$responsavel[i]]] <- list(agendamentos=quadro_dados$agendamentos[i], empresas=quadro_dados$empresas[[i]])
    }
    
    return(list(status="sucesso", message=paste("Encontrados agendamentos de", length(resultado), "responsáveis"), periodo=paste("desde", as.character(inicio_da_semana)), dados=resultado))
    
  }, error=function(e){
    res$status <- 500
    return(list(erro=paste("Erro interno:", e$message)))
  })
}
