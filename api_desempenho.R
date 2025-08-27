source("funcoes.R")

# Carrega bibliotecas necessárias
library(plumber)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(lubridate)

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
    
    if (exists("temp_file") && file.exists(temp_file)) unlink(temp_file)
    
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

# === Endpoints ===

#* Configurar credenciais do Google
#* @post /auth/setup
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
    cat("Erro ao extrair credenciais:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar credenciais:", e$message)))
  })
  
  if ("erro" %in% names(credentials)) return(credentials)
  
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

#* Retorna desempenho diário
#* @get /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  cat("=== GET DESEMPENHO DIÁRIO ===\n")
  tryCatch({
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    dados_sheets <- googlesheets4::read_sheet(sheet_id, sheet = "Página1")
    dados_sheets$data <- as.Date(dados_sheets$data)  # garante Date
    
    desempenho_diario <- dados_sheets |> filter(data == Sys.Date())
    
    cat("Registros encontrados:", nrow(desempenho_diario), "\n")
    if (nrow(desempenho_diario) == 0) {
      return(list(
        status = "sucesso",
        message = "Não houveram ligações hoje",
        data = as.character(Sys.Date()),
        dados = list()
      ))
    }
    
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

#* Retorna desempenho semanal
#* @get /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  cat("=== GET DESEMPENHO SEMANAL ===\n")
  tryCatch({
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    dados_sheets <- googlesheets4::read_sheet(sheet_id, sheet = "Página1")
    dados_sheets$data <- as.Date(dados_sheets$data)  # garante Date
    
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    dados_da_semana <- dados_sheets |> filter(data >= inicio_da_semana)
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

#* Adicionar agendamento
#* @post /adicionar-agendamento
#* @serializer unboxedJSON
function(req, res) {
  cat("=== ADICIONAR AGENDAMENTO ===\n")
  dados_agendamento <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    else if (length(req$args) > 0) req$args
    else stop("Nenhum dado encontrado")
  }, error = function(e) {
    res$status <- 400
    return(list(erro = e$message))
  })
  
  # Validações básicas
  if (is.null(dados_agendamento$responsavel) || dados_agendamento$responsavel == "" ||
      is.null(dados_agendamento$empresa) || dados_agendamento$empresa == "" ||
      is.null(dados_agendamento$data) || dados_agendamento$data == "") {
    res$status <- 400
    return(list(erro = "Campos obrigatórios ausentes"))
  }
  
  tryCatch({
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    dados_existentes <- tryCatch({
      googlesheets4::read_sheet(sheet_id, sheet = "Agendamentos")
    }, error = function(e) data.frame(responsavel=character(0), empresa=character(0), data=as.Date(character(0)), data_adicao=as.POSIXct(character(0))))
    
    dados_agendamento$data <- as.Date(dados_agendamento$data)
    duplicata <- any(dados_existentes$empresa == dados_agendamento$empresa & as.Date(dados_existentes$data) == dados_agendamento$data)
    if (duplicata) return(list(status="duplicata", message="Agendamento já existe"))
    
    novo_agendamento <- data.frame(
      responsavel = dados_agendamento$responsavel,
      empresa = dados_agendamento$empresa,
      data = dados_agendamento$data,
      data_adicao = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    dados_atualizados <- rbind(dados_existentes, novo_agendamento)
    googlesheets4::write_sheet(dados_atualizados, sheet_id, sheet="Agendamentos")
    
    return(list(status="sucesso", message="Agendamento adicionado", agendamento=novo_agendamento))
    
  }, error = function(e) {
    res$status <- 500
    return(list(erro=e$message))
  })
}

#* Retorna quadro da semana
#* @get /quadro-semana
#* @serializer unboxedJSON
function(req, res) {
  cat("=== QUADRO DA SEMANA ===\n")
  tryCatch({
    sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
    dados_agendamentos <- tryCatch({
      googlesheets4::read_sheet(sheet_id, sheet = "Agendamentos")
    }, error = function(e) data.frame(responsavel=character(0), empresa=character(0), data=as.Date(character(0)), data_adicao=as.POSIXct(character(0))))
    
    dados_agendamentos$data <- as.Date(dados_agendamentos$data)
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    agendamentos_semana <- dados_agendamentos[dados_agendamentos$data >= inicio_da_semana, ]
    
    if (nrow(agendamentos_semana) == 0) return(list(status="sucesso", message="Nenhum agendamento encontrado", periodo=paste("desde", as.character(inicio_da_semana)), dados=list()))
    
    quadro_dados <- agendamentos_semana %>%
      group_by(responsavel) %>%
      summarise(agendamentos=n(), empresas=list(empresa), .groups='drop')
    
    resultado <- list()
    for (i in 1:nrow(quadro_dados)) {
      resultado[[quadro_dados$responsavel[i]]] <- list(
        agendamentos = quadro_dados$agendamentos[i],
        empresas = quadro_dados$empresas[[i]]
      )
    }
    
    return(list(status="sucesso", message=paste("Encontrados agendamentos de", length(resultado), "responsáveis"), periodo=paste("desde", as.character(inicio_da_semana)), dados=resultado))
    
  }, error = function(e) {
    res$status <- 500
    return(list(erro=e$message))
  })
}

#* Health check
#* @get /health
function() {
  auth_status <- tryCatch({ gs4_user(); "Autenticado" }, error=function(e) "Não autenticado")
  return(list(status="OK", timestamp=Sys.time(), message="API funcionando corretamente", google_auth_status=auth_status))
}