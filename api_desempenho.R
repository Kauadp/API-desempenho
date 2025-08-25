# Carrega bibliotecas necessárias
suppressMessages({
  library(plumber)
  library(jsonlite)
  library(googlesheets4)
  library(dplyr)
  library(lubridate)
})

# Carrega funções personalizadas (se existir o arquivo)
if (file.exists("funcoes.R")) {
  source("funcoes.R")
}

# Configuração de autenticação Google Sheets
tryCatch({
  # Pega o JSON da variável de ambiente
  creds_json <- Sys.getenv("GS4_CREDENTIALS_JSON")
  
  if (creds_json != "") {
    # Salva temporariamente para autenticar
    tmp_file <- tempfile(fileext = ".json")
    writeLines(creds_json, tmp_file)
    
    # Autentica a service account
    gs4_auth(path = tmp_file)
    cat("✓ Google Sheets autenticado com sucesso\n")
  } else {
    cat("⚠ Variável GS4_CREDENTIALS_JSON não encontrada\n")
  }
}, error = function(e) {
  cat("⚠ Erro na autenticação Google Sheets:", e$message, "\n")
})

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
#* @apiDescription Esta API atualiza e retorna os dados de desempenho de vendas.

# Função auxiliar para parsing de dados
parse_request_data <- function(req, param_name = "agendamentos") {
  cat("=== DEBUG REQUISIÇÃO ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE %||% "não definido", "\n")
  cat("postBody existe:", !is.null(req$postBody), "\n")
  cat("postBody length:", if(!is.null(req$postBody)) nchar(req$postBody) else 0, "\n")
  
  # Tenta diferentes formas de parsing
  dados <- tryCatch({
    # Primeiro: verifica se veio nos args
    if (param_name %in% names(req$args) && !is.null(req$args[[param_name]])) {
      cat("Dados encontrados em req$args$", param_name, "\n")
      dados_raw <- req$args[[param_name]]
      if (is.character(dados_raw)) {
        jsonlite::fromJSON(dados_raw, simplifyVector = FALSE)
      } else {
        dados_raw
      }
    } 
    # Segundo: verifica se veio no postBody
    else if (!is.null(req$postBody) && nchar(req$postBody) > 0) {
      cat("Dados encontrados em postBody\n")
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    }
    # Terceiro: usa os próprios args
    else if (length(req$args) > 0) {
      cat("Usando req$args diretamente\n")
      req$args
    }
    else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing:", e$message, "\n")
    stop(paste("Erro ao processar dados:", e$message))
  })
  
  cat("Dados processados com sucesso\n")
  cat("========================\n")
  return(dados)
}

#* Endpoint para retornar o desempenho do dia
#* @param agendamentos Uma lista nomeada com o número de agendamentos do dia.
#* @post /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    agendamentos <- parse_request_data(req, "agendamentos")
    
    # Verifica se a função api_etl existe
    if (!exists("api_etl")) {
      stop("Função api_etl não encontrada. Verifique o arquivo funcoes.R")
    }
    
    dados_completos <- api_etl(agendamentos)
    
    desempenho_diario <- dados_completos |>
      filter(data == Sys.Date())
    
    return(desempenho_diario)
    
  }, error = function(e) {
    cat("Erro no processamento:", e$message, "\n")
    res$status <- 500
    return(list(
      erro = TRUE,
      mensagem = e$message,
      timestamp = Sys.time()
    ))
  })
}

#* Endpoint para retornar o desempenho da semana  
#* @param agendamentos Uma lista nomeada com o número de agendamentos do dia.
#* @post /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    agendamentos <- parse_request_data(req, "agendamentos")
    
    # Verifica se as funções existem
    if (!exists("api_etl")) {
      stop("Função api_etl não encontrada. Verifique o arquivo funcoes.R")
    }
    if (!exists("desempenho_semana")) {
      stop("Função desempenho_semana não encontrada. Verifique o arquivo funcoes.R")
    }
    
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
    return(list(
      erro = TRUE,
      mensagem = e$message,
      timestamp = Sys.time()
    ))
  })
}

#* Endpoint de teste para verificar se a API está funcionando
#* @get /health
function() {
  return(list(
    status = "OK", 
    timestamp = Sys.time(),
    message = "API funcionando corretamente",
    port = Sys.getenv("PORT", "8000"),
    r_version = R.version.string,
    packages = list(
      plumber = packageVersion("plumber"),
      googlesheets4 = packageVersion("googlesheets4"),
      jsonlite = packageVersion("jsonlite")
    )
  ))
}

#* Endpoint de teste para POST
#* @param dados Dados de teste
#* @post /test-post
#* @serializer unboxedJSON
function(req, res) {
  cat("=== TESTE POST ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE %||% "não definido", "\n")
  cat("postBody exists:", !is.null(req$postBody), "\n")
  cat("postBody length:", if(!is.null(req$postBody)) nchar(req$postBody) else 0, "\n")
  cat("args:", paste(names(req$args), collapse = ", "), "\n")
  
  # Testa o parsing dos dados
  dados_processados <- tryCatch({
    parse_request_data(req, "dados")
  }, error = function(e) {
    list(erro = e$message)
  })
  
  return(list(
    status = "success",
    received_body = if(!is.null(req$postBody)) req$postBody else "vazio",
    content_type = req$HTTP_CONTENT_TYPE %||% "não definido",
    args_names = names(req$args),
    args_content = req$args,
    dados_processados = dados_processados,
    timestamp = Sys.time()
  ))
}

# Mensagem de inicialização
cat("🚀 API de Desempenho de Vendas carregada com sucesso!\n")
cat("📊 Endpoints disponíveis:\n")
cat("  - GET  /health (verificar status)\n")
cat("  - POST /test-post (testar requisições)\n")
cat("  - POST /desempenho-diario\n")
cat("  - POST /desempenho-semanal\n")