source("funcoes.R")
library(plumber)


# Pega o JSON da variável de ambiente
creds_json <- Sys.getenv("GS4_CREDENTIALS_JSON")

# Salva temporariamente para autenticar
tmp_file <- tempfile(fileext = ".json")
writeLines(creds_json, tmp_file)

# Autentica a service account
gs4_auth(path = tmp_file)

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

#* Endpoint para retornar o desempenho do dia
#* @param agendamentos Uma lista nomeada com o número de agendamentos do dia.
#* @post /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  
  # Debug: imprime informações da requisição
  cat("=== DEBUG REQUISIÇÃO ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody raw:", req$postBody, "\n")
  cat("postBody class:", class(req$postBody), "\n")
  cat("postBody length:", length(req$postBody), "\n")
  
  # Tenta diferentes formas de parsing
  agendamentos <- tryCatch({
    # Primeiro verifica se veio nos args (comum no Swagger)
    if ("agendamentos" %in% names(req$args) && !is.null(req$args$agendamentos)) {
      cat("Dados encontrados em req$args$agendamentos:", req$args$agendamentos, "\n")
      jsonlite::fromJSON(req$args$agendamentos, simplifyVector = FALSE)
    } 
    # Se não, verifica se veio no postBody
    else if (!is.null(req$postBody) && req$postBody != "" && nchar(req$postBody) > 0) {
      cat("Dados encontrados em postBody\n")
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    }
    # Se nenhum dos dois, retorna os próprios args (pode ser que o Swagger envie direto)
    else if (length(req$args) > 0) {
      cat("Usando req$args diretamente:", str(req$args), "\n")
      req$args
    }
    else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing JSON:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  # Verifica se houve erro no parsing
  if ("erro" %in% names(agendamentos)) {
    return(agendamentos)
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
#* @post /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  
  # Debug: imprime informações da requisição
  cat("=== DEBUG REQUISIÇÃO SEMANAL ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody raw:", req$postBody, "\n")
  
  # Mesmo tratamento de parsing
  agendamentos <- tryCatch({
    # Primeiro verifica se veio nos args (comum no Swagger)
    if ("agendamentos" %in% names(req$args) && !is.null(req$args$agendamentos)) {
      cat("Dados encontrados em req$args$agendamentos:", req$args$agendamentos, "\n")
      jsonlite::fromJSON(req$args$agendamentos, simplifyVector = FALSE)
    } 
    # Se não, verifica se veio no postBody
    else if (!is.null(req$postBody) && req$postBody != "" && nchar(req$postBody) > 0) {
      cat("Dados encontrados em postBody\n")
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    }
    # Se nenhum dos dois, retorna os próprios args (pode ser que o Swagger envie direto)
    else if (length(req$args) > 0) {
      cat("Usando req$args diretamente:", str(req$args), "\n")
      req$args
    }
    else {
      stop("Nenhum dado encontrado na requisição")
    }
  }, error = function(e) {
    cat("Erro no parsing JSON:", e$message, "\n")
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  if ("erro" %in% names(agendamentos)) {
    return(agendamentos)
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
  return(list(
    status = "OK", 
    timestamp = Sys.time(),
    message = "API funcionando corretamente"
  ))
}

#* Endpoint de teste para POST
#* @param dados Dados de teste
#* @post /test-post
function(req, res) {
  cat("=== TESTE POST ===\n")
  cat("Content-Type:", req$HTTP_CONTENT_TYPE, "\n")
  cat("postBody:", req$postBody, "\n")
  cat("postBody empty?:", is.null(req$postBody) || req$postBody == "", "\n")
  cat("args:", paste(names(req$args), collapse = ", "), "\n")
  
  # Mostra o conteúdo dos args
  for(arg_name in names(req$args)) {
    cat("arg", arg_name, ":", req$args[[arg_name]], "\n")
  }
  
  # Testa o parsing dos dados
  dados_processados <- tryCatch({
    if ("dados" %in% names(req$args) && !is.null(req$args$dados)) {
      cat("Tentando fazer parse de req$args$dados\n")
      jsonlite::fromJSON(req$args$dados, simplifyVector = FALSE)
    } else if (length(req$args) > 0) {
      cat("Usando req$args diretamente\n")
      req$args
    } else {
      list(erro = "Nenhum dado encontrado")
    }
  }, error = function(e) {
    list(erro = e$message)
  })
  
  return(list(
    received_body = req$postBody,
    content_type = req$HTTP_CONTENT_TYPE,
    args_names = names(req$args),
    args_content = req$args,
    dados_processados = dados_processados
  ))
}