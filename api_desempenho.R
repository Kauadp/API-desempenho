# Carrega bibliotecas necessárias
library(plumber)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(lubridate) # importante para floor_date e manipulação de datas
library(tibble)
library(httr)

# --------------------
# Função de ETL
# --------------------
api_etl <- function(agendamento) {
  message("=== Iniciando ETL do CRM ===")
  
  # -------- Datas com fuso horário da API --------
  data_hoje_api <- Sys.time() - hours(3)   # Ajuste GMT-3
  data_hoje <- as.Date(data_hoje_api)
  start <- paste(data_hoje, "00:00:00")
  end <- paste(data_hoje, "23:59:00")
  
  # -------- Configuração da API --------
  full_url <- "https://api.leads2b.com/v2/calls"
  api_key <- Sys.getenv("API_KEY_V2")
  if (api_key == "") stop("API_KEY_V2 não definida no ambiente")
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  
  dados <- data.frame()
  cursor_atual <- NULL
  limite_por_pagina <- 100
  
  # -------- Paginação --------
  repeat {
    query_params <- list(start = start, end = end, limit = limite_por_pagina, cursor = cursor_atual)
    
    response <- tryCatch(
      GET(full_url, config = headers, query = query_params),
      error = function(e) stop("Erro ao consultar API do CRM: ", e$message)
    )
    
    if (status_code(response) != 200) stop(paste("Erro na requisição da API:", status_code(response)))
    
    dados_paginados <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Checar se há dados
    if (is.null(dados_paginados$data) || nrow(dados_paginados$data) == 0) {
      message("Não há dados para o dia ", data_hoje)
      break
    }
    
    dados <- bind_rows(dados, dados_paginados$data)
    
    next_cursor <- dados_paginados$next_cursor
    if (is.null(next_cursor) || next_cursor == "") {
      message("Paginação concluída. Total de registros únicos obtidos: ", nrow(dados))
      break
    }
    
    cursor_atual <- next_cursor
  }
  
  # Se não houver dados, retornar vazio
  if (nrow(dados) == 0) return(data.frame())
  
  dados <- distinct(dados, id_call, .keep_all = TRUE)
  
  # -------- Tratamento de datas --------
  if (!"start" %in% names(dados)) stop("Coluna 'start' não encontrada")
  dados$start <- as.Date(substr(dados$start, 1, 10))
  
  # -------- Ajuste de nomes e relevância --------
  if (!"user" %in% names(dados) || !"name" %in% names(dados$user)) {
    dados$user$name <- NA_character_
    message("Coluna 'user$name' não encontrada, preenchida com NA")
  }
  
  dados <- dados %>%
    mutate(
      name = case_when(
        user$name == "kelly.ewers" ~ "Kelly",
        user$name == "LDR" ~ "Matheus",
        user$name == "Gustavo Dias" ~ "Consultoria",
        TRUE ~ user$name
      ),
      Relevante = ifelse(!is.na(duration) & duration >= 60, 1, 0)
    )
  
  # -------- Agregação de desempenho --------
  desempenho <- dados %>%
    group_by(name) %>%
    summarise(
      ligacoes_totais = n(),
      ligacoes_relevantes = sum(Relevante, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -------- Agendamentos --------
  agendamentos_df <- enframe(agendamento, name = "responsavel", value = "agendamento") %>%
    mutate(agendamento = as.numeric(agendamento))
  
  desempenho <- desempenho %>%
    left_join(agendamentos_df, by = c("name" = "responsavel")) %>%
    mutate(agendamento = coalesce(agendamento, 0))
  
  # -------- Metas e indicadores --------
  metas_na <- c("Kelly", "Priscila Prado", "Matheus", "Consultoria")
  
  desempenho <- desempenho %>%
    mutate(
      meta_ligacoes = ifelse(name %in% metas_na, NA, 120),
      meta_ligacoes_relevantes = ifelse(name %in% metas_na, NA, 24),
      meta_agendamento = case_when(
        name %in% c("Kelly", "Priscila Prado", "Consultoria") ~ NA_real_,
        name == "Matheus" ~ 6/5,
        TRUE ~ 2
      ),
      atingimento_ligacoes = ifelse(!is.na(meta_ligacoes) & meta_ligacoes != 0, ligacoes_totais / meta_ligacoes, 0),
      atingimento_ligacoes_relevantes = ifelse(!is.na(meta_ligacoes_relevantes) & meta_ligacoes_relevantes != 0, ligacoes_relevantes / meta_ligacoes_relevantes, 0),
      atingimento_agendamento = ifelse(!is.na(meta_agendamento) & meta_agendamento != 0, agendamento / meta_agendamento, 0),
      ligacao_x_ligacao_relevante = ifelse(ligacoes_totais != 0, ligacoes_relevantes / ligacoes_totais, 0),
      ligacao_relevante_x_agendamento = ifelse(ligacoes_relevantes != 0, agendamento / ligacoes_relevantes, 0),
      ligacao_x_agendamento = ifelse(ligacoes_totais != 0, agendamento / ligacoes_totais, 0),
      data = data_hoje
    ) %>%
    arrange(desc(agendamento))
  
  # -------- Google Sheets --------
  sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
  
  dados_historicos_atualizados <- tryCatch({
    message("Lendo dados históricos da planilha...")
    dados_historicos <- read_sheet(ss = sheet_id, sheet = "Página1", col_types = "cnnnnnnnnnnnnc")
    
    if ("data" %in% names(dados_historicos)) {
      dados_historicos$data <- as.Date(dados_historicos$data)
      dados_filtrados <- filter(dados_historicos, data != data_hoje)
    } else {
      dados_filtrados <- data.frame()
    }
    
    bind_rows(dados_filtrados, desempenho)
    
  }, error = function(e){
    message("Falha ao ler dados históricos: ", e$message)
    desempenho
  })
  
  tryCatch({
    write_sheet(dados_historicos_atualizados, ss = sheet_id, sheet = "Página1")
    message("Dados salvos no Google Sheets com sucesso!")
  }, error = function(e){
    message("Erro ao salvar no Google Sheets: ", e$message)
  })
  
  message("=== ETL concluído ===")
  return(dados_historicos_atualizados)
}



# --------------------
# Função de ETL Semanal
# --------------------
desempenho_semana <- function(dados_semana) {
  # Ajusta metas especiais antes da multiplicação
  message("Ajustando as metas especiais...")
  dados_semana <- dados_semana %>%
    mutate(
      meta_ligacoes = case_when(
        responsavel %in% c("Kelly","Priscila Prado","Matheus","Consultoria") ~ NA_real_,
        TRUE ~ meta_ligacoes
      ),
      meta_ligacoes_relevantes = case_when(
        responsavel %in% c("Kelly","Priscila Prado","Matheus","Consultoria") ~ NA_real_,
        TRUE ~ meta_ligacoes_relevantes
      ),
      meta_agendamento = case_when(
        responsavel %in% c("Kelly","Priscila Prado","Consultoria") ~ NA_real_,
        responsavel == "Matheus" ~ 6/5,
        TRUE ~ meta_agendamento
      )
    ) %>%
    # Multiplica metas por 5
    mutate(across(c(meta_ligacoes, meta_ligacoes_relevantes, meta_agendamento), ~ .x * 5))
  
  # Função para resumir o desempenho por pessoa
  resumir <- function(pessoa) {
    df <- dados_semana %>%
      filter(responsavel == pessoa) %>%
      group_by(responsavel, meta_ligacoes, meta_ligacoes_relevantes, meta_agendamento) %>%
      summarise(
        ligacoes_totais = sum(ligacoes_totais, na.rm = TRUE),
        ligacoes_relevantes = sum(ligacoes_relevantes, na.rm = TRUE),
        agendamento = sum(agendamento, na.rm = TRUE),
        atingimento_ligacoes = sum(atingimento_ligacoes, na.rm = TRUE),
        atingimento_ligacoes_relevantes = sum(atingimento_ligacoes_relevantes, na.rm = TRUE),
        atingimento_agendamento = sum(atingimento_agendamento, na.rm = TRUE),
        ligacao_x_ligacao_relevante = ifelse(sum(ligacoes_totais, na.rm = TRUE) != 0,
                                             sum(ligacoes_relevantes, na.rm = TRUE) / sum(ligacoes_totais, na.rm = TRUE),
                                             NA_real_),
        ligacao_x_agendamento = ifelse(sum(ligacoes_totais, na.rm = TRUE) != 0,
                                       sum(agendamento, na.rm = TRUE) / sum(ligacoes_totais, na.rm = TRUE),
                                       NA_real_),
        ligacao_relevante_x_agendamento = ifelse(sum(ligacoes_relevantes, na.rm = TRUE) != 0,
                                                 sum(agendamento, na.rm = TRUE) / sum(ligacoes_relevantes, na.rm = TRUE),
                                                 NA_real_),
        .groups = "drop"
      )
    return(df)
  }
  
  # Lista de responsáveis
  responsaveis <- c("Bruna Azevedo","Emilin","Maria Luisa","Stela","Kelly","Priscila Prado",
                    "Consultoria","Matheus","Gabriela Moreira","Marcelo")
  
  # Aplica resumir a todos e combina
  message("Combinando os dados com novas metas semanais...")
  resultado <- bind_rows(lapply(responsaveis, resumir)) %>%
    arrange(desc(agendamento))
  
  return(resultado)
}


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
    cat("Filtrando os dados pelo dia", Sys.Date())
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
  dados_existentes <- tryCatch({
    googlesheets4::read_sheet("1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Agendamentos")
  }, error = function(e) {
    data.frame() # Se não existir ainda, cria um data.frame vazio
  })
  
  tryCatch({
    dados_agendamento <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
    dados_agendamento$data <- as.Date(dados_agendamento$data)
    
    # Validação simples
    if (is.null(dados_agendamento$responsavel) || dados_agendamento$responsavel == "") {
      res$status <- 400
      return(list(erro = "Campo 'responsavel' é obrigatório"))
    }
    
    # Criar novo registro (usando a data atual)
    novo_agendamento <- data.frame(
      responsavel = dados_agendamento$responsavel,
      empresa = dados_agendamento$empresa,
      data = Sys.Date(),
      data_adicao = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Adicionar ao dataframe existente
    dados_atualizados <- rbind(dados_existentes, novo_agendamento)
    
    # Escrever de volta na planilha
    googlesheets4::write_sheet(dados_atualizados, "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4", sheet = "Agendamentos")
    
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
