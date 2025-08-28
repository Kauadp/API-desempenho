# Carrega bibliotecas necessárias
library(plumber)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(lubridate) # importante para floor_date e manipulação de datas
library(tibble)
library(httr)


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

# --------------------
# Configuração CORS
# --------------------

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, Accept, Origin, X-Requested-With")
  res$setHeader("Access-Control-Max-Age", "86400")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @options /*
function(req, res) {
  res$status <- 200
  return(list())
}


#* @apiTitle API de Desempenho de Vendas
#* @apiDescription Esta API atualiza e retorna os dados de desempenho de vendas com autenticação Google Sheets.

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
  
  # Se não houver dados, retornar vazio mas estruturado
  if (nrow(dados) == 0) {
    message("Não há dados para processar - retornando estrutura vazia")
    dados_vazios <- data.frame(
      responsavel = character(0),
      ligacoes_totais = numeric(0),
      ligacoes_relevantes = numeric(0),
      agendamento = numeric(0),
      meta_ligacoes = numeric(0),
      meta_ligacoes_relevantes = numeric(0),
      meta_agendamento = numeric(0),
      atingimento_ligacoes = numeric(0),
      atingimento_ligacoes_relevantes = numeric(0),
      atingimento_agendamento = numeric(0),
      ligacao_x_ligacao_relevante = numeric(0),
      ligacao_relevante_x_agendamento = numeric(0),
      ligacao_x_agendamento = numeric(0),
      data = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
    return(dados_vazios)
  }
  
  dados <- distinct(dados, id_call, .keep_all = TRUE)
  
  # -------- Tratamento de datas --------
  message("Tratando as datas...")
  if (!"start" %in% names(dados)) stop("Coluna 'start' não encontrada")
  dados$start <- as.Date(substr(dados$start, 1, 10))
  
  # -------- Ajuste de nomes e relevância --------
  message("Ajustando os nomes...")
  
  # Verificar se há coluna duration
  if (!"duration" %in% names(dados)) {
    dados$duration <- NA_real_
  }
  
  # Verificar e tratar a estrutura user
  user_name <- rep(NA_character_, nrow(dados))
  
  if ("user" %in% names(dados) && !is.null(dados$user) && !all(is.na(dados$user))) {
    tryCatch({
      if (is.data.frame(dados$user) && "name" %in% names(dados$user)) {
        user_name <- ifelse(is.na(dados$user$name), NA_character_, as.character(dados$user$name))
      } else if (is.list(dados$user)) {
        # Verificar se todos os elementos da lista têm 'name'
        has_name <- sapply(dados$user, function(x) {
          if (is.null(x) || is.na(x)) return(FALSE)
          if (is.list(x)) return("name" %in% names(x))
          return(FALSE)
        })
        
        if (any(has_name)) {
          user_name <- sapply(dados$user, function(x) {
            if (is.null(x) || is.na(x)) return(NA_character_)
            if (is.list(x) && "name" %in% names(x)) {
              return(ifelse(is.null(x$name) || is.na(x$name), NA_character_, as.character(x$name)))
            }
            return(NA_character_)
          })
        }
      }
    }, error = function(e) {
      message("Erro ao extrair user name: ", e$message)
      user_name <<- rep(NA_character_, nrow(dados))
    })
  }
  
  dados <- dados %>%
    mutate(
      responsavel = case_when(
        !is.na(user_name) & user_name == "kelly.ewers" ~ "Kelly",
        !is.na(user_name) & user_name == "LDR" ~ "Matheus", 
        !is.na(user_name) & user_name == "Gustavo Dias" ~ "Consultoria",
        !is.na(user_name) & user_name != "" ~ as.character(user_name),
        TRUE ~ "Desconhecido"
      ),
      # Tratar duration de forma mais segura
      duration_numeric = case_when(
        is.na(duration) ~ 0,
        is.numeric(duration) ~ duration,
        is.character(duration) ~ suppressWarnings(as.numeric(duration)),
        TRUE ~ 0
      ),
      duration_numeric = ifelse(is.na(duration_numeric), 0, duration_numeric),
      Relevante = ifelse(duration_numeric >= 60, 1, 0)
    )
  
  # -------- Agregação de desempenho --------
  message("Agregando o desempenho...")
  desempenho <- dados %>%
    group_by(responsavel) %>%
    summarise(
      ligacoes_totais = n(),
      ligacoes_relevantes = sum(Relevante, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -------- Agendamentos (CORRIGIDO) --------
  message("Configurando os agendamentos...")
  message("Estrutura dos dados de agendamento recebidos:")
  message(paste(capture.output(str(agendamento)), collapse = "\n"))
  
  # Tratamento mais robusto dos dados de agendamento
  if (is.null(agendamento) || length(agendamento) == 0) {
    message("Nenhum agendamento fornecido, usando valores padrão")
    agendamentos_df <- data.frame(
      responsavel = character(0),
      agendamento = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    message("Processando agendamentos...")
    
    # Verificar se é uma lista ou vetor nomeado
    if (is.list(agendamento)) {
      message("Agendamento é uma lista")
      # Converter lista para data.frame
      nomes_responsaveis <- names(agendamento)
      valores_agendamentos <- as.numeric(unlist(agendamento))
      
      if (is.null(nomes_responsaveis)) {
        message("ERRO: Lista de agendamentos sem nomes")
        agendamentos_df <- data.frame(responsavel = character(0), agendamento = numeric(0), stringsAsFactors = FALSE)
      } else {
        agendamentos_df <- data.frame(
          responsavel = nomes_responsaveis,
          agendamento = valores_agendamentos,
          stringsAsFactors = FALSE
        )
        message("Agendamentos convertidos com sucesso:")
        message(paste(capture.output(print(agendamentos_df)), collapse = "\n"))
      }
    } else if (is.vector(agendamento) && !is.null(names(agendamento))) {
      message("Agendamento é um vetor nomeado")
      # Vetor nomeado
      agendamentos_df <- data.frame(
        responsavel = names(agendamento),
        agendamento = as.numeric(agendamento),
        stringsAsFactors = FALSE
      )
    } else {
      message("Tentando usar enframe como fallback...")
      # Tentar usar enframe como fallback
      tryCatch({
        agendamentos_df <- tibble::enframe(agendamento, name = "responsavel", value = "agendamento") %>%
          mutate(agendamento = as.numeric(agendamento)) %>%
          as.data.frame()
      }, error = function(e) {
        message("Erro ao processar agendamentos com enframe: ", e$message)
        message("Tipo dos dados de agendamento: ", class(agendamento))
        message("Estrutura dos dados de agendamento: ", paste(capture.output(str(agendamento)), collapse = "\n"))
        # Usar data.frame vazio como fallback
        agendamentos_df <- data.frame(
          responsavel = character(0),
          agendamento = numeric(0),
          stringsAsFactors = FALSE
        )
      })
    }
  }
  
  message("DataFrame de agendamentos final:")
  message(paste(capture.output(print(agendamentos_df)), collapse = "\n"))
  
  # Fazer o join com os dados de desempenho
  message("Realizando join entre desempenho e agendamentos...")
  message("Colunas em desempenho antes do join: ", paste(names(desempenho), collapse = ", "))
  
  desempenho <- desempenho %>%
    left_join(agendamentos_df, by = "responsavel") %>%
    mutate(agendamento = coalesce(agendamento, 0))
  
  message("Join realizado com sucesso. Colunas após join: ", paste(names(desempenho), collapse = ", "))
  message("Dados após join:")
  message(paste(capture.output(str(desempenho)), collapse = "\n"))
  
  # -------- Metas e indicadores --------
  message("Ajustando as metas...")
  metas_na <- c("Kelly", "Priscila Prado", "Matheus", "Consultoria")
  
  desempenho <- desempenho %>%
    mutate(
      meta_ligacoes = case_when(
        responsavel %in% metas_na ~ NA_real_,
        TRUE ~ 120
      ),
      meta_ligacoes_relevantes = case_when(
        responsavel %in% metas_na ~ NA_real_,
        TRUE ~ 24
      ),
      meta_agendamento = case_when(
        responsavel %in% c("Kelly", "Priscila Prado", "Consultoria") ~ NA_real_,
        responsavel == "Matheus" ~ 6/5,
        TRUE ~ 2
      ),
      # Calcular atingimentos de forma mais segura
      atingimento_ligacoes = case_when(
        is.na(meta_ligacoes) | meta_ligacoes == 0 ~ 0,
        TRUE ~ ligacoes_totais / meta_ligacoes
      ),
      atingimento_ligacoes_relevantes = case_when(
        is.na(meta_ligacoes_relevantes) | meta_ligacoes_relevantes == 0 ~ 0,
        TRUE ~ ligacoes_relevantes / meta_ligacoes_relevantes
      ),
      atingimento_agendamento = case_when(
        is.na(meta_agendamento) | meta_agendamento == 0 ~ 0,
        TRUE ~ agendamento / meta_agendamento
      ),
      # Calcular proporções de forma mais segura
      ligacao_x_ligacao_relevante = case_when(
        ligacoes_totais == 0 ~ 0,
        TRUE ~ ligacoes_relevantes / ligacoes_totais
      ),
      ligacao_relevante_x_agendamento = case_when(
        ligacoes_relevantes == 0 ~ 0,
        TRUE ~ agendamento / ligacoes_relevantes
      ),
      ligacao_x_agendamento = case_when(
        ligacoes_totais == 0 ~ 0,
        TRUE ~ agendamento / ligacoes_totais
      ),
      data = data_hoje
    ) %>%
    arrange(desc(agendamento))
  
  # -------- Google Sheets --------
  message("Lendo os dados históricos da base de dados...")
  sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
  
  dados_historicos_atualizados <- tryCatch({
    message("Lendo dados históricos da planilha...")
    dados_historicos <- read_sheet(ss = sheet_id, sheet = "Página1", col_types = "cnnnnnnnnnnnnc")
    
    if ("data" %in% names(dados_historicos) && nrow(dados_historicos) > 0) {
      dados_historicos$data <- as.Date(dados_historicos$data)
      dados_filtrados <- filter(dados_historicos, data != data_hoje)
    } else {
      dados_filtrados <- data.frame()
    }
    
    # Se desempenho tem dados, fazer bind_rows
    if (nrow(desempenho) > 0) {
      bind_rows(dados_filtrados, desempenho)
    } else {
      # Se não há desempenho novo, retornar apenas dados históricos
      dados_filtrados
    }
    
  }, error = function(e){
    message("Falha ao ler dados históricos: ", e$message)
    desempenho
  })
  
  # Só tentar salvar se há dados para salvar
  if (nrow(dados_historicos_atualizados) > 0) {
    tryCatch({
      write_sheet(dados_historicos_atualizados, ss = sheet_id, sheet = "Página1")
      message("Dados salvos no Google Sheets com sucesso!")
    }, error = function(e){
      message("Erro ao salvar no Google Sheets: ", e$message)
    })
  } else {
    message("Nenhum dado para salvar no Google Sheets")
  }
  
  message("=== ETL concluído ===")
  return(dados_historicos_atualizados)
}


# --------------------
# Função de ETL Semanal
# --------------------
desempenho_semana <- function(dados_semana) {
  message("=== Iniciando processamento semanal ===")
  message("Dados recebidos para processamento semanal:")
  message(paste(capture.output(str(dados_semana)), collapse = "\n"))
  
  # Verificar se a coluna 'responsavel' existe nos dados (CORRIGIDO)
  if (!"responsavel" %in% names(dados_semana)) {
    message("AVISO: Coluna 'responsavel' não encontrada nos dados. Colunas disponíveis: ", paste(names(dados_semana), collapse = ", "))
    return(data.frame())
  }
  
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
    message("Processando pessoa: ", pessoa)
    df <- dados_semana %>%
      filter(responsavel == pessoa) %>%  # CORRIGIDO: usar 'responsavel'
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
  
  message("Processamento semanal concluído. Registros resultantes: ", nrow(resultado))
  return(resultado)
}


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
# Endpoint POST desempenho-diario (melhorado)
# --------------------
#* @post /desempenho-diario
#* @serializer unboxedJSON
function(req, res) {
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") {
      parsed <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
      message("Dados recebidos no endpoint diário: ", jsonlite::toJSON(parsed, auto_unbox = TRUE))
      parsed
    } else {
      message("Nenhum dado recebido no POST body")
      list()
    }
  }, error = function(e) {
    message("Erro ao processar POST body: ", e$message)
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  tryCatch({
    # Verificar se agendamentos está presente
    agendamentos <- if ("agendamentos" %in% names(request_data)) {
      request_data$agendamentos
    } else {
      message("Campo 'agendamentos' não encontrado, usando lista vazia")
      list()
    }
    
    message("Agendamentos processados: ", jsonlite::toJSON(agendamentos, auto_unbox = TRUE))
    
    dados_completos <- api_etl(agendamentos)
    
    # Verificar se dados_completos tem conteúdo válido
    if (is.null(dados_completos) || nrow(dados_completos) == 0) {
      message("Nenhum dado retornado pelo ETL para hoje")
      return(list(
        status = "sucesso",
        message = "Não há ligações para hoje",
        data = as.character(Sys.Date()),
        dados = list()
      ))
    }
    
    dados_completos$data <- as.Date(dados_completos$data)
    
    message("Filtrando os dados pelo dia ", Sys.Date())
    desempenho_diario <- dados_completos %>%
      filter(data == Sys.Date())
    
    if (nrow(desempenho_diario) == 0) {
      return(list(
        status = "sucesso", 
        message = "Não há ligações para hoje",
        data = as.character(Sys.Date()),
        dados = list()
      ))
    }
    
    return(list(
      status = "sucesso",
      message = paste("Encontrados", nrow(desempenho_diario), "registros"),
      data = as.character(Sys.Date()),
      dados = desempenho_diario
    ))
    
  }, error = function(e) {
    message("Erro no processamento ETL diário: ", e$message)
    res$status <- 500
    return(list(erro = paste("Erro interno:", e$message)))
  })
}

# --------------------
# Endpoint POST desempenho-semanal (melhorado)
# --------------------
#* @post /desempenho-semanal
#* @serializer unboxedJSON
function(req, res) {
  request_data <- tryCatch({
    if (!is.null(req$postBody) && req$postBody != "") {
      parsed <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
      message("Dados recebidos no endpoint semanal: ", jsonlite::toJSON(parsed, auto_unbox = TRUE))
      parsed
    } else {
      message("Nenhum dado recebido no POST body")
      list()
    }
  }, error = function(e) {
    message("Erro ao processar POST body: ", e$message)
    res$status <- 400
    return(list(erro = paste("Erro ao processar dados:", e$message)))
  })
  
  tryCatch({
    # Verificar se agendamentos está presente
    agendamentos <- if ("agendamentos" %in% names(request_data)) {
      request_data$agendamentos
    } else {
      message("Campo 'agendamentos' não encontrado, usando lista vazia")
      list()
    }
    
    message("Agendamentos processados: ", jsonlite::toJSON(agendamentos, auto_unbox = TRUE))
    
    dados_completos <- api_etl(agendamentos)
    
    # Verificar se dados_completos tem conteúdo válido
    if (is.null(dados_completos) || nrow(dados_completos) == 0) {
      message("Nenhum dado retornado pelo ETL para esta semana")
      inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
      return(list(
        status = "sucesso",
        message = "Não há ligações para esta semana",
        periodo = paste("desde", as.character(inicio_da_semana)),
        dados = list()
      ))
    }
    
    dados_completos$data <- as.Date(dados_completos$data)
    
    inicio_da_semana <- floor_date(Sys.Date(), "week", week_start = 1)
    dados_semana <- dados_completos %>%
      filter(data >= inicio_da_semana)
    
    if (nrow(dados_semana) == 0) {
      return(list(
        status = "sucesso",
        message = "Não há ligações para esta semana",
        periodo = paste("desde", as.character(inicio_da_semana)),
        dados = list()
      ))
    }
    
    desempenho_final <- desempenho_semana(dados_semana)
    
    return(list(
      status = "sucesso",
      message = paste("Processados", nrow(dados_semana), "registros da semana"),
      periodo = paste("desde", as.character(inicio_da_semana)),
      dados = desempenho_final
    ))
    
  }, error = function(e) {
    message("Erro no processamento ETL semanal: ", e$message)
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