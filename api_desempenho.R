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
  full_url <- "https://api.leads2b.com/v2/calls"
  api_key <- Sys.getenv("API_KEY_V2")
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  dados <- data.frame()
  cursor_atual <- NULL
  limite_por_pagina <- 100
  
  data <- Sys.Date()
  start <- paste(data, "00:00:00")
  end <- paste(data, "23:59:00")
  
  while (TRUE) {
    query_params <- list(
      start = start,
      end = end,
      limit = limite_por_pagina,
      cursor = cursor_atual
    )
    
    response <- GET(full_url, config = headers, query = query_params)
    
    if (status_code(response) != 200) {
      message(paste("Erro na requisição. Status:", status_code(response)))
      break
    }
    
    dados_paginados <- fromJSON(content(response, "text", encoding = "UTF-8"))
    dados_da_pagina <- dados_paginados$data
    next_cursor <- dados_paginados$next_cursor
    
    if (!is.null(dados_da_pagina) && nrow(dados_da_pagina) > 0) {
      dados <- bind_rows(dados, dados_da_pagina)
    }
    
    if (is.null(next_cursor) || next_cursor == "") {
      message("Paginação concluída. Não há mais dados.")
      break
    }
    
    cursor_atual <- next_cursor
  }
  
  dados <- dados |>
    distinct(id_call, .keep_all = TRUE)
  
  message(paste("Total de registros únicos obtidos:", nrow(dados)))
  
  dados$start <- str_sub(dados$start, start = 1, end = 10)
  dados$start <- as.Date(dados$start)
  
  dados <- dados |>
    mutate(
      name = case_when(
        user$name == "kelly.ewers" ~ "Kelly",
        user$name == "LDR" ~ "Matheus",
        user$name == "Gustavo Dias" ~ "Consultoria",
        TRUE ~ user$name
      )
    )
  
  dados$Relevante <- ifelse(dados$duration >= 60, 1, 0)
  
  desempenho <- dados |>
    group_by(user$name) |>
    summarise(
      n = n(),
      n_relevante = sum(Relevante == 1, na.rm = TRUE)
    ) |>
    ungroup()
  
  names(desempenho) <- c("responsavel", "ligacoes_totais", "ligacoes_relevantes")
  
  # Recebe a lista nomeada com os agendamentos
  # Exemplo de entrada: list("Bruna" = 1, "Stela" = 1, "Matheus" = 0)
  agendamento <- agendamento
  
  # A nova lógica para lidar com a lista de agendamentos
  agendamentos_df <- tibble::enframe(agendamento, name = "responsavel", value = "agendamento") |> 
    mutate(agendamento = as.numeric(agendamento))
  
  desempenho <- desempenho |>
    left_join(agendamentos_df, by = "responsavel") |>
    mutate(agendamento = coalesce(agendamento, 0))
  
  desempenho$meta_ligacoes <- ifelse(desempenho$responsavel %in% c("Kelly", "Priscila Prado", "Matheus", "Consultoria"),
                                     NA,
                                     rep(120, dim(desempenho)[[1]]))
  
  desempenho$meta_ligacoes_relevantes <- ifelse(desempenho$responsavel %in% c("Kelly", "Priscila Prado", "Matheus", "Consultoria"),
                                                NA,
                                                rep(24, dim(desempenho)[[1]]))
  
  desempenho$meta_agendamento <- ifelse(desempenho$responsavel %in% c("Kelly", "Priscila Prado", "Consultoria"),
                                        NA,
                                        rep(2, dim(desempenho)[[1]]))
  
  desempenho$meta_agendamento <- ifelse(desempenho$responsavel == "Matheus", 6/5,
                                        desempenho$meta_agendamento)
  
  desempenho$atingimento_ligacoes <- ifelse(desempenho$meta_ligacoes != 0,
                                            desempenho$ligacoes_totais / desempenho$meta_ligacoes,
                                            0)
  
  desempenho$atingimento_ligacoes_relevantes <- ifelse(desempenho$meta_ligacoes_relevantes != 0,
                                                       desempenho$ligacoes_relevantes / desempenho$meta_ligacoes_relevantes,
                                                       0)
  
  desempenho$atingimento_agendamento <- ifelse(desempenho$meta_agendamento != 0,
                                               desempenho$agendamento / desempenho$meta_agendamento,
                                               0)
  
  desempenho$ligacao_x_ligacao_relevante <- ifelse(desempenho$ligacoes_totais != 0,
                                                   desempenho$ligacoes_relevantes / desempenho$ligacoes_totais,
                                                   0)
  
  desempenho$ligacao_relevante_x_agendamento <- ifelse(desempenho$ligacoes_relevantes != 0,
                                                       desempenho$agendamento / desempenho$ligacoes_relevantes,
                                                       0)
  
  desempenho$ligacao_x_agendamento <- ifelse(desempenho$ligacoes_totais != 0,
                                             desempenho$agendamento / desempenho$ligacoes_totais,
                                             0)
  
  desempenho$data <- dados$start[1]
  desempenho <- desempenho[order(desempenho$agendamento, decreasing = T), ]
  
  # --- TRECHO CORRIGIDO PARA GOOGLE SHEETS ---
  
  sheet_id <- "1crNO9HynYJJnHv5PpnzECokEDeatnTNMbWQdtogA1e4"
  data_hoje <- desempenho$data[1]
  
  message(paste("Processando dados para a data:", data_hoje))
  
  # Tenta ler a planilha existente da aba "Página1"
  dados_historicos_atualizados <- tryCatch({
    message("Tentando ler dados históricos da aba 'Página1'...")
    # Mudança: col_types termina com 'c' (character) em vez de 'd' (double) para a coluna data
    dados_historicos <- read_sheet(ss = sheet_id, sheet = "Página1", col_types = "cnnnnnnnnnnnnc")
    
    message(paste("Dados históricos lidos com sucesso. Total de linhas:", nrow(dados_historicos)))
    
    # Verifica e converte a coluna 'data'
    if ("data" %in% names(dados_historicos)) {
      # Primeiro, verifica quantos NAs existem antes da conversão
      na_count_before <- sum(is.na(dados_historicos$data))
      message(paste("Valores NA na coluna data (antes da conversão):", na_count_before, "de", nrow(dados_historicos)))
      
      # Mostra algumas amostras das datas como estão
      message(paste("Primeiras 5 datas como estão:", paste(head(dados_historicos$data, 5), collapse = ", ")))
      
      # Converte de texto "YYYY-mm-dd" para Date
      dados_historicos$data <- as.Date(dados_historicos$data, format = "%Y-%m-%d")
      
      # Verifica quantos NAs existem após a conversão
      na_count_after <- sum(is.na(dados_historicos$data))
      message(paste("Valores NA na coluna data (após conversão):", na_count_after, "de", nrow(dados_historicos)))
      
      # Remove linhas onde a data é NA (dados inválidos)
      dados_validos <- dados_historicos |>
        filter(!is.na(data))
      
      message(paste("Após remover dados com data NA, restaram:", nrow(dados_validos), "linhas válidas"))
      
      if (nrow(dados_validos) > 0) {
        datas_unicas <- unique(dados_validos$data)
        message(paste("Datas únicas nos dados válidos:", paste(datas_unicas, collapse = ", ")))
        message(paste("Range de datas: de", min(datas_unicas), "até", max(datas_unicas)))
        
        # Remove dados da data atual se já existirem (para evitar duplicação)
        dados_filtrados <- dados_validos |>
          filter(data != data_hoje)
        
        message(paste("Após filtrar data atual (", data_hoje, "), restaram:", nrow(dados_filtrados), "linhas históricas"))
      } else {
        message("Nenhum dado histórico válido encontrado.")
        dados_filtrados <- data.frame()
      }
      
    } else {
      message("Coluna 'data' não encontrada nos dados históricos.")
      dados_filtrados <- data.frame()
    }
    
    message(paste("Adicionando", nrow(desempenho), "novas linhas"))
    
    # Combina dados históricos (sem a data atual) + dados novos
    if (nrow(dados_filtrados) > 0) {
      resultado <- bind_rows(dados_filtrados, desempenho)
      message(paste("Total final:", nrow(resultado), "linhas (", nrow(dados_filtrados), "históricas +", nrow(desempenho), "novas)"))
      resultado
    } else {
      message("Usando apenas dados do dia atual (sem dados históricos válidos)")
      desempenho
    }
    
  }, error = function(e) {
    message(paste("Erro ao ler dados históricos:", e$message))
    message("Usando apenas dados do dia atual")
    desempenho
  })
  
  message(paste("Total de linhas para salvar:", nrow(dados_historicos_atualizados)))
  
  # Escreve os dados atualizados na aba "Sheet1"
  tryCatch({
    write_sheet(data = dados_historicos_atualizados, ss = sheet_id, sheet = "Página1")
    message("Dados salvos com sucesso na aba 'Página1' do Google Sheets!")
  }, error = function(e) {
    message(paste("Erro ao salvar no Google Sheets:", e$message))
    # Tenta criar a aba se não existir
    tryCatch({
      sheet_add(ss = sheet_id, sheet = "Sheet1")
      write_sheet(data = dados_historicos_atualizados, ss = sheet_id, sheet = "Página1")
      message("Aba 'Página1' criada e dados salvos com sucesso!")
    }, error = function(e2) {
      message(paste("Erro ao criar aba e salvar:", e2$message))
    })
  })
  
  return(dados_historicos_atualizados)
}


# --------------------
# Função de ETL Semanal
# --------------------
desempenho_semana <- function(desempenho_sem) {
  desempenho_sem$meta_ligacoes <- desempenho_sem$meta_ligacoes*5
  desempenho_sem$meta_ligacoes_relevantes <- desempenho_sem$meta_ligacoes_relevantes*5
  desempenho_sem$meta_agendamento <- desempenho_sem$meta_agendamento*5
  
  desempenho_sem$atingimento_ligacoes <- ifelse(desempenho_sem$meta_ligacoes != 0, 
                                                desempenho_sem$ligacoes_totais/desempenho_sem$meta_ligacoes,
                                                NA)
  desempenho_sem$atingimento_ligacoes_relevantes <- ifelse(desempenho_sem$meta_ligacoes_relevantes != 0,
                                                           desempenho_sem$ligacoes_relevantes/desempenho_sem$meta_ligacoes_relevantes,
                                                           NA)
  desempenho_sem$atingimento_agendamento <- ifelse(desempenho_sem$meta_agendamento != 0,
                                                   desempenho_sem$agendamento/desempenho_sem$meta_agendamento,
                                                   NA)
  desempenho_sem$ligacao_x_ligacao_relevante <- ifelse(desempenho_sem$ligacoes_totais != 0,
                                                       desempenho_sem$ligacoes_relevantes/desempenho_sem$ligacoes_totais,
                                                       NA)
  desempenho_sem$ligacao_relevante_x_agendamento <- ifelse(desempenho_sem$ligacoes_relevantes != 0,
                                                           desempenho_sem$agendamento/desempenho_sem$ligacoes_relevantes,
                                                           NA)
  desempenho_sem$ligacao_x_agendamento <- ifelse(desempenho_sem$ligacoes_totais != 0,
                                                 desempenho_sem$agendamento/desempenho_sem$ligacoes_totais,
                                                 NA)
  soma_sem <- function(pessoa) {
    desempenho_sem |>
      select(!data) |> 
      filter(responsavel == pessoa) |> 
      group_by(
        responsavel,               
        meta_ligacoes,             
        meta_ligacoes_relevantes,  
        meta_agendamento
      ) |>
      summarise(
        # As colunas abaixo serão somadas para todas as linhas dentro do grupo
        ligacoes_totais = sum(ligacoes_totais, na.rm = TRUE),
        ligacoes_relevantes = sum(ligacoes_relevantes, na.rm = TRUE),
        agendamento = sum(agendamento, na.rm = TRUE),
        atingimento_ligacoes = sum(atingimento_ligacoes, na.rm = F),
        atingimento_ligacoes_relevantes = sum(atingimento_ligacoes_relevantes, na.rm = F),
        atingimento_agendamento = sum(atingimento_agendamento, na.rm = F),
        ligacao_x_agendamento = ifelse(ligacoes_totais != 0, sum(agendamento, na.rm =T)/sum(ligacoes_totais, na.rm = T),NA),
        ligacao_x_ligacao_relevante = ifelse(ligacoes_totais != 0, sum(ligacoes_relevantes, na.rm =T)/sum(ligacoes_totais, na.rm =T),NA),
        ligacao_relevante_x_agendamento = ifelse(ligacoes_relevantes != 0, sum(agendamento, na.rm = T)/sum(ligacoes_relevantes, na.rm = T),NA),
        .groups = 'drop' # Remove o agrupamento do resultado final
      )
    
  }
  
  soma_desempenho_sem <- rbind(soma_sem("Bruna Azevedo"),
                               soma_sem("Emilin"),
                               soma_sem("Maria Luisa"),
                               soma_sem("Stela"),
                               soma_sem("Kelly"),
                               soma_sem("Priscila Prado"),
                               soma_sem("Consultoria"),
                               soma_sem("Matheus"),
                               soma_sem("Gabriela Moreira"),
                               soma_sem("Marcelo"))
  
  soma_desempenho_sem <- soma_desempenho_sem[order(soma_desempenho_sem$agendamento, decreasing = T),]
  
  
  return(soma_desempenho_sem)
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
    googlesheets4::read_sheet(sheet_id, sheet = "Agendamentos")
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
