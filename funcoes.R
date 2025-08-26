carregar_pacote <- function(pacotes) {
  for (pacote in pacotes) {
    if (!require(pacote, character.only = TRUE)) {  
      install.packages(pacote, dependencies = TRUE)
    }
    library(pacote, character.only = TRUE)
  }
}

pacotes <- c("ggplot2", "MASS", "ggthemes", "stringr", "lubridate", "tidyverse",
             "kableExtra", "htmltools", "readr", "httr", "jsonlite",
             "gridExtra", "broom", "purrr", "stats", "htmltools", "usethis",
             "randomForest", "car", "xgboost", "caTools", "pscl", "VGAM", "Microsoft365R", "tsibble", "forecast", "zoo", "RSelenium", "caret", "mice", "googlesheets4", "tibble", "plumber")

carregar_pacote(pacotes)


# ---- Gráfico por Meta ----

grafico_meta <- function(dados, pos_var1, pos_var2, x, titulo) {
  dados |>
    ggplot(aes(y = reorder(responsavel, dados[[pos_var1]]))) +
    
    # 2. Primeira camada: Barras de Meta (as "fantasmas")
    geom_col(aes(x = dados[[pos_var2]]),            # Mapeia o valor da Meta para o eixo X
             fill = "lightgray",       # Cor cinza clara para as barras de meta
             alpha = 0.6,              # Transparência (0.6 significa 60% opaco)
             width = 0.7) +            # Largura ligeiramente maior que as barras de dados reais
    
    # 3. Segunda camada: Barras de Dados Reais
    geom_col(aes(x = dados[[pos_var1]], fill = responsavel), # Mapeia Total.de.Ligacoes para X e Responsavel para cor
             width = 0.6) +           # Largura ligeiramente menor que as barras de meta
    
    # 4. Rótulos de texto para os valores Reais (Total.de.Ligacoes)
    geom_text(aes(x = dados[[pos_var1]], label = dados[[pos_var1]]),
              hjust = -0.1,           # Posição ligeiramente à direita da barra
              color = "black",        # Cor do texto
              size = 3.5) +           # Tamanho da fonte
    
    # 5. Rótulos de texto para os valores de Meta
    geom_text(aes(x = dados[[pos_var2]], label = paste0("Meta: ", dados[[pos_var2]])), # Concatena "Meta: " com o valor
              hjust = 1,            # Posição ligeiramente à esquerda da barra de meta (dentro)
              color = "darkgray",     # Cor do texto
              size = 5) +           # Tamanho da fonte
    
    # Aplica o tema
    theme_fivethirtyeight() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.title.x = element_text(size = 7, face = "bold"),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 10, face = "bold")
    ) +
    labs(
      title = titulo,
      x = x, # O eixo agora representa ambos os valores
      y = "Responsável"
    ) +
    # 6. Ajusta a escala do eixo X para acomodar Meta e os rótulos
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)), # 15% de expansão à direita
                       limits = c(0, max(dados[[pos_var1]], dados[[pos_var2]]) * 1.2)) +  # Garante que o limite máximo inclua meta e rótulos
    guides(fill = "none")
}

# ---- Gráfico por Proporção ----

grafico_prop <- function(dados, pos_var, x, titulo) {
  dados %>%
    ggplot(aes(x = dados[[pos_var]], # Eixo X: a proporção
               y = reorder(responsavel, dados[[pos_var]]), # Eixo Y: responsável (ordenado pela proporção)
               fill = responsavel)) + # Cor da barra pelo responsável
    geom_col() + # geom_col é usado para valores já calculados (proporção)
    geom_text(
      aes(label = scales::percent(dados[[pos_var]], accuracy = 0.1)), # Rótulo em formato de porcentagem
      hjust = -0.1,                   # Posição ligeiramente à direita da barra
      color = "black",                # Cor do texto
      size = 3.5                      # Tamanho da fonte do texto
    ) +
    scale_x_continuous(
      labels = scales::percent,       # Formata os ticks do eixo X como porcentagens
      expand = expansion(mult = c(0, 0.15)) # Adiciona 15% de espaço à direita para os rótulos de texto
    ) +
    theme_fivethirtyeight() + # Aplica o tema
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.title.x = element_text(size = 7, face = "bold"), # Título do eixo X visível e em negrito
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 10, face = "bold")
    ) +
    labs(
      title = titulo,
      x = x,
      y = "Responsável"
    ) +
    guides(fill = "none")
}


# ---- Relação entre Variáveis ---- 

grafico_disp <- function(dados, pos_var1, pos_var2, x, y, titulo) {
  dados |> 
    ggplot() +
    geom_point(aes(y = dados[[pos_var2]], x =dados[[pos_var1]], color = responsavel)) +
    theme_fivethirtyeight() +
    theme(axis.title.x = element_text(),
          axis.title.y = element_text(),
          plot.title = element_text(size = 10, face = "bold")) +
    labs(
      title = titulo,
      y = y,
      x = x,
      color = "Responsável"
    ) +
    geom_smooth(aes(y = dados[[pos_var2]], x = dados[[pos_var1]]), method = "lm")
}

desempenho_hoje <- function(dados, data, agendamento) {
  
  dados$Início <- str_sub(dados$Início, start = 1, end = 10)
  dados$Início <- as.Date(dados$Início, format = "%d/%m/%Y")
  dados$Início...data <- as.Date(dados$Início...data, format = "%d/%m/%Y")
  dados$Duração <- hms(dados$Duração)
  
  dados <- dados |>
    mutate(
      Nome.do.usuário = ifelse(Nome.do.usuário == "kelly.ewers", "Kelly", Nome.do.usuário)
    )
  
  dados <- dados |> 
    mutate(
      Nome.do.usuário = ifelse(Nome.do.usuário == "LDR", "Matheus", Nome.do.usuário)
    )
  
  dados <- dados |>
    mutate(
      Nome.do.usuário = ifelse(Nome.do.usuário == "Gustavo Dias", "Consultoria", Nome.do.usuário)
    )
  
  dados <- dados |>
    filter(Tipo == "Ligação")
  
  dados$Relevante <- ifelse(dados$Duração >= 60, 1, 0)
  dados_hoje <- dados |> 
    filter(Início == data)
  
  desempenho <- dados_hoje |>
    group_by(Nome.do.usuário) |>
    summarise(
      n = n(), # Calcula o número TOTAL de atividades para cada usuário (será a sua coluna 'n')
      n_relevante = sum(Relevante == 1, na.rm = TRUE) # Soma 1 para cada vez que Relevante for 1,
      # e 0 caso contrário. Se um usuário não tiver
      # nenhum Relevante == 1, o sum será 0.
      # na.rm = TRUE é uma boa prática se Relevante puder ter NA
    ) |>
    ungroup()
  
  names(desempenho) <- c("responsavel",
                         "ligacoes_totais",
                         "ligacoes_relevantes")
  desempenho$agendamento <- agendamento
  desempenho$meta_ligacoes <- ifelse(desempenho$responsavel == "Kelly" |
                                     desempenho$responsavel == "Priscila Prado" | desempenho$responsavel == "Matheus" | desempenho$responsavel == "Consultoria", 
                                   NA,
                                   rep(120, dim(desempenho)[[1]]))
  desempenho$meta_ligacoes_relevantes <- ifelse(desempenho$responsavel == "Kelly" |
                                       desempenho$responsavel == "Priscila Prado" | desempenho$responsavel == "Matheus" | desempenho$responsavel == "Consultoria", 
                                     NA,
                                     rep(24, dim(desempenho)[[1]]))
  desempenho$meta_agendamento <- ifelse(desempenho$responsavel == "Kelly" |
                                       desempenho$responsavel == "Priscila Prado" | desempenho$responsavel == "Consultoria", 
                                     NA,
                                     rep(2, dim(desempenho)[[1]]))
  desempenho$meta_agendamento <- ifelse(desempenho$responsavel == "Matheus", 6/5,
                                        desempenho$meta_agendamento)
  desempenho$atingimento_ligacoes <- ifelse(desempenho$meta_ligacoes != 0,
                                            desempenho$ligacoes_totais/desempenho$meta_ligacoes,
                                            0)
  desempenho$atingimento_ligacoes_relevantes <- ifelse(desempenho$meta_ligacoes_relevantes !=0,
                                                       desempenho$ligacoes_relevantes/desempenho$meta_ligacoes_relevantes,
                                                       0)
  
  desempenho$atingimento_agendamento <- ifelse(desempenho$meta_agendamento != 0,
                                               desempenho$agendamento/desempenho$meta_agendamento,
                                               0)
  desempenho$ligacao_x_ligacao_relevante <- ifelse(desempenho$ligacoes_totais != 0,
                                                   desempenho$ligacoes_relevantes/desempenho$ligacoes_totais,
                                                   0)
  desempenho$ligacao_relevante_x_agendamento <- ifelse(desempenho$ligacoes_relevantes != 0,
                                                       desempenho$agendamento/desempenho$ligacoes_relevantes,
                                                       0)
  desempenho$ligacao_x_agendamento <- ifelse(desempenho$ligacoes_totais != 0, 
                                             desempenho$agendamento/desempenho$ligacoes_totais,
                                             0)
  desempenho$data <- dados_hoje$Início[1]
  
  desempenho <- desempenho[order(desempenho$agendamento, decreasing = T),]
  
  return(desempenho)
}


funcao_cor_desempenho <- function(x) {
  case_when(
    x < 0.30 ~ "red",
    x >= 0.30 & x < 0.60 ~ "orange",
    x >= 0.60 & x < 0.90 ~ "lightgreen",
    x >= 0.90 ~ "green",
    TRUE ~ "white" # Para NAs ou outros valores
  )
}

criar_tabela <- function(desempenho) {
  
  nomes_colunas_display <- c("Responsável", "Ligações Totais", "Ligações Relevantes", "Agendamentos",
                             "Meta de Ligações", "Meta de Ligações Relevantes", "Meta de Agendamentos",
                             "Ligação por Meta", "Ligação Relevante por Meta", "Agendamento por Meta",
                             "Ligações / Relevantes", # Nome curto para "Ligação por Ligação Relevante"
                             "Relevantes / Agendamentos", # Nome curto para "Ligação Relevante por Agendamento"
                             "Ligações / Agendamentos") # Nome curto para "Ligação por Agendamento"
  
  # --- 1. Calcular a linha de total ---
  linha_total <- desempenho %>%
    summarise(
      responsavel = "Total",
      ligacoes_totais = sum(ligacoes_totais, na.rm = TRUE),
      ligacoes_relevantes = sum(ligacoes_relevantes, na.rm = TRUE),
      agendamento = sum(agendamento, na.rm = TRUE),
      meta_ligacoes = sum(meta_ligacoes, na.rm = T),
      meta_ligacoes_relevantes = sum(meta_ligacoes_relevantes, na.rm = T),
      meta_agendamento = sum(meta_agendamento, na.rm = T),
      atingimento_ligacoes = ligacoes_totais/meta_ligacoes,
      atingimento_ligacoes_relevantes = ligacoes_relevantes/meta_ligacoes_relevantes,
      atingimento_agendamento = agendamento/meta_agendamento,
      ligacao_x_ligacao_relevante = ligacoes_relevantes/ligacoes_totais,
      ligacao_relevante_x_agendamento = agendamento/ligacoes_relevantes,
      ligacao_x_agendamento = agendamento/ligacoes_totais
    )
  
  # --- 2. Unir com o dataframe original ---
  desempenho_com_total <- bind_rows(desempenho, linha_total)
  
  # --- 3. Aplicar formatações (cores, porcentagens, etc.) ---
  desempenho_formatado <- desempenho_com_total %>%
    mutate(
      ligacoes_totais = ifelse(
        is.na(atingimento_ligacoes),
        as.character(ligacoes_totais),
        cell_spec(
          ligacoes_totais, format = "html",
          background = funcao_cor_desempenho(atingimento_ligacoes)
        )
      ),
      ligacoes_relevantes = ifelse(
        is.na(atingimento_ligacoes_relevantes),
        as.character(ligacoes_relevantes),
        cell_spec(
          ligacoes_relevantes, format = "html",
          background = funcao_cor_desempenho(atingimento_ligacoes_relevantes)
        )
      ),
      agendamento = ifelse(
        is.na(atingimento_agendamento),
        as.character(agendamento),
        cell_spec(
          agendamento, format = "html",
          background = funcao_cor_desempenho(atingimento_agendamento)
        )
      ),
      atingimento_ligacoes = ifelse(
        is.na(atingimento_ligacoes),
        "",
        cell_spec(
          sprintf("%.1f%%", atingimento_ligacoes * 100), format = "html",
          background = funcao_cor_desempenho(atingimento_ligacoes)
        )
      ),
      atingimento_ligacoes_relevantes = ifelse(
        is.na(atingimento_ligacoes_relevantes),
        "",
        cell_spec(
          sprintf("%.1f%%", atingimento_ligacoes_relevantes * 100), format = "html",
          background = funcao_cor_desempenho(atingimento_ligacoes_relevantes)
        )
      ),
      atingimento_agendamento = ifelse(
        is.na(atingimento_agendamento),
        "",
        cell_spec(
          sprintf("%.1f%%", atingimento_agendamento * 100), format = "html",
          background = funcao_cor_desempenho(atingimento_agendamento)
        )
      ),
      ligacao_x_ligacao_relevante = ifelse(
        is.na(ligacao_x_ligacao_relevante),
        "",
        cell_spec(sprintf("%.1f%%", ligacao_x_ligacao_relevante * 100), format = "html")
      ),
      ligacao_relevante_x_agendamento = ifelse(
        is.na(ligacao_relevante_x_agendamento),
        "",
        cell_spec(sprintf("%.1f%%", ligacao_relevante_x_agendamento * 100), format = "html")
      ),
      ligacao_x_agendamento = ifelse(
        is.na(ligacao_x_agendamento),
        "",
        cell_spec(sprintf("%.1f%%", ligacao_x_agendamento * 100), format = "html")
      )
    )
  
  # --- 4. Reordenar colunas para exibição ---
  desempenho_final_ordenado <- desempenho_formatado %>%
    select(
      responsavel, ligacoes_totais, ligacoes_relevantes, agendamento,
      meta_ligacoes, meta_ligacoes_relevantes, meta_agendamento,
      atingimento_ligacoes, atingimento_ligacoes_relevantes, atingimento_agendamento,
      ligacao_x_ligacao_relevante, ligacao_relevante_x_agendamento, ligacao_x_agendamento
    )
  
  # --- 5. Gerar tabela com kable e destacar a última linha (total) ---
  desempenho_final_ordenado |>
    kbl(
      col.names = nomes_colunas_display,
      caption = "",
      escape = FALSE
    ) |>
    kable_minimal() |>
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    row_spec(
      nrow(desempenho_final_ordenado), bold = TRUE, background = "#f0f0f0"
    )
}

grafico_grid1 <- function(desempenho, data) {
  
  #  Gráfico de Ligações por Meta 
  
  p1 <- grafico_meta(desempenho, 2, 5, titulo = "Total de Ligações / Meta",
                     x = "Total de Ligações vs. Meta por Responsável")
  
  # Gráfico de Ligações Relevantes por Meta
  
  p2 <- grafico_meta(desempenho, 3, 6, x = "Total de Ligações Relevantes vs. Meta por Responsável",
                     titulo = "Total de Ligações Relevantes / Meta")
  
  # Gráfico de Agendamentos por Meta
  
  p3 <- grafico_meta(desempenho, 4, 7, titulo = "Total de Agendamentos / Meta",
                     x = "Total de Agendamentos vs. Meta por Responsável")
  
  # Gráfico de Proporção Ligação Relevante
  
  p4 <- grafico_prop(desempenho, 11, x = "Proporção de Ligações Relevantes",
                     titulo = "Proporção de Ligação / Relevantes")
  
  # Gráfico de Proporção Ligação Relevante por Agendamento
  
  p5 <- grafico_prop(desempenho, 12, x = "Proporção de Agendamentos",
                     titulo = "Proporção de Agendamentos / Relevantes")
  
  # Gráfico de Proporção Ligação por Agendamento
  
  p6 <- grafico_prop(desempenho, 13, x = "Proporção de Agendamentos",
                     titulo = "Proporção de Agendamentos / Ligação")
  
  grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2, widths = c(1.1, 1, 1),
               heights = c(1.2, 1),
               top = paste("Desempenho Equipe Comercial",data),
               bottom = "Fonte dos Dados: CRM da Empresa")
}


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

grafico_grid2 <- function(desempenho_sem) {
  
  #  Relação entre Ligações e Ligações Relevantes 
  
  p1 <- grafico_disp(desempenho_sem, 2, 3, x = "Ligações", y = "Ligações Relevantes",
               titulo = "Relação entre Ligações e Ligações Relevantes")
  
  # Relação entre Ligações Relevantes e Agendamentos
  
  p2 <- grafico_disp(desempenho_sem, 3, 4, x = "Ligações Relevantes", y = "Agendamentos",
               titulo = "Relação entre Ligações Relevantes e Agendamentos")
  
  # Relação entre Ligações e Agendamentos
  
  p3 <- grafico_disp(desempenho_sem, 2, 4, x = "Ligações", y = "Agendamentos",
               titulo = "Relação entre Ligações e Agendamentos")
  
  grid.arrange(p1,p2,p3, ncol = 3,
               top = "Relação entre Variáveis",
               bottom = "Fonte dos Dados: CRM da Empresa",
               widths = c(1.1, 1, 1))
}


grafico_grid3 <- function(desempenho_sem) {
  p1 <- desempenho_sem |> 
    ggplot(aes(x = data, y = agendamento, color = responsavel)) +
    geom_line(size = 1) + # Adiciona a linha (curva)
    geom_point(, size = 2, alpha = 0.6) + # Adiciona pontos nos dados (opcional)
    labs(
      title = "Agendamentos Diários ao Longo do Tempo por BDR",
      x = "Data",
      y = "Agendamentos",
      color = "Responsável"
    ) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.title.x = element_text(), axis.title.y = element_text())
  
  p2 <- desempenho_sem |>
    group_by(data) |> 
    summarise(Agendamentos = sum(agendamento, na.rm = TRUE), .groups = 'drop') |> 
    ggplot(aes(x = data, y = Agendamentos)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "darkblue", size = 3) + # Adiciona pontos nos dados (opcional)
    labs(
      title = "Agendamentos Diários ao Longo do Tempo",
      x = "Data",
      y = "Agendamentos"
    ) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.title.x = element_text(), axis.title.y = element_text())
  
  grid.arrange(p1,p2, ncol = 2,
               top = "Análise Temporal",
               bottom = "Fonte dos Dados: CRM da Empresa")
  
}

desempenho_mensal <- function(desempenho_sem) {
  desempenho_sem$meta_ligacoes <- desempenho_sem$meta_ligacoes*20
  desempenho_sem$meta_ligacoes_relevantes <- desempenho_sem$meta_ligacoes_relevantes*20
  desempenho_sem$meta_agendamento <- desempenho_sem$meta_agendamento*20
  
  desempenho_sem$atingimento_ligacoes <- ifelse(desempenho_sem$meta_ligacoes != 0, 
                                                desempenho_sem$ligacoes_totais/desempenho_sem$meta_ligacoes,
                                                0)
  desempenho_sem$atingimento_ligacoes_relevantes <- ifelse(desempenho_sem$meta_ligacoes_relevantes != 0,
                                                           desempenho_sem$ligacoes_relevantes/desempenho_sem$meta_ligacoes_relevantes,
                                                           0)
  desempenho_sem$atingimento_agendamento <- ifelse(desempenho_sem$meta_agendamento != 0,
                                                   desempenho_sem$agendamento/desempenho_sem$meta_agendamento,
                                                   0)
  desempenho_sem$ligacao_x_ligacao_relevante <- ifelse(desempenho_sem$ligacoes_totais != 0,
                                                       desempenho_sem$ligacoes_relevantes/desempenho_sem$ligacoes_totais,
                                                       0)
  desempenho_sem$ligacao_relevante_x_agendamento <- ifelse(desempenho_sem$ligacoes_relevantes != 0,
                                                           desempenho_sem$agendamento/desempenho_sem$ligacoes_relevantes,
                                                           0)
  desempenho_sem$ligacao_x_agendamento <- ifelse(desempenho_sem$ligacoes_totais != 0,
                                                 desempenho_sem$agendamento/desempenho_sem$ligacoes_totais,
                                                 0)
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
        atingimento_ligacoes = sum(atingimento_ligacoes, na.rm = T),
        atingimento_ligacoes_relevantes = sum(atingimento_ligacoes_relevantes, na.rm = T),
        atingimento_agendamento = sum(atingimento_agendamento, na.rm = T),
        ligacao_x_agendamento = ifelse(ligacoes_totais != 0, sum(agendamento, na.rm =T)/sum(ligacoes_totais, na.rm = T),0),
        ligacao_x_ligacao_relevante = ifelse(ligacoes_totais != 0, sum(ligacoes_relevantes, na.rm =T)/sum(ligacoes_totais, na.rm =T),0),
        ligacao_relevante_x_agendamento = ifelse(ligacoes_relevantes != 0, sum(agendamento, na.rm = T)/sum(ligacoes_relevantes, na.rm = T),0),
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
  
  return(soma_desempenho_sem)
}

calcular_ligacoes_por_relevante <- function(modelo_ajustado, target_relevante = 1) {
  
  # --- Validações iniciais ---
  if (!inherits(modelo_ajustado, "lm")) {
    stop("O objeto 'modelo_ajustado' deve ser um modelo 'lm'.")
  }
  
  coef_names <- names(coef(modelo_ajustado))
  if (!("(Intercept)" %in% coef_names && 
        any(grepl("log10\\(ligacoes_totais \\+ 1\\)", coef_names)) &&
        any(grepl("responsavel", coef_names))
  )) {
    stop("O modelo não parece ter a estrutura esperada (log10(ligacoes_relevantes + 1) ~ log10(ligacoes_totais + 1) * responsavel).")
  }
  
  # --- 1. Extrair os coeficientes do modelo ---
  coefs_df <- tidy(modelo_ajustado) %>%
    select(term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate)
  
  # --- 2. Obter os responsáveis únicos e a categoria de referência de forma robusta ---
  # CORREÇÃO: Usar modelo_ajustado$xlevels para obter os níveis do fator 'responsavel'
  # Isso é o mais confiável, pois 'xlevels' armazena os níveis exatamente como o lm os usou.
  if ("responsavel" %in% names(modelo_ajustado$xlevels)) {
    responsables_unicos <- modelo_ajustado$xlevels$responsavel
  } else {
    # Fallback se 'responsavel' não estiver diretamente em xlevels, mas estiver nos termos
    # Isso pode acontecer se o fator foi criado com C(responsavel) na fórmula
    # Tenta obter do model.frame como último recurso, se for um fator
    model_data <- stats::model.frame(modelo_ajustado)
    responsavel_col_in_model_data <- names(model_data)[grepl("responsavel", names(model_data))]
    
    if (length(responsavel_col_in_model_data) > 0 && is.factor(model_data[[responsavel_col_in_model_data[1]]])) {
      responsables_unicos <- levels(model_data[[responsavel_col_in_model_data[1]]])
    } else {
      stop("Variável 'responsavel' não encontrada como fator nos níveis ou no dataframe do modelo ajustado.")
    }
  }
  
  # NOVO: Se não houver responsáveis únicos após a tentativa robusta, retorne dataframe vazio
  if (length(responsables_unicos) == 0) {
    warning("Nenhum responsável único encontrado no modelo. Retornando dataframe vazio.")
    return(tibble(
      responsavel = character(),
      effective_intercept = numeric(),
      effective_slope = numeric(),
      total_calls_for_X_relevant_call = numeric(), 
      interpretation = character()
    ))
  }
  
  reference_responsavel <- responsables_unicos[1]
  
  # --- 3. Definir o valor alvo ---
  target_log_relevante_plus_1 <- log10(target_relevante + 1)
  
  # --- 4. Iterar sobre cada responsável para calcular o valor ---
  resultados_por_responsavel <- purrr::map_dfr(responsables_unicos, function(resp) {
    
    # Coeficientes base (referência)
    intercept_base <- coefs_df[["(Intercept)"]]
    slope_base <- coefs_df[["log10(ligacoes_totais + 1)"]]
    
    current_intercept <- intercept_base
    current_slope <- slope_base
    
    # Ajustar para responsáveis que não são a referência
    if (resp != reference_responsavel) {
      # Nomes dos termos no dataframe de coeficientes (ex: "responsavelEmilin" ou "C(responsavel)[T.Emilin]")
      # Construção robusta do nome do termo para corresponder ao que está nos coeficientes
      
      # Primeiro tenta o formato simples
      resp_intercept_term_exact <- paste0("responsavel", resp)
      resp_interaction_term_exact <- paste0("log10(ligacoes_totais + 1):", resp_intercept_term_exact)
      
      # Se não encontrar, tenta o formato C(factor)[T.level]
      if (! (resp_intercept_term_exact %in% names(coefs_df)) ) {
        resp_intercept_term_exact <- paste0(names(modelo_ajustado$xlevels)[1], resp) # Uses the actual factor name in xlevels
        if (! (resp_intercept_term_exact %in% names(coefs_df)) ) { # If still not found, try C() syntax
          resp_intercept_term_exact <- paste0("C(", names(modelo_ajustado$xlevels)[1], ")[T.", resp, "]")
          resp_interaction_term_exact <- paste0("log10(ligacoes_totais + 1):C(", names(modelo_ajustado$xlevels)[1], ")[T.", resp, "]")
        }
      }
      
      intercept_adjust <- coefs_df[[resp_intercept_term_exact]] %||% 0
      slope_adjust <- coefs_df[[resp_interaction_term_exact]] %||% 0
      
      current_intercept <- intercept_base + intercept_adjust
      current_slope <- slope_base + slope_adjust
    }
    
    # --- Cálculo das Ligações Totais Necessárias ---
    needed_total_calls <- NA_real_
    interpretation <- "Estimado"
    
    if (is.na(current_slope) || abs(current_slope) < 1e-9) { 
      needed_total_calls <- NA_real_
      interpretation <- "Não aplicável (inclinação muito próxima de zero ou NA)"
    } else {
      exponent_val <- (target_log_relevante_plus_1 - current_intercept) / current_slope
      
      if (exponent_val > 15) { # Limit for extremely high values
        needed_total_calls <- Inf
        interpretation <- paste0("Número extremamente alto de ligações (>", formatC(10^15-1, format="e", digits=2), ")")
      } else if (exponent_val < -15) { # Limit for extremely low values
        needed_total_calls <- 0
        interpretation <- "Número muito baixo de ligações (quase 0)"
      } else {
        needed_total_calls <- 10^exponent_val - 1
        if (needed_total_calls < 0) {
          needed_total_calls <- 0
          interpretation <- "Não aplicável (resultaria em ligações negativas)"
        }
      }
    }
    
    tibble(
      responsavel = resp,
      effective_intercept = current_intercept,
      effective_slope = current_slope,
      total_calls_for_X_relevant_call = needed_total_calls, 
      interpretation = interpretation
    )
  })
  
  # --- 5. Preparar os resultados finais ---
  resultados_finais <- resultados_por_responsavel %>%
    mutate(total_calls_for_X_relevant_call = round(total_calls_for_X_relevant_call, 2))
  
  return(resultados_finais)
}

criar_tabela_desempenho_relevancia <- function(dados_desempenho) {
  
  # --- Validação básica do input ---
  colunas_esperadas <- c("responsavel", "effective_intercept", "effective_slope", 
                         "total_calls_for_X_relevant_call", "interpretation")
  if (!all(colunas_esperadas %in% names(dados_desempenho))) {
    stop(paste("O dataframe de entrada não possui todas as colunas esperadas. Faltando:",
               paste(setdiff(colunas_esperadas, names(dados_desempenho)), collapse = ", ")))
  }
  
  if (nrow(dados_desempenho) == 0) {
    warning("O dataframe 'dados_desempenho' está vazio. Retornando tabela placeholder.")
    return(kbl(data.frame(Mensagem = "Dados indisponíveis para a tabela."), 
               caption = "Ligações Necessárias para 1 Ligação Relevante por Responsável") |> 
             kable_minimal() |> kable_styling())
  }
  
  # --- 1. Definir os nomes das colunas para exibição na tabela ---
  nomes_colunas_display <- c(
    "Responsável",
    "Intercepto Efetivo",
    "Inclinação Efetiva",
    "Ligações Totais para X Relevantes", 
    "Interpretação"
  )
  
  # --- 2. Preparar o dataframe aplicando formatação e cores ANTES de kbl() ---
  tabela_desempenho_formatada <- dados_desempenho %>%
    mutate(
      # Formata o Intercepto e a Inclinação para 3 casas decimais
      effective_intercept = sprintf("%.3f", effective_intercept),
      effective_slope = sprintf("%.3f", effective_slope),
      
      # CORREÇÃO: Lógica case_when() AGORA é o PRIMEIRO ARGUMENTO de cell_spec()
      total_calls_for_X_relevant_call = cell_spec(
        case_when( # <--- AQUI! Geramos a string a ser exibida
          is.infinite(total_calls_for_X_relevant_call) ~ "Infinito", # Se for infinito
          is.na(total_calls_for_X_relevant_call) ~ "N/A",           # Se for NA
          total_calls_for_X_relevant_call == 0 ~ "0",              # Se for zero
          TRUE ~ sprintf("%.2f", total_calls_for_X_relevant_call)  # Formata números para 2 casas decimais
        ),
        format = "html",
        align = "center" # Alinhamento do texto na célula
      )
      # Se quiser cores condicionais para esta coluna, adicione 'background = sua_funcao_cor(total_calls_for_X_relevant_call_numerico_original)' aqui.
      # Para isso, você precisaria manter a coluna numérica original antes de mutá-la para string, ou passar para a função de cor.
    )
  
  # --- 3. Gerar a tabela com kbl() ---
  tabela_final <- tabela_desempenho_formatada |>
    kbl(
      col.names = nomes_colunas_display, 
      escape = FALSE # MUITO IMPORTANTE
    ) |>
    kable_minimal() |> 
    kable_styling(
      full_width = FALSE, 
      position = "center", 
      bootstrap_options = c("striped", "hover", "condensed") 
    )
  
  return(tabela_final)
}

get_leads2b <- function(endpoint_path, parametros) {
  api_key <- Sys.getenv("API_KEY_V1")
  base_url <- "https://api.leads2b.com"
  full_url <- paste0(base_url, endpoint_path)
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  response <- GET(full_url, headers,
                  query = parametros)
  message(paste("O status da requisição é",status_code(response)))
  dados_leads <- fromJSON(content(response, "text", encoding = "UTF-8"))
  dados_leads <- dados_leads$result
  return(dados_leads)
}


criar_tabela_funil_vendas <- function(funil_v4_data, mes_ano_caption = "Mês/Ano") {
  
  # --- Validações de Input ---
  colunas_esperadas <- c("mql", "sql", "reuniao_agendada", "no_show", 
                         "reuniao_realizada", "oportunidade", "fechamento")
  if (!all(colunas_esperadas %in% names(funil_v4_data))) {
    stop(paste("O dataframe 'funil_v4_data' não possui todas as colunas esperadas para o funil. Faltando:",
               paste(setdiff(colunas_esperadas, names(funil_v4_data)), collapse = ", ")))
  }
  
  if (nrow(funil_v4_data) != 1) {
    warning("O dataframe 'funil_v4_data' deve ter exatamente uma linha com os totais do funil. O resultado pode ser inesperado.")
  }
  
  # Garante que qualquer contagem NA (se uma etapa não ocorreu) seja 0
  funil_v4_data[is.na(funil_v4_data)] <- 0
  
  # --- 1. Preparar os dados para a tabela do funil (formato longo) ---
  funil_tabela_raw <- funil_v4_data %>%
    pivot_longer(
      cols = everything(), 
      names_to = "Etapa_Interna", 
      values_to = "Total" 
    ) %>%
    mutate(
      Etapa_do_Funil = case_when(
        Etapa_Interna == "mql" ~ "MQL",
        Etapa_Interna == "sql" ~ "SQL",
        Etapa_Interna == "reuniao_agendada" ~ "Reunião Agendada",
        Etapa_Interna == "no_show" ~ "No-Show",
        Etapa_Interna == "reuniao_realizada" ~ "Reunião Realizada",
        Etapa_Interna == "oportunidade" ~ "Oportunidade",
        Etapa_Interna == "fechamento" ~ "Fechamento",
        TRUE ~ Etapa_Interna 
      ),
      Etapa_do_Funil = factor(Etapa_do_Funil, levels = c(
        "MQL", "SQL", "Reunião Agendada", "No-Show", "Reunião Realizada", 
        "Oportunidade", "Fechamento"
      ))
    ) %>%
    arrange(Etapa_do_Funil)
  
  # --- 2. REMOÇÃO DA TAXA DE CONVERSÃO ---
  # Remove o cálculo da Taxa_de_Conversao, pois não será exibida
  # Não há mais cálculo de Taxa_de_Conversao aqui.
  
  # Selecionar apenas as colunas necessárias: Etapa_do_Funil e Total
  funil_tabela <- funil_tabela_raw %>%
    select(Etapa_do_Funil, Total) # <--- REMOVIDA Taxa_de_Conversao
  
  
  # --- 3. Preparar o dataframe para kbl() com formatação ---
  funil_tabela_formatada <- funil_tabela %>%
    mutate(
      Total = as.integer(Total) # Garante que Total seja um número inteiro
      # A formatação de Taxa_de_Conversao foi removida
    )
  
  # --- 4. Gerar a Tabela kableExtra ---
  nomes_colunas_display <- c("Etapa do Funil", "Total") # <--- REMOVIDO "Taxa de Conversão"
  
  tabela_final <- kbl(
    funil_tabela_formatada,
    col.names = nomes_colunas_display
  ) |>
    kable_minimal() |> 
    kable_styling(
      full_width = FALSE, 
      position = "center", 
      bootstrap_options = c("striped", "hover", "condensed") 
    ) |>
    # Alinhamento da coluna numérica (Total)
    column_spec(column = 2, extra_css = "text-align: center;") # Coluna 'Total' é agora a segunda
  
  return(tabela_final)
}

criar_tabela_resumo_closer <- function(summarized_closer_data, mes_ano_caption = "Mês/Ano") {
  
  # --- Validação de Input ---
  colunas_esperadas <- c("agendamento", "reuniao_realizada","negociacao", "no_show", "fechamento",
                         "agendamento_x_fechamento", "agendamento_x_noshow")
  if (!all(colunas_esperadas %in% names(summarized_closer_data))) {
    stop(paste("O dataframe 'summarized_closer_data' não possui todas as colunas esperadas. Faltando:",
               paste(setdiff(colunas_esperadas, names(summarized_closer_data)), collapse = ", ")))
  }
  
  if (nrow(summarized_closer_data) != 1) {
    stop("O dataframe 'summarized_closer_data' deve ter exatamente UMA linha com os totais sumarizados.")
  }
  
  # Garante que qualquer NA seja 0 para display consistente
  summarized_closer_data[is.na(summarized_closer_data)] <- 0
  
  # --- 1. Definir os nomes das colunas para exibição ---
  nomes_colunas_display <- c(
    "Agendamentos",
    "Reunião Realizada", # Renomeado de 'agendamento' para exibição
    "Negociação",   # Renomeado de 'negociacao'
    "No-Show",      # Renomeado de 'no_show'
    "Fechamento",   # Renomeado de 'fechamento'
    "Fechamento/Agend.", # Renomeado de 'agendamento_x_fechamento'
    "No-Show/Agend."     # Renomeado de 'agendamento_x_noshow'
  )
  
  # --- 2. Preparar o dataframe para kbl() com formatação ---
  tabela_closer_formatada <- summarized_closer_data %>%
    mutate(
      # Formatar contagens como inteiros
      agendamento = as.integer(agendamento),
      reuniao_realizada = as.integer(reuniao_realizada),
      negociacao = as.integer(negociacao),
      no_show = as.integer(no_show),
      fechamento = as.integer(fechamento),
      
      # Formatar taxas como porcentagens com cell_spec
      agendamento_x_fechamento = cell_spec(
        sprintf("%.1f%%", agendamento_x_fechamento * 100),
        format = "html", align = "center"
      ),
      agendamento_x_noshow = cell_spec(
        sprintf("%.1f%%", agendamento_x_noshow * 100),
        format = "html", align = "center"
      )
    )
  
  # --- 3. Gerar a Tabela kableExtra ---
  tabela_final <- kbl(
    tabela_closer_formatada,
    col.names = nomes_colunas_display, # Usa os nomes de exibição
    escape = FALSE # Crucial para renderizar HTML de cell_spec
  ) |>
    kable_minimal() |> # Aplica um tema minimalista
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    # Alinhamento das colunas de contagem (as 4 primeiras)
    column_spec(column = 1:4, extra_css = "text-align: center;") 
  
  return(tabela_final)
}

criar_tabela_perdidos <- function(perdidos_data, mes_ano_caption = "Mês/Ano") {
  
  # --- Validação de Input ---
  colunas_esperadas <- c("Prospect", "Leads", "Oportunidade", "Total")
  if (!all(colunas_esperadas %in% names(perdidos_data))) {
    stop(paste("O dataframe 'perdidos_data' não possui todas as colunas esperadas. Faltando:",
               paste(setdiff(colunas_esperadas, names(perdidos_data)), collapse = ", ")))
  }
  
  if (nrow(perdidos_data) != 1) {
    stop("O dataframe 'perdidos_data' deve ter exatamente UMA linha com os totais sumarizados.")
  }
  
  # Garante que qualquer NA seja 0 para display consistente
  perdidos_data[is.na(perdidos_data)] <- 0
  
  # --- 1. Transforma para formato longo para exibição na tabela ---
  # Isso transforma as colunas (prospect, leads, oportunidade) em linhas
  perdidos_tabela_long <- perdidos_data %>%
    pivot_longer(
      cols = everything(), # Pivota todas as colunas
      names_to = "Metrica_Interna",
      values_to = "Total"
    )
  # --- 2. Prepara para kbl() com formatação ---
  perdidos_tabela_formatada <- perdidos_tabela_long %>%
    mutate(Total = as.integer(Total)) # Garante que a coluna 'Total' seja um inteiro
  
  # --- 3. Gera a tabela kableExtra ---
  nomes_colunas_display <- c("Etapa", "Total Perdidos") # Nomes das colunas na exibição da tabela
  
  tabela_final <- kbl(
    perdidos_tabela_formatada,
    col.names = nomes_colunas_display, # Título da tabela
    escape = FALSE # Crucial para renderizar HTML (se houver, como cell_spec)
  ) |>
    kable_minimal() |> # Aplica um tema minimalista
    kable_styling(
      full_width = FALSE, # Permite que a tabela seja centralizada
      position = "center", # Centraliza a tabela na página HTML
      bootstrap_options = c("striped", "hover", "condensed") # Adiciona estilos Bootstrap
    ) |>
    # Alinhamento da coluna 'Total'
    column_spec(column = 2, extra_css = "text-align: center;") 
  
  return(tabela_final)
}

calcular_probabilidade_meta_semanal <- function(modelo_ajustado, desempenho_semanal_df, metas_semanais_por_responsavel, num_simulacoes = 10000, hoje_data = Sys.Date()) {
  
  # --- 2.1. Preparar Informações de Contexto ---
  
  # Obter sigma (desvio padrão do erro) do modelo
  sigma_modelo <- 0 
  if (inherits(modelo_ajustado, "lm")) {
    sigma_modelo <- summary(modelo_ajustado)$sigma
  } else if (inherits(modelo_ajustado, "randomForest")) {
    sigma_modelo <- sqrt(tail(modelo_ajustado$mse, 1)) 
  } else {
    stop("Tipo de modelo não suportado. Use 'lm' ou 'randomForest'.")
  }
  message(paste0("DEBUG: Sigma (desvio padrão do erro) do modelo usado: ", round(sigma_modelo, 3)))
  
  # Calcular dias restantes na semana
  dia_da_semana_hoje <- wday(hoje_data, week_start = 1) 
  dias_restantes_na_semana <- 7 - dia_da_semana_hoje 
  if (dias_restantes_na_semana < 0) dias_restantes_na_semana <- 0 
  
  message(paste0("Simulando para ", dias_restantes_na_semana, " dias restantes na semana."))
  
  # Obter ligacoes_relevantes já feitas nesta semana (para cada responsavel)
  ligacoes_relevantes_acumuladas_semana <- desempenho_semanal_df %>%
    filter(data <= hoje_data) %>% 
    group_by(responsavel) %>%
    summarise(relevantes_acumuladas = sum(ligacoes_relevantes, na.rm = TRUE), .groups = 'drop')
  
  # Obter a média diária de ligacoes_totais por responsável
  media_ligacoes_totais_diaria_por_responsavel <- desempenho_semanal_df %>%
    group_by(responsavel) %>%
    summarise(media_diaria_totais = mean(ligacoes_totais, na.rm = TRUE), .groups = 'drop') %>%
    deframe() 
  
  # Obter níveis do fator 'responsavel'
  responsables_levels <- levels(model.frame(modelo_ajustado)$responsavel)
  
  # --- CORREÇÃO AQUI: Identificação robusta da variável preditora contínua no modelo ---
  # Agora procura por 'log_ligacoes' que é o nome da sua coluna no dataframe
  predictor_name_in_model <- "log_ligacoes" 
  
  # Você pode adicionar uma verificação se quiser garantir que essa coluna exista nos dados de treinamento
  # if (!(predictor_name_in_model %in% names(model.frame(modelo_ajustado)))) {
  #   stop(paste("A coluna '", predictor_name_in_model, "' não foi encontrada no dataframe usado para treinar o modelo.", sep=""))
  # }
  
  # --- 2.2. Realizar a Simulação de Monte Carlo ---
  resultados_simulacao_por_responsavel_lista <- list() 
  
  for (resp_nome in responsables_levels) {
    
    meta_semanal_resp <- metas_semanais_por_responsavel[resp_nome]
    if (is.na(meta_semanal_resp)) {
      warning(paste("Meta semanal não encontrada para", resp_nome, ". Pulando este responsável."))
      next 
    }
    
    relevantes_acumuladas_resp <- ligacoes_relevantes_acumuladas_semana %>%
      filter(responsavel == resp_nome) %>%
      pull(relevantes_acumuladas)
    if(length(relevantes_acumuladas_resp) == 0) relevantes_acumuladas_resp <- 0 
    
    resultados_binarios_resp <- numeric(num_simulacoes) 
    
    for (s in 1:num_simulacoes) {
      ligacoes_relevantes_simuladas_restantes_semana <- 0
      
      for (dia in 1:dias_restantes_na_semana) { 
        
        ligacoes_totais_simuladas <- media_ligacoes_totais_diaria_por_responsavel[resp_nome]
        ligacoes_totais_simuladas <- max(0, ligacoes_totais_simuladas)
        if (is.na(ligacoes_totais_simuladas)) ligacoes_totais_simuladas <- 0 
        
        # Preparar dados para a previsão do modelo
        new_data_for_predict <- data.frame(
          responsavel = factor(resp_nome, levels = responsables_levels) 
        )
        # CORREÇÃO: Atribui o valor simulado à coluna com o nome correto do preditor ('log_ligacoes')
        new_data_for_predict[[predictor_name_in_model]] <- log10(ligacoes_totais_simuladas + 1)
        
        previsao_dependente_log <- suppressWarnings(predict(modelo_ajustado, newdata = new_data_for_predict))
        
        if (is.na(previsao_dependente_log) || is.infinite(previsao_dependente_log)) {
          previsao_log_com_erro <- NA_real_
        } else {
          previsao_log_com_erro <- previsao_dependente_log + rnorm(1, mean = 0, sd = sigma_modelo)
        }
        
        relevantes_simuladas_dia <- ifelse(is.na(previsao_log_com_erro), 0, 10^previsao_log_com_erro - 1)
        relevantes_simuladas_dia <- max(0, relevantes_simuladas_dia) 
        
        ligacoes_relevantes_simuladas_restantes_semana <- ligacoes_relevantes_simuladas_restantes_semana + relevantes_simuladas_dia
      }
      
      total_relevantes_simulado_semana_final <- relevantes_acumuladas_resp + ligacoes_relevantes_simuladas_restantes_semana
      
      resultados_binarios_resp[s] <- ifelse(total_relevantes_simulado_semana_final >= meta_semanal_resp, 1, 0)
    }
    
    resultados_simulacao_por_responsavel_lista[[resp_nome]] <- resultados_binarios_resp
  }
  
  # --- 2.3. Calcular a Probabilidade Final para Cada Responsável ---
  probabilidades_finais <- tibble(responsavel = character(), probabilidade_meta_semanal = numeric())
  
  for (resp_nome_final in names(resultados_simulacao_por_responsavel_lista)) {
    prob_meta <- sum(resultados_simulacao_por_responsavel_lista[[resp_nome_final]]) / num_simulacoes
    probabilidades_finais <- probabilidades_finais %>%
      add_row(responsavel = resp_nome_final, probabilidade_meta_semanal = prob_meta)
  }
  
  return(probabilidades_finais %>% mutate(probabilidade_meta_semanal = scales::percent(probabilidade_meta_semanal, accuracy = 0.1)))
}

calcular_prob_meta_semanal_via_fda_pura <- function(desempenho_mes, metas_semanais_por_responsavel, hoje_data = Sys.Date(), min_semanas_historico = 5) {
  
  # 1. Preparar dados históricos para estimação da distribuição (somar por semana COMPLETA)
  #    Isso garante que temos totais semanais para fitdistr
  desempenho_semanal_historico <- desempenho_mes %>%
    mutate(semana = floor_date(data, unit = "week", week_start = 1)) %>%
    group_by(responsavel, semana) %>%
    summarise(ligacoes_relevantes_semanais = sum(ligacoes_relevantes, na.rm = TRUE), .groups = 'drop') %>%
    # Filtra semanas que não estejam completas no início ou fim do histórico total
    # (ou você pode querer incluir semanas incompletas se fizer sentido para a média)
    filter(semana < floor_date(hoje_data, unit = "week", week_start = 1)) # Apenas semanas completas passadas
  
  # 2. Calcular ligacoes_relevantes já feitas NA SEMANA ATUAL (acumulado)
  ligacoes_relevantes_acumuladas_esta_semana <- desempenho_mes %>%
    filter(data >= floor_date(hoje_data, unit = "week", week_start = 1) & data <= hoje_data) %>%
    group_by(responsavel) %>%
    summarise(relevantes_acumuladas = sum(ligacoes_relevantes, na.rm = TRUE), .groups = 'drop')
  
  # 3. Calcular a probabilidade para cada responsável
  probabilidades_finais_fda <- tibble(responsavel = character(), probabilidade_meta_semanal_fda = numeric())
  
  responsables_unicos <- unique(desempenho_mes$responsavel)
  
  for (resp in responsables_unicos) {
    
    meta_semanal_resp <- metas_semanais_por_responsavel[resp]
    if (is.na(meta_semanal_resp)) {
      warning(paste("Meta semanal não encontrada para", resp, ". Pulando este responsável."))
      next # Pula para o próximo responsável
    }
    
    # Histórico de ligacoes relevantes semanais para este responsável
    hist_relevantes_resp <- desempenho_semanal_historico %>%
      filter(responsavel == resp) %>%
      pull(ligacoes_relevantes_semanais)
    
    # Acumulado desta semana para este responsável
    acumulado_resp <- ligacoes_relevantes_acumuladas_esta_semana %>%
      filter(responsavel == resp) %>%
      pull(relevantes_acumuladas)
    if(length(acumulado_resp) == 0) acumulado_resp <- 0 # Se não tiver acumulado ainda
    
    # Calcular o que PRECISA ser feito nos DIAS RESTANTES da semana
    # Isso é a meta MENOS o que já foi acumulado
    # Problema: A distribuição de uma parte da semana não é a mesma da semana completa
    # Vamos simplificar: vamos assumir que a distribuição do TOTAL da semana é NB.
    # A probabilidade P(TOTAL_SEMANA >= META)
    
    if (length(hist_relevantes_resp) < min_semanas_historico) {
      warning(paste("Poucas semanas de histórico para", resp, ". Não é possível estimar parâmetros NB confiavelmente. Pulando."))
      prob_meta <- NA_real_ # Não há dados suficientes para estimar
    } else {
      # Estimar os parâmetros NB para este responsável (apenas do histórico dele)
      fit_nb_resp <- tryCatch({
        fitdistr(hist_relevantes_resp, "negative binomial")
      }, error = function(e) {
        warning(paste("Erro ao ajustar NB para", resp, ":", e$message, ". Retornando NA."))
        return(NULL)
      })
      
      if (is.null(fit_nb_resp)) {
        prob_meta <- NA_real_
      } else {
        mu_estimado_resp <- fit_nb_resp$estimate["mu"]
        size_estimado_resp <- fit_nb_resp$estimate["size"]
        
        # Calcular o parâmetro 'prob' para pnbinom
        prob_param_nbinom_resp <- size_estimado_resp / (size_estimado_resp + mu_estimado_resp)
        
        # Calcular a probabilidade P(Total_Semana >= Meta_Semanal)
        # Se já batemos a meta, prob é 1. Se é impossível bater, prob é 0.
        if (acumulado_resp >= meta_semanal_resp) {
          prob_meta <- 1.0 # Já bateu a meta
        } else {
          # Probabilidade de o TOTAL FINAL da semana ser MAIOR OU IGUAL à meta
          # Assumimos que o total da semana segue a NB estimada
          prob_meta <- 1 - pnbinom(q = meta_semanal_resp - 1, 
                                   size = size_estimado_resp, 
                                   prob = prob_param_nbinom_resp, 
                                   lower.tail = TRUE)
        }
      }
    }
    
    probabilidades_finais_fda <- probabilidades_finais_fda %>%
      add_row(responsavel = resp, probabilidade_meta_semanal_fda = prob_meta)
  }
  
  return(probabilidades_finais_fda %>% mutate(probabilidade_meta_semanal_fda = scales::percent(probabilidade_meta_semanal_fda, accuracy = 0.1)))
}

closer_hoje <- function(dados, data_hoje) {
  dados$Data <- as.Date(dados$Data, format = "%d/%m/%Y")
  dados_hoje <- dados |> 
    filter(Data == data_hoje) |> 
    group_by(Responsavel) |> 
    summarise(
      Total_Reunioes = sum(Total.Reunioes, na.rm = T),
      Meta_Reunioes = 3,
      Atingimento_Reunioes = sum(Total.Reunioes, na.rm = T)/3,
      Total_Fechamentos = sum(Total....TO.DENTRO., na.rm = T),
      Meta_Fechamentos = 1,
      Atingimento_Fechamentos = sum(Total....TO.DENTRO., na.rm = T)/1,
      Total_Contratos = sum(CONTRATO.ASSINADO, na.rm = T),
      Total_NoShows = sum(Total.No.Shows, na.rm = T),
      Total_Vendas = sum(Total.Vendas, na.rm = T),
      Meta_Vendas = 41667,
      Atingimento_Vendas = sum(Total.Vendas, na.rm = T)/Meta_Vendas,
      Reunioes_x_Fechamentos = ifelse(sum(Total.Reunioes, na.rm = TRUE) != 0, 
                                      sum(Total....TO.DENTRO., na.rm = TRUE) / sum(Total.Reunioes, na.rm = TRUE), 
                                      0),
      Ticket_Medio = sum(Ticket.Médio, na.rm = T),
      Data = data_hoje,
      .groups = 'drop'
    )
  return(dados_hoje)
}

criar_tabela_desempenho_consolidado <- function(dados_sumarizados, data_caption = "Data Atual") {
  
  if (!is.data.frame(dados_sumarizados) || nrow(dados_sumarizados) == 0) {
    warning("O dataframe de entrada está vazio. Retornando tabela placeholder.")
    return(kbl(data.frame(Mensagem = "Dados indisponíveis."), caption = paste("Desempenho Consolidado -", data_caption)))
  }
  
  nomes_colunas_display <- c(
    "Responsável", "Total Reuniões", "Meta Reuniões", "Atingimento Reuniões",
    "Total Fechamentos", "Meta Fechamentos", "Atingimento Fechamentos", "Contratos Assinados","Total No-Shows",
    "Total Vendas", "Meta Vendas", "Atingimento Vendas",
    "Reuniões / Fechamentos", "Ticket Médio"
  )
  
  linha_total <- dados_sumarizados %>%
    summarise(
      Responsavel = "Total",
      Total_Reunioes = sum(Total_Reunioes, na.rm = TRUE),
      Meta_Reunioes = sum(Meta_Reunioes, na.rm = TRUE),
      Atingimento_Reunioes = sum(Total_Reunioes, na.rm = TRUE)/sum(Meta_Reunioes, na.rm = TRUE),
      Total_Fechamentos = sum(Total_Fechamentos, na.rm = TRUE),
      Meta_Fechamentos = sum(Meta_Fechamentos, na.rm = TRUE),
      Atingimento_Fechamentos = sum(Total_Fechamentos, na.rm = TRUE)/sum(Meta_Fechamentos, na.rm = TRUE),
      Total_Contratos = sum(Total_Contratos, na.rm = T),
      Total_NoShows = sum(Total_NoShows, na.rm = TRUE),
      Total_Vendas = sum(Total_Vendas, na.rm = T),  # Deixe como NA, para formatar separadamente depois
      Meta_Vendas = sum(Meta_Vendas, na.rm = TRUE),
      Atingimento_Vendas = sum(Total_Vendas, na.rm = T)/sum(Meta_Vendas, na.rm = TRUE),
      Reunioes_x_Fechamentos = sum(Total_Fechamentos, na.rm = TRUE)/sum(Total_Reunioes, na.rm = TRUE),
      Ticket_Medio = sum(Total_Vendas, na.rm = T)/sum(Total_Fechamentos, na.rm = TRUE)
    )
  
  desempenho_com_total <- bind_rows(dados_sumarizados, linha_total)
  
  # Identificar a última linha (total) para formatar diferente
  linha_total_index <- nrow(desempenho_com_total)
  
  tabela_consolidada_formatada <- desempenho_com_total %>%
    mutate(
      Total_Reunioes = ifelse(
        is.na(Atingimento_Reunioes),
        as.character(Total_Reunioes),
        cell_spec(as.integer(Total_Reunioes), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Reunioes))
      ),
      Total_Fechamentos = ifelse(
        is.na(Atingimento_Fechamentos),
        as.character(Total_Fechamentos),
        cell_spec(as.integer(Total_Fechamentos), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Fechamentos))
      ),
      Total_Vendas = ifelse(
        is.na(Atingimento_Vendas),
        paste0("R$ ", ifelse(is.na(Total_Vendas), "", as.integer(Total_Vendas))),
        cell_spec(paste0("R$ ", as.integer(Total_Vendas)), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Vendas))
      ),
      Atingimento_Reunioes = ifelse(
        is.na(Atingimento_Reunioes),
        "",
        cell_spec(sprintf("%.1f%%", Atingimento_Reunioes * 100), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Reunioes))
      ),
      Atingimento_Fechamentos = ifelse(
        is.na(Atingimento_Fechamentos),
        "",
        cell_spec(sprintf("%.1f%%", Atingimento_Fechamentos * 100), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Fechamentos))
      ),
      Atingimento_Vendas = ifelse(
        is.na(Atingimento_Vendas),
        "",
        cell_spec(sprintf("%.1f%%", Atingimento_Vendas * 100), format = "html", align = "center",
                  background = funcao_cor_desempenho(Atingimento_Vendas))
      ),
      Reunioes_x_Fechamentos = ifelse(
        is.na(Reunioes_x_Fechamentos),
        "",
        cell_spec(sprintf("%.1f%%", Reunioes_x_Fechamentos * 100), format = "html", align = "center")
      ),
      Ticket_Medio = ifelse(is.na(Ticket_Medio), "0", Ticket_Medio),
      Meta_Reunioes = as.integer(Meta_Reunioes),
      Meta_Fechamentos = as.integer(Meta_Fechamentos),
      Meta_Vendas = paste0("R$ ", Meta_Vendas),
      Total_NoShows = as.integer(Total_NoShows),
      Total_Contratos = as.integer(Total_Contratos)
    )
  
  tabela_final_ordenada <- tabela_consolidada_formatada %>%
    select(Responsavel, Total_Reunioes, Meta_Reunioes, Atingimento_Reunioes,
           Total_Fechamentos, Meta_Fechamentos, Atingimento_Fechamentos, Total_Contratos, Total_NoShows,
           Total_Vendas, Meta_Vendas, Atingimento_Vendas,
           Reunioes_x_Fechamentos, Ticket_Medio)
  
  tabela_final <- kbl(
    tabela_final_ordenada,
    col.names = nomes_colunas_display,
    escape = FALSE
  ) |>
    kable_minimal() |>
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    column_spec(2:ncol(tabela_final_ordenada), extra_css = "text-align: center;") |>
    row_spec(linha_total_index, bold = TRUE, background = "#f0f0f0")  # Negrito e fundo para linha total
  
  return(tabela_final)
}

closer_semana <- function(dados_sem) {
  dados <- dados_sem |> 
    group_by(Responsavel) |> 
    summarise(
      Total_Reunioes = sum(Total_Reunioes, na.rm = T),
      Meta_Reunioes = 15,
      Atingimento_Reunioes = sum(Total_Reunioes, na.rm = T)/Meta_Reunioes,
      Total_Fechamentos = sum(Total_Fechamentos, na.rm = T),
      Meta_Fechamentos = 5,
      Atingimento_Fechamentos = sum(Total_Fechamentos, na.rm = T)/Meta_Fechamentos,
      Total_Contratos = sum(Total_Contratos, na.rm = T),
      Total_NoShows = sum(Total_NoShows, na.rm = T),
      Total_Vendas = sum(Total_Vendas, na.rm = T),
      Meta_Vendas = 41667*5,
      Atingimento_Vendas = sum(Total_Vendas, na.rm = T)/Meta_Vendas,
      Reunioes_x_Fechamentos = ifelse(Total_Reunioes != 0,
                                      sum(Total_Fechamentos, na.rm = T)/sum(Total_Reunioes, na.rm = T), 0),
      Ticket_Medio = sum(Ticket_Medio, na.rm = T)
    )
  return(dados)
}

closer_mes <- function(dados_mes) {
  dados <- dados_mes |> 
    group_by(Responsavel) |> 
    summarise(
      Total_Reunioes = sum(Total_Reunioes, na.rm = T),
      Meta_Reunioes = 90,
      Atingimento_Reunioes = sum(Total_Reunioes, na.rm = T)/Meta_Reunioes,
      Total_Fechamentos = sum(Total_Fechamentos, na.rm = T),
      Meta_Fechamentos = 30,
      Atingimento_Fechamentos = sum(Total_Fechamentos, na.rm = T)/Meta_Fechamentos,
      Total_NoShows = sum(Total_NoShows, na.rm = T),
      Total_Contratos = sum(Total_Contratos, na.rm = T),
      Total_Vendas = sum(Total_Vendas, na.rm = T),
      Meta_Vendas = 500000,
      Atingimento_Vendas = sum(Total_Vendas, na.rm = T)/Meta_Vendas,
      Reunioes_x_Fechamentos = ifelse(Total_Reunioes != 0,
                                      sum(Total_Fechamentos, na.rm = T)/sum(Total_Reunioes, na.rm = T), 0),
      Ticket_Medio = sum(Ticket_Medio, na.rm = T)
    )
  return(dados)
}

grafico_grid4 <- function(dados_mes, pos_var, nome_x) {
  titulo1 <- paste(nome_x, "Diários ao Longo do Tempo por BDR") 
  titulo2 <- paste(nome_x, "Diários ao Longo do Tempo") 
  p1 <- dados_mes |> 
    ggplot(aes(x = Data, y = .data[[pos_var]], color = Responsavel)) + 
    geom_line(size = 1) + # Adiciona a linha (curva)
    geom_point(, size = 2, alpha = 0.6) + # Adiciona pontos nos dados (opcional)
    labs(
      title = titulo1,
      x = "Data",
      y = nome_x,
      color = "Responsável"
    ) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.title.x = element_text(), axis.title.y = element_text())
  
  p2 <- dados_mes |>
    group_by(Data) |> 
    summarise(Transformada = sum(.data[[pos_var]], na.rm = TRUE), .groups = 'drop') |> 
    ggplot(aes(x = Data, y = Transformada)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "darkblue", size = 3) +
    labs(
      title = titulo2,
      x = "Data",
      y = nome_x
    ) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.title.x = element_text(), axis.title.y = element_text())
  
  grid.arrange(p1,p2, ncol = 2,
               top = "Análise Temporal",
               bottom = "Fonte dos Dados: CRM da Empresa")
}

dados_decisores <- function(malu, bruna, emilin, stela) {
  malu$OBSERVAÇÃO <- ifelse(malu$OBSERVAÇÃO == "", NA, malu$OBSERVAÇÃO)
  bruna$OBSERVAÇÃO <- ifelse(bruna$OBSERVAÇÃO == "", NA, bruna$OBSERVAÇÃO)
  emilin$OBSERVAÇÃO <- ifelse(emilin$OBSERVAÇÃO == "", NA, emilin$OBSERVAÇÃO)
  stela$OBSERVAÇÃO <- ifelse(stela$OBSERVAÇÃO == "", NA, stela$OBSERVAÇÃO)
  decisores <- bind_rows(malu,stela,bruna,emilin)
  
  meta_ligacoes_bdr <- decisores |>
    group_by(BDR) |>
    summarise(meta_ligacoes = n()) |>
    ungroup()
  
  # 2. Calcular as OBSERVAÇÕES TOTAIS e REUNIÕES AGENDADAS (apenas para OBSERVAÇÕES não-NA)
  desempenho_decisores <- decisores |>
    filter(!is.na(OBSERVAÇÃO)) |> # Aqui sim filtramos para contar o que foi observado/agendado
    group_by(BDR) |>
    summarise(
      total_observacoes = n(), # Conta todas as observações não-NA (ligações válidas)
      reunioes_agendadas = sum(OBSERVAÇÃO == "Reunião agendada", na.rm = TRUE) # Conta "Reunião agendada"
    ) |>
    ungroup()
  
  # 3. Juntar as metas de ligações com os dados de desempenho
  #    Usamos um left_join para garantir que todos os BDRs (mesmo os que não tiveram OBSERVAÇÕES não-NA)
  #    tenham sua meta, se a meta_ligacoes_bdr incluir todos eles.
  desempenho_decisores <- left_join(desempenho_decisores, meta_ligacoes_bdr, by = "BDR") |>
    # Se um BDR não teve nenhuma 'OBSERVAÇÃO' não-NA, ele pode aparecer como NA.
    # Preenchemos com 0 para 'total_observacoes' e 'reunioes_agendadas' nesses casos.
    mutate(
      total_observacoes = replace_na(total_observacoes, 0),
      reunioes_agendadas = replace_na(reunioes_agendadas, 0)
    )
  
  
  # 4. Adicionar a linha "Total"
  #    Calcula as somas para todas as colunas relevantes, incluindo a meta total.
  desempenho_decisores <- bind_rows(
    desempenho_decisores,
    desempenho_decisores |>
      summarise(
        BDR = "Total",
        total_observacoes = sum(total_observacoes),
        reunioes_agendadas = sum(reunioes_agendadas),
        meta_ligacoes = sum(meta_ligacoes) # Soma das metas de cada BDR
      )
  )
  
  # 5. Calcular os percentuais de desempenho
  #    Agora os percentuais serão calculados corretamente com a meta definida.
  desempenho_decisores <- desempenho_decisores |>
    mutate(
      # Percentual de ligações: total de observações (válidas) vs. meta total de aparições
      percentual_ligacoes = (total_observacoes / meta_ligacoes),
      # Percentual de agendamentos: reuniões agendadas vs. total de observações válidas
      percentual_agendamentos = (reunioes_agendadas / total_observacoes)
    )
  return(desempenho_decisores)
}

criar_tabela_decisores <- function(desempenho_data, caption_title = "Desempenho Consolidado") {
  
  # Validação básica de input
  colunas_esperadas <- c("BDR", "total_observacoes", "reunioes_agendadas", 
                         "meta_ligacoes", "percentual_ligacoes", "percentual_agendamentos")
  if (!all(colunas_esperadas %in% names(desempenho_data))) {
    stop(paste("O dataframe 'desempenho_data' não possui todas as colunas esperadas. Faltando:",
               paste(setdiff(colunas_esperadas, names(desempenho_data)), collapse = ", ")))
  }
  
  # Garante que NAs em porcentagens ou contagens virem 0 para exibição
  desempenho_data <- desempenho_data %>%
    mutate(across(c(total_observacoes, reunioes_agendadas, meta_ligacoes, 
                    percentual_ligacoes, percentual_agendamentos), 
                  ~ replace_na(., 0)))
  
  # --- Definir os nomes das colunas para exibição ---
  nomes_colunas_display <- c(
    "BDR", "Total Ligações", "Reuniões Agendadas", "Meta Ligações",
    "Percentual Ligações", "Percentual Agendamentos"
  )
  
  # --- Preparar o dataframe para kbl() com formatação e cores ---
  tabela_desempenho_formatada <- desempenho_data %>%
    mutate(
      # Formatar como inteiros e aplicar cores por desempenho
      total_observacoes = as.integer(total_observacoes),
      reunioes_agendadas = as.integer(reunioes_agendadas),
      meta_ligacoes = as.integer(meta_ligacoes),
      
      # Formatar percentuais como strings "X.X%" e aplicar cor
      percentual_ligacoes = cell_spec(
        sprintf("%.1f%%", percentual_ligacoes * 100), 
        format = "html", align = "center",
        background = funcao_cor_desempenho(percentual_ligacoes) # Aplica cor
      ),
      percentual_agendamentos = cell_spec(
        sprintf("%.1f%%", percentual_agendamentos * 100), 
        format = "html", align = "center",
        background = funcao_cor_desempenho(percentual_agendamentos) # Aplica cor
      )
    )
  
  # --- Gerar a Tabela kableExtra ---
  tabela_final <- kbl(
    tabela_desempenho_formatada,
    col.names = nomes_colunas_display,
    escape = FALSE # Crucial para renderizar HTML de cell_spec
  ) |>
    kable_minimal() |>
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    # Alinhar colunas numéricas (todas exceto a primeira)
    column_spec(column = 2:ncol(tabela_desempenho_formatada), extra_css = "text-align: center;") 
  
  return(tabela_final)
}
grafico_decisores <- function(malu, bruna, emilin, stela) {
  malu$OBSERVAÇÃO <- ifelse(malu$OBSERVAÇÃO == "", NA, malu$OBSERVAÇÃO)
  bruna$OBSERVAÇÃO <- ifelse(bruna$OBSERVAÇÃO == "", NA, bruna$OBSERVAÇÃO)
  emilin$OBSERVAÇÃO <- ifelse(emilin$OBSERVAÇÃO == "", NA, emilin$OBSERVAÇÃO)
  stela$OBSERVAÇÃO <- ifelse(stela$OBSERVAÇÃO == "", NA, stela$OBSERVAÇÃO)
  decisores <- bind_rows(malu,stela,bruna,emilin)
  
  decisores |> 
    filter(!is.na(OBSERVAÇÃO)) |> 
    ggplot(aes(x = OBSERVAÇÃO, fill = OBSERVAÇÃO)) +
    geom_bar() +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text()
    ) +
    labs(
      title = "Observações das Ligações",
      x = "Observações",
      y = "Frequência"
    ) +
    guides(fill = "none")
}

calcular_prob_meta_semanal_via_fda_pura_quad <- function(desempenho_mes, metas_semanais_por_responsavel, hoje_data = Sys.Date(), min_semanas_historico = 5) {
  
  # 1. Preparar dados históricos para estimação da distribuição (somar por semana COMPLETA)
  #    Isso garante que temos totais semanais para fitdistr
  desempenho_semanal_historico <- desempenho_mes %>%
    mutate(semana = floor_date(data, unit = "week", week_start = 1)) %>%
    group_by(responsavel, semana) %>%
    summarise(ligacoes_relevantes_semanais = sum(ligacoes_relevantes, na.rm = TRUE), .groups = 'drop') %>%
    # Filtra semanas que não estejam completas no início ou fim do histórico total
    # (ou você pode querer incluir semanas incompletas se fizer sentido para a média)
    filter(semana < floor_date(hoje_data, unit = "week", week_start = 1)) # Apenas semanas completas passadas
  
  # 2. Calcular ligacoes_relevantes já feitas NA SEMANA ATUAL (acumulado)
  ligacoes_relevantes_acumuladas_esta_semana <- desempenho_mes %>%
    filter(data >= floor_date(hoje_data, unit = "week", week_start = 1) & data <= hoje_data) %>%
    group_by(responsavel) %>%
    summarise(relevantes_acumuladas = sum(ligacoes_relevantes, na.rm = TRUE), .groups = 'drop')
  
  # 3. Calcular a probabilidade para cada responsável
  probabilidades_finais_fda <- tibble(responsavel = character(), probabilidade_meta_semanal_fda = numeric())
  
  responsables_unicos <- unique(desempenho_mes$responsavel)
  
  for (resp in responsables_unicos) {
    
    meta_semanal_resp <- metas_semanais_por_responsavel[resp]
    if (is.na(meta_semanal_resp)) {
      warning(paste("Meta semanal não encontrada para", resp, ". Pulando este responsável."))
      next # Pula para o próximo responsável
    }
    
    # Histórico de ligacoes relevantes semanais para este responsável
    hist_relevantes_resp <- desempenho_semanal_historico %>%
      filter(responsavel == resp) %>%
      pull(ligacoes_relevantes_semanais)
    
    # Acumulado desta semana para este responsável
    acumulado_resp <- ligacoes_relevantes_acumuladas_esta_semana %>%
      filter(responsavel == resp) %>%
      pull(relevantes_acumuladas)
    if(length(acumulado_resp) == 0) acumulado_resp <- 0 # Se não tiver acumulado ainda
    
    # Calcular o que PRECISA ser feito nos DIAS RESTANTES da semana
    # Isso é a meta MENOS o que já foi acumulado
    # Problema: A distribuição de uma parte da semana não é a mesma da semana completa
    # Vamos simplificar: vamos assumir que a distribuição do TOTAL da semana é NB.
    # A probabilidade P(TOTAL_SEMANA >= META)
    
    if (length(hist_relevantes_resp) < min_semanas_historico) {
      warning(paste("Poucas semanas de histórico para", resp, ". Não é possível estimar parâmetros NB confiavelmente. Pulando."))
      prob_meta <- NA_real_ # Não há dados suficientes para estimar
    } else {
      # Estimar os parâmetros NB para este responsável (apenas do histórico dele)
      fit_nb_resp <- tryCatch({
        fitdistr(hist_relevantes_resp, "negative binomial")
      }, error = function(e) {
        warning(paste("Erro ao ajustar NB para", resp, ":", e$message, ". Retornando NA."))
        return(NULL)
      })
      
      if (is.null(fit_nb_resp)) {
        prob_meta <- NA_real_
      } else {
        mu_estimado_resp <- fit_nb_resp$estimate["mu"]*4
        size_estimado_resp <- fit_nb_resp$estimate["size"]
        
        # Calcular o parâmetro 'prob' para pnbinom
        prob_param_nbinom_resp <- size_estimado_resp / (size_estimado_resp + mu_estimado_resp)
        
        # Calcular a probabilidade P(Total_Semana >= Meta_Semanal)
        # Se já batemos a meta, prob é 1. Se é impossível bater, prob é 0.
        if (acumulado_resp >= meta_semanal_resp) {
          prob_meta <- 1.0 # Já bateu a meta
        } else {
          # Probabilidade de o TOTAL FINAL da semana ser MAIOR OU IGUAL à meta
          # Assumimos que o total da semana segue a NB estimada
          prob_meta <- 1 - pnbinom(q = meta_semanal_resp - 1, 
                                   size = size_estimado_resp, 
                                   prob = prob_param_nbinom_resp, 
                                   lower.tail = TRUE)
        }
      }
    }
    
    probabilidades_finais_fda <- probabilidades_finais_fda %>%
      add_row(responsavel = resp, probabilidade_meta_semanal_fda = prob_meta)
  }
  
  return(probabilidades_finais_fda %>% mutate(probabilidade_meta_semanal_fda = scales::percent(probabilidade_meta_semanal_fda, accuracy = 0.1)))
}

pzinb <- function(q, pi, mu, size) {
  pi + (1 - pi) * pnbinom(q, size = size, mu = mu)
}

prob_agendamento <- function(desempenho_mes, mu_estimado) {
  modelo_zinb <- zeroinfl(agendamento ~ ligacoes_relevantes, data = desempenho_mes, dist = "negbin")
  
  new_data_for_predict <- data.frame(ligacoes_relevantes = mu_estimado)
  
  pi_zero_predito <- predict(modelo_zinb, newdata = new_data_for_predict, type = "zero")
  
  lambda_predito <- predict(modelo_zinb, newdata = new_data_for_predict, type = "count")
  
  theta_zinb <- modelo_zinb$theta
  
  prob_X_eq_0 <- pi_zero_predito + (1 - pi_zero_predito) * dnbinom(0, size = theta_zinb, mu = lambda_predito)
  
  prob_X_eq_1 <- (1 - pi_zero_predito) * dnbinom(1, size = theta_zinb, mu = lambda_predito)
  
  prob_X_ge_2 <- 1 - prob_X_eq_0 - prob_X_eq_1
  
  print(paste0("Para ligacoes_relevantes = ", round(mu_estimado, 2), " (no cenário):"))
  print(paste0("  Probabilidade P(Agendamentos = 0) = ", round(prob_X_eq_0 * 100, 2), "%"))
  print(paste0("  Probabilidade P(Agendamentos = 1) = ", round(prob_X_eq_1 * 100, 2), "%"))
  print(paste0("  --------------------------------------"))
  print(paste0("  Probabilidade P(Agendamentos >= 2) = ", round(prob_X_ge_2 * 100, 2), "%"))
}

comp_agendamento <- function(desempenho_mes) {
  modelo_zinb <- zeroinfl(agendamento ~ ligacoes_relevantes, data = desempenho_mes, dist = "negbin")
  
  
  lambda_zinb_modelo <- predict(modelo_zinb,
                                newdata = data.frame(
                                  ligacoes_relevantes = mean(desempenho_mes$ligacoes_relevantes,
                                                             na.rm=TRUE)), type = "count")
  theta_zinb_modelo <- modelo_zinb$theta
  pstr0_zinb_modelo <- predict(modelo_zinb,
                               newdata = data.frame(
                                 ligacoes_relevantes = mean(desempenho_mes$ligacoes_relevantes,
                                                            na.rm=TRUE)), type = "zero")
  
  lambda_quad <- lambda_zinb_modelo*4
  
  desempenho_mes$agenda_sim <- rzinegbin(
    n = nrow(desempenho_mes), 
    mu = lambda_zinb_modelo, # Média da parte de contagem
    size = theta_zinb_modelo, # Theta do modelo
    pstr0 = pstr0_zinb_modelo # Probabilidade de zero estrutural
  )
  
  p1 <- desempenho_mes |>
    select(agenda_sim, agendamento) |> # Seleciona apenas as colunas de interesse
    pivot_longer(
      cols = c(agenda_sim, agendamento), # As colunas a serem pivotadas
      names_to = "Tipo_de_Dado",               # Nova coluna para o nome (ex: "simulado", "ligacoes_relevantes")
      values_to = "Valor"                      # Nova coluna para os valores (os números reais)
    ) |> 
    mutate(
      Tipo_de_Dado = case_when(
        Tipo_de_Dado == "agenda_sim" ~ "Simulado",
        Tipo_de_Dado == "agendamento" ~ "Real",
        TRUE ~ Tipo_de_Dado
      ) 
    ) |>
    filter(Valor <= 5) |> 
    ggplot(aes(x = Valor, fill = Tipo_de_Dado, color = Tipo_de_Dado)) + # 'fill' e 'color' mapeiam para 'Tipo_de_Dado'
    geom_density(alpha = 0.5, size = 1) + # 'alpha' para transparência, 'size' para espessura da linha
    
    theme_fivethirtyeight() + # Seu tema preferido
    labs(
      title = "Distribuição Simulada Vs. Real",
      x = "Valores",
      y = "Densidade",
      fill = "Tipo de Dado", # Título da legenda para o preenchimento
      color = "Tipo de Dado" # Título da legenda para a linha (opcional)
    ) +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, face = "bold") # Centraliza e negrita o título
    )
  
  desempenho_mes$agenda_sim_quad <- rzinegbin(
    n = nrow(desempenho_mes), 
    mu = lambda_quad, # Média da parte de contagem
    size = theta_zinb_modelo, # Theta do modelo
    pstr0 = pstr0_zinb_modelo # Probabilidade de zero estrutural
  )
  
  p2 <- desempenho_mes |>
    select(agenda_sim_quad, agendamento) |> # Seleciona apenas as colunas de interesse
    pivot_longer(
      cols = c(agenda_sim_quad, agendamento), # As colunas a serem pivotadas
      names_to = "Tipo_de_Dado",               # Nova coluna para o nome (ex: "simulado", "ligacoes_relevantes")
      values_to = "Valor"                      # Nova coluna para os valores (os números reais)
    ) |> 
    mutate(
      Tipo_de_Dado = case_when(
        Tipo_de_Dado == "agenda_sim" ~ "Simulado",
        Tipo_de_Dado == "agendamento" ~ "Real",
        TRUE ~ Tipo_de_Dado
      ) 
    ) |>
    filter(Valor <= 5) |> 
    ggplot(aes(x = Valor, fill = Tipo_de_Dado, color = Tipo_de_Dado)) + # 'fill' e 'color' mapeiam para 'Tipo_de_Dado'
    geom_density(alpha = 0.5, size = 1) + # 'alpha' para transparência, 'size' para espessura da linha
    
    theme_fivethirtyeight() + # Seu tema preferido
    labs(
      title = "Distribuição Simulada Quadruplicada Vs. Real",
      x = "Valores",
      y = "Densidade",
      fill = "Tipo de Dado", # Título da legenda para o preenchimento
      color = "Tipo de Dado" # Título da legenda para a linha (opcional)
    ) +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, face = "bold") # Centraliza e negrita o título
    )
  grid.arrange(p1,p2, ncol = 2)
}

comp_modelos_lig <- function(desempenho_mes) {
  resultados <- data.frame(
    Real = desempenho_mes$ligacoes_relevantes,
    Predito_lm = 10^(predict(modelo_ligacoes, desempenho_mes)),
    Predito_rf = 10^(predict(modelo_rf, desempenho_mes))
  )
  
  g1 <- resultados |> 
    ggplot(aes(x = Predito_lm, y = Real)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) + # Linha y=x (previsão perfeita)
    labs(
      title = "Valores Reais vs. Valores Preditos lm",
      x = "Ligações Relevantes Preditas",
      y = "Ligações Relevantes Reais"
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text()
    )
  
  g2 <- resultados |> 
    ggplot(aes(x = Predito_rf, y = Real)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) + # Linha y=x (previsão perfeita)
    labs(
      title = "Valores Reais vs. Valores Preditos rf",
      x = "Ligações Relevantes Preditas",
      y = "Ligações Relevantes Reais"
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text()
    )
  
  grid.arrange(g1,g2, ncol = 2)
  
  for (i in 2:length(resultados)) {
    mae <- mean(abs(resultados$Real - resultados[[i]]))
    message(paste("MAE de", names(resultados)[[i]], "=", mae))
  }
}

comp_dis_lig <- function(desempenho_mes) {
  
  qqPlot(
    desempenho_mes$ligacoes_relevantes,        # Seus dados de Ligações Relevantes
    distribution = "nbinom",                 # Comparar com a Distribuição Binomial Negativa
    mu = mu_estimado,                        # Média estimada da sua variável
    size = size_estimado,                    # Parâmetro de dispersão (theta) estimado
    xlab = "Quantis Teóricos da Binomial Negativa",
    ylab = "Quantis Observados de Ligações Relevantes",
    main = "QQ Plot: Ligações Relevantes vs. Binomial Negativa"
  )
  
  qqPlot(
    desempenho_mes$ligacoes_relevantes,        # Seus dados de Ligações Relevantes
    distribution = "pois",                 # Comparar com a Distribuição Binomial Negativa
    lambda = lambda_estimado,
    xlab = "Quantis Teóricos da Poisson",
    ylab = "Quantis Observados de Ligações Relevantes",
    main = "QQ Plot: Ligações Relevantes vs. Poisson"
  )
}
comp_sim_lig <- function(desempenho_mes) {
  desempenho_mes$simulado <- rnbinom(n = nrow(desempenho_mes), size = size_estimado, prob = prob_param_nbinom)
  p1 <- desempenho_mes |>
    select(simulado, ligacoes_relevantes) |> # Seleciona apenas as colunas de interesse
    pivot_longer(
      cols = c(simulado, ligacoes_relevantes), # As colunas a serem pivotadas
      names_to = "Tipo_de_Dado",               # Nova coluna para o nome (ex: "simulado", "ligacoes_relevantes")
      values_to = "Valor"                      # Nova coluna para os valores (os números reais)
    ) |> 
    mutate(
      Tipo_de_Dado = case_when(
        Tipo_de_Dado == "simulado" ~ "Simulado",
        Tipo_de_Dado == "ligacoes_relevantes" ~ "Real",
        TRUE ~ Tipo_de_Dado
      )
    ) |> 
    ggplot(aes(x = Valor, fill = Tipo_de_Dado, color = Tipo_de_Dado)) + # 'fill' e 'color' mapeiam para 'Tipo_de_Dado'
    geom_density(alpha = 0.5, size = 1) + # 'alpha' para transparência, 'size' para espessura da linha
    
    theme_fivethirtyeight() + # Seu tema preferido
    labs(
      title = "Distribuição Simulada Vs. Real",
      x = "Valores",
      y = "Densidade",
      fill = "Tipo de Dado", # Título da legenda para o preenchimento
      color = "Tipo de Dado" # Título da legenda para a linha (opcional)
    ) +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, face = "bold") # Centraliza e negrita o título
    )
  
  desempenho_mes$simulado_quad <- rnbinom(n = nrow(desempenho_mes), size = size_estimado, prob = prob_param_nbinom_quad)
  p2 <- desempenho_mes |>
    select(simulado_quad, ligacoes_relevantes) |> # Seleciona apenas as colunas de interesse
    pivot_longer(
      cols = c(simulado_quad, ligacoes_relevantes), # As colunas a serem pivotadas
      names_to = "Tipo_de_Dado",               # Nova coluna para o nome (ex: "simulado", "ligacoes_relevantes")
      values_to = "Valor"                      # Nova coluna para os valores (os números reais)
    ) |> 
    mutate(
      Tipo_de_Dado = case_when(
        Tipo_de_Dado == "simulado" ~ "Simulado",
        Tipo_de_Dado == "ligacoes_relevantes" ~ "Real",
        TRUE ~ Tipo_de_Dado
      )
    ) |> 
    ggplot(aes(x = Valor, fill = Tipo_de_Dado, color = Tipo_de_Dado)) + # 'fill' e 'color' mapeiam para 'Tipo_de_Dado'
    geom_density(alpha = 0.5, size = 1) + # 'alpha' para transparência, 'size' para espessura da linha
    
    theme_fivethirtyeight() + # Seu tema preferido
    labs(
      title = "Distribuição Simulada Quadruplicada Vs. Real",
      x = "Valores",
      y = "Densidade",
      fill = "Tipo de Dado", # Título da legenda para o preenchimento
      color = "Tipo de Dado" # Título da legenda para a linha (opcional)
    ) +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, face = "bold") # Centraliza e negrita o título
    )
  
  grid.arrange(p1,p2, ncol = 2)
}

calculo_prob_ligacoes <- function() {
  prob_bater_meta <- 1 - pnbinom(q = 24 - 1, size = size_estimado, prob = prob_param_nbinom, lower.tail = TRUE)
  message(paste0("Para média estimada: ","P(X >= ", 24, ") = ", round(prob_bater_meta * 100, 2), "%"))
  prob_bater_meta_quad <- 1 - pnbinom(q = 24 - 1, size = size_estimado, prob = prob_param_nbinom_quad, lower.tail = TRUE)
  message(paste0("Para média quadruplicada: ","P(X >= ", 24, ") = ", round(prob_bater_meta_quad * 100, 2), "%"))
}

calcular_prob_sim_quad_lig <- function(desempenho_mes) {
  sim <-calcular_prob_meta_semanal_via_fda_pura(desempenho_mes, 
                                                metas_semanais_por_responsavel = metas_semanais_por_responsavel, 
                                                hoje_data = as.Date(data_hoje),
                                                min_semanas_historico = 1)
  
  quad <- calcular_prob_meta_semanal_via_fda_pura_quad(desempenho_mes, 
                                                       metas_semanais_por_responsavel = metas_semanais_por_responsavel, 
                                                       hoje_data = as.Date(data_hoje),
                                                       min_semanas_historico = 1) |> 
    select(probabilidade_meta_semanal_fda)
  
  sim$quad <- quad 
  return(sim)
}

criar_tabela_probabilidades_simuladas <- function(prob_data, 
                                                  col_prob1_name, 
                                                  col_prob2_name,
                                                  col_prob1_display_name = "Probabilidade Simulada",
                                                  col_prob2_display_name = "Probabilidade Simulada com Média Quadruplicada") {
  
  # --- 1. Validação de Input ---
  colunas_esperadas_existencia <- c("responsavel", col_prob1_name, col_prob2_name)
  if (!all(colunas_esperadas_existencia %in% names(prob_data))) {
    stop(paste("O dataframe 'prob_data' não possui todas as colunas esperadas. Faltando:",
               paste(setdiff(colunas_esperadas_existencia, names(prob_data)), collapse = ", ")))
  }
  
  if (nrow(prob_data) == 0) {
    warning("O dataframe 'prob_data' está vazio. Retornando tabela placeholder.")
    return(kbl(data.frame(Mensagem = "Dados indisponíveis."), 
               caption = caption_title) |> kable_minimal())
  }
  
  # --- 2. Forçar colunas de probabilidade para character e formatar listas/vetores ---
  prob_data <- prob_data %>%
    mutate(
      !!sym(col_prob1_name) := as.character(!!sym(col_prob1_name)),
      !!sym(col_prob2_name) := sapply(!!sym(col_prob2_name), function(x) {
        if (is.null(x) || all(is.na(x))) return(NA_character_)
        if (is.character(x)) return(x)
        if (is.list(x)) x <- unlist(x)
        paste0(names(x), " = ", x, collapse = "<br>")
      })
    )
  
  # --- 3. Tratar valores NA para exibição ---
  tabela_prob_formatada <- prob_data %>%
    mutate(
      !!sym(col_prob1_name) := replace_na(!!sym(col_prob1_name), "N/A"),
      !!sym(col_prob2_name) := replace_na(!!sym(col_prob2_name), "N/A") 
    )
  
  # --- 4. Definir os nomes das colunas para exibição ---
  nomes_colunas_display <- c(
    "Responsável",
    col_prob1_display_name, 
    col_prob2_display_name  
  )
  
  # --- 5. Aplicar formatação com cell_spec ---
  tabela_prob_formatada <- tabela_prob_formatada %>%
    mutate(
      !!sym(col_prob1_name) := cell_spec(!!sym(col_prob1_name), format = "html", align = "center"),
      !!sym(col_prob2_name) := cell_spec(!!sym(col_prob2_name), format = "html", align = "left")
    ) %>%
    select(responsavel, !!sym(col_prob1_name), !!sym(col_prob2_name))
  
  # --- 6. Gerar tabela kableExtra ---
  tabela_final <- kbl(
    tabela_prob_formatada,
    col.names = nomes_colunas_display,
    escape = FALSE 
  ) |>
    kable_minimal() |>
    kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    column_spec(column = 2, extra_css = "text-align: center;") |>
    column_spec(column = 3, extra_css = "text-align: left;") # Melhor para strings longas com <br>
  
  return(tabela_final)
}
prob_fechamento <- function(dados_mes) {
  modelo_zip_fit_for_dist <- zeroinfl(
    formula = Total_Fechamentos ~ Total_Reunioes | 1, 
    data = dados_mes, 
    dist = "poisson"
  )
  
  lambda_estimado <- exp(coef(modelo_zip_fit_for_dist)[["count_(Intercept)"]])
  pi_estimado <- predict(modelo_zip_fit_for_dist, type = "zero")[1]
  
  prob_X_eq_0 <- pi_estimado + (1 - pi_estimado) * dpois(0, lambda = lambda_estimado)
  prob_X_ge_1 <- 1 - prob_X_eq_0
  
  message(paste0("Probabilidade P(X >= 1) para Total_Fechamentos: ", round(prob_X_ge_1 * 100, 2), "%"))
}

comp_fechamento <- function(dados_mes) {
  modelo_zip_fit_for_dist <- zeroinfl(
    formula = Total_Fechamentos ~ Total_Reunioes | 1, 
    data = dados_mes, 
    dist = "poisson"
  )
  
  lambda_estimado <- exp(coef(modelo_zip_fit_for_dist)[["count_(Intercept)"]])
  pi_estimado <- predict(modelo_zip_fit_for_dist, type = "zero")[1]
  
  dados_mes$fecha_sim <- rzapois(
    n = nrow(dados_mes),
    lambda = lambda_estimado,
    pobs0 = pi_estimado
  )
  
  dados_mes |>
    select(fecha_sim, Total_Fechamentos) |> # Seleciona apenas as colunas de interesse
    pivot_longer(
      cols = c(fecha_sim, Total_Fechamentos), # As colunas a serem pivotadas
      names_to = "Tipo_de_Dado",               # Nova coluna para o nome (ex: "simulado", "ligacoes_relevantes")
      values_to = "Valor"                      # Nova coluna para os valores (os números reais)
    ) |> 
    mutate(
      Tipo_de_Dado = case_when(
        Tipo_de_Dado == "fecha_sim" ~ "Simulado",
        Tipo_de_Dado == "Total_Fechamentos" ~ "Real",
        TRUE ~ Tipo_de_Dado
      ) 
    ) |>
    filter(Valor <= 5) |> 
    ggplot(aes(x = Valor, fill = Tipo_de_Dado, color = Tipo_de_Dado)) + # 'fill' e 'color' mapeiam para 'Tipo_de_Dado'
    geom_density(alpha = 0.5, size = 1) + # 'alpha' para transparência, 'size' para espessura da linha
    
    theme_fivethirtyeight() + # Seu tema preferido
    labs(
      title = "Distribuição Simulada Vs. Real",
      x = "Valores",
      y = "Densidade",
      fill = "Tipo de Dado", # Título da legenda para o preenchimento
      color = "Tipo de Dado" # Título da legenda para a linha (opcional)
    ) +
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      plot.title = element_text(hjust = 0.5, face = "bold") # Centraliza e negrita o título
    )
}

prob_agendamento2 <- function(desempenho_mes, mu_estimado) {
  modelo_zinb <- zeroinfl(agendamento ~ ligacoes_totais, data = desempenho_mes, dist = "negbin")
  
  new_data_for_predict <- data.frame(ligacoes_totais = mu_estimado)
  
  pi_zero_predito <- predict(modelo_zinb, newdata = new_data_for_predict, type = "zero")
  
  lambda_predito <- predict(modelo_zinb, newdata = new_data_for_predict, type = "count")
  
  theta_zinb <- modelo_zinb$theta
  
  prob_X_eq_0 <- pi_zero_predito + (1 - pi_zero_predito) * dnbinom(0, size = theta_zinb, mu = lambda_predito)
  
  prob_X_eq_1 <- (1 - pi_zero_predito) * dnbinom(1, size = theta_zinb, mu = lambda_predito)
  
  prob_X_ge_2 <- 1 - prob_X_eq_0 - prob_X_eq_1
  
  print(paste0("Para ligacoes_relevantes = ", round(mu_estimado, 2), " (no cenário):"))
  print(paste0("  Probabilidade P(Agendamentos = 0) = ", round(prob_X_eq_0 * 100, 2), "%"))
  print(paste0("  Probabilidade P(Agendamentos = 1) = ", round(prob_X_eq_1 * 100, 2), "%"))
  print(paste0("  --------------------------------------"))
  print(paste0("  Probabilidade P(Agendamentos >= 2) = ", round(prob_X_ge_2 * 100, 2), "%"))
}

etl_diario <- function(dados, agendamentos) {
  
  arquivo_historico <- "dados/desempenho_mes.csv"
  data_de_hoje <- Sys.Date()
  
  # Cria a nova linha de dados para o dia atual
  novo_desempenho <- desempenho_hoje(dados, data_de_hoje, agendamentos)
  
  if (file.exists(arquivo_historico)) {
    # Carrega os dados históricos
    dados_historicos <- read.csv(arquivo_historico)
    
    # Converte a coluna 'data' para o tipo Date, se necessário
    if (!inherits(dados_historicos$data, "Date")) {
      dados_historicos$data <- as.Date(dados_historicos$data)
    }
    
    # Remove a linha da data de hoje, se ela já existir
    dados_filtrados <- dados_historicos %>%
      filter(data != data_de_hoje)
    
    # Adiciona a nova linha
    dados_historicos_atualizados <- bind_rows(dados_filtrados, novo_desempenho)
    
  } else {
    # Se não existir, a nova linha é o primeiro dado do histórico
    dados_historicos_atualizados <- novo_desempenho
    print("Arquivo de histórico não encontrado. Criando um novo arquivo.")
  }
  
  # Salva o histórico atualizado no arquivo
  write.csv(dados_historicos_atualizados, arquivo_historico, row.names = FALSE)
  
  return(dados_historicos_atualizados)
}

etl_diario_closer <- function(dados) {
  
  arquivo_historico <- "dados/closer_mes.csv"
  data_de_hoje <- Sys.Date()
  
  # Cria a nova linha de dados para o dia atual
  novo_desempenho <- closer_hoje(dados, data_de_hoje)
  
  if (file.exists(arquivo_historico)) {
    # Carrega os dados históricos
    dados_historicos <- read.csv(arquivo_historico)
    
    # Converte a coluna 'data' para o tipo Date, se necessário
    if (!inherits(dados_historicos$Data, "Date")) {
      dados_historicos$Data <- as.Date(dados_historicos$Data)
    }
    
    # Remove a linha da data de hoje, se ela já existir
    dados_filtrados <- dados_historicos %>%
      filter(Data != data_de_hoje)
    
    # Adiciona a nova linha
    dados_historicos_atualizados <- bind_rows(dados_filtrados, novo_desempenho)
    
  } else {
    # Se não existir, a nova linha é o primeiro dado do histórico
    dados_historicos_atualizados <- novo_desempenho
    print("Arquivo de histórico não encontrado. Criando um novo arquivo.")
  }
  
  # Salva o histórico atualizado no arquivo
  write.csv(dados_historicos_atualizados, arquivo_historico, row.names = FALSE)
  
  return(dados_historicos_atualizados)
}
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
