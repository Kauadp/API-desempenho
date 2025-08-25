# start.R - Script de inicialização para Railway
cat("🚀 Iniciando API no Railway...\n")

# Carrega bibliotecas com verificação
required_packages <- c("plumber", "jsonlite", "googlesheets4", "dplyr", "lubridate")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Pacote", pkg, "não encontrado!"))
  } else {
    cat("✓", pkg, "carregado\n")
  }
}

# Pega a porta do Railway
port <- as.numeric(Sys.getenv("PORT", 8000))
cat("🌐 Porta configurada:", port, "\n")

# Verifica se o arquivo da API existe
if (!file.exists("api.R")) {
  stop("❌ Arquivo api.R não encontrado!")
}

# Carrega e executa a API
cat("📡 Carregando API...\n")
pr <- plumber::plumb("api.R")

# Configurações adicionais para produção
pr$setDocs(TRUE)  # Habilita documentação Swagger
pr$setDebug(FALSE)  # Desabilita debug em produção

cat("🎯 Iniciando servidor na porta", port, "...\n")
pr$run(host = "0.0.0.0", port = port)