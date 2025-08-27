# Imagem base com R
FROM rocker/r-ver:4.3.1

# Instala dependências do sistema necessárias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Define diretório de trabalho
WORKDIR /app

# Instala pacotes essenciais do R
RUN R -e "install.packages(c('plumber', 'dplyr', 'jsonlite', 'lubridate', 'forcats', 'googlesheets4', 'googledrive', 'gargle'), repos='https://cloud.r-project.org')"

# Copia os arquivos da aplicação
COPY api_desempenho.R /app/api_desempenho.R
COPY funcoes.R /app/funcoes.R

# Variáveis de ambiente para autenticação Google
ENV GARGLE_OAUTH_CACHE_PATH=/tmp/.gargle
ENV GARGLE_OAUTH_EMAIL_HINT=""
ENV GARGLE_OAUTH_CLIENT_TYPE="service_account"

# Cria diretórios temporários
RUN mkdir -p /tmp/.gargle /app/logs

# Expõe a porta que a API vai rodar
EXPOSE 8000

# Health check simples
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# Comando para iniciar a API
CMD ["R", "-e", "pr <- plumber::plumb('api_desempenho.R'); pr$run(host='0.0.0.0', port=8000)"]
