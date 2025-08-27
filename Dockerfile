# Dockerfile alternativo mais robusto
FROM rocker/plumber:4.3.1

# Instala dependências do sistema em uma única camada
RUN apt-get update && apt-get install -y \
    build-essential \
    gcc g++ gfortran make cmake pkg-config \
    libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev \
    libcairo2-dev libfontconfig1-dev libfreetype6-dev \
    libpng-dev libtiff-dev libjpeg62-turbo-dev \
    libgit2-dev libharfbuzz-dev libfribidi-dev \
    libudunits2-dev libgdal-dev libproj-dev libgeos-dev \
    wget \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean


# Define diretório de trabalho
WORKDIR /app

# Configura opções do R para compilação
ENV _R_SHLIB_STRIP_=true
ENV MAKEFLAGS="-j2"

# Cria arquivo de configuração R personalizado
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' > /usr/local/lib/R/etc/Rprofile.site && \
    echo 'options(timeout = 300)' >> /usr/local/lib/R/etc/Rprofile.site && \
    echo 'options(Ncpus = 2)' >> /usr/local/lib/R/etc/Rprofile.site

# Instala pacotes R em ordem específica para evitar problemas de dependência

# 1. Pacotes base essenciais
RUN R -e "install.packages(c('remotes', 'pak'), dependencies=TRUE)"

# 2. Pacotes básicos da API
RUN R -e "pak::pkg_install(c('plumber', 'dplyr', 'forcats', 'jsonlite', 'lubridate'))"

# 3. Dependências HTTP e JSON
RUN R -e "pak::pkg_install(c('httr', 'httr2', 'curl', 'openssl', 'base64enc', 'mime'))"

# 4. Gargle (base do Google) com configurações específicas
RUN R -e "pak::pkg_install('gargle')"

# 5. Google Drive
RUN R -e "pak::pkg_install('googledrive')"

# 6. Google Sheets
RUN R -e "pak::pkg_install('googlesheets4')"

# Copia arquivos da aplicação
COPY api_desempenho.R /app/api_desempenho.R
COPY funcoes.R /app/funcoes.R

# Define variáveis de ambiente para Google Auth
ENV GARGLE_OAUTH_CACHE_PATH=/tmp/.gargle
ENV GARGLE_OAUTH_EMAIL_HINT=""
ENV GARGLE_OAUTH_CLIENT_TYPE="service_account"

# Cria diretórios necessários
RUN mkdir -p /tmp/.gargle /app/logs

# Expõe a porta
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8000/health || exit 1

# Comando para executar
CMD ["R", "-e", "pr <- plumber::plumb('api_desempenho.R'); pr$run(host='0.0.0.0', port=8000)"]