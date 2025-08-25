# Use uma versão específica do R para garantir compatibilidade
FROM r-base:4.3.0

# Instala dependências do sistema
RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Define o CRAN mirror
ENV CRAN_MIRROR=https://cloud.r-project.org/

# Instala os pacotes R um por vez para melhor debugging
RUN R -e "options(repos = c(CRAN = '$CRAN_MIRROR')); install.packages('jsonlite', dependencies = TRUE)"
RUN R -e "options(repos = c(CRAN = '$CRAN_MIRROR')); install.packages('plumber', dependencies = TRUE)" 
RUN R -e "options(repos = c(CRAN = '$CRAN_MIRROR')); install.packages('googlesheets4', dependencies = TRUE)"
RUN R -e "options(repos = c(CRAN = '$CRAN_MIRROR')); install.packages('dplyr', dependencies = TRUE)"
RUN R -e "options(repos = c(CRAN = '$CRAN_MIRROR')); install.packages('lubridate', dependencies = TRUE)"

# Cria diretório de trabalho
WORKDIR /app

# Copia arquivos
COPY . .

# Railway usa a variável de ambiente PORT
EXPOSE $PORT

# Comando para executar a API com port dinâmico do Railway
CMD ["R", "-e", "library(plumber); port <- as.numeric(Sys.getenv('PORT', 8000)); pr <- plumb('api.R'); pr$run(host='0.0.0.0', port=port)"]