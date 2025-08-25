# Use a imagem oficial do R
FROM r-base:latest

# Instala dependências do sistema
RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    && rm -rf /var/lib/apt/lists/*

# Instala os pacotes R necessários
RUN R -e "install.packages(c('plumber','googlesheets4','jsonlite'), repos='https://cloud.r-project.org/')"

# Copia os arquivos da API
COPY . /app
WORKDIR /app

# Expõe a porta da API
EXPOSE 8000

# Comando para rodar a API
CMD ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"]
