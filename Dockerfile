# Dockerfile enxuto para API de desempenho
FROM r-base:4.3.1

# Instala apenas dependências do sistema necessárias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Define diretório de trabalho
WORKDIR /app

# Copia arquivos da aplicação
COPY api_desempenho.R /app/api_desempenho.R
COPY funcoes.R /app/funcoes.R

# Instala pacotes R leves (compilação rápida)
RUN R -e "install.packages(c('plumber','dplyr','forcats','jsonlite','jose'), repos='https://cloud.r-project.org')"

# Instala pacotes pesados individualmente (evita timeout)
RUN R -e "install.packages('lubridate', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('googledrive', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('googlesheets4', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('tibble', repos='https://cloud.r-project.org')"
RUN R -e "install.packages('httr', repos='https://cloud.r-project.org')"
# Expõe porta
EXPOSE 8000

# Comando para rodar a API
CMD ["R", "-e", "pr <- plumber::plumb('api_desempenho.R'); pr$run(host='0.0.0.0', port=8000)"]
