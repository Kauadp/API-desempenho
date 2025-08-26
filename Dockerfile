# Dockerfile simplificado usando imagem com mais ferramentas pré-instaladas
FROM rocker/tidyverse:4.3.1

# Instala apenas as dependências específicas que faltam
RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Define diretório de trabalho
WORKDIR /app

# Instala pacotes R usando install2.r (mais eficiente)
RUN install2.r --error \
    plumber \
    gargle \
    googledrive \
    googlesheets4 \
    && rm -rf /tmp/downloaded_packages/

# Copia arquivos da aplicação
COPY api_desempenho.R /app/api_desempenho.R
COPY funcoes.R /app/funcoes.R

# Define variáveis de ambiente
ENV GARGLE_OAUTH_CACHE_PATH=/tmp/.gargle
ENV GARGLE_OAUTH_EMAIL_HINT=""

# Cria diretórios necessários
RUN mkdir -p /tmp/.gargle

# Expõe porta
EXPOSE 8000

# Comando para executar
CMD ["R", "-e", "pr <- plumber::plumb('api_desempenho.R'); pr$run(host='0.0.0.0', port=8000)"]