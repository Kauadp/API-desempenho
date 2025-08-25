# Imagem base do R
FROM r-base:latest

# Instala plumber (obrigatório para rodar a API)
RUN R -e "install.packages('plumber', repos='https://cloud.r-project.org/')"

# Copia os arquivos da API
COPY api_desempenho.R /api_desempenho.R
COPY funcoes.R /funcoes.R

# Expor a porta que o Plumber vai usar
EXPOSE 8000

# Comando para rodar a API
CMD ["R", "-e", "pr <- plumber::plumb('/api_desempenho.R'); pr$run(host='0.0.0.0', port=8000)"]
