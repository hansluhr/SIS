library(tidyverse)
library(janitor)



# Importando a base -------------------------------------------------------


base <- read.dbc::read.dbc("C:/Users/P224552695/Desktop/r/SIS/Bases/SINAN_Tuberculose/TUBEBR24.dbc") |>
  clean_names()

importar_empilhar_dbc <- function(pasta_dbc) {
  # Carregar pacotes necessários
  requireNamespace("read.dbc", quietly = TRUE)
  requireNamespace("data.table", quietly = TRUE)
  requireNamespace("janitor", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  
  # Lista todos os arquivos .dbc na pasta (recursivamente, se desejar)
  arquivos <- list.files(
    path = pasta_dbc,
    pattern = "\\.dbc$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .dbc encontrado na pasta especificada.")
  }
  
  message("🔹 ", length(arquivos), " arquivos encontrados. Importando...")
  
  # Lê, converte e limpa cada arquivo
  lista_dados <- lapply(arquivos, function(arq) {
    message("   → Lendo: ", basename(arq))
    tmp <- read.dbc::read.dbc(arq)
    data.table::setDT(tmp)
    janitor::clean_names(tmp)
  })
  
  #Empilha todos em um único data.table
  dados_empilhados <- data.table::rbindlist(lista_dados, use.names = TRUE, fill = TRUE)
  
  message("✅ Importação concluída! Total de linhas: ", nrow(dados_empilhados))
  return(dados_empilhados)
  
  
}


base_sia <- importar_empilhar_dbc("C:/Users/P224552695/Desktop/r/SIS/Bases/SINAN_Tuberculose")



# Tratamento base SIA tuberculose -----------------------------------------


