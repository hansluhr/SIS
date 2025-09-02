Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Pasta do projeto onde a rotina utilizada está armazenada.
here::i_am("SIH/Rotinas_Rejeitada/criação_base_sih_rejeitada.R")



#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sih/duckdb/sih_rejeitada.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)


#Importação da tabela de municípios
#source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")

#Importação função de tratamento e empilhamto SIH
source("C:/Users/gabli/Desktop/r/SIS/SIH/Rotinas_Rejeitada/funcao_tratamento_empilhamento_sih_rejeitada.R")


dir_dbc,
dir_duckdb,
erros, 
munics,
vars_excluir 




sih_rejeitada <- function(ufs = "all") {
  
  #Vetor de todas as UFs
  todas_ufs <- c(
    "AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
    "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
    "RS","SC","SE","SP","TO")
  
  
  # Se "all", substitui pela lista completa
  if (identical(tolower(ufs), "all")) {
    ufs <- todas_ufs
  }
  
  for(uf in ufs) {
    
  ufs_dbc <-  list.files(
    path = here::here("Bases/sih/dbc_rejeitada"), #Onde estão os dbcs de aihs rejeitadas
    full.names = TRUE,
    pattern = paste0(".*ER", uf) ) 
  }
  
  for(uf_dbc in ufs_dbc) {
  
  tmp <-
    #Iportação e empilhamento dos dbcs e exclusão de variáveis.
    data.table::rbindlist(
      
      future_lapply(ufs_dbc, importar_sih,
                    #Variáveis excluídas. Sem utilidade ou redundante
                    vars_excluir <- c("SEQUENCIA", "REMESSA") ),
      
      use.names = TRUE, fill = TRUE ) |>
    #Adicona labels
    tratar_sih_rejeitada() |>
    
    janitor::clean_names()
  }
  print(tmp)
}



sih_rejeitada(ufs = c("AC","AP"),
              vars_excluir = c("SEQUENCIA", "REMESSA") )

