library(RCurl)
library(stringr)
library(digest)


here::i_am("SIA/Rotinas/criar_sia.R")

# Download dos dbcs SIA -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIA/Rotinas/sia_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
#Baixar arquivos dbcs SIH
baixar_dbc_sia(anos = c(2010:2025), 
               meses = c(1:12), 
               ufs = c("DF"), 
               destino = here::here("Bases/sia/dbc") )
rm(baixar_dbc_sia)

Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)



source("C:/Users/gabli/Desktop/r/SIS/Rotinas Gerais/funcao_cod_procedimentos_SUS.R")
source("C:/Users/gabli/Desktop/r/SIS/Rotinas Gerais/funcao_cod_ocupacoes.R")

source("C:/Users/gabli/Desktop/r/SIS/Rotinas Gerais/funcao_importar_munics.R")
source("C:/Users/gabli/Desktop/r/SIS/SIA/Rotinas/funcao_criação_base_sia_duckdb.R")

importar_empilhar_salvar_sia(
  anos = c(2015:2025),
  mes = c(1:12),
  uf = c("AC"),
  pasta_dbc = here::here("Bases/sia/dbc"),
  pasta_duckdb = here::here("Bases/sia/duckdb/sia.duckdb"),
  tabela = "sia_br")


rm(list = ls() )

Olhar variável 
nome_proced, nome_ocupacao


con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = here::here("Bases/sia/duckdb/sia.duckdb"), #Nome do database que armazena o SIH
                      read_only = FALSE)

data <- 
  tbl(con, "sia_br")

data |>
  filter(is.na(def_munic_munpcn)) |>
  collect() |> view()



data |>
  summarise(x = sum(is.na(def_munic_munpcn))/n())


data |>
  filter(is.na(def_munic_munpcn)) |> view()


DBI::dbDisconnect(con, shutdown = TRUE) ; gc()
rm(list = setdiff(ls(), c("ocupacao","munics","procedimentos") ) ); gc()




# 3. Remova o arquivo
file.remove(here::here("Bases/sia/duckdb/sia.duckdb"))

beepr::beep(sound = 1)
