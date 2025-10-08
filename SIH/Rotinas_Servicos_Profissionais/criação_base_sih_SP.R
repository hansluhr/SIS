Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)

here::i_am("SIH/Rotinas_Reduzida/criação_base_sih.R")

# Download dos dbcs SIH reduzida  -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/Rotinas_comuns_sih/sih_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
#Baixar arquivos dbcs SIH
baixar_dbc_sih(anos = c(2014:2025), 
               meses = c(1:12), 
               ufs = c("RJ"), 
               destino = here::here("Bases/sih/dbc_servicos_prof"),
               tipo  = "reduzida")
rm(baixar_dbc_sih)
