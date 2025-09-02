Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)

here::i_am("SIM/Rotinas/criacao_base_sim.R")


# Download dbcs SIM -------------------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIM/Rotinas/sim_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
baixar_dbc_sim(anos = c(1996:2025), 
              destino = here::here("Bases/sim/dbc") )
rm(baixar_dbc_sim)




# Elaboração base SIM -----------------------------------------------------

#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(), 
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)



