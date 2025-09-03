Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)

here::i_am("SIM/Rotinas/criacao_base_sim.R")


# Download dbcs SIM -------------------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/sim_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
baixar_dbc_sim(anos = c(1996:2025), 
              destino = here::here("Bases/sim/dbc") )
rm(baixar_dbc_sim)




# Elaboração base SIM -----------------------------------------------------

#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(), 
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)

#Importação da tabela de municípios
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")




sim |>
  count(CRM, sort = TRUE)




library(read.dbc)
library(data.table)

rm(list = ls()); gc()
#Variáveis zeradas.
vars_excluir <- c("TPASSINA","NUMERODN","ESTABDESCR")
source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/funcao_tratar_empilhar_sim.R")


# Função para importar e empilhar todos os .dbc de uma pasta
importar_dbc <- function(pasta) {
  # lista de arquivos dbc na pasta
  arquivos <- list.files(
    path = pasta,
    pattern = "\\.dbc$",
    full.names = TRUE
  )
  
  if (length(arquivos) == 0) {
    stop("❌ Nenhum arquivo .dbc encontrado na pasta.")
  }
  
  message("📂 Encontrados ", length(arquivos), " arquivos .dbc")
  
  # importa todos e empilha
  dados <- 
    data.table::rbindlist(
      future_lapply(arquivos, empilhar_sim),
      use.names = TRUE, fill = TRUE ) |>
    tratar_sim() |>
    janitor::clean_names()
    
  message("✅ Importação concluída! Total de registros: ", nrow(dados))
  return(dados)
}


sim <- importar_dbc("C:/Users/gabli/Desktop/r/SIS/Bases/sim/dbc")
glimpse(sim)

sim |>
  summarise(across(everything(), ~ mean(is.na(.x) ) ) )


sim |>
  filter(!is.na(CAUSAMAT)) |>
  view()

sim |> colnames() |> as_tibble() |> filter(!str_starts(value, "DT"))


# var_char <-
  
 cat( paste0('"', names(sim)[!str_starts(names(sim), "DT")], '"', collapse = ", ") )
