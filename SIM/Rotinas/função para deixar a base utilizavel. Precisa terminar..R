



source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIM/Rotinas/funcao_criacao_base_sim_duckdb.R")



importar_empilhar_salvar_sim(
  anos_lista = c(2013:2015),
  pasta_dbc = here::here("Bases/sim/dbc"),
  pasta_duckdb = here::here("Bases/sim/duckdb/sim.duckdb"),
  tabela = "sim_br")
rm(list = setdiff(ls(), c("ocupacao","munics") ) ); gc()
beepr::beep(sound = 1)



library(tidyverse)
library(duckdb)




con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)

data <- 
  tbl(con, "sim_br")


data |>
  filter(is.na(def_ocup)) |>
  count(ocup)


data %>% summarize(across(everything(),
                          ~sum(is.na(.x)))) |> 
  view()



dbDisconnect(con) ; gc()