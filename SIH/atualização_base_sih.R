# Rotina de atualização da base SIH ---------------------------------------
Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Essa rotina atualiza base existente do SIH. 




#Abre conexão com a database
con <- dbConnect(duckdb::duckdb(), dbdir = "C:/Users/gabriel.accioly/Desktop/r/FioCruz/bases/sih/sih_08_abr_25.duckdb", 
                 read_only = FALSE)
data <- tbl(con, "sih")


#Ano e mês da última atualização.
last_year_month <- data |>
  select(ano_mes_cmpt) |>
  mutate(ano_mes_cmpt = ano_mes_cmpt |> as.Date() ) |>
  distinct(ano_mes_cmpt) |> arrange( desc(ano_mes_cmpt) ) |> first() |> collect() |> pull() 


Preciso transformar essa informação para ela entrar na função de baixar e tratar os dbcs




data |> filter(ano_mes_cmpt == last_year_month ) 
select(ano_mes_cmpt) |>
  mutate(mes_)




data |> colnames()

glimpse(data)

#Número de linhas 1,542,444
data |> view()

#Qual o procedimento de maior frequência?
data |>
  count(cod_proc_rea, def_proc_rea, sort = TRUE)


data |>
  dplyr::filter(cod_proc_rea %in% c(310010039,411010034,411010026) ) |>
  count(cod_proc_rea, def_proc_rea, def_sexo, sort  = TRUE)


data |>
  mutate(val_tot = as.numeric(val_tot) ) |>
  summarise(x = mean(val_tot, na.rm = TRUE), .by = uf_resd )

library(duckplyr)
data |>
  as_duckdb_tibble() |> glimpse()
data |>
  count(ano_cmpt,nat_jur) |> view()


#Usar duckdb para manipulação
https://duckplyr.tidyverse.org/
  https://duckplyr.tidyverse.org/articles/limits.html


dbDisconnect(con)
rm(list=ls()); gc()
