Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Pasta do projeto onde a rotina utilizada está armazenada.
here::i_am("CNES/Rotinas/criação_base_CNES.R")

#Função para baixar os dbcs
source("C:/Users/gabriel.accioly/Desktop/r/SIS/CNES/Rotinas/cnes_baixar_dbc_ftp.R")


#Baxiar arquivos dbcs do CNES 
baixar_cnes(anos = c(2025) ,
            meses = c(1:12),
            ufs = c('AC',"RR"),
            cnes = "Estabelecimentos", #Estabelecimentos, Equipamentos, Equipes, leitos
            destino = here::here("bases/cnes/dbc")); gc() #Pasta onde os arquivos DBCs do CNES serão salvos.
rm(baixar_cnes)

#Empilhar base CNES. --------------------------------------------


source("C:/Users/gabriel.accioly/Desktop/r/SIS/CNES/Rotinas/funçao_empilhamento_cnes_duckdb.R")


importar_empilhar_salvar_cnes(
  anos = c(2025),
  mes = c(1),
  uf = c("AC"),
  #Informar o tipo de cnes ("Estabelecimentos","Equipamentos", "Equipes", "Leitos")
  cnes = "Estabelecimentos",
  #tabela = "cnes_br",
  pasta_dbc = here::here("Bases/cnes/dbc") )
  #pasta_duckdb = here::here("Bases/cnes/duckdb/cnes.duckdb"),
  #tabela = "cnes_br")





con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = here::here("Bases/cnes/duckdb/cnes_estabelecimentos.duckdb"), #Nome do database que armazena o SIH
                      read_only = FALSE)

data <- 
  tbl(con, "cnes_estabelecimentos")

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
file.remove(here::here("Bases/cnes/duckdb"))

































# #Abre conexão com a database. Este arquivo armazena a base SIH.
# con <- dbConnect(duckdb::duckdb(),
#                  dbdir = here::here("Bases/sih/duckdb/cnes.duckdb"), 
#                  read_only = FALSE)
# 
# #UFs para empilhar. Colocar todas as UFs desejadas.
# ufs_lista <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", 
#                "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
# #Dentre as UFs desejadas, àquelas para empilhar em blocos. Por causa da limitação de memória.
# ufs_em_blocos <- c("BA","DF","MG","RJ")
# 
# 
# #Lista todos os arquivos .dbc
# arquivos_dbc <- list.files(
#   path = here::here("CNES/bases/dbc"),
#   pattern = "\\.dbc$", full.names = TRUE)
# 
# 
# # Paralelo
# #plan(multisession, workers = availableCores() - 2)
# 
# #Lê e empilha todos os arquivos
# cnes <- data.table::rbindlist(
#   
#   future.apply::future_lapply(arquivos_dbc, function(arq) {
#     
#     message("Importando: ", basename(arq) )
#     read.dbc::read.dbc(arq) |> data.table::setDT() } ),
#   
#     use.names = TRUE, fill = TRUE) |> janitor::clean_names(); gc()
# 
# 
# # ### Remover arquivos dbcs
# # unlink(arquivos_dbc)
# 
# 
# rm(arquivos_dbc,pasta_dbc)
# #Finaliza.
# #plan(sequential) #Resetar plano sequencial ao final
# tictoc::toc()
# 
# 
# 
# 
# 
# 
# cnes |>
#   count(cnes, sort = TRUE)
# 
# library(tidyverse)
# library(janitor)
# load("C:/Users/gabli/Desktop/cnes.RData")


# 
# 
# 
# Existe cnes duplicado. A 
# 
# 
# cnes |>
#   count(cnes, sort = TRUE) |>
#   count(n)
# 
# 
# cnes |>
#   summarise(across(everything(), ~ mean(is.na(.x) ) ) )
# 
# #Acre
# cnes |> 
#   filter(str_detect(codufmun, "12") ) |>
#   select(cnes, dt_atual, competen) |>
#   mutate(across( c(dt_atual, competen ), ~ as.Date(paste0(as.character(.), "01"), format = "%Y%m%d")))
# 
# 
# cnes |> select(dt_atual) |>
#   mutate(dt_atual = as.Date(as.character(dt_atual), format = "%Y%m/") )
