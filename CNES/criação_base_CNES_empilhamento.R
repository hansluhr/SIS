Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Função para baixar os dbcs
#source(file = "C:/Users/gabli/Dropbox/Ipea/Atlas/Rotinas/SIS/CNES/cnes_baixar_dbc_ftp.R")
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/CNES/cnes_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do CNES estabelecimentos serão salvos.
caminho_dbc <- "C:/Users/gabli/Desktop/r/CNES/dbc"

#Baxiar arquivos dbcs do CNES Estabelecimentos
baixar_cnes(anos = c(2008:2025) , meses = c(1:12), ufs = 'AC', 
            destino = caminho_dbc); gc()
rm(baixar_cnes)


#Lista todos os arquivos .dbc
arquivos_dbc <- list.files(
  path = caminho_dbc,
  pattern = "\\.dbc$",
  full.names = TRUE)


# Paralelo
#plan(multisession, workers = availableCores() - 2)
tictoc::tic()

#Lê e empilha todos os arquivos
cnes <- data.table::rbindlist(
  
  future.apply::future_lapply(arquivos_dbc, function(arq) {
    
    message("Importando: ", basename(arq) )
    read.dbc::read.dbc(arq) |> data.table::setDT() } ),
  
    use.names = TRUE, fill = TRUE) |> janitor::clean_names(); gc()


# ### Remover arquivos dbcs
# unlink(arquivos_dbc)


rm(arquivos_dbc,pasta_dbc)
#Finaliza.
#plan(sequential) #Resetar plano sequencial ao final
tictoc::toc()






cnes |>
  count(cnes, sort = TRUE)










library(tidyverse)
library(janitor)
load("C:/Users/gabli/Desktop/cnes.RData")







Existe cnes duplicado. A 


cnes |>
  count(cnes, sort = TRUE) |>
  count(n)


cnes |>
  summarise(across(everything(), ~ mean(is.na(.x) ) ) )

#Acre
cnes |> 
  filter(str_detect(codufmun, "12") ) |>
  select(cnes, dt_atual, competen) |>
  mutate(across( c(dt_atual, competen ), ~ as.Date(paste0(as.character(.), "01"), format = "%Y%m%d")))


cnes |> select(dt_atual) |>
  mutate(dt_atual = as.Date(as.character(dt_atual), format = "%Y%m/") )
