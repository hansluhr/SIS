library(RCurl)
library(stringr)
library(digest)


here::i_am("SIH/Rotinas_Reduzida/criação_base_sih.R")

# Download dos dbcs SIH reduzida  -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("C:/Users/gabli/Desktop/r/SIS/SIA/sia_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
#Baixar arquivos dbcs SIH
baixar_dbc_sih(anos = c(2008:2025), 
               meses = c(1:12), 
               ufs = c("AC"), 
               destino = here::here("Bases/sih/dbc_reduzida"),
               tipo  = "reduzida")
rm(baixar_dbc_sih)


