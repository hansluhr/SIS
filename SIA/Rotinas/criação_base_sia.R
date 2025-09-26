library(RCurl)
library(stringr)
library(digest)


here::i_am("SIA/Rotinas/criação_base_sia.R")

# Download dos dbcs SIA -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("C:/Users/gabli/Desktop/r/SIS/SIA/Rotinas/sia_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
#Baixar arquivos dbcs SIH
baixar_dbc_sia(anos = c(2025), 
               meses = c(1:12), 
               ufs = c("AC","GO"), 
               destino = here::here("Bases/sia/dbc") )
rm(baixar_dbc_sia)



# Função de importar, empilhar e trata - SIA ------------------------------
importar_empilhar_salvar_sia <- function(
    uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
           "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
           "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
    anos,
    meses = 1:12,
    pasta_dbc
) {
  
  for (u in uf) {
    for (a in anos) {
      ano2 <- sprintf("%02d", a %% 100)
      for (m in meses) {
        mes2 <- sprintf("%02d", m)
        
        padrao <- paste0("^PA", u, ano2, mes2)
        
        arquivos <- list.files(
          path = pasta_dbc,
          pattern = paste0(padrao, ".*\\.dbc$"),
          full.names = TRUE
        )
        
        if (length(arquivos) == 0) {
          message("Nenhum arquivo encontrado para ", u, "-", a, "-", m)
          next
        }
        
        # Lê e trata o primeiro arquivo encontrado
        tmp <- read.dbc::read.dbc(arquivos[1]) |>
          data.table::setDT() |>
          janitor::clean_names()
        
        # >>> Aqui entra o passo de salvar no DuckDB depois <<<
        message("Arquivo processado: ", basename(arquivos[1]))
      }
    }
  }
}




importar_empilhar_salvar_sia(
  anos = c(2025),
  mes = c(1:12),
  uf = c("AC"),
  pasta_dbc = "C:/Users/gabli/Desktop/r/SIS/Bases/sia/dbc" )


importar_empilhar_salvar_sia(
  anos = c(2025),
  mes = c(1:12),
  ufs = 
  pasta_dbc = here::here("Bases/sia/dbc"),
  pasta_duckdb = here::here("Bases/sia/duckdb/sia.duckdb"),
  tabela = "sia_br")


rm(list = setdiff(ls(), c("ocupacao","munics") ) ); gc()
beepr::beep(sound = 1)

con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)

data <- 
  tbl(con, "sim_br")

dbDisconnect(con) ; gc()

beepr::beep(sound = 1)
