library(RCurl)
library(stringr)
library(digest)


here::i_am("SIA/Rotinas/criação_base_sia.R")

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



# Função de importar, empilhar e trata - SIA ------------------------------
Sys.setenv(LANG = "en")
source("C:/Users/gabli/Desktop/r/SIS/SIA/Rotinas/funcao_tratar_sia.R")
importar_empilhar_salvar_sia <- function(
    uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
           "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
           "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
    anos,
    meses = 1:12,
    pasta_dbc = here::here("Bases/sia/dbc"), #Pasta de armanzemaneto dos dbcs
    pasta_duckdb, #Pasta de armazenamento do duckdb
    tabela) { #Nome da tabela duckdb) 
  
 tictoc::tic()  
  
  #Conexão com o DuckDB
  con <- duckdb::dbConnect(
    duckdb::duckdb(),
    dbdir = pasta_duckdb,
    read_only = FALSE)
  
  #Termina a conexão a finalizar a função.
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))
  
  #Controle de colunas.
  colunas_sim <- NULL
  tabela_criada <- FALSE
  
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
          tratar_sia() |> janitor::clean_names() 
        
        # >>> Aqui entra o passo de salvar no DuckDB depois <<<
        message("Arquivo processado: ", basename(arquivos[1]))
        
        #return(tmp); gc()
        
        # #Cria a tabela no primeiro bloco
        if (!tabela_criada) {
          duckdb::dbWriteTable(
            con,
            name = tabela,
            value = tmp,
            overwrite = TRUE,
            temporary = FALSE
          )

          colunas_sim <- names(tmp)
          tabela_criada <- TRUE

        } else {
          # Detecta colunas novas
          novas_colunas <- setdiff(names(tmp), colunas_sim)
          if (length(novas_colunas) > 0) {
            message("Novas colunas detectadas: ", paste(novas_colunas, collapse = ", "))
            for (col in novas_colunas) {
              DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", tabela, col))
            }
            colunas_sim <- union(colunas_sim, novas_colunas)
          }

          #Append
          DBI::dbWriteTable(
            con,
            name = tabela,
            value = tmp,
            append = TRUE,
            temporary = FALSE
          )
        }

        rm(tmp); gc()
        
      }
    }
 }
  beepr::beep(); tictoc::toc()
}




importar_empilhar_salvar_sia(
  anos = c(2010:2015),
  mes = c(1:12),
  uf = c("AC","RR"),
  pasta_dbc = here::here("Bases/sia/dbc"),
  pasta_duckdb = here::here("Bases/sia/duckdb/sia.duckdb"),
  tabela = "sia_br")
rm(list = ls() )

library(tidyverse)
library(duckdb)
library(data.table)




Olhar variável 
nome_proced, nome_ocupacao, municipality_data


con <- DBI::dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sia/duckdb/sia.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)

data <- 
  tbl(con, "sia_br")


data |>
  count(pa_ufmun, sort = TRUE)



DBI::dbDisconnect(con, shutdown = TRUE) ; gc()
rm(list = setdiff(ls(), c("ocupacao","munics") ) ); gc()




# 3. Remova o arquivo
file.remove(here::here("Bases/sia/duckdb/sia.duckdb"))

beepr::beep(sound = 1)
