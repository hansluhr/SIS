Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


# Rotina de criação da base SIH -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/sih_baixar_dbc_ftp.R")

#Pasta onde os arquivos DBCs do SIH serão salvos.
caminho_dbc <- "C:/Users/gabli/Desktop/r/SIH/dbc"
#Baixar arquivos dbcs SIH
baixar_dbc_sih(anos = c(2025), 
                   meses = c(1,2,3), 
                   ufs = c("AC","AP","TO"), 
                   destino = caminho_dbc)
rm(baixar_dbc_sih)


caminho_dbc <- "C:/Users/gabli/Desktop/r/SIH/dbc"
#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(), 
                 dbdir = "C:/Users/gabli/Desktop/r/SIH/duckdb/sih_teste.duckdb", #Nome do database que armazena o SIH
                 read_only = FALSE)

#Importação função de tratamento e empilhamto SIH
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/funcao_tratamento_empilhamento_sih.R")

#Variáveis excluídas. Estão zeradas.
vars_excluir <- c("GESTOR_DT","VAL_SADT","VAL_RN","VAL_ACOMP","VAL_ORTP",
                  "VAL_SANGUE","VAL_SADTSR","VAL_TRANSP","VAL_OBSANG","VAL_PED1AC","RUBRICA",
                  "NUM_PROC","TOT_PT_SP","CPF_AUT","GESTOR_CPF","INFEHOSP")


#UFs para empilhar. Colocar todas as UFs desejadas.
ufs_lista <- c("AC","AP","TO")
#Dentre as UFs desejadas, àquelas para empilhar em blocos. Por causa da limitação de memória.
ufs_em_blocos <- c("AP")


#Inicializa controle de colunas
colunas_sih <- NULL
tabela_criada <- FALSE

# Paralelo
plan(multisession, workers = availableCores() - 2)
tictoc::tic()
#Para as UF que desejo empilhar.
for (uf in ufs_lista) {
  
  #Lista com caminho dos dbcs
  ufs_dbc <- list.files(
    path = caminho_dbc, #Onde estão os dbcs
    full.names = TRUE,
    pattern = paste0(".*RD", uf) )
  
  blocos <- if (uf %in% ufs_em_blocos) {
    #Para uf que estão na lista para empilhar em blcos de 12
    #Faz o split dos caminhos do dbc em 12.
    split(ufs_dbc, ceiling(seq_along(ufs_dbc) / 12))
  } else 
    #UFs fora da lsita de split, mantém o empilhamento completo.
  {
    
    list(ufs_dbc)
  }
  
  #Para UFs que serão empilhadas por parte.
  for (bloco in blocos) {
    
    tmp <- data.table::rbindlist(
      future_lapply(bloco, empilhar_sih),
      use.names = TRUE, fill = TRUE ) |>
      tratar_sih() |>
      janitor::clean_names()
    
    # Cria a tabela no primeiro bloco
    if (!tabela_criada) {
      dbWriteTable(
        con, #Conexão com a database
        name = "sih", #Tabela na database que desejo preencher
        value = tmp, #Nome da tabela utilizada como input. Desejo subir essa tabela.
        overwrite = TRUE,
        temporary = FALSE)
      
      colunas_sih <- names(tmp)
      tabela_criada <- TRUE
      
    } 
    else 
    {
      # Detecta e adiciona colunas novas
      novas_colunas <- setdiff(names(tmp), colunas_sih)
      
      if (length(novas_colunas) > 0) {
        message("📌 Novas colunas detectadas: ", paste(novas_colunas, collapse = ", "))
        
        for (col in novas_colunas) {
          dbExecute(con, sprintf("ALTER TABLE sih ADD COLUMN %s TEXT", col))
        }
        
        colunas_sih <- union(colunas_sih, novas_colunas)
      }
      
      # Insere os dados
      dbWriteTable(
        con, #Conexão com a database
        name = "sih", #Tabela na database que desejo preencher
        value = tmp, #Nome da tabela utilizada como input. Desejo subir essa tabela.
        append = TRUE,
        temporary = FALSE)
    }
    
    rm(tmp); gc()
  }
}


#Finaliza
plan(sequential) #Resetar plano sequencial ao final
rm(list = setdiff(ls(), c("con") ) ); gc()
# rm(caminho_dbc, empilhar_sih, vars_excluir, uf, ufs_dbc, ufs_lista, blocos, bloco, colunas_sih,
#    tabela_criada,ufs_em_blocos,tratar_sih, novas_colunas); gc()
tictoc::toc()


dbDisconnect(con)
rm(list=ls()); gc()




#Abre conexão com a database
con <- dbConnect(duckdb::duckdb(), dbdir = "C:/Users/gabli/Desktop\r/SIH/duckdb/sih_teste.duckdb", 
                 read_only = FALSE)
data <- 
  tbl(con, "sih")

data |>
  count(def_proc_rea, sort = TRUE) |>
  filter(is.na(def_proc_rea) )





