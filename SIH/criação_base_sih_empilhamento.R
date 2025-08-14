Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)



#Função para baixar os dbcs
source(file = "C:/Users/gabli/Dropbox/Ipea/Atlas/Rotinas/SIH/sih_baixar_dbc_ftp.R")
baixar_arquivos_RD(anos = c(2008,2024) , meses = c(1:12), ufs = c("AC","AP","TO"), 
                   destino = "C:/Users/gabli/Desktop/r/SIH/dbc")
rm(baixar_arquivos_RD)

#Abre conexão com a database
con <- dbConnect(duckdb::duckdb(), dbdir = "C:/Users/gabli/Desktop/r/SIH/duckdb/sih_teste.duckdb", read_only = FALSE)

#Tratamento dos dbcs - SIH. Coloca labels e municípios
source(file = "C:/Users/gabli/Dropbox/Ipea/Atlas/Rotinas/SIH/funcao_base_sih_labels.R")

#Variáveis excluídas. Estão zeradas.
vars_excluir <- c("GESTOR_DT","VAL_SADT","VAL_RN","VAL_ACOMP","VAL_ORTP",
                  "VAL_SANGUE","VAL_SADTSR","VAL_TRANSP","VAL_OBSANG","VAL_PED1AC","RUBRICA",
                  "NUM_PROC","TOT_PT_SP","CPF_AUT","GESTOR_CPF","INFEHOSP")


#Função para importar, filtrar e selecionar variáveis
empilhar_sih <- function(arquivo, 
                        variaveis = NULL, #Variáveis que desejo manter. NULL seleciona todas as variáveis não excluidas.
                        excluir = vars_excluir) {
  message("Importando: ", arquivo)
  dados <- read.dbc::read.dbc(arquivo) |> setDT()
  
  #Excluir variáveis sem preenchimento\Zeradas
  vars_excluir <- intersect(toupper(vars_excluir), names(dados))
  if (length(vars_excluir) > 0) {
    dados[, (vars_excluir) := NULL]
  }
  
  #Selecionar variáveis desejadas. Mantém variáveis disponíveis se não indicar nenhuma variável. 
  if (!is.null(variaveis)) {
    vars_sel <- intersect(variaveis, names(dados))
    dados <- dados[, ..vars_sel]
  }
  
  #Nos dados de 2013 precisa renomar variável de dado secundário
  #Se o nome do arquivo contém "13", renomeia a variável diag_secun
  if (stringr::str_detect(arquivo, "13") && "DIAG_SECUN" %in% names(dados)) {
    dados <- dados |> rename(DIAGSEC1 = DIAG_SECUN)
  }
  
  #Em 2014 a variável diagsec1 é introduzida, mas não é utilizada. 100% missing 
  #Como diagnóstico secundário ainda é utilizado diag_secun. 
  #Vou excluir diagsec, pois não é utilizada e renomear diag_secun para diagsec1. 
  if (stringr::str_detect(arquivo, "14")) {
    if ("DIAGSEC1" %in% names(dados)) dados <- dados |> select(-DIAGSEC1)
    if ("DIAG_SECUN" %in% names(dados)) dados <- dados |> rename(DIAGSEC1 = DIAG_SECUN)
  }
  
  return(dados)
}


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
    path = "C:/Users/gabli/Desktop/r/SIH/dbc", #Onde estão os dbcs
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
rm(empilhar_sih, vars_excluir, uf, ufs_dbc, ufs_lista, blocos, bloco, colunas_sih,
   tabela_criada,ufs_em_blocos,tratar_sih); gc()
tictoc::toc()



data <- tbl(con, "sih")
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
