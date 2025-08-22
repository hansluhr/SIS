# Rotina de atualização da base SIH ---------------------------------------
Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Essa rotina atualiza base existente do SIH. 
here::i_am("SIH/Rotinas/criação_base_sih.R")


#Abre conexão com a database
con <- dbConnect(duckdb::duckdb(), dbdir = here::here("Bases/sih/duckdb/sih_teste.duckdb"), 
                 read_only = FALSE)
data <- tbl(con, "sih")


#Eu quero o último ano mês de computação de cada uf de internação
data |>
  mutate(ano_mes_cmpt = ano_mes_cmpt |> as.Date() )  |>
  group_by(uf_int) |>
  summarise(date = max(ano_mes_cmpt) ) |> arrange(date)
  
Atualizar:
  Todos: maio e junho
  Acre: abril, maio e junh

source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/Rotinas/sih_baixar_dbc_ftp.R")
#Baixar arquivos dbcs SIH
baixar_dbc_sih(anos = c(2025), 
                 meses = c(4,5,6), 
                 ufs = c("AC","AP","TO"), 
                 destino = here::here("Bases/sih/dbc") )
rm(baixar_dbc_sih)

  
#Tratar e empilhar.

#Importação da tabela procedimentos
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_cod_procedimentos_SUS.R")

#Importação da tabela de municípios
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")


#Importação função de tratamento e empilhamto SIH
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/Rotinas/funcao_tratamento_empilhamento_sih.R")


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
    path = here::here("Bases/sih/dbc"), #Onde estão os dbcs
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
        append = TRUE,
        overwrite = FALSE, # não sobrescreve a tabela
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
        overwrite = FALSE, # não sobrescreve a tabela
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



data <- tbl(con, "sih")

data |>
  count(ano_mes_cmpt)




dbWriteTable(
  con,
  name = "sih",
  value = novos_dados,
  append = TRUE,     # 👈 empilha
  overwrite = FALSE, # não sobrescreve a tabela
  temporary = FALSE
)
  
  
  


#Ano e mês da última atualização.
last_year_month <- 
  data |>
  select(ano_mes_cmpt) |>
  mutate(ano_mes_cmpt = ano_mes_cmpt |> as.Date() ) |>
  distinct(ano_mes_cmpt) |> arrange( desc(ano_mes_cmpt) ) |> first() |> collect() |> pull() 







#Quero saber se todas as UFs estão no mesmo ano_mes_cmpt
data$ |> 
  select(def_uf)


colnames(data)


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
