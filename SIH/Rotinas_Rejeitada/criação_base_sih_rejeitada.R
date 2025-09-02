Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)

#Pasta do projeto onde a rotina utilizada está armazenada.
here::i_am("SIH/Rotinas_Rejeitada/criação_base_sih_rejeitada.R")

# Rotina de criação da base SIH -------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
#source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/Rotinas_comuns_sih/sih_baixar_dbc_ftp.R")

#Baixar arquivos dbcs das AIHs rejeitadas com código de erro.
#baixar_dbc_sih(anos = 2011,
#                meses = c(1:12),
#                ufs = c("AC"), #UFs de interesse.
#                destino = here::here("Bases/sih/dbc_rejeitada"), #Pasta destino dos dbcs
#                tipo  = "rejeitada") #Tipo de AIH de interesse
#rm(baixar_dbc_sih)


#Empilhar base SIH Rejeitada. --------------------------------------------
#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sih/duckdb/sih_rejeitada.duckdb"), #Nome do database que armazena o SIH?
                read_only = FALSE)

#UFs para empilhar. Colocar todas as UFs desejadas.
ufs_lista <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", 
                "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
#Dentre as UFs desejadas, àquelas para empilhar em blocos. Por causa da limitação de memória.
ufs_em_blocos <- c("BA","DF","MG","RJ")


#Inicializa controle de colunas
colunas_sih <- NULL
tabela_criada <- FALSE
#Importação função de tratamento e empilhamto SIH
source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/Rotinas_Rejeitada/funcao_tratamento_empilhamento_sih_rejeitada.R")

#Paralelo
#plan(multisession, workers = availableCores() - 2)
tictoc::tic()
#Para as UF que desejo empilhar.
for (uf in ufs_lista) {

  #Lista com caminho dos dbcs
  ufs_dbc <- list.files(
    path = here::here("Bases/sih/dbc_rejeitada"), #Onde estão os dbcs de aihs rejeitadas
    full.names = TRUE,
    pattern = paste0(".*ER", uf) )

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

    tmp <-
      #Iportação e empilhamento dos dbcs e exclusão de variáveis.
      data.table::rbindlist(

      future_lapply(bloco, importar_sih,
                    #Variáveis excluídas. Sem utilidade ou redundante
                    vars_excluir <- c("SEQUENCIA", "REMESSA") ),

      use.names = TRUE, fill = TRUE ) |>
      #Adicona labels
      tratar_sih_rejeitada() |>

      janitor::clean_names()

  # Cria a tabela no primeiro bloco
   if (!tabela_criada) {
     dbWriteTable(
       con, #Conexão com a database
       name = "sih_rejeitada", #Tabela na database que desejo preencher
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
         dbExecute(con, sprintf("ALTER TABLE sih_rejeitada ADD COLUMN %s TEXT", col))
       }

       colunas_sih <- union(colunas_sih, novas_colunas)
     }

     # Insere os dados
     dbWriteTable(
       con, #Conexão com a database
       name = "sih_rejeitada", #Tabela na database que desejo preencher
       value = tmp, #Nome da tabela utilizada como input. Desejo subir essa tabela.
       append = TRUE,
       temporary = FALSE)
   }
   
    rm(tmp); gc()
    
  }
  
}


#Finaliza
#plan(sequential) #Resetar plano sequencial ao final
rm(list = setdiff(ls(), c("con") ) ); gc()
tictoc::toc()


dbDisconnect(con)
rm(list=ls()); gc()


data <- 
  tbl(con, "sih_rejeitada")

data |>
  filter(is.na(def_erro)) |> 
  collect() |> View()


  count(ano_cmpt,co_erro) |> arrange(desc(n))
  collect() |>  View()






  


data |>
  select(everything()) %>%  #replace to your needs
  summarize(across(everything(), ~ sum(is.na(.)))) |> 
  collect() |>
  View()
