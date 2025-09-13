Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)

here::i_am("SIM/Rotinas/criacao_base_sim.R")


# Download dbcs SIM -------------------------------------------------------
#Chamar função para importar arquivos DBCs do FTP DataSuS 
source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/sim_baixar_dbc_ftp.R")

# #Pasta onde os arquivos DBCs do SIH serão salvos.
# baixar_dbc_sim(anos = c(1996:2025), 
#               destino = here::here("Bases/sim/dbc") )
# rm(baixar_dbc_sim)




# Elaboração base SIM -----------------------------------------------------
#Abre conexão com a database. Este arquivo armazena a base SIH.
# con <- dbConnect(duckdb::duckdb(), 
#                  dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
#                  read_only = FALSE)

#Importação da tabela de municípios
# source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")

#Importação tabela ocupações
# Link direto para o arquivo bruto no GitHub
url <- "https://github.com/hansluhr/SIS/raw/main/Bases%20Gerais/cbo_ocupacao.xlsx"
# Arquivo temporário
destfile <- tempfile(fileext = ".xlsx")
# Download
download.file(url, destfile, mode = "wb")
# Importar no R
ocupacao <- readxl::read_excel(destfile)
rm(url,destfile)

#Importação da tabela de municípios
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")


#Importar e empilhar os dbcs
source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/funcao_tratar_empilhar_sim.R")

sim <- importar_empilhar_dbc(
                    pasta_dbc = here::here("Bases/sim/dbc_copy"),
                    var_select = NULL) |>
  tratar_sim()


rm(list = setdiff(ls(), c("sim","ocupacao","munics") ) ); gc()
beepr::beep(sound = 1)


sim |>
  summarise(across(everything(), ~ mean(is.na(.x) ) ) )

ac <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIS/Bases/sim/dbc/DOAC2023.dbc") |>
  janitor::clean_names()



# Importação duckdb -------------------------------------------------------
Sys.setenv(LANG = "en")
library(tidyverse)
library(future.apply)
library(data.table)
library(duckdb)


#Importação tabela ocupações
# Link direto para o arquivo bruto no GitHub
url <- "https://github.com/hansluhr/SIS/raw/main/Bases%20Gerais/cbo_ocupacao.xlsx"
# Arquivo temporário
destfile <- tempfile(fileext = ".xlsx")
# Download
download.file(url, destfile, mode = "wb")
# Importar no R
ocupacao <- readxl::read_excel(destfile)
rm(url,destfile)


#Importação da tabela de municípios
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")




here::i_am("SIM/Rotinas/criacao_base_sim.R")

#Anos de interesse ao empilhar.
ano_dobr <- c(1996)

#Fução de tratamento
source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/funcao_tratar_empilhar_sim.R")

#Abre conexão com a database. Este arquivo armazena a base SIH.
con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)
#Variáveis excluídas. Estão zeradas.
vars_excluir <- c("TPASSINA","NUMERODN","ESTABDESCR")

#UFs para empilhar. Colocar todas as UFs desejadas.
anos_lista <- c(1996)

#Inicializa controle de colunas
colunas_sim <- NULL
tabela_criada <- FALSE

#Paralelo

#Para as UF que desejo empilhar.
for (ano in anos_lista) {
  
  #Lista com caminho dos dbcs
  anos_dbc <- list.files(
    path = here::here("Bases/sim/dbc"), #Onde estão os dbcs
    full.names = TRUE,
    pattern = paste0("DOBR", ano) )
  
  tmp <- data.table::rbindlist(
      future_lapply(anos_dbc, empilhar_sim),
      use.names = TRUE, fill = TRUE ) |>
      tratar_sim() 
    
    # Cria a tabela no primeiro bloco
    if (!tabela_criada) {
      dbWriteTable(
        con, #Conexão com a database
        name = "sim_br", #Tabela na database que desejo preencher
        value = tmp, #Nome da tabela utilizada como input. Desejo subir essa tabela.
        overwrite = TRUE,
        temporary = FALSE)
      
      colunas_sim <- names(tmp)
      tabela_criada <- TRUE
      
    } 
    else 
    {
      # Detecta e adiciona colunas novas
      novas_colunas <- setdiff(names(tmp), colunas_sim)
      
      if (length(novas_colunas) > 0) {
        message("Novas colunas detectadas: ", paste(novas_colunas, collapse = ", "))
        
        for (col in novas_colunas) {
          dbExecute(con, sprintf("ALTER TABLE sim_br ADD COLUMN %s TEXT", col))
        }
        
        colunas_sim <- union(colunas_sim, novas_colunas)
      }
      
      # Insere os dados
      dbWriteTable(
        con, #Conexão com a database
        name = "sim_br", #Tabela na database que desejo preencher
        value = tmp, #Nome da tabela utilizada como input. Desejo subir essa tabela.
        append = TRUE,
        temporary = FALSE)
    }
    
    rm(tmp); gc()
  }


rm(list = setdiff(ls(), c("con","ocupacao","munics") ) ); gc()
beepr::beep(sound = 1)




data <- 
  tbl(con, "sim_br")

dbDisconnect(con)
rm(list = setdiff(ls(), c("ocupacao","munics") ) ); gc()


comunsvoim


ac <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIS/Bases/sim/dbc_Copy/DOAC2011.dbc") |>
  janitor::clean_names() |> select(starts_with("com"))

br <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIS/Bases/sim/dbc/DOBR1996.dbc") |>
  clean_names()


br
 
