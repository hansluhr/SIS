
#Essa função importa, trata e empilha em duckdb os dbcs do SIA.


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
          
          dplyr::filter(
            
            stringr::str_detect(PA_PROC_ID, "^09") ) |>
          
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


