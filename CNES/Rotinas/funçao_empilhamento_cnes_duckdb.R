
#Essa função importa, trata e empilha em duckdb os dbcs do CNES.


# Função de importar, empilhar e trata - SIA ------------------------------
Sys.setenv(LANG = "en")

source("C:/Users/gabriel.accioly/Desktop/r/SIS/CNES/Rotinas/funcao_tratamento_cnes.R")

importar_empilhar_salvar_cnes <- function(
    uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
           "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
           "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
    anos,
    meses = 1:12,
    cnes = c("Estabelecimentos","Equipamentos", "Equipes", "Leitos"),
    pasta_dbc = here::here("Bases/cnes/dbc") ) { #Pasta de armanzemaneto dos dbcs
    
  
  tictoc::tic()  
  
  #Condicional da pasta duckdb
  #Nome do arquivo duckdb. Poderia ser tudo junto. Vou fazer separado.
  pasta_duckdb <- 
    dplyr::case_when(
      
    #Equipes
    cnes == "Equipes" ~
        here::here("Bases/cnes/duckdb/cnes_equipes.duckdb"),
    
    #Estabelecimentos
    cnes == "Estabelecimentos" ~ 
        here::here("Bases/cnes/duckdb/cnes_estabelecimentos.duckdb"),
  
    #Leitos
    cnes == "Leitos" ~ 
      here::here("Bases/cnes/duckdb/cnes_leitos.duckdb"),   
          
      
    #Equipamentos
    cnes == "Equipamentos" ~ 
      here::here("Bases/cnes/duckdb/cnes_equipamentos.duckdb")  )
  
  
   #Condicional da tabela duckdb
   #Nome da tabela dentro do arquivo duckdb. 
   tabela <- 
     dplyr::case_when(
       #Equipes
       cnes == "Equipes" ~ "cnes_equipes"
       
       #Estabelecimentos
       cnes == "Estabelecimentos" ~ "cnes_estabelecimentos",
       
       #Leitos
       cnes == "Leitos" ~ "cnes_leitos",
       
       #Equipamentos
       cnes == "Equipamentos" ~ "cnes_equipamentos")
  
  
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
        
        #Necessário para identificar os dbcs
        prefixo <- dplyr::case_when(
          
          cnes == "Equipes" ~ "EP",
          
          cnes == "Estabelecimentos"  ~ "ST",
          
          cnes == "Leitos" ~ "LT",
          
          cnes == "Equipamentos" ~ "EQ")
        
        
        padrao <- paste0(prefixo, u, ano2, mes2)
        
        arquivos <- list.files(
          path = pasta_dbc, #Pasta onde estão os dbcs
          pattern = paste0(padrao, ".*\\.dbc$"),
          full.names = TRUE)
        
        if (length(arquivos) == 0) {
          message("Nenhum arquivo encontrado para ", u, "-", a, "-", m)
          next
        }
        
        #Aqui colocar condicional sobre o tmp
        #Vou trocar a função de tratamento de acordo com o cnes
        
        
        # Lê e trata o primeiro arquivo encontrado
        tmp <- read.dbc::read.dbc(arquivos[1]) |>
          
          data.table::setDT() |>
          
          tratar_cnes() |> 
          
          janitor::clean_names() 
        
        # >>> Aqui entra o passo de salvar no DuckDB depois <<<
        message("Arquivo processado: ", basename(arquivos[1]))
        
        #return(tmp); gc()
        
        #Aqui colocar condicional sobre a tabela e 
        
        
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


