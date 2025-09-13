


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



here::i_am("SIM/Rotinas/criacao_base_sim_duckdb.R")

#Tratamento dos dbcs

Sys.setenv(LANG = "en")
library(tidyverse)
library(DBI)
library(duckdb)
library(data.table)
library(future.apply)


source("C:/Users/gabli/Desktop/r/SIS/SIM/Rotinas/funcao_tratar_empilhar_sim.R")


#Função principal
importar_empilhar_salvar_sim <- function(
    anos_lista, #Anos dos dbcs de interesse
    pasta_dbc,
    duckdb_file,
    tabela = "sim_br",
    vars_excluir = c("TPASSINA", "NUMERODN", "ESTABDESCR") ) {
  
  #Conexão com o DuckDB
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = duckdb_file,
    read_only = FALSE)
  
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  #Controle
  colunas_sim <- NULL
  tabela_criada <- FALSE
  
  for (ano in anos_lista) {
    message("=== Processando ano: ", ano, " ===")
    
    # Lista de arquivos do ano
    anos_dbc <- list.files(
      path = pasta_dbc,
      full.names = TRUE,
      pattern = paste0("DOBR", ano)
    )
    
    if (length(anos_dbc) == 0) {
      warning("Nenhum arquivo encontrado para o ano ", ano)
      next
    }
    
    # Importa, empilha e trata
    tmp <- read.dbc::read.dbc(anos_dbc) |>
  
      #Transforma em data.table
      data.table::setDT() |>
      
      janitor::clean_names() |>
      
      tratar_sim()

    # Cria a tabela no primeiro bloco
    if (!tabela_criada) {
      dbWriteTable(
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
          dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s TEXT", tabela, col))
        }
        colunas_sim <- union(colunas_sim, novas_colunas)
      }
      
      # Append
      dbWriteTable(
        con,
        name = tabela,
        value = tmp,
        append = TRUE,
        temporary = FALSE
      )
    }
    
    rm(tmp); gc()
  }
  
  message("✔ Importação concluída! Dados salvos em ", duckdb_file, " tabela: ", tabela)
}



importar_empilhar_salvar_sim(
  anos_lista = c(1996, 2021),
  pasta_dbc = here::here("Bases/sim/dbc"),
  duckdb_file = here::here("Bases/sim/duckdb/sim.duckdb"),
  tabela = "sim_br")


con <- dbConnect(duckdb::duckdb(),
                 dbdir = here::here("Bases/sim/duckdb/sim.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)


data <- 
  tbl(con, "sim_br")



rm(list = setdiff(ls(), c("con","ocupacao","munics") ) ); gc()
beepr::beep(sound = 1)

dbDisconnect(con)















# Backup ------------------------------------------------------------------

# Requisitos: read.dbc, janitor, dplyr, stringr, tibble, future.apply (opcional)
importar_e_tratar_dbc <- function(pasta_base,
                                  anos = NULL,                     # vetor de anos (numéricos ou strings). NULL = todos
                                  treat_fn = tratar_sim,           # função de tratamento (deve existir)
                                  var_select = NULL,               # se não NULL, mantém só essas variáveis depois do tratamento
                                  excluir = c("tpassina","numerodn","estabdescr"), # colunas a remover (nomes em minúscula)
                                  combine = TRUE,                  # TRUE => retorna um tibble único; FALSE => lista por arquivo
                                  parallel = TRUE) {               # usar future.apply (requer plano já configurado)
  # pacotes usados internamente
  requireNamespace("read.dbc", quietly = TRUE)
  requireNamespace("janitor", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  if (parallel) requireNamespace("future.apply", quietly = TRUE)
  
  # validação rápida
  if (!is.function(treat_fn)) {
    stop("treat_fn deve ser uma função (ex.: tratar_sim).")
  }
  arquivos <- list.files(path = pasta_base, pattern = "\\.dbc$", full.names = TRUE)
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .dbc encontrado em: ", pasta_base)
  }
  
  # extrair ano do nome do arquivo (4 dígitos imediatamente antes de .dbc)
  anos_arquivos <- stringr::str_extract(basename(arquivos), "\\d{4}(?=\\.dbc)")
  
  # filtrar por anos solicitados (se fornecidos)
  if (!is.null(anos)) {
    anos_chr <- as.character(anos)
    arquivos <- arquivos[anos_arquivos %in% anos_chr]
    anos_arquivos <- anos_arquivos[anos_arquivos %in% anos_chr]
  }
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .dbc encontrado para os anos solicitados.")
  }
  
  # função interna para processar um arquivo
  processar_arquivo <- function(arquivo) {
    message("Lendo: ", basename(arquivo))
    
    df <- read.dbc::read.dbc(arquivo) |>
      janitor::clean_names() |>            # nomes em snake_case (minúsculos)
      tibble::as_tibble()
    
    # remover colunas 'excluir' se existirem
    cols_excluir <- intersect(excluir, names(df))
    if (length(cols_excluir) > 0) {
      df <- dplyr::select(df, -dplyr::any_of(cols_excluir))
    }
    
    # aplicar função de tratamento (assume que treat_fn aceita e devolve data.frame / tibble)
    df <- treat_fn(df)
    
    # se var_select foi passado, manter só essas (após o tratamento)
    if (!is.null(var_select)) {
      df <- dplyr::select(df, dplyr::any_of(var_select))
    }
    
    # anexar metadados úteis
    df <- dplyr::mutate(
      df,
      source_file = basename(arquivo),
      source_year = stringr::str_extract(basename(arquivo), "\\d{4}")
    )
    
    df
  }
  
  # executar (paralelo ou sequencial)
  if (parallel) {
    res_list <- future.apply::future_lapply(arquivos, processar_arquivo)
  } else {
    res_list <- lapply(arquivos, processar_arquivo)
  }
  
  # nomear lista pelos arquivos (opcional)
  names(res_list) <- basename(arquivos)
  
  # devolver combinado ou lista
  if (combine) {
    # dplyr::bind_rows preserva tipos e adiciona NA quando faltar colunas
    result <- dplyr::bind_rows(res_list)
    return(result)
  } else {
    return(res_list)
  }
}

