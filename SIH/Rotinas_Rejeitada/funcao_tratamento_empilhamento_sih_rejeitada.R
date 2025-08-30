

# Função de tratamento do SIH ---------------------------------------------
tratar_sih_rejeitada <- function(data) {
  
  #Transformando variáveis categóricas em character.
  #Vou padronizar a classe da variável, permitindo empilhar.
  # #Variáveis que deseja converter para character
  vars_char <- c("SEQUENCIA", "REMESSA", "CNES", "AIH",  "DT_INTER", "DT_SAIDA",
                 "MUN_MOV", "UF_ZI", "MUN_RES" , "UF_RES", "CO_ERRO")
  
  #Transformando variáveis de interesse para character.
  cols_existentes <- intersect(vars_char, names(data))
  if (length(cols_existentes) > 0) {
    data[, (cols_existentes) := lapply(.SD, as.character), .SDcols = cols_existentes]
  }
  
   # # #Tratamento de datas
    data[, `:=`(
    DT_INTER = ymd(DT_INTER),
    DT_SAIDA = ymd(DT_SAIDA),
    ANO_MES_CMPT = as.Date(paste(ANO, MES, "01", sep = "-") ) ) ]
  
  ######################################################################################################################
  #Segunda parte da alteração das variáveis.
  data[, `:=`(

    #Variáveis derivadas da internação
    ANO_INTER = as.factor(lubridate::year(DT_INTER)),
    MES_INTER = as.factor(lubridate::month(DT_INTER)),
    DWK_INTER = lubridate::wday(DT_INTER, label = TRUE),
    ANO_MES_INTER = format(DT_INTER, "%Y-%m-01"),
   
    #Variáveis derivadas da saída
    ANO_SAIDA = as.factor(lubridate::year(DT_SAIDA)),
    MES_SAIDA = as.factor(lubridate::month(DT_SAIDA)),
    DWK_SAIDA = lubridate::wday(DT_SAIDA, label = TRUE),
    ANO_MES_SAIDA =  format(DT_SAIDA, "%Y-%m-01"),

    #Correções nos códigos do DF. Existem códigos das regiões administrativas. Conserta para código do DF.
    MUN_MOV = fifelse(startsWith(as.character(MUN_MOV), "53"), "530010", as.character(MUN_MOV) ),
    MUN_RES = fifelse(startsWith(as.character(MUN_RES), "53"), "530010", as.character(MUN_RES) ) ) ]
  
    

# Atribuição label erros da aih -------------------------------------------

    
     #COLOCAR MENSAGEM INDICANDO DOWNLOADO COM SUCESSO. 
     
    destfile <- tempfile(fileext = ".xlsx")
    #Arquivo temporário
   download.file(url = "https://raw.githubusercontent.com/hansluhr/SIS/main/Bases%20Gerais/aih_erros.xlsx",
                destfile, 
                 mode = "wb") 
   
erros <- readxl::read_excel(path = destfile ) |>
     janitor::clean_names() |> 
     select(!c(id), def_erro = descricao_erro)
rm(destfile)


 #left_join com o código de rejeição da AIH.
    data <- 
      merge(x = data,
            y = erros,
            by.x = "CO_ERRO",
            by.y = "cod_erro",
            all.x = TRUE)
    
    
# Atribuição de Municípios --------------------------------------------------------------
  
   #Importação da tabela de municípios
   source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_importar_munics.R")  
    
  #left_Join com município de residência
  data <- merge(
    x = data,
    y = munics,
    by.x = "MUN_RES",
    by.y = "code_muni",
    all.x = TRUE,
    suffixes = c("", "_resd") )

  #Renomear colunas de residência
  setnames(data,
           old = c("name_muni", "MUN_RES", "code_state", "abbrev_state", "name_state", "name_region"),
           new = c("def_munic_resd", "cod_munic_resd", "code_state_resd", "abbrev_state_resd", "def_uf_resd", "region_resd"))

  #left_join com município de internação
  data <- merge(
    x = data,
    y = munics,
    by.x = "MUN_MOV",
    by.y = "code_muni",
    all.x = TRUE,
    suffixes = c("", "_int") )

  #Renomear colunas de internação
  setnames(data,
           old = c("ANO", "MES", "name_muni", "MUN_MOV", "code_state", "abbrev_state", "name_state", "name_region"),
           new = c("ano_cmpt", "mes_cmpt", "def_munic_int", "cod_munic_int", "code_state_int", "abbrev_state_int", "def_uf_int", "region_int"))
  
  
  #Ordenação das variáveis
  data <- data |>
    dplyr::relocate(cod_munic_int, .before = def_munic_int) |>
    dplyr::relocate(cod_munic_resd, .before = def_munic_resd) |>
    dplyr::relocate(CO_ERRO, .before = def_erro)
  
  
  data <- tibble::as_tibble(data) 
  data <- droplevels(data.table::as.data.table(data))
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
                                                    FUN = stringi::stri_unescape_unicode) ) )
  
}



# Função utilizada para empilhar o SIH ------------------------------------
importar_sih <- function(arquivo, 
                         #variaveis = NULL, #Variáveis que desejo manter. NULL seleciona todas as variáveis não excluidas.
                         excluir = vars_excluir) {
  message("Importando: ", arquivo)
  #Importa o DBC
  dados <- read.dbc::read.dbc(arquivo) |> 
    #Transforma o dbc em data.table. Necessário para trabamento de dados.
    data.table::setDT()
  
  #Excluir variáveis sem preenchimento\Zeradas\Sem Utilidade
  vars_excluir <- intersect(toupper(vars_excluir), names(dados))
  if (length(vars_excluir) > 0) {
    dados[, (vars_excluir) := NULL]
  }

  return(dados)
}
