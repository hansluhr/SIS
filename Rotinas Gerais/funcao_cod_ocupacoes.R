





FAZER ROTINA SIMILAR A PROCEDIMENTOS PARA O OCUPAÇÔES.







ac <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIS/Bases/sim/dbc/DOAC2023.dbc") |>
  clean_names()




sim |>
  count(ano,natural,cod_uf_natu) |>
  view()


rio::import("C:/Users/gabli/Desktop/tb_ocupacao08.txt")

x <- data.table::fread("C:/Users/gabli/Desktop/tb_ocupacao08.txt",
                       header = FALSE,
                       sep = "\t")

y <- data.table::fread("C:/Users/gabli/Desktop/tb_ocupacao25.txt",
                    header = FALSE,
               sep = "\t")


x <- read.delim(
  "C:/Users/gabli/Desktop/tb_ocupacao08.txt",
  header = FALSE,
  encoding = "latin1",
  #O arquivo txt contém somente uma coluna
  #Estou chamando essa coluna de cod_proc
  col.names = c("cod_proc") ) |>   #Tratamento do tb_procedimento importado 
  dplyr::mutate(
    #Separação da coluna cod_proc em código do procedimento e descrição do procedimento 
    cod = stringr::str_sub(cod_proc, 1, 6), #Pega os 10 primeiros dígitos do código. No dbcs do sih o código do procedimento está com 9 dígitos
    resto  = stringr::str_sub(cod_proc, 7) |> stringr::str_trim(), .keep = "none")

y <- read.delim(
  "C:/Users/gabli/Desktop/tb_ocupacao25.txt",
  header = FALSE,
  encoding = "latin1",
  #O arquivo txt contém somente uma coluna
  #Estou chamando essa coluna de cod_proc
  col.names = c("cod_proc") ) |>   #Tratamento do tb_procedimento importado 
  dplyr::mutate(
    #Separação da coluna cod_proc em código do procedimento e descrição do procedimento 
    cod = stringr::str_sub(cod_proc, 1, 6), #Pega os 10 primeiros dígitos do código. No dbcs do sih o código do procedimento está com 9 dígitos
    resto  = stringr::str_sub(cod_proc, 7) |> stringr::str_trim(), .keep = "none")     # Extrai o texto a partir do 11º dígito.

  
  
  


full_join(x,y, join_by("cod") ) |> view()
