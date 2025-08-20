#Importando pacotes
library(RCurl)
library(stringr)

#URL do ftp
ftp_base <- "ftp://ftp2.datasus.gov.br/pub/sistemas/tup/downloads/"

#Lista de todos os arquivos no FTP
arquivos <- getURL(
  ftp_base,
  dirlistonly = TRUE,
  ftp.use.epsv = FALSE) |>
  str_split("\r?\n") |>
  unlist()

#Quero a tabela de procedimentos.
#O arquivo começa com TabelaUnificada_. Faço um filtro para manter arquivos começanco com TabelaUnificada_
arquivos <- arquivos[str_detect(arquivos, "^TabelaUnificada_.*\\.zip$")]

#Ordenação. O mais novo aparece primeiro.
#A data de upload aparece no nome do arquivo. Faz sentido fazer isso.
arquivos <- sort(arquivos, decreasing = TRUE)

#Pega o mais recente. O primeiro da lista.
arquivos <- arquivos[1]
message("Arquivo mais recente encontrado no FTP: ", arquivos)

#Faz download do arquivo mais recente.
destino <- file.path(tempdir(), arquivos) #Pasta temporária para armazenar o zip.
download.file(
  url = paste0(ftp_base, arquivos),
  destfile = destino,
  mode = "wb")

#Lê o arquivo tb_procedimento.txt de dentro do zip
procedimentos <- read.delim(
  unz(destino, "tb_procedimento.txt"),
  header = FALSE,
  encoding = "latin1",
  col.names = c("cod_proc") ) |>
  dplyr::mutate(
    cod = str_sub(cod_proc, 1, 10) |> forcats::as_factor(),      #Pega os 10 primeiros dígitos do código. No dbcs do sih o código do procedimento está com 9 dígitos
    resto  = str_sub(cod_proc, 11),         # Extrai o texto a partir do 11º dígito.
    #remove tudo a partir de: [espaços]* + dígito + letra(s) + muitos dígitos
    proc = resto |>
      str_replace("\\s*[0-9][A-Z]{1,2}\\d{6,}.*$", "") |>
      #fallback: se não houver esse marcador, cortar antes de blocos de 10+ dígitos
      (\(x) ifelse(x == resto, str_replace(resto, "\\d{10,}.*$", ""), x)) () |>
      
      str_trim() |> #Remove espaçoes excedentes.
      
      str_to_title() |> #Primeira letra maiúscula
      
      forcats::as_factor() ) |> #Transforma em factor.
  dplyr::select(-c(resto,cod_proc) ) |>
  data.table::setDT()
 
rm(list = setdiff(c(ftp_base,destino,arquivos) ,c("procedimentos") ))






