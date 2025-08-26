# Importando pacotes
library(RCurl)
library(stringr)

#URL do FTP
ftp_base <- "ftp://ftp2.datasus.gov.br/pub/sistemas/tup/downloads/"

#Lista de todos os arquivos no FTP
arquivos <- getURL(
  ftp_base,
  dirlistonly = TRUE,
  ftp.use.epsv = FALSE) |>
  str_split("\r?\n") |>
  unlist()

#O arquivo começa com TabelaUnificada_. Faço um filtro para manter arquivos começanco com TabelaUnificada_ e terminando em .zip
#Neste zip está o arquivo de interesse tb_procedimento.txt
arquivos <- arquivos[str_detect(arquivos, "^TabelaUnificada_.*\\.zip$")]

#Vou criar função para auxiliar no empilhamento.
#A função faz o downlaod e acesso do zip, e seguida faz leitura e tratamento do arquivo tb_procedimento.txt

procedimento_zip <- function(arquivo) {
  #Informa o arquivo zip importado e tratado
  message("Processando: ", arquivo)
  
  #Cria pasta destino temporária para arquivo zip importado e tratado
  destino <- file.path(tempdir(), arquivo)
  
  # Faz download
  download.file(
    url = paste0(ftp_base, arquivo),
    destfile = destino, #Pasta temporária do zip
    mode = "wb")
  
  #Extração da versão do arquivo zip. 
  #Objeto com a versão do arquivo zip
  versao <- stringr::str_extract(arquivo, "\\d{6}") |> as.integer()
  
  #Lê e trata tb_procedimento.txt
  dados <- read.delim(
    unz(destino, "tb_procedimento.txt"), #Extração do zip.
    header = FALSE,
    encoding = "latin1",
    #O arquivo txt contém somente uma coluna
    #Estou chamando essa coluna de cod_proc
    col.names = c("cod_proc") ) |> 
    
    #Tratamento do tb_procedimento importado 
    dplyr::mutate(
      #Separação da coluna cod_proc em código do procedimento e descrição do procedimento 
      cod = stringr::str_sub(cod_proc, 1, 10) |> as.integer(), #Pega os 10 primeiros dígitos do código. No dbcs do sih o código do procedimento está com 9 dígitos
      resto  = stringr::str_sub(cod_proc, 11), # Extrai o texto a partir do 11º dígito.
      #remove tudo a partir de: [espaços]* + dígito + letra(s) + muitos dígitos
      proc = resto |>
        stringr::str_replace("\\s*[0-9][A-Z]{1,2}\\d{6,}.*$", "") |>
        #fallback: se não houver esse marcador, cortar antes de blocos de 10+ dígitos
        (\(x) ifelse(x == resto, stringr::str_replace(resto, "\\d{10,}.*$", ""), x))() |>
        
        stringr::str_trim() |> #Remove espaçoes excedentes.
        
        stringr::str_to_title() |> #Primeira letra maiúscula
        
        forcats::as_factor(), #Transforma procedimento em factor
      
      versao_cod_proc = versao) |> #Cria coluna versão
    dplyr::select(-c(resto, cod_proc)) |>
    data.table::setDT()
  
  return(dados)
}

#Aplica a função para todos os arquivos do fpt e empilha resultados
procedimentos <- lapply(arquivos, 
                        procedimento_zip) |> 
  #Empilhamento da lista.
  data.table::rbindlist()

# Remove duplicados, mantendo sempre o mais recente por `cod`
setorder(procedimentos, cod, -versao)   # ordena por cod e versão desc
procedimentos <- procedimentos[!duplicated(cod)]


