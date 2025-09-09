#URL do FTP
ftp_base <- "ftp://ftp2.datasus.gov.br/pub/sistemas/tup/downloads/"

#Lista de todos os arquivos no FTP
arquivos <- 
  RCurl::getURL(
  ftp_base, #URL do FTP
  dirlistonly = TRUE,
  ftp.use.epsv = FALSE) |>
  stringr::str_split("\r?\n") |>
  unlist()

#Existem vários arquivos neste ftp. 
#O arquivo de interesse começa com TabelaUnificada_. 
#Aqui faço um filtro para manter os arquivos começanco com TabelaUnificada_ e terminando em .zip
#Neste zip está o arquivo de interesse tb_procedimento.txt
arquivos <- arquivos[stringr::str_detect(arquivos, "^TabelaUnificada_.*\\.zip$")]

#arquivos <- arquivos[stringr::str_detect(arquivos, "^TabelaUnificada_2025.*\\.zip")]

#Vou criar função para auxiliar no empilhamento.
#A função faz o downlaod e acesso do zip, e seguida faz leitura e tratamento do arquivo tb_procedimento.txt

ocupacoes_ftp_zip <- 
  function(arquivo) {
  #Informa o arquivo zip importado
  message("Processando: ", arquivo)
  
  #Cria pasta destino temporária para arquivo zip importado
  destino <- file.path(tempdir(), arquivo)
  
  #Faz download
  download.file(
    url = paste0(ftp_base, arquivo), #link do ftp de interesse + arquivo de interesse
    destfile = destino, #Pasta temporária do zip
    mode = "wb")
  
  #Extração da versão do arquivo zip. 
  #Objeto com a versão do arquivo zip
  versao <- stringr::str_extract(arquivo, "\\d{6}") |> as.integer()
  
  #Lê e trata tb_procedimento.txt
  dados <- read.delim(
    unz(destino, "tb_ocupacao.txt"), #Extração do zip.
    header = FALSE,
    encoding = "latin1",
    #O arquivo txt contém somente uma coluna
    #Estou chamando essa coluna de cod_ocup
    col.names = c("cod_ocup") ) |> 
    
    #Tratamento do tb_ocupacao.txt importado 
    dplyr::mutate(
      #Separação da coluna cod_ocup em código do procedimento e descrição do procedimento 
      #Pega os 6 primeiros dígitos do código. 
      cod = stringr::str_sub(cod_ocup, 1, 6),
      #Descrição da ocupação.
      def_ocup  = stringr::str_sub(cod_ocup, 7) |> 
      #Remove espaçoes excedentes.  
      stringr::str_trim() |>
      #Transforma em factor
      forcats::as_factor(),
      #Cria coluna versão
      versao_cod_proc = versao) |> 
    dplyr::select(-c(cod_ocup) ) |>
    data.table::setDT() 
  
    return(dados)
}

#Aplica a função para todos os arquivos do fpt e empilha resultados
ocupacao <- lapply(arquivos, 
                   ocupacoes_ftp_zip) |> 
  #Empilhamento da lista.
  data.table::rbindlist() 

ocupacao <- ocupacao |>
#Adicionar cbos que não estão no txt do ftp.
  dplyr::bind_rows(
    dplyr::tibble(
      cod = c(NA_character_, "999993", "999992", "999994", "999991", "998999", "322310",
              "223610"),
      def_ocup = c("Missing", 
                   "APOSENTADO/PENSIONISTA",
                   "DONA DE CASA",
                   "DESEMPREGADO CRONICO OU CUJA OCUPACAO HABITUAL NAO FOI POSSIVEL OBTER",
                   "ESTUDANTE",
                   "Ignorada",
                   "Técnicos em óptica e optometria",
                   "FONOAUDIOLOGO"),
      #repete o valor automaticamente
      versao_cod_proc = dplyr::first(ocupacao$versao_cod_proc) ) ) |>
  #Primeira letra maiúscula
  dplyr::mutate(def_ocup = def_ocup |> stringr::str_to_title() )

#Remove duplicados, mantendo o mais recente por cod
data.table::setorder(ocupacao, cod, -versao_cod_proc) #ordena por cod e versão desc
ocupacao <- ocupacao[!duplicated(cod)]
beepr::beep(sound = 1)


