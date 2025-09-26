library(RCurl)
library(stringr)
library(digest)

baixar_dbc_sia <- function(anos,
                           meses,
                           ufs = "ALL",
                           destino = "dados_sihsus/") {

  #Criar diretório de destino se não existir
  if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)
  
  ### Esse trecho faz listagem de todos os dbcs no ftp. 
  #A listagem contém todos os tipos de AIHs.
  #Etapa preparatória para download dos dbcs.
  
  #URL do FTP do DataSUS
  ftp_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  #lista com dbcs disponíveis no FTP 
  arquivos <- getURL(ftp_url, #Endereço FTP onde estão os dbcs rejeitados 
                     ftp.use.epsv = FALSE, 
                     dirlistonly = TRUE, #Lista com somente os diretórios.
                     timeout = 120, #Tempo para tentar conexão
                     connecttimeout = 60)
  #Listagem do todos os dbcs sem informações desnecessárias.
  lista_arquivos <- unlist(strsplit(arquivos, "\r\n"))
  
  
  #Se parâmetro ufs "ALL", então usar todas as UFs.
  if (identical(ufs, "ALL")) {
    ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", 
             "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  }
  
  #Converter anos para os últimos 2 dígitos (exemplo: 2013 → "13")
  anos_str <- substr(as.character(anos), 3, 4)  
  #Garantir que os meses tenham 2 dígitos (exemplo: 8 → "08")
  meses_str <- sprintf("%02d", meses)  
  
  #Dowloado dos dbcs rejeitados
  #Para as UF selecionadas
  for (uf in ufs) {
    
    message("\n Processando UF: ", uf)
    #No ano de interesse
    for (ano in anos_str) {
      #No mês de interesse    
      for (mes in meses_str) {
        
        #A função faz o download de AIHs reduzidas e rejeitadas. Para realizar o download
        #é necessário identificar em lista_arquivos, os dbcs de interesse. 
        #A sequência a seguir identifica os dbcs de interesse. 
        
        #O prefixo do dbc identifica o tipo de AIH. 
        #RD é reduzida e ER é rejeitada com erro. 
        #Se o parâmetro tipo for reduzida, então o prefixo será RD, de outro caso ER
        #de rejeitada com erro.
        
        
        #Criar padrão de busca para os dbcs de interesse, na uf, ano e mês 
        padrao <- paste0("^PA", uf, ano, mes, "[a-zA-Z]*", "\\.dbc$")
        #Esse padrão é utilizado para extrair o nomes dos dbcs de interesse, 
        #de listagem com todos os dbcs (arquivos_filtrados)
        arquivos_filtrados <- lista_arquivos[str_detect(lista_arquivos, padrao)]
        
        #No caso do ftp não conter dbcs da UF, ano e mês de interesse.
        #Apresenta mensagem de erro.
        if (length(arquivos_filtrados) == 0) {
          
          message("Nenhum arquivo encontrado para ", uf, " - ", ano, mes)
          
        } else 
          #Caso exista no FTP o dbc para a UF no mes e ano.
          #Primeiro passo é verificar se existe atualização para os dbcs de interesse.
          #vai baixar o dbc e comparar com o dbc armazenado na pasta de destino.
        {
          for (arquivo in arquivos_filtrados) {
            #Dentre as ufs de interesse (arquivos_filtros)
            url_completa <- paste0(ftp_url, arquivo)
            #Pasta destino do download.
            destino_arquivo <- file.path(destino, arquivo)
            
            if (file.exists(destino_arquivo)) {  
              #Hash do arquivo local.
              hash_local <- digest(destino_arquivo, algo = "sha256", file = TRUE)
              
              #Pasta temporária usada na comparação entre dbc na raiz e dbc novo.
              temp_file <- tempfile()
              #Faz download para verificar necessidade de atualização.
              #Download em pasta temporária para verificar se 
              download.file(url_completa, 
                            destfile = temp_file, 
                            mode = "wb", 
                            quiet = TRUE,
                            method = "libcurl")
              #Hash do dbc baixado
              hash_remoto <- digest(temp_file, algo = "sha256", file = TRUE)
              
              if (hash_local == hash_remoto) {
                #DBCs iguais, então aparece mensagem de arquivo identico e nada é feito.
                #O arquivo será deletado em unlink
                message("Arquivo já existe e é idêntico: ", arquivo, " (Ignorado)")
                
              } else {
                #Havendo necessidade de atualização, o dbc é movido da pasta temporária
                #para a pasta destino. (file.copy)
                message("Arquivo diferente detectado! Atualizando: ", arquivo)
                
                file.copy(temp_file, destino_arquivo, overwrite = TRUE)
              }
              #Remover arquivo temporário
              unlink(temp_file)  
              
            } else 
              #Quando o dbc baixado não está na pasta de destino.
              #Realizar o download do dbc.
            {
              message("Baixando: ", arquivo)
              #Downloado do dbc aih rejeitado\reduzido.
              tryCatch(
                {
                  download.file(url_completa,
                                destfile = destino_arquivo, 
                                quite = TRUE,
                                mode = "wb",
                                method = "libcurl")
                  message("Arquivo salvo em: ", destino_arquivo)
                },
                error = function(e) {
                  message("Erro ao baixar: ", arquivo, " - ", e$message)
                }
              )
            }
          }
        }
      }
    }
  }
  
  message("\n✅ Todos os downloads concluídos!")
}
