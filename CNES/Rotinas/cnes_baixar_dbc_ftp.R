library(RCurl)
library(stringr)
library(digest)

baixar_cnes <- function(anos, 
                        meses, 
                        ufs = "ALL", 
                        destino = "dados_sihsus/",
                        cnes = c("Estabelecimentos","Equipamentos", "Equipes", "Leitos") ) {
  
  #Criar diretório de destino se não existir
  if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)
  
  #URL do FTP do DataSUS - CNES
  ftp_url <- 
    dplyr::case_when(
    #Equipes
    cnes == "Equipes" ~   
      "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/",
    
    #Estabelecimentos
    cnes == "Estabelecimentos" ~ 
                       "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/",
    #Leitos
    cnes == "Leitos" ~
      "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/",
    
    #Equipamentos
    cnes == "Equipamentos" ~ 
      "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/")

  #Listar arquivos disponíveis no FTP **apenas uma vez**
  arquivos <- getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  #Retira \r\n do nome dos arquivos e faz listagem dos arquivos.
  lista_arquivos <- unlist(strsplit(arquivos, "\r\n"))
  
  #Se o usuário escolher "ALL", coletar todas as UFs
  if (identical(ufs, "ALL")) {
    ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", 
             "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  }
  
  #Converter anos para os últimos 2 dígitos (exemplo: 2013 → "13")
  anos_str <- substr(as.character(anos), 3, 4)  
  #Garantir que os meses tenham 2 dígitos (exemplo: 8 → "08")
  meses_str <- sprintf("%02d", meses)  
  
  #Baixar arquivos
  for (uf in ufs) {
    
    message("Processando UF: ", uf)
    
    for (ano in anos_str) {
      
      for (mes in meses_str) {
        #Criar padrão de busca para os arquivos daquela UF, ano e mês
        #Padrão são as UFs ano_mes de interesse, que serão filtradas na lista_arquivos.
        #Aqui é feito ajute para compatibilizar os nomes de interesse 
        
        prefixo <- dplyr::case_when(
          cnes == "Equipes" ~ "EP",
          
          cnes == "Estabelecimentos"  ~ "ST",
          
          cnes == "Leitos" ~ "LT",
          
          cnes == "Equipamentos" ~ "EQ")
        
        padrao <- paste0(prefixo, uf, ano, mes, ".dbc")
        
        #Aqui são os arquivos de interesse. 
        arquivos_filtrados <- lista_arquivos[str_detect(lista_arquivos, padrao)]
        
        if (length(arquivos_filtrados) == 0) {
          message("Nenhum arquivo encontrado para ", uf, " - ", ano, mes)
        } else {
          for (arquivo in arquivos_filtrados) {
            #URL do arquivo de interesse
            url_completa <- paste0(ftp_url, arquivo)
            destino_arquivo <- file.path(destino, arquivo)
            
            if (file.exists(destino_arquivo)) {  
              # Calcular hash do arquivo local
              hash_local <- digest(destino_arquivo, algo = "sha256", file = TRUE)
              
              # Fazer download temporário do arquivo para verificar hash
              temp_file <- tempfile()
              download.file(url_completa, destfile = temp_file, mode = "wb", quiet = TRUE)
              hash_remoto <- digest(temp_file, algo = "sha256", file = TRUE)
              
              if (hash_local == hash_remoto) {
                message("Arquivo já existe e é idêntico: ", arquivo, " (Ignorado)")
              } else {
                message("Arquivo diferente detectado! Atualizando: ", arquivo)
                file.copy(temp_file, destino_arquivo, overwrite = TRUE)
              }
              
              unlink(temp_file)  # Remover arquivo temporário
              
            } else {
              message("Baixando: ", arquivo)
              tryCatch(
                {
                  download.file(url_completa, destfile = destino_arquivo, mode = "wb")
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
