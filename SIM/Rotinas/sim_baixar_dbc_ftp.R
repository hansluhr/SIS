library(RCurl)
library(stringr)
library(digest)

baixar_dbc_sim <- function(anos,
                           destino = "dados_sim/") {
  
  # Criar diretório de destino se não existir
  if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)
  
  # URL do FTP do DataSUS
  ftp_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
  
  # Listar arquivos disponíveis no FTP
  arquivos <- getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  arquivos <- unlist(strsplit(arquivos, "\r\n"))
  
  # Converter anos para 4 dígitos (já esperado nos arquivos do SIM)
  anos_str <- as.character(anos)
  
  # Baixar arquivos
  for (ano in anos_str) {
    message("\n🔹 Download do ano: ", ano)
    
    # Criar padrão de busca
    padrao <- paste0("^DOBR", ano, "\\.dbc$")
    
    arquivos_filtrados <- arquivos[str_detect(arquivos, padrao)]
    
    if (length(arquivos_filtrados) == 0) {
      message("❌ Nenhum arquivo encontrado para o ano ", ano)
    } else {
      for (arquivo in arquivos_filtrados) {
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
            message("✔ Arquivo já existe e é idêntico: ", arquivo, " (Ignorado)")
          } else {
            message("🔄 Arquivo diferente detectado! Atualizando: ", arquivo)
            file.copy(temp_file, destino_arquivo, overwrite = TRUE)
          }
          
          unlink(temp_file)
          
        } else {
          message("📥 Baixando: ", arquivo)
          tryCatch(
            {
              download.file(url_completa, destfile = destino_arquivo, mode = "wb")
              message("✔ Arquivo salvo em: ", destino_arquivo)
            },
            error = function(e) {
              message("❌ Erro ao baixar: ", arquivo, " - ", e$message)
            }
          )
        }
      }
    }
  }
  
  message("\n✅ Todos os downloads concluídos!")
}
