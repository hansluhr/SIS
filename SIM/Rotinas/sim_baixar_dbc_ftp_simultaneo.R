library(RCurl)
library(stringr)
library(digest)
library(curl)

baixar_dbc_sim_curl <- function(anos,
                                destino = "dados_sim/") {
  
  if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)
  
  ftp_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
  
  arquivos <- getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  arquivos <- unlist(strsplit(arquivos, "\r\n"))
  
  anos_str <- as.character(anos)
  pool <- new_pool()
  
  for (ano in anos_str) {
    message("\n🔹 Preparando download do ano: ", ano)
    
    padrao <- paste0("^DOBR", ano, "\\.dbc$")
    arquivos_filtrados <- arquivos[str_detect(arquivos, padrao)]
    
    if (length(arquivos_filtrados) == 0) {
      message("❌ Nenhum arquivo encontrado para o ano ", ano)
    } else {
      for (arquivo in arquivos_filtrados) {
        url_completa <- paste0(ftp_url, arquivo)
        destino_arquivo <- file.path(destino, arquivo)
        
        if (file.exists(destino_arquivo)) {
          message("🔍 Verificando arquivo existente: ", arquivo)
          hash_local <- digest(destino_arquivo, algo = "sha256", file = TRUE)
          
          # cada arquivo tem seu temp_file único
          temp_file <- tempfile(pattern = paste0("chk_", arquivo, "_"))
          
          local({
            arq <- arquivo
            dest <- destino_arquivo
            tmp <- temp_file
            hlocal <- hash_local
            
            curl_fetch_multi(
              url_completa,
              done = function(res) {
                writeBin(res$content, tmp)
                hash_remoto <- digest(tmp, algo = "sha256", file = TRUE)
                
                if (hlocal == hash_remoto) {
                  message("✔ Arquivo já existe e é idêntico: ", arq, " (Ignorado)")
                } else {
                  message("🔄 Arquivo diferente detectado! Atualizando: ", arq)
                  file.copy(tmp, dest, overwrite = TRUE)
                }
                unlink(tmp)
              },
              fail = function(err) {
                message("❌ Erro no download de ", arq, ": ", err$message)
              },
              pool = pool
            )
          })
          
        } else {
          local({
            arq <- arquivo
            dest <- destino_arquivo
            
            curl_fetch_multi(
              url_completa,
              done = function(res) {
                writeBin(res$content, dest)
                message("✔ Download concluído: ", dest)
              },
              fail = function(err) {
                message("❌ Erro no download de ", arq, ": ", err$message)
              },
              pool = pool
            )
          })
        }
      }
    }
  }
  
  multi_run(pool = pool)
  message("\n✅ Todos os downloads concluídos!")
}



baixar_dbc_sim_curl(anos = 2020:2022)
