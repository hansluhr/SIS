# Pacotes
library(RCurl)
library(stringr)

# URL base
ftp_base <- "ftp://ftp2.datasus.gov.br/pub/sistemas/tup/downloads/"

# 1) Lista todos os arquivos disponíveis no FTP
arquivos <- getURL(
  ftp_base, 
  dirlistonly = TRUE,
  ftp.use.epsv = FALSE) |> 
  str_split("\r?\n") |> 
  unlist()

# 2) Filtra apenas os arquivos que começam com "TabelaUnificada_"
arquivos_zip <- arquivos[str_detect(arquivos, "^TabelaUnificada_.*\\.zip$")]

# 3) Ordena (assumindo que os nomes têm ordem temporal no sufixo, ex: data no nome)
arquivos_zip <- sort(arquivos_zip, decreasing = TRUE)

# 4) Pega o mais recente
arquivo_recente <- arquivos_zip[1]
message("📦 Arquivo mais recente encontrado: ", arquivo_recente)

# 5) Faz download
destino <- file.path(tempdir(), arquivo_recente)
download.file(
  url = paste0(ftp_base, arquivo_recente),
  destfile = destino,
  mode = "wb")

message("✅ Download concluído: ", destino)

# 6) (Opcional) Descompactar
unzip(destino, exdir = "C:/Users/gabli/Desktop/r/SIH/New Folder")


read.delim("C:/Users/gabli/Desktop/r/SIH/New Folder/tb_procedimento.txt", header = FALSE,
           encoding = "latin1",
           col.names = c("cod_proc")) |>
  
    mutate(
      codigo_proc = str_sub(cod_proc, 1, 10),      # 10 primeiros dígitos
      resto  = str_sub(cod_proc, 11),         # texto após o código
      
      # remove tudo a partir de: [espaços]* + dígito + letra(s) + muitos dígitos
      descricao_proc = resto %>%
        str_replace("\\s*[0-9][A-Z]{1,2}\\d{6,}.*$", "") %>%
        # fallback: se não houver esse marcador, cortar antes de blocos de 10+ dígitos
        (\(x) ifelse(x == resto, str_replace(resto, "\\d{10,}.*$", ""), x))() %>%
        str_trim()
    ) %>%
      select(-resto) |> view()



