### Função para importação de municípios

### Elaboração de tabela com todos os municípios brasileiros e municípios ignorados da saúde.  


#1º Passo é capturar ano com informação mais recente. 
#Vou pegar ano com informação mais rencete e colocar na função de importar os municípios.
#Necessário para listagem de municípios atualizadas.
geobr::list_geobr() |>
  dplyr::filter(`function` == "`read_municipality`") |>
  
  dplyr::mutate(years = stringr::str_sub(years, start = -4),  .keep = "used") |>
  dplyr::pull() |>
  
#2º Passo: Download das informações sobre municípios conhecidos.
#Vou criar munics (tibble) e utilizar no join com a base de interesse.
  geobr::read_municipality(year = _ ) |> dplyr::as_tibble() |>
  #Código dos municípios com 6 dígitos.
  dplyr::mutate(code_muni = code_muni |> stringr::str_sub(start = 1, end = 6),
         #Transforma em factor
         dplyr::across( c(code_muni, name_state, code_state), ~  forcats::as_factor(.x) ) ) |>
  #Excluindo variáveis não utilizadas.
  dplyr::select(!c(code_region, geom)) |>
  #Elimando geo
  sf::st_drop_geometry(data_all) |>

#3º Bind rows das informações sobre municípios ignorados.
  ###Adiciona informações sobre os municípios ignorados
  dplyr::bind_rows(

    dplyr::tribble(~code_muni,~name_muni,~code_state,~abbrev_state,~name_state, ~name_region,
            "000000", "Ignorado ou exterior",    "00", "IGN", "Ignorado ou exterior", "Ignorado ou exterior",
            "110000", "Município ignorado - RO", "11",  "RO", "Rondônia", "Norte",
            "130000", "Município ignorado - AM", "13",  "AM", "Amazonas", "Norte",
            "150000", "Município ignorado - PA", "15",  "PA", "Pará", "Norte",
            "210000", "Município ignorado - MA", "21",  "MA", "Maranhão", "Nordeste",
            "170000", "Município ignorado - TO", "17",  "TO", "Tocantins", "Norte",
            "240000", "Município ignorado - RN", "24",  "RN", "Rio Grande do Norte", "Nordeste",
            "260000" ,"Município ignorado - PE", "26",  "PE", "Pernambuco", "Nordeste",
            "280000", "Município ignorado - SE", "28",  "SE", "Sergipe", "Nordeste",
            "310000", "Município ignorado - MG", "31",  "MG", "Minas Gerais", "Sudeste",
            "330000", "Município ignorado - RJ", "33",  "RJ", "Rio de Janeiro", "Sudeste",
            "410000", "Município ignorado - PR", "41",  "PR", "Paraná", "Sul",
            "430000", "Município ignorado - RS", "43",  "RS", "Rio Grande do Sul", "Sul",
            "510000", "Município ignorado - MT", "51",  "MT", "Mato Grosso", "Centro Oeste",
            "520000", "Município ignorado - GO", "52",  "GO", "Goiás", "Centro Oeste",
            "120000", "Município ignorado - AC", "12",  "AC", "Acre", "Norte",
            "140000", "Município ignorado - RR", "14",  "RR", "Roraima", "Norte",
            "160000", "Município ignorado - AP", "16",  "AP", "Amapá",  "Norte",
            "220000", "Município ignorado - PI", "22",  "PI", "Piauí", "Nordeste",
            "230000", "Município ignorado - CE", "23",  "CE", "Ceará",  "Nordeste",
            "250000", "Município ignorado - PB", "25",  "PB", "Paraíba","Nordeste",
            "270000", "Município ignorado - AL", "27",  "AL", "Alagoas", "Nordeste",
            "290000", "Município ignorado - BA", "29",  "BA", "Bahia", "Nordeste",
            "320000", "Município ignorado - ES", "32",  "ES", "Espírito Santo", "Sul",
            "350000", "Município ignorado - SP", "35",  "SP", "São Paulo", "Sudeste",
            "420000", "Município ignorado - SC", "42",  "SC", "Santa Catarina", "Sul",
            "500000", "Município ignorado - MS", "50",  "MS", "Mato Grosso do Sul", "Sul") ) |>
  #Conversão para factor. Exceto código do município.
  dplyr::mutate( dplyr::across(.cols = !c(code_muni), forcats::as_factor  ) ) -> munics
