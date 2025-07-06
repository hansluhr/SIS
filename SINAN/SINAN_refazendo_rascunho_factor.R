#Colocar labels
#Elaborar ordem
#Substituir NAs,
#Substituir não mencionados



sinan |>
  mutate(
  cs_escol_n = cs_escol_n |> 
  #Substituir observaçõa NA, pelo level Missing
  fct_na_value_to_level(level = "Missing") |>
  #dplyr::recode_factor
  fct_recode(
  "Analfabeto" = "00",
  '1ª a 4ª série incompleta do EF' = "01",
  "4ª série completa do EF (antigo 1° grau)" = "02",
  "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)" = "03",
  "Ensino fundamental completo (antigo ginásio ou 1° grau)" = "04",
  "Ensino médio incompleto (antigo colegial ou 2° grau)" = "05",
  "Ensino médio completo (antigo colegial ou 2° grau)" = "06",
  "Educação superior incompleta" = "07",
  "Educação superior completa" = "08",
  "Ignorado" = "09",
  "Não se aplica" = "10", .default = "Missing") ) |> tabyl(cs_escol_n)

sinan |>
  mutate(
  #Número de envolvidos
  num_envolv = case_match(num_envolv, "1" ~ "Um", "2" ~ "Dois ou mais", "9" ~ "Ignorado", .default = "Missing",
                          .ptype = factor(levels = levels(num_envolv)) 
                          )  ) |> select(num_envolv) |> glimpse()




sinan |>
  mutate(
  #Escolaridade
    cs_escol_n = 
      case_when(cs_escol_n == "00" ~ "Analfabeto",
                cs_escol_n == "01" ~ "1ª a 4ª série incompleta do EF", 
                cs_escol_n == "02" ~ "4ª série completa do EF (antigo 1° grau)",
                cs_escol_n == "03" ~ "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
                cs_escol_n == "04" ~ "Ensino fundamental completo (antigo ginásio ou 1° grau)",
                cs_escol_n == "05" ~ "Ensino médio incompleto (antigo colegial ou 2° grau)", 
                cs_escol_n == "06" ~ "Ensino médio completo (antigo colegial ou 2° grau)",
                cs_escol_n == "07" ~ "Educação superior incompleta", 
                cs_escol_n == "08" ~ "Educação superior completa", 
                cs_escol_n == "09" ~ "Ignorado", 
                cs_escol_n == "10" ~ "Não se aplica",
                .default = "Missing") |> 
      #Ordem dos Levels
      fct_relevel( c("Analfabeto","1ª a 4ª série incompleta do EF","4ª série completa do EF (antigo 1° grau)",
                                                        "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
                                                        "Ensino fundamental completo (antigo ginásio ou 1° grau)",
                                                        "Ensino médio incompleto (antigo colegial ou 2° grau)", 
                                                        "Ensino médio completo (antigo colegial ou 2° grau)",
                                                        "Educação superior incompleta", 
                                                        "Educação superior completa", 
                                                        "Ignorado", 
                                                        "Não se aplica" ) ) ) %>% tabyl(cs_escol_n)

    