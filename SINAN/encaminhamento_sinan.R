library(tidyverse)
library(janitor)

#Importando base SINAN
load("C:/Users/b224552695/Desktop/r/Sinan/sinan_09_2022_preliminar_transtorno.RData")
year <- c(2022)


#Outra maneira de identificar se houve encaminhamento 
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> 
  mutate(houve_enca = case_when(if_any(c(rede_sau:defen_publ), ~.x == "Sim") ~ "Sim", 
                                if_any(c(rede_sau:defen_publ), is.na) ~ "Informação Incompleta", 
                                if_all(c(rede_sau:defen_publ), ~.x == "Ignorado") ~ "Ignorado",
                                if_all(c(rede_sau:defen_publ), ~.x == "Não") ~ "Não",
                                #Como Tratar os outros casos ?
                                .default = "Desconhecido") ) -> sinan

#Total e proporção dos encaminhamentos, por tipo de encaminhamento
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "Mulher" & ano_not %in% year) |> 
  summarise(n = n(), .by = houve_enca) |>
  mutate(prop_enc = n/sum(n)*100) |> adorn_totals()


#Melhor maneira de fazer isso é atribuir o nome da variável de encaminhamento a level sim da variável.
#Assim, posso fazer count direto da variável.


# Função para capturar os nomes das variáveis que atendem a condição "Sim"
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- NA_character_
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}

# Adicionar uma nova variável que captura os nomes das variáveis categóricas que atendem a condição "Sim"
sinan |> 
   filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> 
   select(rede_sau:defen_publ) %>% 
   mutate(across(everything() , ~ fct_na_value_to_level(.,"Informação Incompleta")  ) ) %>%
   mutate(nomes_var_sim = apply(., 1, capturar_nomes_sim) ) |> #view() #filter(is.na(nomes_var_sim)) |> view()
   select(nomes_var_sim) |> view()
   count(nomes_var_sim, sort = TRUE) |> filter(!is.na(nomes_var_sim)) |>  summarise(n = sum(n)) #Está batendo com o houve atendimento
  
  

