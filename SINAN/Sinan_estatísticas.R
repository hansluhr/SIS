library(tidyverse)
library(janitor)

#Importando base
load("C:/Users/b224552695/Desktop/r/Sinan/sinan_09_2022_refeito.RData")

year <- c(2014:2022)


# Provável Autor ----------------------------------------------------------
##Histograma dos prováveis autores da notificação, por idade.
sinan |> filter(cs_sexo == "Mulher" & (les_autop!="Sim" & rel_propri!="Sim") ) |> drop_na(idade) |>
  #slice_sample(n = 10000) |>
  #mutate(nome_viol = nome_viol |> fct_lump(prop = 0.0001) ) |>
  mutate(nome_rel = nome_rel |> fct_lump(prop = 0.01) ) |> #tabyl(nome_rel) |> adorn_totals()
  ggplot() +
  geom_histogram(aes(x=idade,fill = nome_rel),binwidth  = 3) 


#Tabela dos prováveis autores, por faixa etária - Mulher
sinan |> filter(les_autop!="Sim" & rel_propri!="Sim") |> drop_na(idade) |>
  mutate(nome_rel = nome_rel |> fct_lump(prop = 0.005) ) |>
  group_by(idade = (idade %/% 10) * 10 ) |>
  tabyl(nome_rel,idade,cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") #|> rio::export(x=_,"autor_sinan.xlsx")
  
  
#Tabela dos prováveis autores, por faixa etária - Mulher
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & rel_propri!="Sim" & grupo_viol == "Doméstica" ) |> drop_na(idade) |>
  mutate(nome_rel = nome_rel |> fct_lump(prop = 0.005) ) |>
  group_by(idade = (idade %/% 10) * 10 ) |>
  tabyl(nome_rel,idade) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |> rio::export(x=_,"autor_mulher_sinan.xlsx")


#Tabela Grupos de violência
sinan |> filter(cs_sexo == "Mulher" & (les_autop!="Sim" & rel_propri!="Sim") ) |>
  mutate(nome_rel = nome_rel |> fct_lump(prop = 0.001) ) |>
  tabyl(nome_rel,grupo_viol) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |> view()





# Encaminhamento Violência doméstica --------------------------------------
#Houve encaminhamento ?
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |>
  tabyl(houve_enca)

# sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |> 
#   summarise(n = n(), .by = houve_enca) |>
#   mutate(prop_enc = n/sum(n)*100) |> adorn_totals()


#Se houve encaminhamento, qual foi o encaminhamento?
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |> 
  mutate(nome_enca = nome_enca |> fct_lump_prop(prop = 0.005) ) |>
  tabyl(nome_enca) |> adorn_totals(where = c("row") ) %>%
  adorn_pct_formatting(digits = 1) |> view()
  
  
#Se houve encaminhamento, quantas foram para ao menos deleg?
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |> 
  #Encamimanehto ao menos ao mpu
  filter(str_detect(nome_enca,"deleg") ) |> count(nome_enca, sort = TRUE) |> 
  summarise(deleg = sum(n) )


#Se houve encaminhamento, quantas foram para ao menos deleg?
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |> 
  #Encamimanehto ao menos ao mpu
  filter(str_detect(nome_enca,"deleg") ) |> count(nome_enca, sort = TRUE) |> 
  summarise(deleg = sum(n) )


#Se houve encaminhamento, qual a proporção dos encaminhamentos
sinan |> filter(cs_sexo == "Mulher" & les_autop!="Sim" & grupo_viol == "Doméstica" & ano_not %in% year) |> 
  count(nome_enca, sort = TRUE, name = "n_enca") |> 
  mutate(prop_enc = n_enca/sum(n_enca)*100) |>  adorn_totals() |> head()



  #Encamimanehto ao menos ao mpu
  filter(str_detect(nome_enca,"deleg") ) |> count(nome_enca, sort = TRUE) |> 
  summarise(deleg = sum(n) )



  
  
#Tabela Instrumento violência doméstica
sinan |>  filter(cs_sexo == "Mulher" & les_autop!="Sim" & rel_propri!="Sim" & grupo_viol == "Doméstica") |>
  count(nome_viol,nome_instr,nome_rel,nome_enca, sort = TRUE) |> view()







# Violência doméstica e suicídio ------------------------------------------
#Violência doméstica, suicídio
sinan |> filter(cs_sexo == "Mulher" & grupo_viol == "Doméstica" &
                  #Violência doméstica foi autoprovocada
                  les_autop == "Sim") |>
  count(viol_espec, sort = TRUE) |> view()
#Pode ser interpretado como tentativas de suicídio causadas por violência doméstica 
#Serve de retrato das tentativas de suicídio. Não deve ser considerado 


#Violência doméstica, suicídio e autor 
sinan |> filter(cs_sexo == "Mulher" & grupo_viol == "Doméstica" &
                  #Violência doméstica foi autoprovocada
                  les_autop == "Sim") |>
    count(viol_espec,nome_viol, nome_rel, sort = TRUE) |> view()
#Pode ser interpretado como tentativas de suicídio causadas por violência doméstica 
#Serve de retrato das tentativas de suicídio. Não deve ser considerado 


