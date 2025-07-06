library(tidyverse)
#library(lubridate)
#library(readr)
library(rio)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_09_2021_preliminar.RData")
# Homossexuais ------------------------------------------------------------
sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2019 & orient_sex == "Homossexual (gay-lésbica)" & grupo_viol!= "Autoprovocada") %>% 
  droplevels() -> base2019

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2020 & orient_sex == "Homossexual (gay-lésbica)" & grupo_viol!= "Autoprovocada") %>% 
  droplevels() -> base2020


#Agressão por município
base2017 %>% tabyl(id_municip,show_na = T,show_missing_levels = T)%>% 
  adorn_totals("row") %>% arrange(id_municip) -> x
sum(is.na(x$id_municip)) 


#Agressão por unidade
base2019 %>% tabyl(id_unidade,show_na = T,show_missing_levels = T)  %>% 
  adorn_totals("row") %>% arrange(id_unidade) -> x
sum(is.na(x$id_unidade)) 


#Valor Bruto
((tabyl(base2018,orient_sex)/tabyl(base2017,orient_sex))-1)*100
tabyl(base2017,orient_sex)
tabyl(base2018,orient_sex)
tabyl(base2019,orient_sex)
tabyl(base2020,orient_sex)

#Seleciona municípios que constam nas duas bases - Atlerar os anos com mudanças de atlas.
base2019 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2020 %>% filter(id_municip != "") -> b2
#x <- semi_join(base2018,base2017, by = "id_municip", keep = TRUE) #Mantém todos os municípios observados em 2018 com ocorrência em 2017.
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2020 <- base2020 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2020 %>% filter(id_municip != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.(Mesmos Municípios)
tabyl(base_municipios_2020, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2020)


#Criando bases com unidades de saúde comuns entre os anos.
#Seleciona unidades que constam nas duas bases - Alterar os anos a medida que o altas avançar.
base2019 %>% filter(id_unidade != "") -> b1 #Seleciona unidades de saúde conhecidas.
base2020 %>% filter(id_unidade != "") -> b2
vetor_unidade = intersect(b1$id_unidade,b2$id_unidade)
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
base_unidades_2020 <- base2020 %>% filter(id_unidade %in% vetor_unidade)

#Unidades conhecidas
tabyl(base2020 %>% filter(id_unidade != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Unidades iguais as do período anterior. (Mesma Unidade)
tabyl(base_unidades_2020, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidade,base_unidades_2020,base2019,base2020)


# Bissexuais --------------------------------------------------------------
sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2019 & orient_sex == "Bissexual" & grupo_viol!="Autoprovocada") %>%
  droplevels() -> base2019

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2020 & orient_sex == "Bissexual" & grupo_viol!="Autoprovocada") %>%
  droplevels() -> base2020


#Agressão por município
base2020 %>% tabyl(id_municip,show_na = T,show_missing_levels = T)%>% 
  adorn_totals("row") %>% arrange(id_municip) -> x
sum(is.na(x$id_municip)) 

#Agressão por unidade
base2019 %>% tabyl(id_unidade,show_na = T,show_missing_levels = T)  %>% 
  adorn_totals("row") %>% arrange(id_unidade) -> x
sum(is.na(x$id_unidade)) 


#Valor Bruto
((tabyl(base2018,orient_sex)/tabyl(base2017,orient_sex))-1)*100
tabyl(base2017,orient_sex)
tabyl(base2018,orient_sex)
tabyl(base2019,orient_sex)

#Seleciona municípios que constam nas duas bases - Atlerar os anos com mudanças de atlas.
base2019 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2020 %>% filter(id_municip != "") -> b2
#x <- semi_join(base2018,base2017, by = "id_municip", keep = TRUE) #Mantém todos os municípios observados em 2018 com ocorrência em 2017.
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2020 <- base2020 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2020 %>% filter(id_municip != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos municípios)
tabyl(base_municipios_2020, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2020)

#Criando bases com unidades de saúde comuns entre os anos.
#Seleciona unidades que constam nas duas bases - Alterar os anos a medida que o altas avançar.
base2019 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2020 %>% filter(id_municip != "") -> b2
vetor_unidade = intersect(b1$id_unidade,b2$id_unidade)
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
base_unidades_2020 <- base2020 %>% filter(id_unidade %in% vetor_unidade)

#Unidades conhecidos
tabyl(base2020 %>% filter(id_unidade != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Unidades iguais ao do período anterior. (Mesma Unidade)
tabyl(base_unidades_2020, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidade,base_unidades_2019)


# População LGBTQI+ -------------------------------------------------------
# sinan <- import("D:/Users/B224552695/Desktop/Trabalhos/Sinan/sinan_br_09_19.csv")
# 
# #Ano de notificação
# sinan %>% mutate(ano = year(dt_notific)) -> sinan
# 
# ###Criando grupo agressor - Como melhorar essa rotina?
# sinan %>% mutate(agr_fam =
#                    #Doméstica
#                    case_when(rel_pai==1 | rel_mae== 1 | rel_mad== 1 | rel_pad==1 | rel_conj==1 | rel_excon==1 |
#                                rel_namo==1 | rel_exnam==1 | rel_filho==1 | rel_irmao==1 | rel_cuida==1 ~ 1, TRUE ~ 2),
#                  #Comunitária
#                  agr_extfam = case_when(rel_conhec==1 | rel_desco==1 ~ 1, TRUE ~ 2),
#                  #Institucional
#                  agr_inst = case_when(rel_patrao==1 | rel_inst==1 | rel_pol==1 ~ 1, TRUE ~ 2),
#                  #*Autoprovocada*
#                  agr_propr = case_when(rel_propri==1 ~ 1, TRUE ~ 2),
#                  #*Outros*
#                  agr_outro = case_when(rel_outros==1 ~ 1, TRUE ~ 2),
#                  #Qualquer uma das anteriores
#                  agr = case_when(rel_pai==1 | rel_mae== 1 | rel_mad== 1 | rel_pad==1 | rel_conj==1 | rel_excon==1 | 
#                                    rel_namo==1 | rel_exnam==1 | rel_filho==1 | rel_irmao==1 | rel_cuida==1 | rel_conhec==1 |
#                                    rel_desco==1 | rel_patrao==1 | rel_inst==1 | rel_pol==1 | rel_propri==1 | rel_outros==1 ~ 1, TRUE ~ 2)) -> sinan
# 
# 
# # sinan %>% tabyl(ano,agr_fam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_extfam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_inst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_propr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_outro, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# 
# 
# #Individualizando o grupo de agressão.
# sinan %>% mutate(g_fam = case_when(agr_fam==1 & (agr_extfam!=1 & agr_inst!=1 & agr_propr!=1 & agr_outro!=1) ~ 1)) -> sinan
# sinan %>% mutate(g_extrafam = case_when(agr_extfam==1 & (agr_fam==2 & agr_inst==2 & agr_propr==2 & agr_outro==2) ~ 1)) -> sinan
# sinan %>% mutate(g_inst = case_when(agr_inst==1 & (agr_fam!=1 & agr_extfam!=1 & agr_propr!=1 & agr_outro!=1) ~ 1)) -> sinan
# sinan %>% mutate(g_auto = case_when(agr_propr==1 & (agr_fam!=1 & agr_inst!=1 & agr_extfam!=1 & agr_outro!=1) ~ 1)) -> sinan
# sinan %>% mutate(g_outros = case_when(agr_outro==1 & (agr_fam!=1 & agr_inst!=1 & agr_extfam!=1 & agr_propr!=1) ~ 1)) -> sinan
# 
# # sinan %>% tabyl(ano,g_fam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,g_extrafam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_inst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_propr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# # sinan %>% tabyl(ano,agr_outro, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# 
# 
# #Variável única do grupo de agressão.
# sinan %>% mutate(grupo_viol = case_when(g_fam == 1 ~ "Doméstica",
#                                         g_extrafam==1 ~ "Comunitária",
#                                         g_inst==1 ~ "Institucional", 
#                                         g_auto==1 ~ "Autoprovocada")) -> sinan
# sinan %>% mutate(grupo_viol = replace(grupo_viol,
#                                       (g_outros == 1 | (agr == 1 & grupo_viol == "")),"Misto")) -> sinan
# 
# #COLOCAR O MISTO
# 
# sinan %>% tabyl(ano,grupo_viol, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
# 
# 
# sinan %>% select(!c(agr_fam, agr_extfam, agr_fam, agr_extfam,  agr_inst,  agr_propr, 
#                     agr_outro, agr, g_fam, g_extrafam, g_inst, g_auto, g_outros)) -> sinan


sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2017 & (orient_sex == "Homossexual (gay-lésbica)" |orient_sex == "Bissexual") & grupo_viol!="Autoprovocada") -> base2017

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2018 & (orient_sex == "Homossexual (gay-lésbica)" |orient_sex == "Bissexual") & grupo_viol!="Autoprovocada") -> base2018


sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2019 & (orient_sex == "Homossexual (gay-lésbica)" |orient_sex == "Bissexual") & grupo_viol!="Autoprovocada") -> base2019

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2020 & (orient_sex == "Homossexual (gay-lésbica)" |orient_sex == "Bissexual") & grupo_viol!="Autoprovocada") -> base2020

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2021 & (orient_sex == "Homossexual (gay-lésbica)" |orient_sex == "Bissexual") & grupo_viol!="Autoprovocada") -> base2021





rm(sinan)

#Violência Física - Valor Bruto
tabyl(base2017 %>% filter(viol_fisic == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2018 %>% filter(viol_fisic == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2019 %>% filter(viol_fisic == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))

#Violência psicológica - Valor Bruto
tabyl(base2017 %>% filter(viol_psico == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2018 %>% filter(viol_psico == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2019 %>% filter(viol_psico == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))

#Tortura - Valor bruto
tabyl(base2017 %>% filter(viol_tort == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2018 %>% filter(viol_tort == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2019 %>% filter(viol_tort == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))

#Outros - valor Bruto
tabyl(base2017 %>% filter(viol_outr == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2018 %>% filter(viol_outr == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))
tabyl(base2019 %>% filter(viol_outr == "Sim"),orient_sex) %>% adorn_totals(where = c("row", "col"))

#Mesmo município - Violência Física - LGBTQI
base2018 %>% filter(viol_fisic == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2019 %>% filter(viol_fisic == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2019 <- base2019 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2019 %>% filter(id_municip != "" & viol_fisic == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos Municípios)
tabyl(base_municipios_2019 %>% filter(viol_fisic == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2019)


#Mesmo município - Violência psicológica - LGBTQI
base2018 %>% filter(viol_psico == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2019 %>% filter(viol_psico == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2019 <- base2019 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos
tabyl(base2019 %>% filter(id_municip != "" & viol_psico == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_municipios_2019 %>% filter(viol_psico == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2019)


#Mesmo município - Tortura - LGBTQI 
base2018 %>% filter(viol_tort == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2019 %>% filter(viol_tort == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2019 <- base2019 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos
tabyl(base2019 %>% filter(id_municip != "" & viol_tort == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_municipios_2019 %>% filter(viol_tort == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2019)


##Mesmo município - Outros - LGBTQI 
base2019 %>% filter(id_municip != "" & viol_outr == "Sim") ->b1 #Seleciona municípios com violência física.
base2018 %>% filter(id_municip != "" & viol_outr == "Sim") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2019 <- base2019 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos
tabyl(base2019 %>% filter(id_municip != "" & viol_outr == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_municipios_2019 %>% filter(viol_outr == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2019)


###Mesma unidade LGBTI
#Mesmo unidade - Violência Física - LGBTQI
base2018 %>% filter(viol_fisic == "Sim" & id_unidade!= "") ->b1 #Seleciona unidades com violência física.
base2019 %>% filter(viol_fisic == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona unidades que constam nas duas bases.
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
#base_unidades_2018 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2019 <- base2019 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos
tabyl(base2019 %>% filter(id_unidade != "" & viol_fisic == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_unidades_2019 %>% filter(viol_fisic == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2019)


#Mesmo município - psicológica - LGBTQI Violência
#Mesmo município - Violência psicológica - LGBTQI
base2018 %>% filter(viol_psico == "Sim" & id_unidade!= "") ->b1 #Seleciona municípios com violência física.
base2019 %>% filter(viol_psico == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com Unidades comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2019 <- base2019 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos
tabyl(base2018 %>% filter(id_unidade != "" & viol_psico == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Unidades iguais ao do período anterior.
tabyl(base_unidades_2019 %>% filter(viol_psico == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2019)


#Mesmo Unidade - Tortura - LGBTQI 
base2018 %>% filter(viol_tort == "Sim" & id_unidade!= "") ->b1 #Seleciona municípios com violência física.
base2019 %>% filter(viol_tort == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com Unidades comuns entre os anos.
#base_unidades_2018 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2019 <- base2019 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos
tabyl(base2019 %>% filter(id_unidade != "" & viol_tort == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_unidades_2019 %>% filter(viol_tort == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2019)


##Mesmo Unidade - Outros - LGBTQI 
base2019 %>% filter(id_unidade != "" & viol_outr == "Sim") ->b1 #Seleciona municípios com violência física.
base2018 %>% filter(id_unidade != "" & viol_outr == "Sim") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2019 <- base2019 %>% filter(id_unidade %in% vetor_unidades)

#Municípios conhecidos
tabyl(base2019 %>% filter(id_unidade != "" & viol_outr == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.
tabyl(base_unidades_2019 %>% filter(viol_outr == "Sim"),orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2019)
