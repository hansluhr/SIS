library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)


#Atlas do campo
# PNADc >= 2016 --------------------------------------------------------------
PNADc_anual <- get_pnadc(year = 2020, interview = 5,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto"),
  regioes = factor(case_when(
    #Norte
    UF == "Rondônia" | UF == "Acre" | UF == "Amazonas" | UF == "Roraima" | UF == "Pará" |
      UF == "Amapá" | UF == "Tocantins" ~ "Norte",
    #Nordeste
    UF == "Maranhão" | UF == "Piauí" | UF == "Ceará" | UF == "Rio Grande do Norte" |
      UF == "Paraíba" | UF == "Pernambuco" | UF == "Alagoas" | UF == "Sergipe" |
      UF == "Bahia" ~ "Nordeste",
    #Sudeste
    UF == "Minas Gerais" | UF == "Espírito Santo" | UF == "Rio de Janeiro" | 
      UF == "São Paulo" ~ "Sudeste",
    #Sul
    UF == "Paraná" | UF == "Santa Catarina" | UF == "Rio Grande do Sul" ~ "Sul",
    #Centro oeste
    UF == "Mato Grosso do Sul" | UF == "Mato Grosso" | UF == "Goiás" | 
      UF == "Distrito Federal" ~ "Centro Oeste"))) -> PNADc_anual

#Caso baixar pnadc sem labels 
#  regioes = case_when(
#    UF %in% 11:17 ~ "Norte",
#    UF %in% 50:53 ~ "Centro Oeste",
#    UF %in% 21:29 ~ "Nordeste",
#    UF %in% 31:35 ~ "Sudeste",
#    UF %in% 41:43 ~ "Sul")
#       ) -> PNADc_anual

#Total de observações
sum(PNADc_anual$variables$one)

#População total
svytotal(~one, design = PNADc_anual, na.rm = T)
svytotal(~V2007, design = PNADc_anual, na.rm = T)

#População Geral - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
uf1 <- svyby(formula=~one, by=~UF, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Geral - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~regioes, design=PNADc_anual, FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(UF = regioes) -> reg1

#Criando arquivo PNADC e Juntando uf e regiões.
PNADc <- rbind(uf1,reg1) %>% rename(Pop.Geral = statistic)
rm(uf1,reg1)


#População Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=PNADc_anual, FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=PNADc_anual, FUN=svytotal, 
                  na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Geral = statistic.V2007Homem,
         Mulher.Geral = statistic.V2007Mulher)

rm(uf_urb,reg_urb,ufreg)


#População Urbano - UFs
uf_urb <- svyby(formula=~one, by=~UF, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Urbano - Regiões
reg_urb <- svyby(formula=~one, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e UFs e região urbana
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis 
  rename(PoP.Urbana = statistic) 
rm(uf_urb,reg_urb,ufreg)


#População Rural - UFs
uf_urb <- svyby(formula=~one, by=~UF, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Rural - Regiões
reg_urb <- svyby(formula=~one, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e UFs e região urbana
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis 
  rename(PoP.Rural = statistic) 
rm(uf_urb,reg_urb,ufreg)



#População Sexo - Urbano - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
                  na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Urbano = statistic.V2007Homem,
         Mulher.Urbano = statistic.V2007Mulher)

rm(uf_urb,reg_urb,ufreg)


#População Sexo - Rural - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Rural - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                  na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Rural = statistic.V2007Homem,
         Mulher.Rural = statistic.V2007Mulher)

rm(uf_urb,reg_urb,ufreg)



#População Cor - Geral - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=PNADc_anual, FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Geral - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=PNADc_anual, FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                 Pop.Preta = statistic.V2010Preta,
                 Pop.Amarela = statistic.V2010Amarela,
                 Pop.Parda = statistic.V2010Parda,
                 Pop.Indígena = statistic.V2010Indígena,
                 Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Geral = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                         "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


#População Cor - Urbano - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=subset(PNADc_anual,V1022 == "Urbana"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Urbano - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                 Pop.Preta = statistic.V2010Preta,
                 Pop.Amarela = statistic.V2010Amarela,
                 Pop.Parda = statistic.V2010Parda,
                 Pop.Indígena = statistic.V2010Indígena,
                 Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Urbano = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                         "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


#População Cor - Rural - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=subset(PNADc_anual,V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Rural - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                 Pop.Preta = statistic.V2010Preta,
                 Pop.Amarela = statistic.V2010Amarela,
                 Pop.Parda = statistic.V2010Parda,
                 Pop.Indígena = statistic.V2010Indígena,
                 Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Rural = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                         "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


export(PNADc,"Pnadc_urb_rul.xlsx")











# PNADc < 2016 ------------------------------------------------------------
#variaveis_selecionadas <- c("UF", "Trimestre","V1032", "V2007", "V2010","V2009")
PNADc_anual <- get_pnadc(year = 2016, interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto"),
  regioes = factor(case_when(
    #Norte
    UF == "Rondônia" | UF == "Acre" | UF == "Amazonas" | UF == "Roraima" | UF == "Pará" |
      UF == "Amapá" | UF == "Tocantins" ~ "Norte",
    #Nordeste
    UF == "Maranhão" | UF == "Piauí" | UF == "Ceará" | UF == "Rio Grande do Norte" |
      UF == "Paraíba" | UF == "Pernambuco" | UF == "Alagoas" | UF == "Sergipe" |
      UF == "Bahia" ~ "Nordeste",
    #Sudeste
    UF == "Minas Gerais" | UF == "Espírito Santo" | UF == "Rio de Janeiro" | 
      UF == "São Paulo" ~ "Sudeste",
    #Sul
    UF == "Paraná" | UF == "Santa Catarina" | UF == "Rio Grande do Sul" ~ "Sul",
    #Centro oeste
    UF == "Mato Grosso do Sul" | UF == "Mato Grosso" | UF == "Goiás" | 
      UF == "Distrito Federal" ~ "Centro Oeste"))) -> PNADc_anual

#Caso baixar pnadc sem labels 
#  regioes = case_when(
#    UF %in% 11:17 ~ "Norte",
#    UF %in% 50:53 ~ "Centro Oeste",
#    UF %in% 21:29 ~ "Nordeste",
#    UF %in% 31:35 ~ "Sudeste",
#    UF %in% 41:43 ~ "Sul")
#       ) -> PNADc_anual

#Total de observações
sum(PNADc_anual$variables$one)

#População total
svytotal(~one, design = PNADc_anual, na.rm = T)
svytotal(~V2007, design = PNADc_anual, na.rm = T)

#População Geral - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
uf1 <- svyby(formula=~one, by=~UF, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Geral - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~regioes, design=PNADc_anual, FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(UF = regioes) -> reg1

#Criando arquivo PNADC e Juntando uf e regiões.
PNADc <- rbind(uf1,reg1) %>% rename(Pop.Geral = statistic)
rm(uf1,reg1)


#População Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=PNADc_anual, FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=PNADc_anual, FUN=svytotal, 
                  na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Geral = statistic.V2007Masculino,
         Mulher.Geral = statistic.V2007Feminino)

rm(uf_urb,reg_urb,ufreg)


#População Urbano - UFs
uf_urb <- svyby(formula=~one, by=~UF, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Urbano - Regiões
reg_urb <- svyby(formula=~one, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e UFs e região urbana
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis 
  rename(PoP.Urbana = statistic) 
rm(uf_urb,reg_urb,ufreg)


#População Rural - UFs
uf_urb <- svyby(formula=~one, by=~UF, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Rural - Regiões
reg_urb <- svyby(formula=~one, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e UFs e região urbana
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis 
  rename(PoP.Rural = statistic) 
rm(uf_urb,reg_urb,ufreg)



#População Sexo - Urbano - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
               na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Urbano = statistic.V2007Masculino,
         Mulher.Urbano = statistic.V2007Feminino)

rm(uf_urb,reg_urb,ufreg)


#População Sexo - Rural - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2007, by=~UF, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
#População Sexo - Rural - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg_urb <-  svyby(formula=~V2007, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                  na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Juntando PNADc e ufreg - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  rename(Homem.Rural = statistic.V2007Masculino,
         Mulher.Rural = statistic.V2007Feminino)

rm(uf_urb,reg_urb,ufreg)



#População Cor - Geral - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=PNADc_anual, FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Geral - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=PNADc_anual, FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reigões para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                 Pop.Preta = statistic.V2010Preta,
                 Pop.Amarela = statistic.V2010Amarela,
                 Pop.Parda = statistic.V2010Parda,
                 Pop.Indígena = statistic.V2010Indígena,
                 Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Geral = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                        "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


#População Cor - Urbano - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=subset(PNADc_anual,V1022 == "Urbana"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Urbano - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=subset(PNADc_anual, V1022 == "Urbana"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                  Pop.Preta = statistic.V2010Preta,
                  Pop.Amarela = statistic.V2010Amarela,
                  Pop.Parda = statistic.V2010Parda,
                  Pop.Indígena = statistic.V2010Indígena,
                  Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Urbano = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                     "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


#População Cor - Rural - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf_urb <- svyby(formula=~V2010, by=~UF, design=subset(PNADc_anual,V1022 == "Rural"), FUN=svytotal, 
                na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Cor - Rural - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg_urb <- svyby(formula=~V2010, by=~regioes, design=subset(PNADc_anual, V1022 == "Rural"), FUN=svytotal, 
                 na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg_urb %>% rename(UF = regioes) -> reg_urb

ufreg <- rbind(uf_urb,reg_urb)

#Renomeando e criando variáveis
ufreg %>% rename(Pop.Branca = statistic.V2010Branca,
                 Pop.Preta = statistic.V2010Preta,
                 Pop.Amarela = statistic.V2010Amarela,
                 Pop.Parda = statistic.V2010Parda,
                 Pop.Indígena = statistic.V2010Indígena,
                 Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Negro.Rural = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg

#Juntando PNADc e ufreg  - Uf
PNADc <- left_join(PNADc,ufreg, by = "UF") %>% select(!c("Pop.Branca","Pop.Preta","Pop.Amarela","Pop.Parda",
                                                         "Pop.Indígena","Pop.Ignorado","Pop.Não_Branca","Pop.Não_Negra"))
rm(uf_urb,reg_urb,ufreg)


export(PNADc,"Pnadc_urb_rul.xlsx")













