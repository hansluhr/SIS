library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)

# PNADc  --------------------------------------------------------------
#Variáveis desejadas
vars <- c("UF", "Trimestre","V1032", "V2007", "V2010","V2009")
#Precisa atualizar até 2019
PNADc_anual <- get_pnadc(year = 2023, interview = 1,
                         labels = TRUE, design = TRUE)

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

#Total de observa??es
sum(PNADc_anual$variables$one)

#População total
svytotal(~one, design = PNADc_anual, na.rm = T)
svytotal(~V2007, design = PNADc_anual, na.rm = T)

#População total - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
uf1 <- svyby(formula=~one, by=~UF, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População total - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~regioes, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(UF = regioes) -> reg1

#Criando arquivo PNADC e Juntando uf e regi?es.
PNADc <- rbind(uf1,reg1)
rm(uf1,reg1)

#População Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf2 <- svyby(formula=~V2007, by=~UF, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg2 <-  svyby(formula=~V2007, by=~regioes, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg2 %>% rename(UF = regioes) -> reg2

ufreg2 <- rbind(uf2,reg2)

#Juntando PNADc e ufreg2  - Uf
PNADc <- left_join(PNADc,ufreg2, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  # rename(Pop = statistic,
  #        PoP.Homem = statistic.V2007Masculino,
  #        PoP.Mulher = statistic.V2007Feminino)
  #Renomeando variáveis ano > 2016
  rename(Pop = statistic,
         PoP.Homem = statistic.V2007Homem,
         PoP.Mulher = statistic.V2007Mulher)

rm(uf2,reg2,ufreg2)

#População cor - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf3 <- svyby(formula=~V2010, by=~UF, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População cor - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg3 <- svyby(formula=~V2010, by=~regioes, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg3 %>% rename(UF = regioes) -> reg3

ufreg3 <- rbind(uf3,reg3)

#Renomeando e criando variáveis
ufreg3 %>% rename(Pop.Branca = statistic.V2010Branca,
               Pop.Preta = statistic.V2010Preta,
               Pop.Amarela = statistic.V2010Amarela,
               Pop.Parda = statistic.V2010Parda,
               Pop.Indígena = statistic.V2010Indígena,
               Pop.Ignorado = statistic.V2010Ignorado) %>%
mutate(Pop.Negro = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg3

#Juntando PNADc e ufreg3  - Uf
PNADc <- left_join(PNADc,ufreg3, by = "UF") 
rm(uf3,reg3,ufreg3)

#População Cor e Homem - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
uf4<- svyby(formula=~V2010, by=~UF, subset(PNADc_anual, V2007 == "Homem"), FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Homem - Regioes
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Homem"), na.rm = T)
reg4 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Homem"), FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg4 %>% rename(UF = regioes) -> reg4
#Jutando uf e regiões
ufreg4 <- rbind(uf4,reg4)
#Renomeando e criando variáveis
ufreg4 %>% rename(H.Branca = statistic.V2010Branca,
               H.Preta = statistic.V2010Preta,
               H.Amarela = statistic.V2010Amarela,
               H.Parda = statistic.V2010Parda,
               H.Indígena = statistic.V2010Indígena,
               H.Ignorado = statistic.V2010Ignorado) %>%
  mutate(H.Negro = H.Preta + H.Parda,
         H.Não_Branca = H.Preta + H.Amarela + H.Parda + H.Indígena,
         H.Não_Negra = H.Branca + H.Amarela + H.Indígena) -> ufreg4

#Juntando PNADc e ufreg4  - Uf
PNADc <- left_join(PNADc,ufreg4, by = "UF")
rm(uf4,reg4,ufreg4)


#População Cor e Mulher - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
uf5 <- svyby(formula=~V2010, by=~UF, subset(PNADc_anual, V2007 == "Mulher"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Mulher - Regiões
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
reg5 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Mulher"), FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reigões para permitir o rbind.
reg5 %>% rename(UF = regioes) -> reg5
#Jutando uf e regi?es
ufreg5 <- rbind(uf5,reg5)
#Renomeando e criando vari?veis
ufreg5 %>% rename(M.Branca = statistic.V2010Branca,
               M.Preta = statistic.V2010Preta,
               M.Amarela = statistic.V2010Amarela,
               M.Parda = statistic.V2010Parda,
               M.Indígena = statistic.V2010Indígena,
               M.Ignorado = statistic.V2010Ignorado) %>%
  mutate(M.Negro = M.Preta + M.Parda,
         M.Não_Branca = M.Preta + M.Amarela + M.Parda + M.Indígena,
         M.Não_Negra = M.Branca + M.Amarela + M.Indígena) -> ufreg5

#Juntando PNADc e ufreg5  - Uf
PNADc <- left_join(PNADc,ufreg5, by = "UF")
rm(uf5,reg5,ufreg5)

#Acrescentando Ciclas das UFs
PNADc %>% mutate(cuf = case_when(
  UF == "Rondônia" ~ "RO",UF == "Acre" ~ "AC",UF == "Amazonas"  ~ "AM",
  UF == "Roraima" ~ "RR", UF == "Pará" ~ "PA",  UF == "Amapá" ~ "AP", 
  UF == "Tocantins" ~ "TO", UF == "Maranhão" ~ "MA", UF == "Piauí" ~ "PI",
  UF == "Ceará" ~ "CE", UF == "Rio Grande do Norte" ~ "RN", UF == "Paraíba" ~ "PB",
  UF == "Pernambuco" ~ "PE", UF == "Alagoas" ~ "AL", UF == "Sergipe" ~ "SE",
  UF == "Bahia" ~ "BA", UF == "Minas Gerais" ~ "MG", UF == "Espírito Santo"  ~ "ES",
  UF == "Rio de Janeiro" ~ "RJ", UF == "São Paulo" ~ "SP", UF == "Paraná" ~ "PR",
  UF == "Santa Catarina" ~ "SC", UF == "Rio Grande do Sul" ~ "RS", UF == "Mato Grosso do Sul" ~ "MS",
  UF == "Mato Grosso" ~ "MT", UF == 'Goiás' ~ "GO", UF == "Distrito Federal" ~ "DF"), 
  .after = UF) -> PNADc

#Check sidra tabelas 6786, 6706, 6408 e 6407
export(PNADc,"pnadc.xlsx")
rm(PNADc)

# População Jovem -------------------------------------------------------------------
#População Jovem - total - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
uf1 <- svyby(formula=~one, by=~UF, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem - total - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~regioes, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(UF = regioes) -> reg1

#Criando arquivo PNADC e Juntando uf e regi?es.
PNADc_jovem <- rbind(uf1,reg1)
rm(uf1,reg1)

#População Jovem - Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf2 <- svyby(formula=~V2007, by=~UF, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem - Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg2 <-  svyby(formula=~V2007, by=~regioes, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
               na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg2 %>% rename(UF = regioes) -> reg2

ufreg2 <- rbind(uf2,reg2)

#Juntando PNADc e ufreg2  - Uf
PNADc_jovem <- left_join(PNADc_jovem,ufreg2, by = "UF") %>%
  # #Renomeando vari?veis ano < 2016
  # rename(Pop = statistic,
  # PoP.Homem = statistic.V2007Masculino,
  # PoP.Mulher = statistic.V2007Feminino)
  #Renomeando vari?veis ano > 2016
  rename(Pop = statistic,
         PoP.Homem = statistic.V2007Homem,
         PoP.Mulher = statistic.V2007Mulher)
  
rm(uf2,reg2,ufreg2)

#População Jovem cor - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf3 <- svyby(formula=~V2010, by=~UF, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem cor - Regi?es
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg3 <- svyby(formula=~V2010, by=~regioes, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg3 %>% rename(UF = regioes) -> reg3

ufreg3 <- rbind(uf3,reg3)

#Renomeando e criando vari?veis
ufreg3 %>% rename(Pop.Branca = statistic.V2010Branca,
                  Pop.Preta = statistic.V2010Preta,
                  Pop.Amarela = statistic.V2010Amarela,
                  Pop.Parda = statistic.V2010Parda,
                  Pop.Indígena = statistic.V2010Indígena,
                  Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Pop.Negro = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg3

#Juntando PNADc e ufreg3  - Uf
PNADc_jovem <- left_join(PNADc_jovem,ufreg3, by = "UF") 
rm(uf3,reg3,ufreg3)

#População Jovem Cor e Homem - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
uf4<- svyby(formula=~V2010, by=~UF, subset(PNADc_anual, V2007 == "Homem" & Fxet == "Jovem"), 
      FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Jovem Cor e Homem - Regioes
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
reg4 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Homem" & Fxet == "Jovem"), 
 FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg4 %>% rename(UF = regioes) -> reg4
#Jutando uf e regiões
ufreg4 <- rbind(uf4,reg4)

#Renomeando e criando variáveis
ufreg4 %>% rename(H.Branca = statistic.V2010Branca,
                  H.Preta = statistic.V2010Preta,
                  H.Amarela = statistic.V2010Amarela,
                  H.Parda = statistic.V2010Parda,
                  H.Indígena = statistic.V2010Indígena,
                  H.Ignorado = statistic.V2010Ignorado) %>%
  mutate(H.Negro = H.Preta + H.Parda,
         H.Não_Branca = H.Preta + H.Amarela + H.Parda + H.Indígena,
         H.Não_Negra = H.Branca + H.Amarela + H.Indígena) -> ufreg4

#Juntando PNADc e ufreg4  - Uf
PNADc_jovem <- left_join(PNADc_jovem,ufreg4, by = "UF")
rm(uf4,reg4,ufreg4)


#Popula??o Cor e Mulher - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
uf5 <- svyby(formula=~V2010, by=~UF, subset(PNADc_anual, V2007 == "Mulher"& Fxet == "Jovem"),
FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Popula??o Cor e Mulher - Regi?es
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
reg5 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Mulher"& Fxet == "Jovem"), 
FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg5 %>% rename(UF = regioes) -> reg5
#Jutando uf e regi?es
ufreg5 <- rbind(uf5,reg5)

#Renomeando e criando vari?veis
ufreg5 %>% rename(M.Branca = statistic.V2010Branca,
                  M.Preta = statistic.V2010Preta,
                  M.Amarela = statistic.V2010Amarela,
                  M.Parda = statistic.V2010Parda,
                  M.Indígena = statistic.V2010Indígena,
                  M.Ignorado = statistic.V2010Ignorado) %>%
  mutate(M.Negro = M.Preta + M.Parda,
         M.Não_Branca = M.Preta + M.Amarela + M.Parda + M.Indígena,
         M.Não_Negra = M.Branca + M.Amarela + M.Indígena) -> ufreg5

#Juntando PNADc e ufreg5  - Uf
PNADc_jovem <- left_join(PNADc_jovem,ufreg5, by = "UF")
rm(uf5,reg5,ufreg5)

#Acrescentando Ciclas das UFs
PNADc_jovem %>% mutate(cuf = case_when(
  UF == "Rondônia" ~ "RO",UF == "Acre" ~ "AC",UF == "Amazonas"  ~ "AM",
  UF == "Roraima" ~ "RR", UF == "Pará" ~ "PA",  UF == "Amapá" ~ "AP", 
  UF == "Tocantins" ~ "TO", UF == "Maranhão" ~ "MA", UF == "Piauí" ~ "PI",
  UF == "Ceará" ~ "CE", UF == "Rio Grande do Norte" ~ "RN", UF == "Paraíba" ~ "PB",
  UF == "Pernambuco" ~ "PE", UF == "Alagoas" ~ "AL", UF == "Sergipe" ~ "SE",
  UF == "Bahia" ~ "BA", UF == "Minas Gerais" ~ "MG", UF == "Espírito Santo"  ~ "ES",
  UF == "Rio de Janeiro" ~ "RJ", UF == "São Paulo" ~ "SP", UF == "Paraná" ~ "PR",
  UF == "Santa Catarina" ~ "SC", UF == "Rio Grande do Sul" ~ "RS", UF == "Mato Grosso do Sul" ~ "MS",
  UF == "Mato Grosso" ~ "MT", UF == 'Goiás' ~ "GO", UF == "Distrito Federal" ~ "DF"), 
  .after = UF) -> PNADc_jovem

export(PNADc_jovem,"pnadc_jovem.xlsx")
rm(PNADc_jovem)




# Rotinda demanda TSE -----------------------------------------------------
variaveis_selecionadas <- c("UF", "Trimestre","V1032", "V2007","V2009","VD3004")

PNADc_anual <- get_pnadc(year = 2018, var = variaveis_selecionadas,
                         interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

#Criando faixa et?ria
PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 16:17 ~ "16 a 17 anos",
    V2009 %in% 18:19 ~ "18 a 19", 
    V2009 %in% 20:29 ~ "20 a 29",
    V2009 %in% 30:39 ~ "30 a 39",
    V2009 %in% 40:49 ~ "40 a 49",
    V2009 %in% 50:59 ~ "50 a 59",
    V2009 %in% 60:69 ~ "60 a 69",
    V2009 >69 ~ "70 ou mais"),
  regioes = factor(case_when(
    #Norte
    UF == "Rond?nia" | UF == "Acre" | UF == "Amazonas" | UF == "Roraima" | UF == "Par?" |
      UF == "Amap?" | UF == "Tocantins" ~ "Norte",
    #Nordeste
    UF == "Maranh?o" | UF == "Piau?" | UF == "Cear?" | UF == "Rio Grande do Norte" |
      UF == "Para?ba" | UF == "Pernambuco" | UF == "Alagoas" | UF == "Sergipe" |
      UF == "Bahia" ~ "Nordeste",
    #Sudeste
    UF == "Minas Gerais" | UF == "Esp?rito Santo" | UF == "Rio de Janeiro" | 
      UF == "S?o Paulo" ~ "Sudeste",
    #Sul
    UF == "Paran?" | UF == "Santa Catarina" | UF == "Rio Grande do Sul" ~ "Sul",
    #Centro oeste
    UF == "Mato Grosso do Sul" | UF == "Mato Grosso" | UF == "Goi?s" | 
      UF == "Distrito Federal" ~ "Centro Oeste"))) -> PNADc_anual

#Acrescentando Ciclas das UFs
PNADc_anual %>% mutate(cuf = case_when(
  UF == "Rond?nia" ~ "RO",UF == "Acre" ~ "AC",UF == "Amazonas"  ~ "AM",
  UF == "Roraima" ~ "RR", UF == "Par?" ~ "PA",  UF == "Amap?" ~ "AP", 
  UF == "Tocantins" ~ "TO", UF == "Maranh?o" ~ "MA", UF == "Piau?" ~ "PI",
  UF == "Cear?" ~ "CE", UF == "Rio Grande do Norte" ~ "RN", UF == "Para?ba" ~ "PB",
  UF == "Pernambuco" ~ "PE", UF == "Alagoas" ~ "AL", UF == "Sergipe" ~ "SE",
  UF == "Bahia" ~ "BA", UF == "Minas Gerais" ~ "MG", UF == "Esp?rito Santo"  ~ "ES",
  UF == "Rio de Janeiro" ~ "RJ", UF == "S?o Paulo" ~ "SP", UF == "Paran?" ~ "PR",
  UF == "Santa Catarina" ~ "SC", UF == "Rio Grande do Sul" ~ "RS", UF == "Mato Grosso do Sul" ~ "MS",
  UF == "Mato Grosso" ~ "MT", UF == 'Goi?s' ~ "GO", UF == "Distrito Federal" ~ "DF"), 
  .after = UF) -> PNADc_anual

#Criando a escolariedade desejada
PNADc_anual %>% mutate(esc = case_when(
VD3004 == "Sem instru??o e menos de 1 ano de estudo" ~ "Sem instru??o (analfabeto)",
VD3004 == "Fundamental incompleto ou equivalente" ~ "Fundamental incompleto ou equivalente",
VD3004 == "Fundamental completo ou equivalente" | VD3004 == "M?dio incompleto ou equivalente" ~ "Fundamental completo e m?dio incompleto",
VD3004 == "M?dio completo ou equivalente" | VD3004 == "Superior incompleto ou equivalente" ~ "M?dio completo e superior incompleto",
VD3004 == "Superior completo" ~ "Superior completo")) -> PNADc_anual


#Popula??o por faixa et?ria.
svyby(~Fxet, by=~cuf, subset(PNADc_anual, V2007 == "Homem"),FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) %>%
  export(.,"pop.xlsx")

https://rpubs.com/leobarone/pnadc_srvyr

#Grau de instru??o
svyby(~VD3004, by=~UF, subset(PNADc_anual, V2009 > 14),FUN=svytotal, na.rm=TRUE, 
      vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Maiores de 16 anos.
svyby(~esc, by=~cuf, subset(PNADc_anual, V2009 >= 16 & V2007 == "Mulher"),FUN=svytotal, na.rm=TRUE, 
      vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) %>% export(.,"esc_mulher.xlsx")

svyby(~esc, by=~cuf, subset(PNADc_anual, V2009 >= 16 & V2007 == "Homem"),FUN=svytotal, na.rm=TRUE, 
      vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) %>% export(.,"esc_homem.xlsx")

#Maior ou igual a 30 anos
svyby(~esc, by=~cuf, subset(PNADc_anual, V2009 >= 30 & V2007 == "Mulher"),FUN=svytotal, na.rm=TRUE, 
      vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) %>% export(.,"esc_mulher.xlsx")

svyby(~esc, by=~cuf, subset(PNADc_anual, V2009 >= 30 & V2007 == "Homem"),FUN=svytotal, na.rm=TRUE, 
      vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) %>% export(.,"esc_homem.xlsx")



PNADc_anual %>% filter(V2009 >= 14) %>%
group_by(UF, VD3004) %>% 
  summarise(n = survey_total())

PNADc_anual %>% filter(V2009 > 15 & V2007 == "Homem" ) %>%
  group_by(UF, esc) %>% 
  summarise(n = survey_total()) %>% view()

PNADc_anual %>% filter(V2009 >= 14) %>%
  group_by(V2007, VD3004) %>% 
  summarise(n = survey_total())


# População idoso 60+ -----------------------------------------------------
library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)
#library(rio)
#Variáveis desejadas
variaveis_selecionadas <- c("UF", "Trimestre","V1032", "V2007", "V2010","V2009")

PNADc_anual <- get_pnadc(year = 2023, interview = 1,labels = T,design = TRUE)
PNADc_anual <- as_survey(PNADc_anual)
PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 >= 60 ~ "Idoso"),
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

#Total de observa??es
sum(PNADc_anual$variables$one)

#Popula??o total
svytotal(~one, design = subset(PNADc_anual, Fxet == "Idoso"), na.rm = T)
svytotal(~V2007, design = subset(PNADc_anual, Fxet == "Idoso"), na.rm = T)


#População total - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
uf1 <- svyby(formula=~one, by=~UF, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População total - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~regioes, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(UF = regioes) -> reg1

#Criando arquivo PNADC e Juntando uf e regi?es.
PNADc <- rbind(uf1,reg1)
rm(uf1,reg1)

#População Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
uf2 <- svyby(formula=~V2007, by=~UF, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg2 <-  svyby(formula=~V2007, by=~regioes, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
               na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg2 %>% rename(UF = regioes) -> reg2

ufreg2 <- rbind(uf2,reg2)

#Juntando PNADc e ufreg2  - Uf
PNADc <- left_join(PNADc,ufreg2, by = "UF") %>%
  #Renomeando variáveis ano < 2016
  # rename(Pop = statistic,
  # PoP.Homem = statistic.V2007Masculino,
  # PoP.Mulher = statistic.V2007Feminino)
  #Renomeando variáveis ano > 2016
  rename(Pop = statistic,
         PoP.Homem = statistic.V2007Homem,
         PoP.Mulher = statistic.V2007Mulher)

rm(uf2,reg2,ufreg2)

#População cor - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
uf3 <- svyby(formula=~V2010, by=~UF, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População cor - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg3 <- svyby(formula=~V2010, by=~regioes, design=subset(PNADc_anual, Fxet == "Idoso"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reig?es para permitir o rbind.
reg3 %>% rename(UF = regioes) -> reg3

ufreg3 <- rbind(uf3,reg3)

#Renomeando e criando variáveis
ufreg3 %>% rename(Pop.Branca = statistic.V2010Branca,
                  Pop.Preta = statistic.V2010Preta,
                  Pop.Amarela = statistic.V2010Amarela,
                  Pop.Parda = statistic.V2010Parda,
                  Pop.Indígena = statistic.V2010Indígena,
                  Pop.Ignorado = statistic.V2010Ignorado) %>%
  mutate(Pop.Negro = Pop.Preta + Pop.Parda,
         Pop.Não_Branca = Pop.Preta + Pop.Amarela + Pop.Parda + Pop.Indígena,
         Pop.Não_Negra = Pop.Branca + Pop.Amarela + Pop.Indígena) -> ufreg3

#Juntando PNADc e ufreg3  - Uf
PNADc <- left_join(PNADc,ufreg3, by = "UF") 
rm(uf3,reg3,ufreg3)

#População Cor e Homem - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Homem"), na.rm = T)
uf4 <- svyby(formula=~V2010, by=~UF, design = subset(PNADc_anual, V2007 == "Homem" & Fxet == "Idoso"), FUN=svytotal, 
            na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Homem - Regioes
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Homem"), na.rm = T)
reg4 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Homem" & Fxet == "Idoso"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg4 %>% rename(UF = regioes) -> reg4
#Jutando uf e regiões
ufreg4 <- rbind(uf4,reg4)
#Renomeando e criando variáveis
ufreg4 %>% rename(H.Branca = statistic.V2010Branca,
                  H.Preta = statistic.V2010Preta,
                  H.Amarela = statistic.V2010Amarela,
                  H.Parda = statistic.V2010Parda,
                  H.Indígena = statistic.V2010Indígena,
                  H.Ignorado = statistic.V2010Ignorado) %>%
  mutate(H.Negro = H.Preta + H.Parda,
         H.Não_Branca = H.Preta + H.Amarela + H.Parda + H.Indígena,
         H.Não_Negra = H.Branca + H.Amarela + H.Indígena) -> ufreg4

#Juntando PNADc e ufreg4  - Uf
PNADc <- left_join(PNADc,ufreg4, by = "UF")
rm(uf4,reg4,ufreg4)


#População Cor e Mulher - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
uf5 <- svyby(formula=~V2010, by=~UF, subset(PNADc_anual, V2007 == "Mulher" & Fxet == "Idoso"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Mulher - Regiões
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
reg5 <- svyby(formula=~V2010, by=~regioes, subset(PNADc_anual, V2007 == "Mulher" & Fxet == "Idoso"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reigões para permitir o rbind.
reg5 %>% rename(UF = regioes) -> reg5
#Jutando uf e regi?es
ufreg5 <- rbind(uf5,reg5)
#Renomeando e criando vari?veis
ufreg5 %>% rename(M.Branca = statistic.V2010Branca,
                  M.Preta = statistic.V2010Preta,
                  M.Amarela = statistic.V2010Amarela,
                  M.Parda = statistic.V2010Parda,
                  M.Indígena = statistic.V2010Indígena,
                  M.Ignorado = statistic.V2010Ignorado) %>%
  mutate(M.Negro = M.Preta + M.Parda,
         M.Não_Branca = M.Preta + M.Amarela + M.Parda + M.Indígena,
         M.Não_Negra = M.Branca + M.Amarela + M.Indígena) -> ufreg5

#Juntando PNADc e ufreg5  - Uf
PNADc <- left_join(PNADc,ufreg5, by = "UF")
rm(uf5,reg5,ufreg5)

#Acrescentando Ciclas das UFs
PNADc %>% mutate(cuf = case_when(
  UF == "Rondônia" ~ "RO",UF == "Acre" ~ "AC",UF == "Amazonas"  ~ "AM",
  UF == "Roraima" ~ "RR", UF == "Pará" ~ "PA",  UF == "Amapá" ~ "AP", 
  UF == "Tocantins" ~ "TO", UF == "Maranhão" ~ "MA", UF == "Piauí" ~ "PI",
  UF == "Ceará" ~ "CE", UF == "Rio Grande do Norte" ~ "RN", UF == "Paraíba" ~ "PB",
  UF == "Pernambuco" ~ "PE", UF == "Alagoas" ~ "AL", UF == "Sergipe" ~ "SE",
  UF == "Bahia" ~ "BA", UF == "Minas Gerais" ~ "MG", UF == "Espírito Santo"  ~ "ES",
  UF == "Rio de Janeiro" ~ "RJ", UF == "São Paulo" ~ "SP", UF == "Paraná" ~ "PR",
  UF == "Santa Catarina" ~ "SC", UF == "Rio Grande do Sul" ~ "RS", UF == "Mato Grosso do Sul" ~ "MS",
  UF == "Mato Grosso" ~ "MT", UF == 'Goiás' ~ "GO", UF == "Distrito Federal" ~ "DF"), 
  .after = UF) -> PNADc

#Check sidra tabelas 6786, 6706 e 6408
rio::export(PNADc,"pnadc_idoso.xlsx")
rm(PNADc)