library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)

Com o uso da fun??o get_pnadc, os dados estar?o preparados,
inclusive com os r?tulos das vari?veis, a disponibiliza??o das
vari?veis de deflatores e a incorpora??o do plano amostral, para
serem analisados corretamente.

# PNADc  --------------------------------------------------------------
#Variáveis desejadas
#variaveis_selecionadas <- c("UF", "Trimestre","V1032", "V2007", "V2010","V2009")

#Informar ano desejado.
ano <- 2022

PNADc_anual <- get_pnadc(year = ano, interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto") ) -> PNADc_anual

#Total de observações
sum(PNADc_anual$variables$one)

#População total - Municípios
#svytotal(~interaction(Capital, one), design = PNADc_anual,na.rm = T)
mun1 <- svyby(formula=~one, by=~Capital, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População total - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~RM_RIDE, design=PNADc_anual, FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(Capital = RM_RIDE) -> reg1

#Criando arquivo PNADC e Juntando uf e regiões.
PNADc <- rbind(mun1,reg1)
rm(mun1,reg1)

#População Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
mun2 <- svyby(formula=~V2007, by=~Capital, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg2 <-  svyby(formula=~V2007, by=~RM_RIDE, design=PNADc_anual, FUN=svytotal, 
               na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa Regiões para permitir o rbind.
reg2 %>% rename(Capital = RM_RIDE) -> reg2

ufreg2 <- rbind(mun2,reg2)

#Juntando PNADc e ufreg2  - Uf
PNADc <- left_join(PNADc,ufreg2, by = "Capital") %>%
  #Renomeando vari?veis ano < 2016
  # rename(Pop = statistic,
  # PoP.Homem = statistic.V2007Masculino,
  # PoP.Mulher = statistic.V2007Feminino)
  #Renomeando variáveis ano >= 2016
  rename(Pop = statistic,
         PoP.Homem = statistic.V2007Homem,
         PoP.Mulher = statistic.V2007Mulher)

rm(mun2,reg2,ufreg2)

#População cor - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
mun3 <- svyby(formula=~V2010, by=~Capital, design=PNADc_anual, FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População cor - Regiões
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg3 <- svyby(formula=~V2010, by=~RM_RIDE, design=PNADc_anual, FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg3 %>% rename(Capital = RM_RIDE) -> reg3

ufreg3 <- rbind(mun3,reg3)

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
PNADc <- left_join(PNADc,ufreg3, by = "Capital") 
rm(mun3,reg3,ufreg3)

#População Cor e Homem - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
mun4<- svyby(formula=~V2010, by=~Capital, subset(PNADc_anual, V2007 == "Homem"), FUN=svytotal, 
            na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Homem - Regioes
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
reg4 <- svyby(formula=~V2010, by=~RM_RIDE, subset(PNADc_anual, V2007 == "Homem"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg4 %>% rename(Capital = RM_RIDE) -> reg4
#Jutando uf e regiões
ufreg4 <- rbind(mun4,reg4)
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
PNADc <- left_join(PNADc,ufreg4, by = "Capital")
rm(mun4,reg4,ufreg4)


#População Cor e Mulher - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
mun5 <- svyby(formula=~V2010, by=~Capital, subset(PNADc_anual, V2007 == "Mulher"), FUN=svytotal, 
na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Popula??o Cor e Mulher - Regiões
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
reg5 <- svyby(formula=~V2010, by=~RM_RIDE, subset(PNADc_anual, V2007 == "Mulher"), FUN=svytotal, 
na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reig?es para permitir o rbind.
reg5 %>% rename(Capital = RM_RIDE) -> reg5
#Jutando uf e regi?es
ufreg5 <- rbind(mun5,reg5)
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
PNADc <- left_join(PNADc,ufreg5, by = "Capital")
rm(mun5,reg5,ufreg5)


#Informações sobre os municípios
library(geobr)
#Informações sobre as capitais
munic_code <- 
  read_capitals(as_sf = FALSE) |> as_tibble() |>
  #Mantém código do município com seis dígitos
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6), 
         across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) 



#Transformando a PNADc
PNADc <- PNADc |> as_tibble() |>
  mutate(
    ano = ano,
    #Remove "Município de"
    Capital = gsub("Município de ", "", Capital) |> as_factor(),       
    #Remove os parênteses e o conteúdo dentro deles
    Capital = gsub("\\s*\\(.*?\\)", "", Capital) ) |> 
   #Adicionando código do município e UF e nome da UF.
   left_join(munic_code, by = join_by("Capital" == "name_muni" ) ) |>
   #Ordenando Colunas
   relocate( c(code_muni, name_state, code_state, ano), .after = Capital)

rio::export(PNADc,"pnadc_cidades.xlsx")

rm(list = ls() )

# População Jovem -------------------------------------------------------------------
library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)

#Informar ano desejado.
ano <- 2023

PNADc_anual <- get_pnadc(year = ano, interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto") ) -> PNADc_anual


#População Jovem - total - UF
#svytotal(~interaction(UF, one), design = PNADc_anual,na.rm = T)
mun1 <- svyby(formula=~one, by=~Capital, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem - total - Regiões
#svytotal(~interaction(regioes, one), design = PNADc_anual, na.rm = T)
reg1 <- svyby(formula=~one, by=~RM_RIDE, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg1 %>% rename(Capital = RM_RIDE) -> reg1

#Criando arquivo PNADC e Juntando uf e regiões.
PNADc_jovem <- rbind(mun1,reg1)
rm(mun1,reg1)

#População Jovem - Sexo - UF
#svytotal(~interaction(UF, V2007), design = PNADc_anual, na.rm = T)
mun2 <- svyby(formula=~V2007, by=~Capital, design = subset(PNADc_anual, Fxet == "Jovem"),
FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem - Sexo - Regiões
#svytotal(~interaction(regioes, V2007), design = PNADc_anual, na.rm = T)
reg2 <-  svyby(formula=~V2007, by=~RM_RIDE, design = subset(PNADc_anual, Fxet == "Jovem"), 
FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg2 %>% rename(Capital = RM_RIDE) -> reg2

ufreg2 <- rbind(mun2,reg2)

#Juntando PNADc e ufreg2  - Uf
PNADc_jovem <- left_join(PNADc_jovem,ufreg2, by = "Capital") %>%
  #Renomeando vari?veis ano < 2016
  #  rename(Pop = statistic,
  # PoP.Homem = statistic.V2007Masculino,
  # PoP.Mulher = statistic.V2007Feminino)
  #Renomeando vari?veis ano > 2016
  rename(Pop = statistic,
         PoP.Homem = statistic.V2007Homem,
         PoP.Mulher = statistic.V2007Mulher)

rm(mun2,reg2,ufreg2)

#População Jovem cor - UF
#svytotal(~interaction(UF, V2010), design = PNADc_anual, na.rm = T)
mun3 <- svyby(formula=~V2010, by=~Capital, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
             na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#População Jovem cor - Regi?es
#svytotal(~interaction(regioes, V2010), design = PNADc_anual, na.rm = T)
reg3 <- svyby(formula=~V2010, by=~RM_RIDE, design = subset(PNADc_anual, Fxet == "Jovem"), FUN=svytotal, 
              na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg3 %>% rename(Capital = RM_RIDE) -> reg3

ufreg3 <- rbind(mun3,reg3)

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
PNADc_jovem <- left_join(PNADc_jovem,ufreg3, by = "Capital") 
rm(mun3,reg3,ufreg3)

#População Jovem Cor e Homem - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
mun4<- svyby(formula=~V2010, by=~Capital, subset(PNADc_anual, V2007 == "Homem" & Fxet == "Jovem"), 
            FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Jovem Cor e Homem - Regioes
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Masculino"), na.rm = T)
reg4 <- svyby(formula=~V2010, by=~RM_RIDE, subset(PNADc_anual, V2007 == "Homem" & Fxet == "Jovem"), 
              FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
#Alterando columa reigões para permitir o rbind.
reg4 %>% rename(Capital = RM_RIDE) -> reg4
#Jutando uf e regiões
ufreg4 <- rbind(mun4,reg4)
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
PNADc_jovem <- left_join(PNADc_jovem,ufreg4, by = "Capital")
rm(mun4,reg4,ufreg4)


#População Cor e Mulher - UF
#svytotal(~interaction(UF, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
mun5 <- svyby(formula=~V2010, by=~Capital, subset(PNADc_anual, V2007 == "Mulher"& Fxet == "Jovem"),
             FUN=svytotal,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#População Cor e Mulher - Regi?es
#svytotal(~interaction(regioes, V2010), subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)
reg5 <- svyby(formula=~V2010, by=~RM_RIDE, subset(PNADc_anual, V2007 == "Mulher"& Fxet == "Jovem"), 
              FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Alterando columa reigões para permitir o rbind.
reg5 %>% rename(Capital = RM_RIDE) -> reg5
#Jutando uf e regiões
ufreg5 <- rbind(mun5,reg5)
#Renomeando e criando variáveis
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
PNADc_jovem <- left_join(PNADc_jovem,ufreg5, by = "Capital")
rm(mun5,reg5,ufreg5)


#Informações sobre os municípios
library(geobr)
#Informações sobre as capitais
munic_code <- 
  read_capitals(as_sf = FALSE) |> as_tibble() |>
  #Mantém código do município com seis dígitos
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6), 
         across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) 

#Transformando a PNADc
PNADc_jovem <- PNADc_jovem |> as_tibble() |>
  mutate(
    ano = ano,
    #Remove "Município de"
    Capital = gsub("Município de ", "", Capital) |> as_factor(),       
    #Remove os parênteses e o conteúdo dentro deles
    Capital = gsub("\\s*\\(.*?\\)", "", Capital) ) |> 
  #Adicionando código do município e UF e nome da UF.
  left_join(munic_code, by = join_by("Capital" == "name_muni" ) ) |>
  #Ordenando Colunas
  relocate( c(code_muni, name_state, code_state, ano), .after = Capital)

rio::export(PNADc_jovem,"pnadc_cidades_jovem.xlsx")

rm(list = ls() )


# An?lise capital do Rio --------------------------------------------------
#Rotina utilizada no trabalho de monetiza??o das perdas educacionais
library(PNADcIBGE)
library(rio)
library(tidyverse)
library(srvyr)
library(survey)

#Vari?veis desejadas
variaveis_selecionadas <- c("UF", "Capital","Trimestre","V1032", "V2007", "V2010","V2009")

PNADc_anual <- get_pnadc(year = 2019, interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Crian?a",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto")) -> PNADc_anual


#Transformar em valor real rendimentos nominais.
#https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Documentacao_Geral/PNADcIBGE_Deflator_Anual_Visita.pdf
PNADc_anual$variables <- transform(PNADc_anual$variables, VD4017_real=VD4017*CO2e)

#Popula??o total
svytotal(~one, design = PNADc_anual, na.rm = T)


#Total de observa??es
sum(PNADc_anual$variables$one)

#Popula??o total
svytotal(~one, design = PNADc_anual, na.rm = T)

#Rendimento mensal efetivo do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
svyby(formula=~VD4017_real, by=~VD3004, 
      design=subset(PNADc_anual,(V2009 >= 25 & V2009 <=30) & (V2010 == "Preta" & 
      V2007 == "Homem" &  Capital == "Munic?pio de Rio de Janeiro (RJ)")),
      FUN=svymean, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)

#Estimando Raz?es - Pessoas fazendo faculdade \ Pessoas que fizem ensino m?dio.
svyratio(numerator=~((V2009 >= 25 & V2009 <=30) &
    VD3004=="Superior incompleto ou equivalente" &
    V2007 =="Homem" & V2010 == "Preta"  & Capital == "Munic?pio de Rio de Janeiro (RJ)"), 
  
  
  denominator=~((V2009 >= 25 & V2009 <=30) &
  VD3004=="M?dio completo ou equivalente" & 
  V2007 =="Homem" & V2010 == "Preta" & Capital == "Munic?pio de Rio de Janeiro (RJ)"), 
  design=PNADc_anual, na.rm=TRUE)

#Rendimento por idade
svyby(formula=~VD4017_real, by=~V2009, 
      design=subset(PNADc_anual,Capital == "Munic?pio de Rio de Janeiro (RJ)"),
      FUN=svymean, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)

#Contando pessoas por idade.
svyby(formula=~one, by=~V2009, 
      design=subset(PNADc_anual,Capital == "Munic?pio de Rio de Janeiro (RJ)"),
      FUN=svytotal, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)



# Pop. feminina idade reprodutiva -----------------------------------------
library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)
#Vari?veis desejadas
variaveis_selecionadas <- c("UF", "Trimestre","V1032", "V2007", "V2010","V2009")

PNADc_anual <- get_pnadc(year = 2019, interview = 1,labels = T,design = TRUE)

PNADc_anual <- as_survey(PNADc_anual)

PNADc_anual %>% mutate(
  one = 1,
  repre = case_when(V2009 %in% 10:49  ~ 1, TRUE ~ 0)) -> PNADc_anual


#Popula??o total - Munic?pios
svytotal(~one, design = subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)

svytotal(~V2010, design = subset(PNADc_anual, V2007 == "Feminino"), na.rm = T)

svytotal(~repre, design = subset(PNADc_anual,V2007 == "Feminino" & Capital == "Munic?pio de Rio de Janeiro (RJ)") , na.rm = T)



#Unidade da Federa??o.
uf <- svyby(formula=~V2010, by =~ UF, 
            design=subset(PNADc_anual,repre == 1 & V2007 == "Mulher" & UF == "Rio de Janeiro"),
            FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F) 
uf %>% dplyr::rename(UF.Capital = UF) -> uf

#Munic?pio
mun <- svyby(formula=~V2010, by =~ Capital, 
      design=subset(PNADc_anual, repre == 1 & V2007 == "Mulher" & Capital == "Munic?pio de Rio de Janeiro (RJ)"),
      FUN=svytotal, na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
mun %>% rename(UF.Capital = Capital) -> mun

#Jutando uf e regi?es
ufmun <- rbind(uf,mun)
#Renomeando e criando vari?veis
ufmun %>% rename(M.Branca = statistic.V2010Branca,
                  M.Preta = statistic.V2010Preta,
                  M.Amarela = statistic.V2010Amarela,
                  M.Parda = statistic.V2010Parda,
                  M.Ind?gena = statistic.V2010Ind?gena,
                  M.Ignorado = statistic.V2010Ignorado) %>%
  mutate(ano = 2019,
    M.Negro = M.Preta + M.Parda,
         M.N?o_Branca = M.Preta + M.Amarela + M.Parda + M.Ind?gena,
         M.N?o_Negra = M.Branca + M.Amarela + M.Ind?gena) -> ufmun



export(ufmun,"x.xlsx")
rm(uf,mun,ufmun)

