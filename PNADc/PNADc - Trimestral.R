# Análise capital do Rio --------------------------------------------------
#Rotina utilizada no trabalho de monetização das perdas educacionais
library(PNADcIBGE)
library(rio)
library(tidyverse)
library(srvyr)
library(survey)

#Variáveis desejadas
variaveis_selecionadas <- c("UF", "Capital","Trimestre","V1032", "V2007", "V2010","V2009")

PNADc_tri <- get_pnadc(year = 2020, quarter = 1,labels = T, design = TRUE)

PNADc_tri <- as_survey(PNADc_tri)

PNADc_tri %>% mutate(
  one = 1,
  Fxet =  case_when(
    V2009 %in% 0:14 ~ "Criança",
    V2009 %in% 15:29 ~ "Jovem", 
    V2009 > 30 ~ "Adulto")) -> PNADc_tri


#Transformar em valor real rendimentos nominais.
PNADc_tri$variables <- transform(PNADc_tri$variables, VD4017_real=VD4017*Efetivo)



#Total de observações
sum(PNADc_tri$variables$one)

#População total
svytotal(~one, design = PNADc_tri, na.rm = T)


#População total - Municípios
#svytotal(~interaction(UF, one), design = PNADc_tri,na.rm = T)
svyby(formula=~one, by=~Capital, design=PNADc_tri, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Total por sexo
svytotal(x=~V2007, design=PNADc_tri, na.rm=TRUE)
svyby(formula=~V2007, by=~Capital, design=PNADc_tri, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)

#Total por raça\cor
svytotal(x=~V2010, design=PNADc_tri, na.rm=TRUE)

#Total por sexo + raça\cor
svytotal(x=~V2007 + V2010, design=PNADc_tri, na.rm=TRUE)

svyby(formula=~V2010, by=~ V2007,design=PNADc_tri,
      FUN=svytotal, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)

svyby(formula=~interaction(V2010,V2007), by=~ Capital,design=PNADc_tri,
      FUN=svytotal, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)


#Estimando Médias
#Tabela 5429 - Rendimento médio nominal do trabalho principal, efetivamente recebido no mês de referência,
#pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho (Reais)
svymean(x=~VD4017, design=PNADc_tri, na.rm=TRUE)


#Tabela 5429 - #Rendimento médio nominal de todos os trabalhos, efetivamente recebido no mês de referência, 
#pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho (Reais)
svymean(x=~VD4020, design=PNADc_tri, na.rm=TRUE)


#Rendimento médio efetivamente recebido do trabalho principal + idade = 32
svymean(x=~VD4017, design=subset(PNADc_tri,V2009 == 32) , na.rm=TRUE)

#Rendimento médio efetivamente recebido do trabalho principal + nível de instrução
svymean(x=~VD4017+VD3004, design=PNADc_tri , na.rm=TRUE)

#Rendimento médio efeitivamente recebido do trabalho principal + nível de instrução + Rio de Janeiro
svyby(formula=~VD4017, by=~VD3004, 
      design=subset(PNADc_tri,V2009 == 32 & Capital == "Município de Rio de Janeiro (RJ)"),
      FUN=svymean, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)



#Rendimento médio real efetivamente recebido - Usei Esse. 
svyby(formula=~VD4017_real, by=~VD3004, 
      design=subset(PNADc_tri,(V2009 >= 25 & V2009 <=30) & (V2010 == "Preta" & 
      V2007 == "Homem" &  Capital == "Município de Rio de Janeiro (RJ)")),
      FUN=svymean, keep.var = F,na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE)




#Estimando Razões - Pessoas fazendo faculdade \ Pessoas que fizem ensino médio.
svyratio(numerator=~((V2009 >= 25 & V2009 <=30) & 
     VD3004=="Superior incompleto ou equivalente" &
     V2007 =="Homem" & V2010 == "Preta" & Capital == "Município de Rio de Janeiro (RJ)"), 
  
  denominator=~((V2009 >= 25 & V2009 <=30) & 
    VD3004=="Médio completo ou equivalente" & V2007 =="Homem" & 
    V2010 == "Preta" & Capital == "Município de Rio de Janeiro (RJ)"), 
  design=PNADc_tri, na.rm=TRUE)


#7014	Média de horas efetivamente trabalhadas
svymean(x=~VD4020, design=PNADc_tri, na.rm=TRUE)