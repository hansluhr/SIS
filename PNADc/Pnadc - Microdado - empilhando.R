library(PNADcIBGE)
library(tidyverse)
library(srvyr)
library(survey)
library(rio)


#Variáveis desejadas
vars <- c("UF", "Trimestre","V1032", "V2007","V2009","VD3004")
#years <- 2012:2013


pnad_trim2 = list()

for(ano in 2015:2016) { 
  PnadC <- get_pnadc(year = ano, interview = 1, vars = vars, design = FALSE, labels = T)
  pnad_trim2[[(ano-2011)]] <- PnadC
}
#Tibble com microdados da pnadc
PNADc_anual <- dplyr::bind_rows(pnad_trim2)
rm(pnad_trim2, PnadC)

#Adiciona plano amostral
PNADc_anual <- pnadc_design(data_pnadc=PNADc_anual)













svyby(formula=~V2007, by=~Ano, design=PNADc_anual, FUN=svytotal, 
      na.rm=TRUE, vartype=NULL, keep.names = F,row.names=FALSE,keep.var = F)
