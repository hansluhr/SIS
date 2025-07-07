#Carregando pacotes
library(tidyverse)
library(microdatasus)
library(janitor)
asd
# Importação de dados. ----------------------------------------------------
#http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0102-311X2019001104001
#https://github.com/rfsaldanha/microdatasus/wiki

# SIM-DOEXT 1996 ----------------------------------------------------------
#SIM-DOEXT baixa o Brasil inteiro.
sim_doext <- fetch_datasus(year_start = 1996, year_end = 2022,
                           vars = c("DTOBITO",  "IDADE", "SEXO", "RACACOR", "ESTCIV", "ESC",  "CODMUNOCOR",
                                    "CAUSABAS","CODMUNRES"),
                           information_system = "SIM-DOEXT") %>% clean_names()
sim_doext %>% mutate(
  #Causa e loca do óbito.
  causa_letra = substr(causabas,1,1),
  causa_num = as.numeric(substr(causabas,2,3)),
  local_obito = as.numeric(substr(causabas,4,4)),
  #Idades                          
  idade = as.double(as.character(idade)),
  #Idade desconhecida
  idade = case_when(idade == 999 | idade == 0  ~ as.double(NA), TRUE ~ idade),
  #Menor de 1 ano
  idade = case_when(idade > 0 & idade <= 400  ~ 0, TRUE ~ idade),
  #Idade em anos
  idade = case_when(idade>400 & idade <999 ~ idade -400, TRUE ~ idade),
  #Inten??o
  intencao = as.factor(case_when(
    ###acidente(==1)
    #acidente-envenenamento
    (causa_letra  == "X" & causa_num > 39 & causa_num < 50)  |
      #acidente-enforcamento
      (causa_letra  == "W" & causa_num > 74 & causa_num < 77) | #/*Certo*/
      #acidente-afogamento
      (causa_letra  == "W" & causa_num > 64 & causa_num < 75) | #/*Certo*/
      #acidente-impacto /*Queda*/
      (causa_letra  == "W" & causa_num < 25 | 
         causa_letra  == "W" & causa_num >26 & causa_num < 32 | 
         causa_letra  == "W" & causa_num >34 & causa_num < 44 | 
         causa_letra  == "W" & causa_num == 49) |
      #acidente-fogo
      (causa_letra  == "X" & causa_num < 10) | 
      #acidente-contundente
      (causa_letra  == "W" & causa_num == 51 | causa_letra  == "W" & causa_num == 50) |
      #acidente-veiculo
      (causa_letra  == "V" &  causa_num > 0) | # /*Certo*/
      #acidente-PAF
      (causa_letra  == "W" & causa_num > 31 & causa_num < 35)  | #/*Estava em Indeterminado*/
      #Acidente - Perfurante   
      (causa_letra  == "W" & causa_num > 24 & causa_num < 27)  | #/*Estava em Indeterminado*/
      #Acidente - Desconhecido /*X:59 ? acidentes n?o especificados e pode contem homic?dio mal registrado, visto no artigo sobre qualidade da declara??o de ?bito em SP.*/
      (causa_letra  == "X" & causa_num > 57 & causa_num < 60) ~ "Acidente", #/*Categoria inseriada, x58-x59 estavam em intencao outros.*/
    
    
    ##Sucid?dio
    #suicidio-envenenamento
    (causa_letra  == "X" & causa_num > 59 & causa_num < 70) | # /*Certo*/
      #suicidio-enforcamento
      (causa_letra  == "X" & causa_num == 70) | # /*Certo*/
      #suicidio-afogamento
      (causa_letra  == "X" & causa_num == 71) | #/*Certo*/
      #suicidio-PAF
      (causa_letra  == "X" & causa_num > 71 & causa_num < 75) | # /*Certo*/
      #suicidio-impacto/ve?culo
      (causa_letra  == "X" & causa_num == 75 | causa_letra  == "X" & causa_num > 79 & causa_num < 83) | #/*Certo*/
      #suicidio-fogo
      (causa_letra  == "X" & causa_num > 75 & causa_num < 78) | # /*Certo*/
      #suicidio-perfurante
      (causa_letra  == "X" & causa_num ==78)  | #/*Certo*/
      #suicidio-contundente
      (causa_letra  == "X" & causa_num ==79) | #/*Certo*/
      #suicidio-desconhecido
      (causa_letra  == "X" & causa_num > 82 & causa_num < 85) ~ "Suicídio",
    
    
    ##homicidio (==3)
    #homicidio-envenenamento
    (causa_letra  == "X" & causa_num > 84 & causa_num < 91) | # /*Certo*/
      #homicidio-enforcamento
      (causa_letra  == "X" & causa_num == 91) | #/*Certo*/
      #homicidio-afogamento
      (causa_letra  == "X" & causa_num == 92) | #/*Certo*/
      #homicidio-PAF
      (causa_letra  == "X" & causa_num > 92 & causa_num < 96) | #/*Certo*/
      #homicidio-impacto/Ve?culo
      (causa_letra  == "X" & causa_num == 96 | 
         causa_letra  == "Y" & causa_num > 0 & causa_num < 04) | #/*Ve?culo aqui*/
      #homicidio-fogo
      (causa_letra  == "X" & causa_num > 96 & causa_num < 99) | # /*Certo*/
      #homicidio-perfurante
      (causa_letra  == "X" & causa_num == 99) | #/*Certo*/
      #homicidio-contundente
      (causa_letra  == "Y" & causa_num == 0 | 
         causa_letra  == "Y" & causa_num > 03 & causa_num < 06) | # /*Certo*/
      #homicidio-desconhecido
      (causa_letra  == "Y" & causa_num > 05 & causa_num < 10) ~ "Homicídio",
    
    
    #indeterminado (==4)
    #indeterminado-envenenamento
    (causa_letra  == "Y" & causa_num > 09 & causa_num < 20) | #/*Certo*/
      #indeterminado-enforcamento
      (causa_letra  == "Y" & causa_num == 20) | #/*Certo*/
      #indeterminado-afogamento
      (causa_letra  == "Y" & causa_num == 21) | #/*Certo*/
      #indeterminado-PAF
      (causa_letra  == "Y" & causa_num > 21 & causa_num < 25) | 
      #indeterminado-impacto/Ve?culo
      (causa_letra  == "Y" & causa_num == 25 | causa_letra  == "Y" & causa_num == 30 | 
         causa_letra  == "Y" & causa_num == 31 | causa_letra  == "Y" & causa_num == 32) | #/*Ve?culo aqui*/
      #indeterminado-fogo
      (causa_letra  == "Y" & causa_num > 25 & causa_num < 28) | #/*Certo*/
      #indeterminado-perfurante
      (causa_letra  == "Y" & causa_num == 28) | #/*| causa_letra  == "W" & causa_num > 24 & causa_num < 27 Foi para Acidente perfurante*/
      #indeterminado-contundente
      (causa_letra  == "Y" & causa_num == 29) | #/*Certo*/
      #indeterminado-desconhecido
      causa_letra  == "Y" & causa_num > 32 & causa_num < 35 ~ "Indeterminado", 
    
    #h_legal (==6) interven??es legais e execuexecu??eses legais. Essa categoria n?o entrar? no Mlogit, ? apenas para validar o total de ?bitos
    causa_letra  == "Y" & causa_num == 35 | causa_letra  == "Y" & causa_num == 36 ~ "h_legal",
    
    #outros (==7) Essa categoria nao entrara no Mlogit, ? apenas para validar o total de ?bitos
    #/*Penetra??o de corpo estranho no ou atrav?s de olho ou orif?cio natural + Penetra??o de corpo ou objeto estranho atrav?s da pele + Contato com agulha hipod?rmica*/
    causa_letra  == "W" & causa_num > 43 & causa_num < 47  | 
      #/* Esmagado, empurrado ou pisoteado por multid?o ou debandada em massa de pessoas + ... + Afogamento e submers?o durante banho em banheira*/
      causa_letra  == "W" & causa_num > 51 & causa_num < 66  |
      #/* Risco a respira??o devido a desmoronamento, queda de terra e de outras subst?ncias + ... + Exposi??o ? corrente el?trica, ? radia??o e ?s temperaturas e press?es extremas do ambiente*/ 
      causa_letra  == "W" & causa_num > 76  | 
      #/*Contato com uma fonte de calor ou com subst?ncias quentes + Contato com animais e plantas venenosos + Exposi??o ?s for?as da natureza*/
      causa_letra  == "X" & causa_num > 09 & causa_num < 40 | 
      #/*X:59 ? acidentes n?o especificados e pode contem homic?dio mal registrado, visto no artigo sobre qualidade da declara??o de ?bito em SP.*/
      #/*x58-x59 foi para acidente de instrumento desconhecido*/
      #/*Excesso de esfor?os, viagens e priva??es + Exposi??o acidental a outros fatores e aos n?o especificados - */
      causa_letra  == "X" & causa_num > 49 & causa_num < 60 | 
      #/*Excesso de esfor?os, viagens e priva??es*/
      causa_letra  == "X" & causa_num > 49 & causa_num < 58 | 
      #/*Complica??es de assist?ncia m?dica e cir?rgica + ... + Seq?elas de causas externas de morbidade e de mortalidade*/
      causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros") ) )


