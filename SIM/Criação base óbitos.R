#Carregando pacotes
library(tidyverse)
library(microdatasus)
library(janitor)

# Importação dos microdados -----------------------------------------------
sim <- fetch_datasus(year_start = 2008, year_end = 2023,
                      information_system = "SIM-DO") %>% clean_names()


# Tratamento dos microdados -----------------------------------------------
#Adicionar intenção, instrumento e local do incidente.
sim %>% 
  mutate(
  #Causa e loca do incidente
  causa_letra = substr(causabas,1,1),
  causa_num = as.numeric(substr(causabas,2,3)),
  local_incd = as.numeric(substr(causabas,4,4)),
  #Idades                          
  idade = as.double(as.character(idade)),
  #Idade desconhecida
  idade = case_when(idade == 999 | idade == 0  ~ as.double(NA), TRUE ~ idade),
  #Menor de 1 ano
  idade = case_when(idade > 0 & idade <= 400  ~ 0, TRUE ~ idade),
  #Idade em anos
  idade = case_when(idade>400 & idade <999 ~ idade -400, TRUE ~ idade),
  #Intenção
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
      #Acidente - Desconhecido /*X:59 é acidentes não especificados e pode contem homicídio mal registrado, visto no artigo sobre qualidade da declarações de óbito em SP.*/
      (causa_letra  == "X" & causa_num > 57 & causa_num < 60) ~ "Acidente", #/*Categoria inseriada, x58-x59 estavam em intencao outros.*/
    
    #Suicidio
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
    
    #h_legal (==6) intervenções legais e execuções legais. 
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
      causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros")), 
  
  ###Instrumento (Dicionário: 1=Envenenamento; 2=Enforcamento; 3=Afogamento; 4=PAF; 5=Impacto; 6=Fogo; 7=Perfurante; 8=Contundente; 9=Desconhecido; 10=veiculo)
  instrumento = as.factor(case_when(
  #Envenenamento (==1) 
  causa_letra  == "X" & causa_num > 39  & causa_num < 50 | # /*Acidente envenenamento*/
  causa_letra  == "X" & causa_num > 59 & causa_num < 70 |  #/*Self harm envenenamento*/
  causa_letra  == "X" & causa_num > 84 & causa_num < 91 |  #/*Ag. envenenamento*/
  causa_letra  == "Y" & causa_num > 09 & causa_num < 20 ~ "Envenenamento",       #/*Ind. envenenamento*/
    
  #Enforcamento (==2) Tudo Certo
  causa_letra  == "W" & causa_num > 74 & causa_num < 77 |  #/*Acidente enforcamento*/
  causa_letra  == "X" & causa_num == 70 |  #/*Self harm enforcamento*/
  causa_letra  == "X" & causa_num == 91 |  #/*Ag. enforcamento*/
  causa_letra  == "Y" & causa_num == 20 ~ "Enforcamento", #/*Ind. Enforcamento*/ 
    
    
  #Afogamento (==3) Por que afogamento de intencao outros? (Categoria Sequelas?)
  causa_letra  == "W" & causa_num > 64 & causa_num < 75 | # /*Acidente afogamento*/
  causa_letra  == "X" & causa_num == 71 |  # /*Self harm afogamento*/
  causa_letra  == "X" & causa_num == 92 |  # /Ag. afogamento*/
  causa_letra  == "Y" & causa_num == 21 ~ "Afogamento", # /*Ind. afogamento*/
    
  #PAF (==4) - Não tem acidente por arma de fogo
  causa_letra  == "W" & causa_num > 31 & causa_num < 35 | #Acidente - PAF*/
  causa_letra  == "X" & causa_num > 71 & causa_num < 75 |  #/*Self harm - PAF*/
  causa_letra  == "X" & causa_num > 92 & causa_num < 96 |  #/*ag. - PAF*/
  causa_letra  == "Y" & causa_num > 21 & causa_num < 25 |  #/*Ind. - PAF*/
  causa_letra  == "Y" & causa_num ==35 &  local_incd == 0 ~ "PAF",  #/*h_legal - PAF*/
  #causa_letra  == "Y" & causa_num ==35 &  local_incd == 1 - Foi para instrumento fogo
    
  #Impacto (==5) - Olhar acidente impacto
  causa_letra  == "W" & causa_num < 25 | # /Acidente: Queda de altura + atingido por objeto + esmagado por objeto*/ 
  causa_letra  == "W" & causa_num >26 & causa_num < 32 |  #/*2Acidente: Contato com objetos*/ 
  causa_letra  == "W" & causa_num >34 & causa_num < 44 |  #/*3Acidente: Explos?o + fogos + Exposi??o a jato press?o, barulho e vibra??o*/ 
  causa_letra  == "W" & causa_num ==49 |  #/*Acidente: Exposi??o for?a mec indeterminada.*/ 
  causa_letra  == "X" & causa_num == 75 |  #/*Self harm: Explos?o*/ 
  causa_letra  == "X" & causa_num > 79 & causa_num < 82 |  #/*Self harm: Queda + deitar na frente de objeto movendo.*/ 
  causa_letra  == "X" & causa_num ==96 |  #/*Agress?o mat explossivo*/ 
  causa_letra  == "Y" & causa_num > 0 & causa_num < 03 |  #/*Ag. empurado de altura + colocado na frente de objeto movendo.*/ 
  causa_letra  == "Y" & causa_num == 25 |  #/*Ind. Explos?o*/ 
  causa_letra  == "Y" & causa_num == 30 |  #/*Ind. Queda altura indet*/ 
  causa_letra  == "Y" & causa_num == 31 ~ "Impacto",  #/*Ind. Queda + deitar na frente de objeto movendo indet.*/
    
  #Fogo (==6) 
  causa_letra  == "X" & causa_num < 10 |  #/*Acidente Exposi??o a fuma?a, fogo e chamas*/  
  causa_letra  == "X" & causa_num > 75 & causa_num < 78 |  #/*Self harm de fuma?a, fogo, chamas, vapor*/ 
  causa_letra  == "X" & causa_num > 96 & causa_num < 99 |  #/*Ag. de fuma?a, fogo, chamas, vapor */ 
  causa_letra  == "Y" & causa_num > 25 & causa_num < 28 |  #/*Ind. de fuma?a, fogo, chamas, vapor */ 
  causa_letra  == "Y" & causa_num == 35 & local_incd ==2 |  #/*h_legal involvendo fuma?a*/
  causa_letra  == "Y" & causa_num ==35 &  local_incd == 1 ~ "Fogo", # /*h_legal involvendo explos?o*/
    
  #Perfurante (==7) 
  causa_letra  == "X" & causa_num ==78 |  #/*self objeto afiado*/ 
  causa_letra  == "X" & causa_num ==99 |  #/*ag. objeto afiado*/ 
  causa_letra  == "Y" & causa_num ==28 |  #/*Ind. objeto afiado*/ 
  causa_letra  == "W" & causa_num > 24 & causa_num < 27 |  #/*Acidente objeto afiado. Estava indo para indeterminado*/ 
  causa_letra  == "Y" & causa_num == 35 & local_incd ==4 ~ "Perfurante", #/*h_legal objeto afiado*/
    
  #Contundente (==8) 
  causa_letra  == "W" & causa_num ==51 |  #/*Acidente - Colis?o entre duas pessoas*/ 
  causa_letra  == "X" & causa_num ==79 |  #/*self por objeto contundente*/ 
  causa_letra  == "Y" & causa_num ==0 |  #/*ag. por objeto contundente*/ 
  causa_letra  == "Y" & causa_num > 03 & causa_num < 06 |  #/*Ag. por meio de for?a corporal + Ag. sexual por meio de for?a f?sica*/ 
  causa_letra  == "W" & causa_num == 50 |  #/*Acidente - Golpe, pancada, ponta p?*/
  causa_letra  == "Y" & causa_num == 29 |  #/*Ind. Objento contundente*/ 
  causa_letra  == "Y" & causa_num == 35 & local_incd ==3 ~ "Contundente", #/*h_legal objeto contundente*/ 
    
    #Desconhecido (==9) A segunga categoria cont?m neglig?ncia que n?o ? desconhecida. Cad? acidente
    causa_letra  == "X" & causa_num > 82 & causa_num < 85 |  #/*self. Outros meios especificados + self outros meios n?o especificados*/
      causa_letra  == "Y" & causa_num > 05 & causa_num < 10 |  #/*Ag. Neglig?ncia + Ag. Outros maus tratos + Ag. Outros meios especificados + Ag. outros meios n?o especificados*/
      causa_letra  == "Y" & causa_num > 32 & causa_num < 35 |  #/*Ind. Outros fatos ou eventos espcificados + fatos ou eventos n?o espcificados*/
      causa_letra  == "Y" & causa_num == 35 & local_incd ==5 |  #/*h_legal Execu??o legal - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 35 & local_incd == 6 |  #/*h_legal Execu??o legal por outros meios especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 35 & local_incd ==7 |  #/*h_legal Execu??o legal por meios n?o especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 36 | # /*Opera??es de guerra*/
      causa_letra  == "X" & causa_num > 57 & causa_num < 60 ~ "Desconhecido", #/*Acidente instrumento desconhecido. Categoria inseriada, n?o estava na rotina.*/
    
    
    #veículo (==10) 1.Acidente 2.Homicídio (y03, impacto) , 3.Indeterminado (y32,impacto) 4.Suicídio(x82,impacto)
    causa_letra  == "V" & causa_num > 0 | causa_letra  == "Y" & causa_num == 03 | 
    causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo") ) ) 




#Colocar no formato data depois de acertar as datas.  
#Consertando dtobito
sim %>% mutate(dtobito = as.numeric(as.character(dtobito)),
                     dtobito = case_when(
                       dtobito == 1996 ~ 1011996,dtobito == 1997 ~ 1011997,dtobito == 1998 ~ 1011998,dtobito == 1999  ~ 1011999,
                       dtobito == 2000 ~ 1012000,dtobito == 2002 ~ 1012002,dtobito == 11996 ~ 1011996,dtobito == 11997 ~ 1011997,
                       dtobito == 11998 ~ 1011998,dtobito == 11999 ~ 1011999,dtobito == 21996 ~ 1021996,dtobito == 21997 ~ 1021997,
                       dtobito == 21998 ~ 1021998,dtobito == 31996 ~ 1031996,dtobito == 31997 ~ 1031997,dtobito == 31998 ~ 1031998,
                       dtobito == 31999 ~ 1031999,dtobito == 41996 ~ 1041996,dtobito == 41997 ~ 1041997,dtobito == 41998 ~ 1041998,
                       dtobito == 41999 ~ 1041999,dtobito == 51996 ~ 1051996,dtobito == 51997 ~ 1051997,dtobito == 51998 ~ 1051998,
                       dtobito == 51999 ~ 1061999,dtobito == 61996 ~ 1061996,dtobito == 61997 ~ 1061997,dtobito == 61998 ~ 1061998,
                       dtobito == 61999 ~ 1061999,dtobito == 71996 ~ 1071996,dtobito == 71997 ~ 1071997,dtobito == 71998 ~ 1071998,
                       dtobito == 71999 ~ 1071999,dtobito == 81996 ~ 1081996,dtobito == 81997 ~ 1081997,dtobito == 81998 ~ 1081998,
                       dtobito == 81999 ~ 1081999,dtobito == 91996 ~ 1091996,dtobito == 91997 ~ 1091997,dtobito == 91998 ~ 1091998,
                       dtobito == 91999 ~ 1091999,dtobito == 101996 ~ 1101996,dtobito == 101997 ~ 1101997,dtobito == 101998 ~ 1101998,
                       dtobito == 101999 ~ 1101999,dtobito == 111996 ~ 1111996,dtobito == 111997 ~ 1111997,dtobito == 111998 ~ 1111998,
                       dtobito == 111999 ~ 1111999,dtobito == 121996 ~ 1121996,dtobito == 121997 ~ 1121997,dtobito == 121998 ~ 1121998,
                       dtobito == 121999 ~ 1121999,dtobito == 21999 ~ 1021999,dtobito == 22000 ~ 1022000, TRUE ~ dtobito),
                     dtobito = dmy(dtobito),
                     ano = as.factor(year(dtobito)),
                     mes = as.factor(month(dtobito,label = TRUE) ),
                     dia = as.factor(wday(dtobito,label = TRUE) ) ) -> sim

sim %>% tabyl(ano,show_na = TRUE, show_missing_levels = TRUE)

# Regiões, Estados e Municípios ----------------------------------------------------------
sim %>% mutate(
  #Código da UF de ocorrência
  cod_uf_ocor = as.numeric(substr(codmunocor,1,2)),
  uf_ocor = as.factor(recode(cod_uf_ocor,'11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá", '17'= "Tocantins", 
                             '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
                             '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
                             '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
                             '52'= "Goiás", '53'= "Distrito Federal", '99'= "CNRAC") ) ) -> sim
sim %>% tabyl(uf_ocor) %>% adorn_totals()
glimpse(sim)

#Região geográfica de ocorrência
sim |>
  mutate(reg_ocor = as.factor(case_when(
    #Região Norte
    uf_ocor %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    uf_ocor %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    uf_ocor %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    uf_ocor %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ) ) -> sim
sim |> tabyl(uf_ocor,reg_ocor)

#Colocando UFs de residência
sim %>% mutate(
  #Código da UF de residência
  cod_uf_resd = as.numeric(substr(codmunres,1,2)),
  uf_resd = as.factor(recode(cod_uf_resd,'11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá", '17'= "Tocantins", 
                             '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
                             '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
                             '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
                             '52'= "Goiás", '53'= "Distrito Federal", '99'= "CNRAC") ) ) -> sim
sim %>% tabyl(uf_resd) %>% adorn_totals()
glimpse(sim)

#Região geográfica de Residência
sim |>
  mutate(reg_resd = as.factor(case_when(
    #Região Norte
    uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ) ) -> sim
sim |> tabyl(uf_resd,reg_resd)


#Download das informações sobre municípios.
munics <- geobr::read_municipality(year = 2022) |> as_tibble() |>
  #Código dos municípios com 6 dígitos.
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6),
         #Transforma em factor
         across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) |>
  #Excluindo variáveis não utilizadas.
  select(!c(code_region, geom)) |>
  #Elimando geo
  sf::st_drop_geometry(data_all) |>
  
  ###Adiciona informações sobre os municípios ignorados
  bind_rows(
    
    tribble(~code_muni,~name_muni,~code_state,~abbrev_state,~name_state, ~name_region,
            "000000", "Ignorado ou exterior",    "00", "IGN", "Ignorado ou exterior", "Ignorado ou exterior",
            "110000", "Município ignorado - RO", "11",  "RO", "Rondônia", "Norte",
            "130000", "Município ignorado - AM", "13",  "AM", "Amazonas", "Norte",
            "150000", "Município ignorado - PA", "15",  "PA", "Pará", "Norte",
            "210000", "Município ignorado - MA", "21",  "MA", "Maranhão", "Nordeste",
            "170000", "Município ignorado - TO", "17",  "TO", "Tocantins", "Norte",
            "240000", "Município ignorado - RN", "24",  "RN", "Rio Grande do Norte", "Nordeste",
            "260000" ,"Município ignorado - PE", "26",  "PE", "Pernambuco", "Nordeste",
            "280000", "Município ignorado - SE", "28",  "SE", "Sergipe", "Nordeste",
            "310000", "Município ignorado - MG", "31",  "MG", "Minas Gerais", "Sudeste",
            "330000", "Município ignorado - RJ", "33",  "RJ", "Rio de Janeiro", "Sudeste",
            "410000", "Município ignorado - PR", "41",  "PR", "Paraná", "Sul",
            "430000", "Município ignorado - RS", "43",  "RS", "Rio Grande do Sul", "Sul",
            "510000", "Município ignorado - MT", "51",  "MT", "Mato Grosso", "Centro Oeste",
            "520000", "Município ignorado - GO", "52",  "GO", "Goiás", "Centro Oeste",
            "120000", "Município ignorado - AC", "12",  "AC", "Acre", "Norte",
            "140000", "Município ignorado - RR", "14",  "RR", "Roraima", "Norte",
            "160000", "Município ignorado - AP", "16",  "AP", "Amapá",  "Norte",
            "220000", "Município ignorado - PI", "22",  "PI", "Piauí", "Nordeste",
            "230000", "Município ignorado - CE", "23",  "CE", "Ceará",  "Nordeste",
            "250000", "Município ignorado - PB", "25",  "PB", "Paraíba","Nordeste",
            "270000", "Município ignorado - AL", "27",  "AL", "Alagoas", "Nordeste",
            "290000", "Município ignorado - BA", "29",  "BA", "Bahia", "Nordeste",
            "320000", "Município ignorado - ES", "32",  "ES", "Espírito Santo", "Sul",
            "350000", "Município ignorado - SP", "35",  "SP", "São Paulo", "Sudeste",
            "420000", "Município ignorado - SC", "42",  "SC", "Santa Catarina", "Sul",
            "500000", "Município ignorado - MS", "50",  "MS", "Mato Grosso do Sul", "Sul") ) |>
  #Conversão para factor. Exceto código do município.
  mutate(across(.cols = !c(code_muni), as_factor  ) )


# Transformações ----------------------------------------------------------
#Escolariedade 
sim %>% tabyl(ano,esc)

sim |> 
  mutate(
    esc = case_match(.x = esc, "1" ~ "Nenhuma", "2" ~ "1 a 3 anos", "3" ~  "4 a 7 anos", "4" ~  "8 a 11 anos",
                     "5" ~  "12 anos e mais", NA ~ "Ignorado", .default = "Ignorado") |> as_factor() |>  
      #Ordem dos Levels de escolaridade
      fct_relevel("Nenhuma", 
                  "1 a 3 anos",
                  "4 a 7 anos",
                  "8 a 11 anos",
                  "12 anos e mais",
                  "Ignorado") ) -> sim

sim %>%  tabyl(ano,esc) %>% adorn_totals(where = c("row","col"))
sim %>%  tabyl(uf_ocor,esc) %>% adorn_totals(where = c("row","col"))

#sexo
sim %>% tabyl(sexo)
sim |> mutate(sexo = case_match(.x = sexo,"9" ~ "Ignorado", "0" ~ "Ignorado", "1" ~ "Homem", "2" ~ "Mulher", 
                                      .default = "Ignorado") |> as_factor()) -> sim
sim %>% tabyl(ano,sexo) %>% adorn_totals(where = c("row","col")) 
sim %>% tabyl(uf_ocor,sexo) %>% adorn_totals(where = c("row","col")) 

#Racacor
sim %>% tabyl(racacor) %>% adorn_totals()
sim |> mutate(racacor = case_match(.x = racacor, "1" ~ "Branca", "2" ~ "Preta", "3" ~ "Amarela", "4" ~ "Parda", "5" ~ "Indigena",
                                         "9" ~ "Ignorado", .default = "Ignorado" ) |> as_factor()) -> sim
sim %>% tabyl(ano,racacor) %>% adorn_totals(where = c("row","col") )
sim %>% tabyl(uf_ocor, racacor) %>% adorn_totals(where = c("row","col"))

#Estado Civíl
sim %>% tabyl(estciv) %>% adorn_totals()
sim |> mutate(estciv = case_match(.x = estciv, "1" ~"Solteiro", 
                                        "2" ~ "Casado", "3" ~ "Viúvo", "4" ~ "Divorciado", "5" ~ "União Estável", "9" ~ "Ignorado",
                                        "0" ~ "Ignorado", .default = "Ignorado" ) |> as_factor()) -> sim
sim %>% tabyl(ano,estciv) %>% adorn_totals(where = c("row","col") )
sim %>% tabyl(uf_ocor, estciv) %>% adorn_totals(where = c("row","col"))

#Local do incidente - Variável criada
sim %>% mutate(
  local_incd = recode(local_incd,"0" = "Residencial", "1"= "Hab. Coletiva", "2"="Área de administração pública*", 
                       "3"="Esportiva", "4"="Rua/Estrada", "5"="Comercial", "6"="Industrial",  
                       "7"= "Fazenda", "8"="Outros", "9"= "Ignorado" ),
  local_incd = replace_na(local_incd,"Ignorado"),
  #Local de óbito de intervenção legal é rua/estrada
  local_incd = as.factor(case_when(
    #Em intervenção legal o terceiro dígito não é o local. Vou assumir rua\estrada.
    intencao == "h_legal" ~ "Rua/Estrada",
    #Local de óbito de acidente de transporte V01-V99. O terceiro dígito não é local do incidente.
    intencao == "Acidente" & instrumento == "Veículo" ~ "Rua/Estrada", 
    #Y06.- Negligência e abandono ou Y07.- Outras síndromes de maus tratos. Residência?
    TRUE ~ local_incd) ) ) -> sim

#Local de óbito de Y06 e Y07 Negligência, Abandono e maus tratos. Olhei as idades e não parece ser abandono de criança.
#Olhar o keep. Autoria conhecida pode ser residência. desconhecida rua\estrada

sim %>% tabyl(intencao, local_incd) %>% adorn_totals()

#Instrumento
sim %>% mutate(instrumento = replace_na(instrumento,"Desconhecido")) -> sim
sim %>%  tabyl(instrumento) %>% adorn_totals()

glimpse(sim)


