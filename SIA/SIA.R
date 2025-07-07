library(microdatasus)
library(tidyverse)
library(janitor)



#Produção ambulatorial
sia_pa <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIA/PARJ1903.dbc") %>% clean_names() %>% as_tibble()
#Cid Principal
sia %>% tabyl(pa_cidpri) %>% arrange(desc(n)) %>% slice_head(n=10)
sia %>% tabyl(pa_docorig) %>% arrange(desc(n)) %>% slice_head(n=10)

#Importando SIA Produção ambulatorial
sia_pa <- fetch_datasus(year_start = 2024, year_end = 2024, month_start = 7, month_end = 7, uf = "RJ",
                     information_system = "SIA-PA") %>% clean_names() %>% as_tibble()

sia_pa |> filter(str_detect(pa_cidpri, 'X') ) |> 
  
  summarise(
    
    n = n(),
    mean_idade = pa_idade |> as.numeric() |> mean(),
    sd_idade = pa_idade |> as.numeric() |> sd()  )




#Removendo pa_ das variáveis.
for (col in 1:ncol(sia_pa)){
  colnames(sia_pa)[col] <-  sub("pa_", "", colnames(sia_pa)[col])
}

#Cid10 principal 85% está preenchido como 0000
sia_pa %>% tabyl(cidpri) %>% arrange(desc(n)) %>% slice_head(n=10)

#CID10 princiapal por tipo de instrumento do procedimento ambulatorial 
sia_pa %>% tabyl(cidpri,docorig) %>% adorn_percentages("col") %>%
  adorn_pct_formatting() %>% 
  slice_head(n=10) #Quando preenchidos, instrumentos B e C o cidpri é preenchido com 0000 

#
sia_pa %>% tabyl(cidsec,docorig) %>% slice_head(n=10) #Quando preenchidos, instrumentos B e C o cidpri é preenchido com 0000 


#A cid10 principal e secundária não está preenchia na maioria dos registros.




sia_pa %>% 
  #Excluindo instrumentos com cidpri preenchidos com 000
  filter(docorig != "B" & docorig != "C") %>% 
  #Selecionando variáveis utilizadas
   select(coduni,gestao,ufmun,tpups,mvm,cmp,proc_id,docorig,autoriz,cbocod,motsai,obito,encerr,perman,alta,transf,cidpri,
                  cidsec,cidcas,catend,idade,sexo,racacor,codoco,etnia) %>%
  #Renomeando datas.
  rename(dt_proc = mvm,dt_int = cmp)  %>%
  #Retirar primeira letra da cid primária. Utilizado na criação das variáveis intenção e instrumento
  mutate(causa_letra = substr(cidpri,1,1),
         causa_num = as.numeric(substr(cidpri,2,3)),
        local_obito = as.numeric(substr(cidpri,4,4))) %>% 
  #Mantém somente causas externas.
  filter(causa_letra %in% c("X","Y","W","V")) %>% droplevels() %>%
  #Criação das variáveis intenção, instrumento e local
  mutate(
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
    #Acidente - Desconhecido /*X:59 é acidentes não especificados e pode contem homicídio mal registrado, visto no artigo sobre qualidade da declaração de óbito em SP.*/
    (causa_letra  == "X" & causa_num > 57 & causa_num < 60) ~ "Acidente", #/*Categoria inseriada, x58-x59 estavam em intencao outros.*/
  
  
  ##Sucidídio
  #suicidio-envenenamento
  (causa_letra  == "X" & causa_num > 59 & causa_num < 70) | # /*Certo*/
    #suicidio-enforcamento
    (causa_letra  == "X" & causa_num == 70) | # /*Certo*/
    #suicidio-afogamento
    (causa_letra  == "X" & causa_num == 71) | #/*Certo*/
    #suicidio-PAF
    (causa_letra  == "X" & causa_num > 71 & causa_num < 75) | # /*Certo*/
    #suicidio-impacto/veículo
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
    #homicidio-impacto/Veículo
    (causa_letra  == "X" & causa_num == 96 | 
       causa_letra  == "Y" & causa_num > 0 & causa_num < 04) | #/*Veículo aqui*/
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
    #indeterminado-impacto/Veículo
    (causa_letra  == "Y" & causa_num == 25 | causa_letra  == "Y" & causa_num == 30 | 
       causa_letra  == "Y" & causa_num == 31 | causa_letra  == "Y" & causa_num == 32) | #/*Veículo aqui*/
    #indeterminado-fogo
    (causa_letra  == "Y" & causa_num > 25 & causa_num < 28) | #/*Certo*/
    #indeterminado-perfurante
    (causa_letra  == "Y" & causa_num == 28) | #/*| causa_letra  == "W" & causa_num > 24 & causa_num < 27 Foi para Acidente perfurante*/
    #indeterminado-contundente
    (causa_letra  == "Y" & causa_num == 29) | #/*Certo*/
    #indeterminado-desconhecido
    causa_letra  == "Y" & causa_num > 32 & causa_num < 35 ~ "Indeterminado", 
  
  #h_legal (==6) intervenções legais e execuexecuçõeses legais. Essa categoria não entrará no Mlogit, é apenas para validar o total de óbitos
  causa_letra  == "Y" & causa_num == 35 | causa_letra  == "Y" & causa_num == 36 ~ "h_legal",
  
  #outros (==7) Essa categoria nao entrara no Mlogit, é apenas para validar o total de óbitos
  #/*Penetração de corpo estranho no ou através de olho ou orifício natural + Penetração de corpo ou objeto estranho através da pele + Contato com agulha hipodérmica*/
  causa_letra  == "W" & causa_num > 43 & causa_num < 47  | 
    #/* Esmagado, empurrado ou pisoteado por multidão ou debandada em massa de pessoas + ... + Afogamento e submersão durante banho em banheira*/
    causa_letra  == "W" & causa_num > 51 & causa_num < 66  |
    #/* Risco a respiração devido a desmoronamento, queda de terra e de outras substâncias + ... + Exposição à corrente elétrica, à radiação e às temperaturas e pressões extremas do ambiente*/ 
    causa_letra  == "W" & causa_num > 76  | 
    #/*Contato com uma fonte de calor ou com substâncias quentes + Contato com animais e plantas venenosos + Exposição às forças da natureza*/
    causa_letra  == "X" & causa_num > 09 & causa_num < 40 | 
    #/*X:59 é acidentes não especificados e pode contem homicídio mal registrado, visto no artigo sobre qualidade da declaração de óbito em SP.*/
    #/*x58-x59 foi para acidente de instrumento desconhecido*/
    #/*Excesso de esforços, viagens e privações + Exposição acidental a outros fatores e aos não especificados - */
    causa_letra  == "X" & causa_num > 49 & causa_num < 60 | 
    #/*Excesso de esforços, viagens e privações*/
    causa_letra  == "X" & causa_num > 49 & causa_num < 58 | 
    #/*Complicações de assistência médica e cirúrgica + ... + Seqüelas de causas externas de morbidade e de mortalidade*/
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
    causa_letra  == "Y" & causa_num ==35 &  local_obito == 0 ~ "PAF",  #/*h_legal - PAF*/
  #causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 - Foi para instrumento fogo
  
  #Impacto (==5) - Olhar acidente impacto
  causa_letra  == "W" & causa_num < 25 | # /Acidente: Queda de altura + atingido por objeto + esmagado por objeto*/ 
    causa_letra  == "W" & causa_num >26 & causa_num < 32 |  #/*2Acidente: Contato com objetos*/ 
    causa_letra  == "W" & causa_num >34 & causa_num < 44 |  #/*3Acidente: Explosão + fogos + Exposição a jato pressão, barulho e vibração*/ 
    causa_letra  == "W" & causa_num ==49 |  #/*Acidente: Exposição força mec indeterminada.*/ 
    causa_letra  == "X" & causa_num == 75 |  #/*Self harm: Explosão*/ 
    causa_letra  == "X" & causa_num > 79 & causa_num < 82 |  #/*Self harm: Queda + deitar na frente de objeto movendo.*/ 
    causa_letra  == "X" & causa_num ==96 |  #/*Agressão mat explossivo*/ 
    causa_letra  == "Y" & causa_num > 0 & causa_num < 03 |  #/*Ag. empurado de altura + colocado na frente de objeto movendo.*/ 
    causa_letra  == "Y" & causa_num == 25 |  #/*Ind. Explosão*/ 
    causa_letra  == "Y" & causa_num == 30 |  #/*Ind. Queda altura indet*/ 
    causa_letra  == "Y" & causa_num == 31 ~ "Impacto",  #/*Ind. Queda + deitar na frente de objeto movendo indet.*/
  
  #Fogo (==6) 
  causa_letra  == "X" & causa_num < 10 |  #/*Acidente Exposição a fumaça, fogo e chamas*/  
    causa_letra  == "X" & causa_num > 75 & causa_num < 78 |  #/*Self harm de fumaça, fogo, chamas, vapor*/ 
    causa_letra  == "X" & causa_num > 96 & causa_num < 99 |  #/*Ag. de fumaça, fogo, chamas, vapor */ 
    causa_letra  == "Y" & causa_num > 25 & causa_num < 28 |  #/*Ind. de fumaça, fogo, chamas, vapor */ 
    causa_letra  == "Y" & causa_num == 35 & local_obito ==2 |  #/*h_legal involvendo fumaça*/
    causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 ~ "Fogo", # /*h_legal involvendo explosão*/
  
  #Perfurante (==7) 
  causa_letra  == "X" & causa_num ==78 |  #/*self objeto afiado*/ 
    causa_letra  == "X" & causa_num ==99 |  #/*ag. objeto afiado*/ 
    causa_letra  == "Y" & causa_num ==28 |  #/*Ind. objeto afiado*/ 
    causa_letra  == "W" & causa_num > 24 & causa_num < 27 |  #/*Acidente objeto afiado. Estava indo para indeterminado*/ 
    causa_letra  == "Y" & causa_num == 35 & local_obito ==4 ~ "Perfurante", #/*h_legal objeto afiado*/
  
  #Contundente (==8) 
  causa_letra  == "W" & causa_num ==51 |  #/*Acidente - Colisão entre duas pessoas*/ 
    causa_letra  == "X" & causa_num ==79 |  #/*self por objeto contundente*/ 
    causa_letra  == "Y" & causa_num ==0 |  #/*ag. por objeto contundente*/ 
    causa_letra  == "Y" & causa_num > 03 & causa_num < 06 |  #/*Ag. por meio de força corporal + Ag. sexual por meio de força física*/ 
    causa_letra  == "W" & causa_num == 50 |  #/*Acidente - Golpe, pancada, ponta pé*/
    causa_letra  == "Y" & causa_num == 29 |  #/*Ind. Objento contundente*/ 
    causa_letra  == "Y" & causa_num == 35 & local_obito ==3 ~ "Contundente", #/*h_legal objeto contundente*/ 
  
  #Desconhecido (==9) A segunga categoria contém negligência que não é desconhecida. Cadê acidente
  causa_letra  == "X" & causa_num > 82 & causa_num < 85 |  #/*self. Outros meios especificados + self outros meios não especificados*/
    causa_letra  == "Y" & causa_num > 05 & causa_num < 10 |  #/*Ag. Negligência + Ag. Outros maus tratos + Ag. Outros meios especificados + Ag. outros meios não especificados*/
    causa_letra  == "Y" & causa_num > 32 & causa_num < 35 |  #/*Ind. Outros fatos ou eventos espcificados + fatos ou eventos não espcificados*/
    causa_letra  == "Y" & causa_num == 35 & local_obito ==5 |  #/*h_legal Execução legal - Não é desconhecido, mas deve ser zero. Pena de morte*/
    causa_letra  == "Y" & causa_num == 35 & local_obito == 6 |  #/*h_legal Execução legal por outros meios especificados - Não é desconhecido, mas deve ser zero. Pena de morte*/
    causa_letra  == "Y" & causa_num == 35 & local_obito ==7 |  #/*h_legal Execução legal por meios não especificados - Não é desconhecido, mas deve ser zero. Pena de morte*/
    causa_letra  == "Y" & causa_num == 36 | # /*Operações de guerra*/
    causa_letra  == "X" & causa_num > 57 & causa_num < 60 ~ "Desconhecido", #/*Acidente instrumento desconhecido. Categoria inseriada, não estava na rotina.*/
  
  #veículo (==10) 1.Acidente 2.Homicídio (y03, impacto) , 3.Indeterminado (y32,impacto) 4.Suicídio(x82,impacto)
  causa_letra  == "V" & causa_num > 0 | causa_letra  == "Y" & causa_num == 03 | 
  causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo"))) -> sia_pa


  
  

sia_pa %>% tabyl(intencao,instrumento) %>% adorn_totals(where = c("row","col"))


sia_pa %>% tabyl(dt_int,dt_proc)


base %>% filter(str_detect(pa_cidsec,"X")) %>% tabyl(pa_proc_id) %>% arrange(desc(n)) %>% slice_head(n=10)

base %>% tabyl(pa_cidsec)






















#Laudos Diversos
base_ad <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIA/ADRJ1903.dbc") %>% clean_names()

#Cid princiap
base_ad %>% tabyl(ap_cidpri) %>% arrange(desc(n)) %>% slice_head(n=10)

#Retirar primeira letra da cid primária
base_ad %>% mutate(causa_letra = substr(ap_cidpri,1,1)) %>% filter(causa_letra %in% c("X","Y")) %>% droplevels() %>%
  tabyl(ap_cidpri) %>% arrange(desc(n)) %>% slice_head(n=10)
