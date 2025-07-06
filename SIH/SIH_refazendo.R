library(tidyverse)
library(janitor)

#Importando base de SIHs
#Pasta com os arquivos
filenames <- list.files("C:/Users/gabli/Desktop/r/SIH/dbc",full.names = TRUE,pattern=".dbc")
#Lista dos arquivos
list_data <- lapply(filenames, read.dbc::read.dbc) 
#Bind da lista de dbcs
sih <- data.table::rbindlist(list_data, use.names = TRUE, fill=TRUE) %>% clean_names() |>
  #Mantém somente internações normais
  filter(ident == 1) |> 
  #Selecionando variáveis de interesse
  select(uf_zi,ano_cmpt,mes_cmpt,n_aih,munic_res,nasc,sexo,dt_inter,dt_saida,
         diag_princ,munic_mov,cod_idade,idade,esc = instru,#Rename de instru, evita confundir com instrumento
         cbor,diagsec1,
         dias_perm,morte,raca_cor,cod_etn = etnia) #Rename de etnia facilitar join
rm(filenames,list_data)
gc()

sih |> count(ano_cmpt)

  
#Criar variável cid10 da causa externa.
sih |> 
  #Pegar primeira letra do diagnóstico principal. E utilizar o filtro dos caps XIX e XX
  mutate(causa_letra_dig_pri = substr(diag_princ,1,1) ) |> 
  #Mantém caps XIX e XX na variável diagnóstico principal.
  #Diag secundário com cap XIX ou XX é excluído, seguindo a descrição do manual.
  filter(causa_letra_dig_pri %in% c("S","T","V", "W", "X", "Y") ) |> 
  #Criar variável CID10 Causa externa.
  mutate(cid_externa = case_when(
    #Quando diag_princ for cap XX, então cid10 da causa externa externa é o diag_princ
    causa_letra_dig_pri %in% c("V", "W", "X", "Y") ~ diag_princ, 
    #Quando diag_princ for cap XIX e o primeiro diagnóstico secundário for cap XX
    #então a cid10 da causa externa é o primeiro diagnóstico secundário.
    causa_letra_dig_pri %in% c("S","T") & substr(diagsec1,1,1) %in% c("V", "W", "X", "Y") ~ diagsec1,
    #O primeiro diagnóstico secundário não é cap XX. Então a cid10 da causa externa é o diag_princ
    .default = diag_princ) ) |>
  #Na variável causa externa, mantendo somente AIHs com cid10_externa no capítulo XX.
  filter(substr(cid_externa,1,1) %in% c("V", "W", "X", "Y") ) |>
  #Elimina variável não utilizada
  select(!c(causa_letra_dig_pri) ) -> sih


#Intenção, instrumento e local do incidente
sih |> mutate(
  #Causa e loca do óbito.
  causa_letra = substr(cid_externa,1,1),
  causa_num = as.numeric(substr(cid_externa,2,3)),
  local_obito = as.numeric(substr(cid_externa,4,4)),
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
  #Acidente - Desconhecido /*X:59 ? acidentes não especificados e pode contem homicídio mal registrado, visto no artigo sobre qualidade da declaração de óbito em SP.*/
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
      causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros")), 
  
  
  ###Instrumento (Dicion?rio: 1=Envenenamento; 2=Enforcamento; 3=Afogamento; 4=PAF; 5=Impacto; 6=Fogo; 7=Perfurante; 8=Contundente; 9=Desconhecido; 10=veiculo)
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
    
    #PAF (==4) - N?o tem acidente por arma de fogo
    causa_letra  == "W" & causa_num > 31 & causa_num < 35 | #Acidente - PAF*/
      causa_letra  == "X" & causa_num > 71 & causa_num < 75 |  #/*Self harm - PAF*/
      causa_letra  == "X" & causa_num > 92 & causa_num < 96 |  #/*ag. - PAF*/
      causa_letra  == "Y" & causa_num > 21 & causa_num < 25 |  #/*Ind. - PAF*/
      causa_letra  == "Y" & causa_num ==35 &  local_obito == 0 ~ "PAF",  #/*h_legal - PAF*/
    #causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 - Foi para instrumento fogo
    
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
      causa_letra  == "Y" & causa_num == 35 & local_obito ==2 |  #/*h_legal involvendo fuma?a*/
      causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 ~ "Fogo", # /*h_legal involvendo explos?o*/
    
    #Perfurante (==7) 
    causa_letra  == "X" & causa_num ==78 |  #/*self objeto afiado*/ 
      causa_letra  == "X" & causa_num ==99 |  #/*ag. objeto afiado*/ 
      causa_letra  == "Y" & causa_num ==28 |  #/*Ind. objeto afiado*/ 
      causa_letra  == "W" & causa_num > 24 & causa_num < 27 |  #/*Acidente objeto afiado. Estava indo para indeterminado*/ 
      causa_letra  == "Y" & causa_num == 35 & local_obito ==4 ~ "Perfurante", #/*h_legal objeto afiado*/
    
    #Contundente (==8) 
    causa_letra  == "W" & causa_num ==51 |  #/*Acidente - Colis?o entre duas pessoas*/ 
      causa_letra  == "X" & causa_num ==79 |  #/*self por objeto contundente*/ 
      causa_letra  == "Y" & causa_num ==0 |  #/*ag. por objeto contundente*/ 
      causa_letra  == "Y" & causa_num > 03 & causa_num < 06 |  #/*Ag. por meio de for?a corporal + Ag. sexual por meio de for?a f?sica*/ 
      causa_letra  == "W" & causa_num == 50 |  #/*Acidente - Golpe, pancada, ponta p?*/
      causa_letra  == "Y" & causa_num == 29 |  #/*Ind. Objento contundente*/ 
      causa_letra  == "Y" & causa_num == 35 & local_obito ==3 ~ "Contundente", #/*h_legal objeto contundente*/ 
    
    #Desconhecido (==9) A segunga categoria contém negligência que não é desconhecida. Cadê acidente
    causa_letra  == "X" & causa_num > 82 & causa_num < 85 |  #/*self. Outros meios especificados + self outros meios n?o especificados*/
      causa_letra  == "Y" & causa_num > 05 & causa_num < 10 |  #/*Ag. Neglig?ncia + Ag. Outros maus tratos + Ag. Outros meios especificados + Ag. outros meios n?o especificados*/
      causa_letra  == "Y" & causa_num > 32 & causa_num < 35 |  #/*Ind. Outros fatos ou eventos espcificados + fatos ou eventos n?o espcificados*/
      causa_letra  == "Y" & causa_num == 35 & local_obito ==5 |  #/*h_legal Execu??o legal - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 35 & local_obito == 6 |  #/*h_legal Execu??o legal por outros meios especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 35 & local_obito ==7 |  #/*h_legal Execu??o legal por meios n?o especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
      causa_letra  == "Y" & causa_num == 36 | # /*Opera??es de guerra*/
      causa_letra  == "X" & causa_num > 57 & causa_num < 60 ~ "Desconhecido", #/*Acidente instrumento desconhecido. Categoria inseriada, não estava na rotina.*/
    
    
    #veículo (==10) 1.Acidente 2.Homicídio (y03, impacto) , 3.Indeterminado (y32,impacto) 4.Suicídio(x82,impacto)
    causa_letra  == "V" & causa_num > 0 | causa_letra  == "Y" & causa_num == 03 | 
    causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo") ) ) -> sih


sih |> 
  filter(intencao != "Outros") -> sih


#Check ano de computação x intenção da internação.
sih |> 
  tabyl(ano_cmpt,intencao)


#O resultado base com o encontrado em internações por local de internação
#Por que? Qual a diferença entre local de internção e local de residência?
#Checks
sih |> tabyl(intencao,instrumento) |> adorn_totals(where = c("col") )


#Descartar causa_letra e causa_num
# sih |> select(!c(causa_letra, causa_num ) ) -> sih

### Características das internações
#Level das variáveis
#O dicionário da documentação não informa o label das variáveis.
#label retirado de:
#https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacoes-hospitalares-do-sus-sihsus/documentacao/
sih |> mutate(
  #Raça\COr
  raca_cor = case_match(raca_cor, "01" ~ "Branca", "02" ~ "Preta", "03" ~ "Parda", "04" ~ "Amarela", 
                            "05" ~ "Indígena", "99" ~ "Desconhecido", .default = "Missing") |> as_factor(),
  #Sexo
  sexo = case_match(sexo, "1" ~ "Masculino", "3" ~ "Feminino", .default = "Missing") |> as_factor(),
  
  #Acertando os anos
  # ano_cmpt = case_when(ano_cmpt == "95" ~ "1995",
  #                    ano_cmpt == "96" ~ "1996",
  #                    ano_cmpt == "97" ~ "1997", TRUE ~ ano_cmpt),
  # ano_cmpt = as.numeric(ano_cmpt),
  #Criando datas e transformando na classe date
  
  #Data de computação
  ano_mes_cmpt = paste0(as.character(ano_cmpt), "-", as.character(mes_cmpt) ) |> zoo::as.yearmon(),
  
  
  
  #Data de internação (Atendimento)
  dt_inter = ymd(dt_inter),#Data diária
  ano_inter = year(dt_inter),
  mes_inter = month(dt_inter),
  dwk_inter = wday(dt_inter), #Dia da semana da interenação
  ano_mes_inter = zoo::as.yearmon(dt_inter,"%Y%b"), #Data da internação mensal.
  
  #Data de saída
  dt_saida = ymd(dt_saida),#Data diária
  ano_saida = year(dt_saida),
  mes_saida = month(dt_saida),
  dwk_saida = wday(dt_saida),
  dtt_saida = zoo::as.yearmon(dt_saida,"%Y%b"),
  
  #Precisa inserir as ufs
  #Retira 1 e 2 dígito do Código do município de residência do paciente
  uf_resd = as.numeric(substr(munic_res, start = 1, stop = 2)),
  #Levels das UFs
  uf_resd = as.factor(recode(uf_resd,'11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá", '17'= "Tocantins", 
                        '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
                        '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
                        '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
                        '52'= "Goiás", '53'= "Brasília", '99'= "CNRAC") ), 
  
  #Grau de instrução do paciente
  esc =  case_match(esc, "0" ~ "ignorado/não se aplica", 
                    "1" ~ "Analfabeto", "2" ~ "1º Grau", "3" ~ "2º grau", "4" ~  "3º Grau", .default = "Missing") |> as_factor(),
  
  #Indica se o paciente teve saída com morte
  morte = recode(morte,"1" = "Sim", "0" = "Não") |> as_factor() ) -> sih


sih |> filter(intencao %in% c("h_legal","Homicídio") ) |>
  tabyl(dtt_inter,ano_mes_cmpt) |> adorn_totals()



sih |> select(ano_cmpt,mes_cmpt) |>
  mutate(ano_mes_cmpt = paste0(as.character(ano_cmpt), "-", as.character(mes_cmpt) ) |> zoo::as.yearmon(),
        
  




sih |> filter(intencao %in% c("h_legal","Homicídio") ) |>
  tabyl(dtt_inter,ano_cmpt)




sih |>
  tabyl(intencao,ano_inter)


#Checks
sih |> tabyl(intencao,raca_cor) |> adorn_totals(where = c("col","row") )

###Atribuindo Etnia
#Importando etnia
etn <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Dicionário/SIH/bases_auxiliares/ETNIA.xlsx") |>
  rename(cod_etn = cod,etnia = value) |>
  mutate(cod_etn = cod_etn |> as_factor(),etnia = etnia |> as_factor() ) 

#Precisa colocar ou retirar os zeros de cot_etn
sih |> mutate(
  #Removendo o zeros a esquerda
  cod_etn = str_remove(cod_etn, "^0+") |> as_factor() ) |>
  left_join(x = _ ,etn, by = join_by("cod_etn") ) |> 
  filter(raca_cor == "Indígena" & intencao == "Homicídio") |>
  tabyl(etnia) |> filter(n>0) |> view() |> rio::export(x=_,"etn.xlsx")
  filter(!is.na(etn) ) |>
  arrange(desc(n)) |> tibble() |> view()


sih |> tabyl(intencao,raca_cor)

# DBC do SIH da secretaria de saúde RJ ---------------------------------------------------------------------
sih <- read.dbc("C:/Users/gabli/Desktop/r/SIH/rdirj2402.dbc") |> 
  clean_names() |> 
  #Excluindo internações de longa duração
  filter(c_ident == 1) |>
  
  #Matém somente AIHs com diagnóstico principal cap XIX ou XX. Somente causa externa
  mutate(causa_letra_dig_pri = substr(c_diagprin,1,1) ) |> #Mais fácil criar causa_letra_dig_pri do que fazer filtro várias vezes
  #Mantém caps XIX e XX.
  filter(causa_letra_dig_pri %in% c("S","T","V", "W", "X", "Y") ) |> 
  
  #Criar variável CID10 Causa externa.
  mutate(cid_externa = case_when(
    #Quando diag_princ for cap XX, então cid10 da causa externa externa é o diag_princ
    causa_letra_dig_pri %in% c("V", "W", "X", "Y") ~ c_diagprin, 
    #Quando diag_princ for cap XIX, então a cid10 da causa externa é o primeiro diagnóstico segundário.
    causa_letra_dig_pri %in% c("S","T") & substr(c_diagsec1,1,1) %in% c("V", "W", "X", "Y") ~ c_diagsec1,
    .default = c_diagprin) ) |>
  #Mantém somente variáveis utilizadas.
  select(cid_externa) |>
  #Na variável causa externa, mantendo somente AIHs com cid10 externa no capítulo XX.
  filter(substr(cid_externa,1,1) %in% c("V", "W", "X", "Y") )

