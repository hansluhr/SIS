#Função para baixar os dbcs
source(file = "C:/Users/gabli/Dropbox/Ipea/Atlas/Rotinas/SIH/sih_baixar_dbc_ftp3.R")
baixar_arquivos_RD(anos = 2024, meses = c(1:3), ufs = "RJ", destino = "C:/Users/gabli/Desktop/r/SIH/dbc")
rm(baixar_arquivos_RD)

#Baixar 2024??


#Importarção e agregação de arquivos dbcs
library(tidyverse)
library(janitor)

# Diretório com os arquivos .dbc
pasta_arquivos <- "C:/Users/gabli/Desktop/r/SIH/dbc"

#Lista de arquivos importados
arquivos_dbc <- list.files(pasta_arquivos, full.names = TRUE,
                            pattern = ".*13|14|15|16|17|18|19|20|21|22|23|24")

# arquivos_dbc <- list.files(pasta_arquivos, full.names = TRUE, 
#                            pattern = "RD[A-Z]{2}(1[3-9]|2[0-4])[0-9]{2}\\.dbc")


# Função para importar, filtrar e selecionar variáveis de cada arquivo
importar_filtrar <- function(arquivo) {
  
  message("Importando: ", arquivo)
  
  #Lê o arquivo .dbc
  dados <- read.dbc::read.dbc(arquivo) %>% clean_names()
  
    #Nos dados de 2013 precisa renomar variável de dado secundário
    # Se o nome do arquivo contém "13", renomeia a variável diag_secun
    if (str_detect(arquivo, "13") ) {
      dados <- dados |> rename(diagsec1 = diag_secun)
    }
    
  #Em 2014 a variável diagsec1 é introduzida, mas não é utilizada. 100% missing 
  #Como diagnóstico secundário ainda é utilizado diag_secun. 
  #Vou excluir diagsec, pois não é utilizada e renomear diag_secun para diagsec1. 
  if (str_detect(arquivo, "14") ) {
    dados <- dados |> 
      select(!c(diagsec1)) |> rename(diagsec1 = diag_secun)
  }
  
  #Filtro e seleção das variáveis de interesse.
  dados |> 
    
    #Mantém internações normais.
    filter(ident == 1) %>%  
    #Variáveis de interesse
    select(
      uf_zi, ano_cmpt, mes_cmpt, n_aih, munic_res, nasc, sexo, dt_inter, dt_saida,
      diag_princ, munic_mov, cod_idade, idade, esc = instru, # Renomeia instru
      cbor, diagsec1, dias_perm, morte, raca_cor, cod_etn = etnia # Renomeia etnia 
      )  }


# Aplicar a função a todos os arquivos e combinar os resultados
sih <- data.table::rbindlist(lapply(arquivos_dbc, importar_filtrar), use.names = TRUE, fill = TRUE)
gc()
# Exibe um resumo da base final
#glimpse(sih)
rm(arquivos_dbc,pasta_arquivos,importar_filtrar)



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
  select(!c(causa_letra_dig_pri) ) |> 

#Intenção, instrumento e local do incidente
  mutate(
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



#Check ano de computação x intenção da internação.
sih |> 
  tabyl(ano_cmpt,intencao) |> adorn_totals()

sih |> filter(intencao %in% c("h_legal", "Homicídio")) |>
  tabyl(ano_cmpt)

# # Criação intencao_homic --------------------------------------------------
# sih %>% 
#   mutate(intencao_homic = as.factor(
#     case_when(intencao == "Homicídio" | intencao == "h_legal" ~ "Homicídio",
#               TRUE ~ intencao) ) ) -> sih


#Transformações nas variáveis de interesse.
#O dicionário não informa o label das variáveis.
#label retirado de:
#https://pcdas.icict.fiocruz.br/conjunto-de-dados/sistema-de-informacoes-hospitalares-do-sus-sihsus/documentacao/
sih |> #slice_sample(n = 1000) |>
  
  as_tibble() |> 
  
  #Excluindo intenções não utilizadas
  drop_na(intencao) |> filter(intencao != "Outros") |> 
  
  #Renomeando variáveis
  rename(
  #Data de Nascimento
  dt_nasc = nasc,
  #local do incidente
  local_incd =  local_obito) |> 
  
  #Transformações nas variáveis de interesse.
  mutate(
    #Variável com agregação de agressão e h_legal
    intencao_homic = as.factor(
      case_when(intencao == "Homicídio" | intencao == "h_legal" ~ "Homicídio",
                TRUE ~ intencao) ),
    #Instrumento. Alguns h_legal contém somente 3 dígitos e não apresentam o instrumento.
    #Vou considerar instrumento desconhecido.
    instrumento = replace_na(instrumento,"Desconhecido"),
    
    #Data de nascimento
    dt_nasc = dt_nasc |> ymd(),
    
    #Acertando datas - #Criando datas e transformando na classe date
    #Data de processamento. valores antigos estão diferentes do padrão corrente.
    #ano_cmpt = case_when(ano_cmpt == "95" ~ "1995",
    #ano_cmpt == "96" ~ "1996",
    #ano_cmpt == "97" ~ "1997", TRUE ~ ano_cmpt),
    ano_mes_cmpt = as.Date(paste(ano_cmpt, mes_cmpt, "01", sep = "-") ), 

    #Data de internação
    dt_inter = ymd(dt_inter), #Data diária
    ano_inter = year(dt_inter) |> as_factor(),
    mes_inter = month(dt_inter) |> as_factor(),
    dwk_inter = lubridate::wday(dt_inter, label = TRUE), #Dia da semana da interenação
    ano_mes_inter = zoo::as.yearmon(dt_inter,"%Y%b"), #Data da internação mensal.
    
    #Data de saída
    dt_saida = ymd(dt_saida), #Data diária
    ano_saida = year(dt_saida) |> as_factor(),
    mes_saida = month(dt_saida) |> as_factor(),
    dwk_saida = lubridate::wday(dt_saida, label = TRUE),
    dtm_saida = zoo::as.yearmon(dt_saida,"%Y%b"), 
    
    #Label das variáveis de interesse
    across( c(sexo, raca_cor, esc, morte, local_incd), ~ case_when(
        
      #label de sexo
      . == 1 & cur_column() == "sexo" ~ "Homem",
      . == 2 & cur_column() == "sexo" ~ "Mulher", . == 3 & cur_column() == "sexo" ~ "Mulher", 
      
      #Label de raça\cor
      . == "01" & cur_column() == "raca_cor" ~ "Branca", . == "02" & cur_column() == "raca_cor" ~ "Preta",
      . == "03" & cur_column() == "raca_cor" ~ "Parda",  . == "04" & cur_column() == "raca_cor" ~ "Amarela",
      . == "05" & cur_column() == "raca_cor" ~ "Indígena", . == "99" & cur_column() == "raca_cor" ~ "Sem Informação", 
      
      #Label de escolaridade
      . == "1" & cur_column() == "esc" ~ "Analfabeto", . == "2" & cur_column() == "esc" ~ "1º Grau",
      . == "3" & cur_column() == "esc" ~ "2º Grau",    . == "4" & cur_column() == "esc" ~ "3º Grau",
      
      #Label de morte 
      . == 0 & cur_column() == "morte" ~ "Não", . == 1 & cur_column() == "morte" ~ "Sim",
      
      #Label do incidente. Elevado mal preenchimento
      . == 0 & cur_column() == "local_incd" ~ "Residencial", 
      . == 1 & cur_column() == "local_incd" ~ "Hab. Coletiva",
      . == 2 & cur_column() == "local_incd" ~ "Área de administração pública*",
      . == 3 & cur_column() == "local_incd" ~ "Esportiva",
      . == 4 & cur_column() == "local_incd" ~ "Rua/Estrada",
      . == 5 & cur_column() == "local_incd" ~ "Comercial",
      . == 6 & cur_column() == "local_incd" ~ "Industrial",
      . == 7 & cur_column() == "local_incd" ~ "Fazenda",
      . == 8 & cur_column() == "local_incd" ~ "Outros",
      . == 9 & cur_column() == "local_incd" ~ "Ignorado", .default = "Missing" ) |> as_factor() ),
      
      #Local de óbito de intervenção legal é rua/estrada
      local_incd = case_when(intencao == "h_legal" ~ "Rua/Estrada", 
                             intencao == "Acidente" & instrumento == "Veículo" ~ "Rua/Estrada",
                             TRUE ~ local_incd),
      #Correção dos códigos de áeras adm do Distrito Federal. 
      #Alterando para o código de Brasília. Quando começa em 53, colocar código de Brasília.
      munic_res =  case_when(str_starts(munic_res, "53") ~ "530010",
                                  .default = munic_res) ) -> sih


#Adicionar município de residência
#Download das informações sobre municípios.
geobr::read_municipality(year = 2022) |> as_tibble() |>
  #Código dos municípios com 6 dígitos.
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6),
  #Transforma em factor     
  across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) |> 
  #Excluindo variáveis não utilizadas.
  select(!c(code_region, geom)) |> sf::st_drop_geometry(data_all) |>

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

#Join do nome dos municípios a base de internações
left_join(x = sih, y = _, #base de municípios entra aqui
            
            by = join_by("munic_res" == "code_muni" ) ) |> 
    
  rename(munic_resd = name_muni, cod_munic_resd = munic_res, code_state_resd = code_state, 
         abbrev_state_resd = abbrev_state, uf_resd = name_state, region_resd = name_region) -> sih


# Etnia -------------------------------------------------------------------
#Importando etnia
etn <- readxl::read_excel("C:/Users/p224552695/Dropbox/Ipea/Atlas/Dicionário/SIH/bases_auxiliares/ETNIA.xlsx") |>
  rename(cod_etn = cod,etnia = value) |>
  mutate(cod_etn = cod_etn |> as_factor(), etnia = etnia |> as_factor() ) 

#Precisa colocar ou retirar os zeros de cot_etn
sih |> 
  mutate(
  #Removendo o zeros a esquerda no código de etnia na base sih
  cod_etn = str_remove(cod_etn, "^0+") |> as_factor() ) |>
  
  left_join(x = _ ,etn, by = join_by("cod_etn") ) -> sih
rm(etn)

#Existe missing no preenchimento do código da etnia

  
  #filter(raca_cor == "Indígena" & intencao == "Homicídio") |>
  tabyl(etnia) |> filter(n>0) |> rio::export(x=_,"etn.xlsx")
filter(!is.na(etn) ) |>
  arrange(desc(n)) |> tibble() |> view()







#Checks
sih |> filter(intencao %in% c("h_legal", "Homicídio")) |>
  tabyl(uf_resd,ano_inter) |> adorn_totals(where = c("row","col"))  
  


# Código da ocupação ------------------------------------------------------
#Tamanho do código cbo na variável id_ocupa_n
sinan |>  mutate(len = id_ocupa_n |> as.character() |> str_length() ) |>
  tabyl(len) |> adorn_pct_formatting() # filter(len < 6) |> select(id_ocupa_n,len) |> view()
#O tabyl indica a maioria das notificações não estão preenchidas de maneira correta.

#Existe id_ocupca_n com letra após a cbo, com o código somente XXX e com ponto no código.
#Resolvi pegar somente os três primeiros dígitos para contornar esses problemas. Portanto, vamos utilizar subgrupo da CBO

#Importando código SubGrupo. Planilha oriunda do MT
cbo <- readr::read_delim("C:/Users/gabli/Desktop/r/cbo/CBO2002 - SubGrupo.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),trim_ws = TRUE) |> clean_names() |>
  mutate(across(where(is.character), ~ str_to_title(.x) |> as.factor() ) ) |>
  rename(ocupa = titulo)

#Atribuição dos labels de ocupação.
sinan |>
  mutate(
    #Na base sinan, transforma código da ocupação para 3 dígitos. Compatível com subgrupo cbo. 
    id_ocupa_n6 = id_ocupa_n,
    id_ocupa_n = id_ocupa_n |> str_sub(start = 1, end = 3) ) |>
  #id_ocupa_n com três dígitos estão somente com 
  left_join(x= _, y = cbo, by = join_by("id_ocupa_n" == "codigo") ) |> 
  #Observações com NA no código da ocupação vão aparecer com label Missing
  mutate(ocupa = ocupa |> fct_na_value_to_level("Missing")  ) -> sinan
rm(cbo)    
gc()



# Base em Parquet ---------------------------------------------------------
library(arrow)
library(tidyverse)
pq_path <- c("C:/Users/p224552695/Desktop/r/SIH/parquet")
sih |>
  group_by(ano_inter) |>
  write_dataset(path = pq_path, format = "parquet")


library(tidyverse)
library(janitor)

sih |> filter(intencao_homic == "Homicídio") |>
  count(raca_cor)

sih |> filter(raca_cor == "Indígena" & intencao_homic == "Homicídio") |>
  count(etnia, sort = TRUE) |> head(10)


sih |> filter(raca_cor == "Indígena" & intencao_homic == "Homicídio") |>
  tabyl(instrumento) |> arrange(desc(n))

