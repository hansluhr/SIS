
# Função de tratamento do SIH ---------------------------------------------
tratar_sim <- function(data) {
  
  
  #Vou padronizar a classe da variável, permitindo empilhar.
  #Algumas variáveis trocam de classe, transformar todas em character permite
  #empilhar. Estou transformando em character todas as variáveis, exceto variáveis 
  #iniciando em DT e idades. 
  
  #Seleciona as variáveis para transformação vars_char
  vars_char <- names(data)[
    !stringr::str_starts(names(data), "DT") & names(data) != "IDADE" & names(data) != "IDADEMAE"]
  
  #Transformando variáveis de interesse para character.
  cols_existentes <- intersect(vars_char, names(data))
  if (length(cols_existentes) > 0) {
    data[, (cols_existentes) := lapply(.SD, as.character), .SDcols = cols_existentes]
  }


# Transformações utilizando mutate ----------------------------------------
  data |> 

    mutate(
    
    causa_letra = substr(causabas,1,1),
    causa_num = as.numeric(substr(causabas,2,3) ),
    local_incd = as.numeric(substr(causabas,4,4) ),

# Idade ------------------------------------------------------------------
    #Idade                          
    idade = as.double(as.character(idade)),
    #Idade desconhecida
    idade = case_when(idade == 999 | idade == 0  ~ as.double(NA), TRUE ~ idade),
    #Menor de 1 ano
    idade = case_when(idade > 0 & idade <= 400  ~ 0, TRUE ~ idade),
    #Idade em anos
    idade = case_when(idade > 400 & idade < 999 ~ idade - 400, TRUE ~ idade),
    

# Intenção da causa externa -----------------------------------------------
    #Intenção
    intencao = case_when(
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
      
      
      ##Sucidídio
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
        causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros") |> as_factor(), 
    

# Instrumento causa externa -----------------------------------------------
    instrumento =  case_when(
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
      
      #veículo 1.Acidente 2.Homicídio (y03, impacto) , 3.Indeterminado (y32,impacto) 4.Suicídio(x82,impacto)
      causa_letra  == "V" & causa_num > 0 | causa_letra  == "Y" & causa_num == 03 | 
      causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo") |> as_factor(), 

      #Variável de intencionalidades para facilitar contagens.
      intencao_homic = case_when(intencao %in% c("Homicídio","h_legal") ~ "Homicídio",
                                 .default = intencao) |> as_factor(),  
      #Consertando dtobito
      dtobito = as.numeric(as.character(dtobito)),
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

     #Transformando data de factor para date  
      across(.cols = starts_with("dt"), .fns = ~ lubridate::dmy(.) ),
      ano = as.factor(lubridate::year(dtobito)),
      mes = as.factor( lubridate::month(dtobito,label = TRUE) ),
      dia = as.factor(lubridate::wday(dtobito,label = TRUE) ), 
       
      
      #Código dos municípios de ocorrência e residência com seis dígitos
      #No microdado do SIM. A partir de 2006 o código do município aparece com 6 dígitos. 
      #Vou deixar todos os municípios em todos os anos com 6 dígitos.
      across(.cols =  c(codmunnatu, #Código município de naturalidade do falecido. 
                        codmunres,  #Código município de residência
                        codmunocor), #Código município de ocorrência.
             .fns = ~ substr(., start = 1, stop = 6) ), 
      
      #Pegar o código da uf de ocorrência e uf de residência
      across(.cols = c(codmunocor, codmunresd), .fns = ~ as.numeric( substr(.,1,2) ), #Dois primeiros dígitos são o código da UF
             .names = "cod_uf_{str_sub(.col, start = 7, end = 10)}"),
      
      #Nome da UF de ocorrência e UF de residência. Utilizar o geobr é melhor?
      across(.cols = c(cod_uf_ocor, cod_uf_resd),
             .fns = ~ as.factor(recode(.,
                                       '11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá",'17'= "Tocantins", 
                                       '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
                                       '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
                                       '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
                                       '52'= "Goiás", '53'= "Distrito Federal", '99'= "CNRAC") ), .names = "uf_{str_sub(.col, start = 8, end = 11)}"),  
      
      #Nome da região de ocorrência e região de residência
      across(.cols = c(uf_ocor, uf_resd),
             .fns = ~ as.factor(case_when( 
               #Região Norte
               . %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
               #Região Nordeste
               . %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
               #Região Centro-Oeste
               . %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
               #Região Sudeste
               . %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ), .names = "reg_{str_sub(.col, start = 4, end = 7)}"), 
      
      ###Características do morto
      ##Escolaridade
      esc = case_match(.x = esc, "1" ~ "Nenhuma", "2" ~ "1 a 3 anos", "3" ~  "4 a 7 anos", "4" ~  "8 a 11 anos",
                       "5" ~  "12 anos e mais", NA ~ "Ignorado", .default = "Ignorado") |> as_factor() |>  
        #Ordem dos Levels de escolaridade
        fct_relevel("Nenhuma", 
                    "1 a 3 anos",
                    "4 a 7 anos",
                    "8 a 11 anos",
                    "12 anos e mais",
                    "Ignorado"),
      #Sexo
      sexo = case_match(.x = sexo,"9" ~ "Ignorado", "0" ~ "Ignorado", "1" ~ "Homem", "2" ~ "Mulher",
                        .default = "Ignorado") |> as_factor(),
      
      #Raça\cor
      racacor = case_match(.x = racacor, "1" ~ "Branca", "2" ~ "Preta", "3" ~ "Amarela", "4" ~ "Parda", "5" ~ "Indigena",
                           "9" ~ "Ignorado", .default = "Ignorado" ) |> as_factor(),
      #Estado Civil
      estciv = case_match(.x = estciv, "1" ~"Solteiro", 
                          "2" ~ "Casado", "3" ~ "Viúvo", "4" ~ "Divorciado", "5" ~ "União Estável", "9" ~ "Ignorado",
                          "0" ~ "Ignorado", .default = "Ignorado" ) |> as_factor(),
      
      #Local do incidente - Variável criada
      local_incd = recode(local_incd,"0" = "Residencial", "1"= "Hab. Coletiva", "2"="Área de administração pública*", 
                           "3"="Esportiva", "4"="Rua/Estrada", "5"="Comercial", "6"="Industrial",  
                           "7"= "Fazenda", "8"="Outros", "9"= "Ignorado" ),
      
      #Preenchimento dos NAs.
      local_incd = replace_na(local_incd,"Ignorado"), #NA deveria ser missing.
      
      #Local de óbito de intervenção legal é rua/estrada
      
      local_incd = as.factor(case_when(
        #Em intervenção legal o terceiro dígito não é o local. Vou assumir rua\estrada.
        intencao == "h_legal" ~ "Rua/Estrada",
        #Local de óbito de acidente de transporte V01-V99. O terceiro dígito não é local do incidente.
        intencao == "Acidente" & instrumento == "Veículo" ~ "Rua/Estrada", 
        #Y06.- Negligência e abandono ou Y07.- Outras síndromes de maus tratos. Residência?
        TRUE ~ local_incd) ) ) |>
  #Local de óbito de Y06 e Y07 Negligência, Abandono e maus tratos. Olhei as idades e não parece ser abandono de criança.
  #Olhar o keep. Autoria conhecida pode ser residência. desconhecida rua\estrada
  #Instrumento missing. Ao excluir intencao outros não deve existir instrumento NA.
  #instrumento = replace_na(instrumento,"Desconhecido").
  #Exclusão de variáveis não utilizadas ------------------------------------
  select(!c(causa_letra,causa_num) )
    
  
  
  ######################################################################################################################

    # #Correções nos códigos do DF. Existem códigos das regiões administrativas. Conserta para código do DF.
    # MUNIC_MOV = fifelse(startsWith(as.character(MUNIC_MOV), "53"), "530010", as.character(MUNIC_MOV)),
    # MUNIC_RES = fifelse(startsWith(as.character(MUNIC_RES), "53"), "530010", as.character(MUNIC_RES)),

  
  # #ICSAP: Identificação por CID
  # if("DIAG_PRINC" %in% names(data) ) {
  #   data[,
  #        ICSAP := as_factor( fcase(
  #          DIAG_PRINC %in% c("A370", "A378", "A379", "A360", "A361", "A363", "A369", "A33", "A34", "A35", "B06", "B052", "A950", "A959", "B161", "B162", "B169", "G000",
  #                            "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A190", "A191", "A192", "A198", "A199",
  #                            "A150", "A151", "A152", "A153", "A160", "A161", "A162", "A154", "A155", "A156", "A157", "A158", "A159", "A163", "A164", "A165",
  #                            "A166", "A167", "A168", "A169", "A180", "A181", "A182", "A183", "A184", "A185", "A186", "A187", "A188", "I00", "I010", "I011",
  #                            "I012", "I018", "I019", "I020", "I029", "A511", "A512", "A513", "A514", "A515", "A519", "A520", "A521", "A522", "A523",
  #                            "A527", "A528", "A529", "A530", "A539", "B500", "B509", "B518", "B519", "B520", "B528", "B53", "B54", "B260", "B268", "B269",
  #                            "B770", "B778", "B779"), "cidgrupo1",
  #          
  #          DIAG_PRINC %in% c("E86", "A00", "A010", "A020", "A021", "A029", "A039", "A040",
  #                            "A041", "A042", "A044", "A046", "A047", "A048", "A049",
  #                            "A050", "A054", "A058", "A059", "A060", "A064", "A068", "A069",
  #                            "A071", "A073", "A078", "A079", "A080", "A083", "A084", "A085", "A09"), "cidgrupo2",
  #          
  #          DIAG_PRINC %in% c("D500", "D501", "D508", "D509"), "cidgrupo3",
  #          
  #          DIAG_PRINC %in% c("E40", "E41", "E42", "E43", "E440", "E441", "E45", "E46", "E500", "E508", "E51", "E52",
  #                            "E539", "E54", "E55", "E56", "E57", "E58", "E59", "E60", "E61", "E62", "E638", "E64"), "cidgrupo4",
  #          
  #          DIAG_PRINC %in% c("H660", "H661", "H662", "H663", "H664", "H669", "J00",
  #                            "J010", "J018", "J019", "J029", "J030", "J038", "J039", "J060", "J068", "J069", "J31"), "cidgrupo5",
  #          
  #          DIAG_PRINC %in% c("J13", "J14", "J153", "J154", "J158", "J159", "J181"), "cidgrupo6",
  #          
  #          DIAG_PRINC %in% c("J45", "J46"), "cidgrupo7",
  #          
  #          DIAG_PRINC %in% c("J200", "J201", "J203", "J205", "J209", "J210", "J218", "J219", "J40", "J410", "J411",
  #                            "J418", "J42", "J43", "J47", "J44", "J450", "J451", "J458", "J459"), "cidgrupo8",
  #          
  #          DIAG_PRINC %in% c("I10", "I110", "I119"), "cidgrupo9",
  #          
  #          DIAG_PRINC %in% c("I200", "I201", "I208", "I209"), "cidgrupo10",
  #          
  #          DIAG_PRINC %in% c("I500", "I501", "I509", "J81"), "cidgrupo11",
  #          
  #          DIAG_PRINC %in% c("I63", "I64", "I65", "I66", "I67", "I69"), "cidgrupo12",
  #          
  #          DIAG_PRINC %in% c("E100", "E101", "E110", "E111", "E140", "E141", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109",
  #                            "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E121", "E125", "E126", "E127", "E128", "E129", "E130", "E131",
  #                            "E132", "E133", "E135", "E136", "E137", "E138", "E139", "E142", "E143", "E144", "E145", "E146", "E147", "E148", "E149"), "cidgrupo13",
  #          
  #          DIAG_PRINC %in% c("G400", "G401", "G402", "G403", "G404", "G405", "G406", "G407", "G408", "G409", "G41"), "cidgrupo14",
  #          
  #          DIAG_PRINC %in% c("N10", "N11", "N12", "N300", "N301", "N302", "N303", "N304", "N308", "N309", "N340", "N341", "N342", "N343", "N390"), "cidgrupo15",
  #          
  #          DIAG_PRINC %in% c("A446", "L01", "L020", "L021", "L022", "L023", "L024", "L028", "L029", "L030", "L031",
  #                            "L032", "L033", "L038", "L039", "L040", "L041", "L042", "L043", "L048", "L049", "L080", "L088", "L089"), "cidgrupo16",
  #          
  #          DIAG_PRINC %in% c("N700", "N701", "N709", "N710", "N719", "N72", "N730", "N732", "N733", "N734", "N735",
  #                            "N736", "N738", "N739", "N750", "N751", "N758", "N760", "N762", "N764", "N766", "N768"), "cidgrupo17",
  #          
  #          DIAG_PRINC %in% c("K25", "K26", "K27", "K28", "K920", "K921", "K922"), "cidgrupo18",
  #          
  #          DIAG_PRINC %in% c("O23", "A500", "A501", "A502", "A503", "A504", "A505", "A506", "A507", "A509", "P35"), "cidgrupo19", default = "Outros") ) ]
  # }
  # 
  # #Nacionalidade
  # if("NACIONAL" %in% names(data) ) {
  #   data[,
  #        def_NACIONAL := as_factor(fcase(
  #          NACIONAL == "170" , "Abissinia",NACIONAL == "171" , "Acores",
  #          NACIONAL == "172" , "Afar frances", NACIONAL == "241" , "Afeganistao",
  #          NACIONAL == "093" ,  "Albania",  NACIONAL == "030" , "Alemanha",
  #          NACIONAL == "174" , "Alto volta", NACIONAL == "094" ,  "Andorra",
  #          NACIONAL == "175" , "Angola", NACIONAL == "334" , "Antartica francesa",
  #          NACIONAL == "337" , "Antartico argentino",NACIONAL == "333" , "Antartico britanico, territorio",
  #          NACIONAL == "336" , "Antartico chileno", NACIONAL == "338" , "Antartico noruegues",
  #          NACIONAL == "028" ,  "Antigua e. dep. barbuda", NACIONAL == "029" , "Antilhas holandesas",
  #          NACIONAL == "339" , "Apatrida", NACIONAL == "242" , "Arabia saudita",
  #          NACIONAL == "176" , "Argelia", NACIONAL == "021" , "Argentina",
  #          NACIONAL == "347" , "Armenia", NACIONAL == "289" , "Arquipelago de bismark",
  #          NACIONAL == "175" , "Angola", NACIONAL == "285" , "Arquipelago manahiki",
  #          NACIONAL == "286" , "Arquipelago midway", NACIONAL == "033" , "Aruba",
  #          NACIONAL == "175" , "Angola", NACIONAL == "198" , "Ascensao e tristao da cunha,is",
  #          NACIONAL == "287" , "Ashmore e cartier", NACIONAL == "288" , "Australia",
  #          NACIONAL == "095" ,  "Austria", NACIONAL == "138" , "Azerbaijao",
  #          NACIONAL == "243" , "Bahrein", NACIONAL == "342" , "Bangladesh",
  #          NACIONAL == "044" ,  "Barbados", NACIONAL == "139" , "Bashkista",
  #          NACIONAL == "177" , "Bechuanalandia", NACIONAL == "031" , "Belgica",
  #          NACIONAL == "046" ,  "Belize", NACIONAL == "178" , "Benin",
  #          NACIONAL == "083" ,  "Bermudas", NACIONAL == "246" , "Bhutan",
  #          NACIONAL == "244" , "Birmania", NACIONAL == "022" ,  "Bolivia", NACIONAL == "134" , "Bosnia herzegovina",
  #          NACIONAL == "179" , "Botsuana", NACIONAL == "010" , "Brasil",
  #          NACIONAL == "245" , "Brunei", NACIONAL == "096" , "Bulgaria",
  #          NACIONAL == "238" , "Burkina fasso", NACIONAL == "180" , "Burundi",
  #          NACIONAL == "141" , "Buryat", NACIONAL == "343" , "Cabo verde", NACIONAL == "181" , "Camaroes",
  #          NACIONAL == "034" ,  "Canada", NACIONAL == "142" , "Carelia", NACIONAL ==  "247" , "Catar",
  #          NACIONAL == "143" , "Cazaquistao", NACIONAL ==  "248" , "Ceilao",
  #          NACIONAL == "182" , "Ceuta e melilla", NACIONAL == "183" , "Chade",
  #          NACIONAL == "144" , "Chechen ingusth", NACIONAL == "023" , "Chile",
  #          NACIONAL == "042" ,  "China", NACIONAL == "249" , "China (taiwan)",
  #          NACIONAL == "097" ,  "Chipre", NACIONAL == "145" , "Chuvash", NACIONAL == "275" , "Cingapura",
  #          NACIONAL == "026" ,  "Colombia", NACIONAL == "040" , "Comunidade das bahamas",
  #          NACIONAL == "054" ,  "Comunidade dominicana", NACIONAL == "185" , "Congo",
  #          NACIONAL == "043" ,  "Coreia", NACIONAL == "186" , "Costa do marfim",
  #          NACIONAL == "051" ,  "Costa rica", NACIONAL == "250" , "Coveite",
  #          NACIONAL == "130" , "Croacia", NACIONAL == "052" , "Cuba", NACIONAL == "053" , "Curacao",
  #          NACIONAL == "146" , "Dagesta", NACIONAL == "187" , "Daome",
  #          NACIONAL == "340" , "Dependencia de ross", NACIONAL == "098" , "Dinamarca",
  #          NACIONAL == "188" , "Djibuti", NACIONAL ==  "099" , "Eire",
  #          NACIONAL == "251" , "Emirados arabes unidos", NACIONAL == "027" , "Equador",
  #          NACIONAL == "100" , "Escocia", NACIONAL == "136" , "Eslovaquia",
  #          NACIONAL == "132" , "Eslovenia", NACIONAL == "035" , "Espanha",
  #          NACIONAL == "129" , "Estado da cidade do vaticano",
  #          NACIONAL == "057" ,  "Estados assoc. das antilhas",
  #          NACIONAL == "036" ,  "Estados unidos da america (eua)",
  #          NACIONAL == "147" , "Estonia", NACIONAL == "190" , "Etiopia",
  #          NACIONAL == "252" , "Filipinas", NACIONAL == "102" , "Finlandia", NACIONAL == "037" , "Franca",
  #          NACIONAL == "192" , "Gambia", NACIONAL == "193" , "Gana", NACIONAL == "194" , "Gaza",
  #          NACIONAL == "148" , "Georgia", NACIONAL == "103" , "Gibraltar",
  #          NACIONAL == "149" , "Gorno altai", NACIONAL == "032" , "Gra-bretanha",
  #          NACIONAL == "059"  , "Granada", NACIONAL == "104" , "Grecia", NACIONAL == "084" , "Groenlandia",
  #          NACIONAL == "292" , "Guam", NACIONAL == "061" , "Guatemala",
  #          NACIONAL == "087" , "Guiana francesa", NACIONAL == "195" , "Guine",
  #          NACIONAL == "344" , "Guine bissau", NACIONAL == "196" , "Guine equatorial",
  #          NACIONAL == "105" , "Holanda", NACIONAL == "064" , "Honduras",
  #          NACIONAL == "063" , "Honduras britanicas", NACIONAL == "253" , "Hong-kong",
  #          NACIONAL == "106" , "Hungria", NACIONAL == "254" , "Iemen",
  #          NACIONAL == "345" , "Iemen do sul", NACIONAL == "197" , "Ifni",
  #          NACIONAL == "300" , "Ilha johnston e sand", NACIONAL == "069" , "Ilha milhos",
  #          NACIONAL == "293" , "Ilhas baker", NACIONAL == "107" , "Ilhas baleares",
  #          NACIONAL == "199" , "Ilhas canarias", NACIONAL == "294" , "Ilhas cantao e enderburg",
  #          NACIONAL == "295" , "Ilhas carolinas", NACIONAL == "297" , "Ilhas christmas",
  #          NACIONAL == "184" , "Ilhas comores", NACIONAL == "290" , "Ilhas cook",
  #          NACIONAL == "108" , "Ilhas cosmoledo (lomores)",
  #          NACIONAL == "117" , "Ilhas de man", NACIONAL == "109" , "Ilhas do canal",
  #          NACIONAL == "296" , "Ilhas do pacifico", NACIONAL == "058" , "Ilhas falklands",
  #          NACIONAL == "101" , "Ilhas faroes", NACIONAL == "298" , "Ilhas gilbert",
  #          NACIONAL == "060" , "Ilhas guadalupe", NACIONAL == "299" , "Ilhas howland e jarvis",
  #          NACIONAL == "301" , "Ilhas kingman reef", NACIONAL == "305" , "Ilhas macdonal e heard",
  #          NACIONAL == "302" , "Ilhas macquaire", NACIONAL == "067" , "Ilhas malvinas",
  #          NACIONAL == "303" , "Ilhas marianas", NACIONAL == "304" , "Ilhas marshall",
  #          NACIONAL == "306" , "Ilhas niue", NACIONAL == "307" , "Ilhas norfolk",
  #          NACIONAL == "315" , "Ilhas nova caledonia", NACIONAL == "318" , "Ilhas novas hebridas",
  #          NACIONAL == "308" , "Ilhas palau", NACIONAL == "320" , "Ilhas pascoa",
  #          NACIONAL == "321" , "Ilhas pitcairin", NACIONAL == "309" , "Ilhas salomao",
  #          NACIONAL == "326" , "Ilhas santa cruz", NACIONAL == "065" , "Ilhas serranas",
  #          NACIONAL == "310" , "Ilhas tokelau", NACIONAL == "080" , "Ilhas turca",
  #          NACIONAL == "047" , "Ilhas turks e caicos", NACIONAL == "082" , "Ilhas virgens americanas",
  #          NACIONAL == "081" , "Ilhas virgens britanicas",
  #          NACIONAL == "311" , "Ilhas wake", NACIONAL == "332" , "Ilhas wallis e futuna",
  #          NACIONAL == "255" , "India", NACIONAL ==  "256" , "Indonesia",
  #          NACIONAL == "110" , "Inglaterra", NACIONAL == "257" , "Ira",
  #          NACIONAL == "258" , "Iraque", NACIONAL == "112" , "Irlanda",
  #          NACIONAL == "111" , "Irlanda do norte",
  #          NACIONAL == "113" , "Islandia", NACIONAL == "259" , "Israel",
  #          NACIONAL == "039" , "Italia", NACIONAL == "114" , "Iugoslavia",
  #          NACIONAL == "066" , "Jamaica", NACIONAL == "041" , "Japao",
  #          NACIONAL == "260" , "Jordania",
  #          NACIONAL == "150" , "Kabardino balkar", NACIONAL == "312" , "Kalimatan",
  #          NACIONAL == "151" , "Kalmir", NACIONAL == "346" , "Kara kalpak",
  #          NACIONAL == "152" , "Karachaevocherkess", NACIONAL == "153" , "Khakass",
  #          NACIONAL == "261" , "Kmer/camboja", NACIONAL == "154" , "Komi",
  #          NACIONAL == "262" , "Kuwait", NACIONAL == "263" , "Laos",
  #          NACIONAL == "200" , "Lesoto", NACIONAL == "155" , "Letonia",
  #          NACIONAL == "264" , "Libano",
  #          NACIONAL == "201" , "Liberia", NACIONAL == "202" , "Libia",
  #          NACIONAL == "115" , "Liechtenstein",
  #          NACIONAL == "156" , "Lituania", NACIONAL == "116" , "Luxemburgo",
  #          NACIONAL == "265" , "Macau", NACIONAL == "205" , "Madagascar",
  #          NACIONAL == "203" , "Madeira", NACIONAL == "266" , "Malasia",
  #          NACIONAL == "204" , "Malawi",
  #          NACIONAL == "267" , "Maldivas", NACIONAL == "206" , "Mali",
  #          NACIONAL == "157" , "Mari", NACIONAL ==  "207" , "Marrocos",
  #          NACIONAL == "068" , "Martinica",
  #          NACIONAL == "268" , "Mascate", NACIONAL == "208" , "Mauricio",
  #          NACIONAL == "209" , "Mauritania",NACIONAL ==  "085" , "Mexico",
  #          NACIONAL == "284" , "Mianma", NACIONAL == "210" , "Mocambique",
  #          NACIONAL == "158" , "Moldavia", NACIONAL == "118" , "Monaco", NACIONAL == "269" , "Mongolia",
  #          NACIONAL == "070" , "Monte serrat", NACIONAL == "137" , "Montenegro",
  #          NACIONAL == "240" , "Namibia", NACIONAL == "314" , "Nauru",
  #          NACIONAL == "270" , "Nepal", NACIONAL == "211" , "Nguane",
  #          NACIONAL == "071" , "Nicaragua",
  #          NACIONAL == "213" , "Nigeria", NACIONAL == "119" , "Noruega",
  #          NACIONAL == "316" , "Nova guine",
  #          NACIONAL == "317" , "Nova zelandia", NACIONAL == "271" , "Oman",
  #          NACIONAL == "159" , "Ossetia setentrional", NACIONAL == "121" , "Pais de gales",
  #          NACIONAL == "122" , "Paises baixos", NACIONAL == "272" , "Palestina",
  #          NACIONAL == "072" , "Panama", NACIONAL == "073" , "Panama(zona do canal)",
  #          NACIONAL == "214" , "Papua nova guine", NACIONAL == "273" , "Paquistao",
  #          NACIONAL == "024" , "Paraguai", NACIONAL == "089" , "Peru",
  #          NACIONAL == "322" , "Polinesia francesa", NACIONAL ==  "123" , "Polonia",
  #          NACIONAL == "074" , "Porto rico", NACIONAL == "045" , "Portugal",
  #          NACIONAL == "215" , "Pracas norte africanas", NACIONAL == "216" , "Protetor do sudoeste africano",
  #          NACIONAL == "217" , "Quenia", NACIONAL == "160" , "Quirguistao",
  #          NACIONAL == "075" , "Quitasueno", NACIONAL == "189" , "Republica arabe do egito",
  #          NACIONAL == "218" , "Republica centro africana",
  #          NACIONAL == "173" , "Republica da africa do sul", NACIONAL == "140" , "Republica da bielorrussia",
  #          NACIONAL == "133" , "Republica da macedonia", NACIONAL == "56" , "Republica de el salvador",
  #          NACIONAL == "291" , "Republica de fiji", NACIONAL == "120" , "Republica de malta",
  #          NACIONAL == "191" , "Republica do gabao", NACIONAL == "062" , "Republica do haiti",
  #          NACIONAL == "212" , "Republica do niger", NACIONAL == "055" , "Republica dominicana",
  #          NACIONAL == "088" , "Republica guiana", NACIONAL == "135" , "Republica tcheca",
  #          NACIONAL == "020" , "Reservado", NACIONAL == "048" , "Reservado",
  #          NACIONAL == "049" , "Reservado", NACIONAL == "050" , "Reservado",
  #          NACIONAL == "219" , "Reuniao", NACIONAL == "220" , "Rodesia (zimbabwe)",
  #          NACIONAL == "124" , "Romenia", NACIONAL == "076" , "Roncador",
  #          NACIONAL == "221" , "Ruanda", NACIONAL == "274" , "Ruiquiu,is",
  #          NACIONAL == "348" , "Russia", NACIONAL == "222" , "Saara espanhol",
  #          NACIONAL == "323" , "Sabah", NACIONAL == "324" , "Samoa americana",
  #          NACIONAL == "325" , "Samoa ocidental", NACIONAL == "125" , "San marino",
  #          NACIONAL == "223" , "Santa helena", NACIONAL == "077" , "Santa lucia",
  #          NACIONAL == "078" , "Sao cristovao", NACIONAL == "224" , "Sao tome e principe",
  #          NACIONAL == "079" , "Sao vicente", NACIONAL == "327" , "Sarawak",
  #          NACIONAL == "349" , "Senegal", NACIONAL == "276" , "Sequin",
  #          NACIONAL == "226" , "Serra leoa", NACIONAL == "131" , "Servia",
  #          NACIONAL == "225" , "Seychelles",
  #          NACIONAL == "277" , "Siria", NACIONAL == "227" , "Somalia, republica",
  #          NACIONAL == "278" , "Sri-lanka", NACIONAL == "086" , "St. pierre et miquelon",
  #          NACIONAL == "228" , "Suazilandia", NACIONAL == "229" , "Sudao",
  #          NACIONAL == "126" , "Suecia", NACIONAL == "038" , "Suica",
  #          NACIONAL == "090" , "Suriname", NACIONAL == "127" , "Svalbard e Jan Maye",
  #          NACIONAL == "161" , "Tadjiquistao", NACIONAL == "279" , "Tailandia",
  #          NACIONAL == "230" , "Tanganica", NACIONAL == "350" , "Tanzania",
  #          NACIONAL == "162" , "Tartaria", NACIONAL == "128" , "Tchecoslovaquia",
  #          NACIONAL == "335" , "Terr. antartico da australia",
  #          NACIONAL == "341" , "Terras austrais", NACIONAL == "231" , "Territ. britanico do oceano indico",
  #          NACIONAL == "328" , "Territorio de cocos", NACIONAL == "319" , "Territorio de papua",
  #          NACIONAL == "329" , "Timor", NACIONAL == "233" , "Togo", NACIONAL == "330" , "Tonga",
  #          NACIONAL == "232" , "Transkei", NACIONAL == "280" , "Tregua, estado",
  #          NACIONAL == "091" , "Trinidad e tobago", NACIONAL == "234" , "Tunisia",
  #          NACIONAL == "163" , "Turcomenistao", NACIONAL ==  "281" , "Turquia",
  #          NACIONAL == "331" , "Tuvalu", NACIONAL == "164" , "Tuvin",
  #          NACIONAL == "165" , "Ucrania", NACIONAL == "166" , "Udmurt", NACIONAL == "235" , "Uganda",
  #          NACIONAL == "167" , "Uniao sovietica", NACIONAL == "025" , "Uruguai",
  #          NACIONAL == "168" , "Uzbequistao", NACIONAL == "092" , "Venezuela",
  #          NACIONAL == "282" , "Vietna do norte",NACIONAL ==  "283" , "Vietna do sul",
  #          NACIONAL == "169" , "Yakut", NACIONAL == "236" , "Zaire",
  #          NACIONAL == "237" , "Zambia", NACIONAL == "239" , "Zimbabwe", default = as.character(NACIONAL) ) ) ]
  # }
  # 
  # 
 
  # #Municípios --------------------------------------------------------------
# 
   
  #Rename do código munic de resdiência. Útil para criar variável com o nome padrão de resd
  # rename(codmunresd = codmunres ) |>
    
  
#     #left_Join com município de residência
#   data <- merge(
#     x = data,
#     y = munics,
#     by.x = "MUNIC_RES",
#     by.y = "code_muni",
#     all.x = TRUE,
#     suffixes = c("", "_resd")
#   )
#   
#   #Renomear colunas de residência
#   setnames(data,
#            old = c("name_muni", "MUNIC_RES", "code_state", "abbrev_state", "name_state", "name_region"),
#            new = c("def_munic_resd", "cod_munic_resd", "code_state_resd", "abbrev_state_resd", "def_uf_resd", "region_resd"))
#   
#   #left_join com município de internação
#   data <- merge(
#     x = data,
#     y = munics,
#     by.x = "MUNIC_MOV",
#     by.y = "code_muni",
#     all.x = TRUE,
#     suffixes = c("", "_int")
#   )
#   
#   #Renomear variáveis
#   setnames(data,
#            old = c("NASC", "ESPEC", "PROC_SOLIC", "PROC_REA", "INSTRU"),
#            new = c("DT_NASC", "COD_ESPEC", "COD_PROC_SOLIC", "COD_PROC_REA", "ESC"))
#   
#   #Renomear colunas de internação
#   setnames(data,
#            old = c("name_muni", "MUNIC_MOV", "code_state", "abbrev_state", "name_state", "name_region"),
#            new = c("def_munic_int", "cod_munic_int", "code_state_int", "abbrev_state_int", "def_uf_int", "region_int"))
#   
  # data <- tibble::as_tibble(data) 
  # data <- droplevels(data.table::as.data.table(data))
  # data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
  #                                                   FUN = stringi::stri_unescape_unicode)))
  
}



# Função utilizada para empilhar o SIH ------------------------------------
empilhar_sim <- function(arquivo, 
                         variaveis = NULL, #Variáveis que desejo manter. NULL seleciona todas as variáveis não excluidas.
                         excluir = vars_excluir) {
  message("Importando: ", arquivo)
  dados <- read.dbc::read.dbc(arquivo) |> janitor::clean_names()
  
  #Excluir variáveis sem preenchimento\Zeradas
  vars_excluir <- intersect(toupper(vars_excluir), names(dados))
  if (length(vars_excluir) > 0) {
    dados[, (vars_excluir) := NULL]
  }
  
  #Selecionar variáveis desejadas. Mantém variáveis disponíveis se não indicar nenhuma variável. 
  if (!is.null(variaveis)) {
    vars_sel <- intersect(variaveis, names(dados))
    dados <- dados[, ..vars_sel]
  }
  
    return(dados)
}
