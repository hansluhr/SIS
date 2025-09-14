
# Função de tratamento do SIH ---------------------------------------------
tratar_sim <- function(data) {
  
  
  #Vou padronizar a classe da variável, permitindo empilhar.
  #Algumas variáveis trocam de classe, transformar todas em character permite
  #empilhar. Estou transformando em character todas as variáveis, exceto variáveis 
  #iniciando em DT e idades. 
  
  #Seleciona as variáveis para transformação vars_char.
  #Exclundi as variáveis abaixo
  vars_char <- names(data)[
    #!stringr::str_starts(names(data), "DT") & 
    names(data) != "IDADE" & names(data) != "IDADEMAE"]
  
  #Transformando variáveis de interesse para character.
  cols_existentes <- intersect(vars_char, names(data))
  if (length(cols_existentes) > 0) {
    data[, (cols_existentes) := lapply(.SD, as.character), .SDcols = cols_existentes]
  }


# Transformações utilizando mutate ----------------------------------------
  data <- 
    data |> 

    #Rename do código munic de resdiência. 
    #Útil para criar variável com o nome padrão de resd
    #Rename do código do munic svoiml. faciliar o nome padrão.
    rename(
      codmunresd = any_of("codmunres"),
      codmunsvoi = any_of("comunsvoim") ) |>
    
    mutate(
    #Letra e número da causa básica
    causa_letra = substr(causabas,1,1),
    causa_num = as.numeric(substr(causabas,2,3) ),
    #Local do incidente
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
        causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros",
      
        #Outras intenções são causa natural
        .default = "Natural") |> as_factor(), 
    

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
      causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo", 
      
      #Outros capítulos são mortes naturais
      .default = "Natural") |> as_factor(), 

      # # #Variável de intencionalidades para facilitar contagens.
      intencao_homic = case_when(intencao %in% c("Homicídio","h_legal") ~ "Homicídio",
                                 .default = intencao) |> as_factor(),

      #Consertando dtobito
      #dtobito = as.numeric(as.character(dtobito)),

      dtobito = case_when(
        dtobito == "1996" ~ "1011996",dtobito == "1997" ~ "1011997",dtobito == "1998" ~ "1011998",dtobito == "1999"  ~ "1011999",
        dtobito == "2000" ~ "1012000",dtobito == "2002" ~ "1012002",dtobito == "11996" ~ "1011996",dtobito == "11997" ~ "1011997",
        dtobito == "11998" ~ "1011998",dtobito == "11999" ~ "1011999",dtobito == "21996" ~ "1021996",dtobito == "21997" ~ "1021997",
        dtobito == "21998" ~ "1021998",dtobito == "31996" ~ "1031996",dtobito == "31997" ~ "1031997",dtobito == "31998" ~ "1031998",
        dtobito == "31999" ~ "1031999",dtobito == "41996" ~ "1041996",dtobito == "41997" ~ "1041997",dtobito == "41998" ~ "1041998",
        dtobito == "41999" ~ "1041999",dtobito == "51996" ~ "1051996",dtobito == "51997" ~ "1051997",dtobito == "51998" ~ "1051998",
        dtobito == "51999" ~ "1061999",dtobito == "61996" ~ "1061996",dtobito == "61997" ~ "1061997",dtobito == "61998" ~ "1061998",
        dtobito == "61999" ~ "1061999",dtobito == "71996" ~ "1071996",dtobito == "71997" ~ "1071997",dtobito == "71998" ~ "1071998",
        dtobito == "71999" ~ "1071999",dtobito == "81996" ~ "1081996",dtobito == "81997" ~ "1081997",dtobito == "81998" ~ "1081998",
        dtobito == "81999" ~ "1081999",dtobito == "91996" ~ "1091996",dtobito == "91997" ~ "1091997",dtobito == "91998" ~ "1091998",
        dtobito == "91999" ~ "1091999",dtobito == "101996" ~ "1101996",dtobito == "101997" ~ "1101997",dtobito == "101998" ~ "1101998",
        dtobito == "101999" ~ "1101999",dtobito == "111996" ~ "1111996",dtobito == "111997" ~ "1111997",dtobito == "111998" ~ "1111998",
        dtobito == "111999" ~ "1111999",dtobito == "121996" ~ "1121996",dtobito == "121997" ~ "1121997",dtobito == "121998" ~ "1121998",
        dtobito == "121999" ~ "1121999",dtobito == "21999" ~ "1021999",dtobito == "22000" ~ "1022000", .default = dtobito),

     #Transformando as data para a classe date  
      across(.cols = starts_with("dt"), 
             .fns = ~ lubridate::dmy(
               iconv(as.character(.), from = "latin1", to = "UTF-8") ) ),

     #Criando variáveis relacionadas a data.
      ano = lubridate::year(dtobito) |> as_factor(),
      mes = lubridate::month(dtobito,label = TRUE) |> as_factor(),
      dia = lubridate::wday(dtobito,label = TRUE) |> as_factor(), 
       
      #Código dos municípios de ocorrência e residência com seis dígitos
      #No microdado do SIM. A partir de 2006 o código do município aparece com 6 dígitos. 
      #Vou deixar todos os municípios em todos os anos com 6 dígitos.
      across(.cols = any_of( 
                      c("codmunnatu", #Código município de naturalidade do falecido. 
                        "codmunresd", #Código município de residência
                        "codmunocor", #Código município de ocorrência. 
                        "codmuncart", #Código do município do cartório
                        "codmunsvoi") ), #Código do município do SVO ou do IML
             .fns = ~ substr(., start = 1, stop = 6) ), 
      
      #Pegar o código da uf de ocorrência, uf de residência e uf de naturalidade
      across(.cols =  any_of( c("codmunocor", "codmunresd", 
                               "codmunnatu", "codmuncart", 
                               "codmunsvoi") ),
             .fns = ~ as.numeric( substr(.,1,2) ), #Dois primeiros dígitos são o código da UF
             #Extração do nome a partir do 7º e até 10º dígito do nome das variáveis de origem. (codmunxxx)
             .names = "cod_uf_{str_sub(.col, start = 7, end = 10)}"),
      
      #Nome da UF de ocorrência e UF de residência.
      across(.cols = any_of( 
                      c("cod_uf_ocor", "cod_uf_resd", 
                       "cod_uf_natu", "cod_uf_cart", 
                       "cod_uf_svoi") ),
             .fns = ~  recode(.,
                              '11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", 
                              '14'= "Roraima", '15'= "Pará",'16'= "Amapá",'17'= "Tocantins", 
                              '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", 
                              '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
                              '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", 
                              '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
                              '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", 
                              '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
                              '52'= "Goiás", '53'= "Distrito Federal", '99'= "CNRAC", 
                              .default = "Cod Munic Erro",
                              .missing = "Missing") |> as_factor(), 
             .names = "def_uf_{str_sub(.col, start = 8, end = 11)}"),  
       
      #Nome da região de ocorrência e região de residência.
      across(.cols = any_of( 
                     c("def_uf_ocor", "def_uf_resd", 
                       "def_uf_natu", "def_uf_cart", 
                       "def_uf_svoi") ),
             
             .fns = ~ case_when(
               #Região Norte
               . %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
               #Região Nordeste
               . %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
               #Região Centro-Oeste
               . %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
               #Região Sudeste
               . %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
               #Região Sul
               . %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina" ) ~ "Sul",
               
               .default ="Missing") |> as_factor(), 
             
             .names = "def_reg_{str_sub(.col, start = 8, end = 11)}"),
      
      #Escolaridade em anos (esc)
      #Escolaridade da mãe em anos (escmae)
      across(.cols = any_of( c("esc", "escmae") ),
             .fns = ~ case_match(.x = ., 
            "1" ~ "Nenhuma", "2" ~ "1 a 3 anos", 
            "3" ~  "4 a 7 anos", "4" ~  "8 a 11 anos",
            "5" ~  "12 anos e mais", NA ~ "Missing", .default = "Ignorado",
            #Transformando em factor e atribuindo ordem.
            .ptype = factor(levels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos",
                            "8 a 11 anos", "12 anos e mais", "Missing", "Ignorado"), ordered = TRUE ) ),
      #Atribuição de nome na variável
      .names = "def_{str_sub(.col)}"),

       #Escolaridade 2010. Nível da última série concluída pelo falecido (esc2010)
       #Escolaridade  2010.  Nível  da  última  série  concluída  pela  mãe. (escmae2010)
       across(.cols = any_of( c("esc2010", "escmae2010") ), 
       .fns = ~ case_match(.x = .,
       "0" ~  "Sem escolaridade", "1" ~  "Fundamental I (1ª a 4ª série)",
       "2" ~ "Fundamental II (5ª a 8ª série)", "3" ~ "Médio (antigo 2º Grau)",
       "4" ~  "Superior incompleto", "5" ~  "Superior completo", 
       NA ~ "Missing", .default = "Ignorado",
       #Transformando em factor e atribuindo ordem.                          
       .ptype = factor(
       #Ordem dos levels
       levels = c("Sem escolaridade","Fundamental I (1ª a 4ª série)",
                  "Fundamental II (5ª a 8ª série)","Médio (antigo 2º Grau)",
                  "Superior incompleto", "Superior completo", "Missing", "Ignorado"), ordered = TRUE) ),
       #Atribuição de nome na variável
       .names = "def_{str_sub(.col)}"),
   
       #Escolaridade do falecido agregada (formulário a partir de 2010). ESCFALAGR1
       #Escolaridade  da  mãe  agregada  (formulário  a  partir  de  2010). escmaeagr1
       across(.cols = any_of( c("escfalagr1", "escmaeagr1") ), 
       .fns = ~ case_match(.x = .,
        "00" ~ "Sem escolaridade", "01" ~  "Fundamental I Incompleto",
        "02" ~ "Fundamental I Completo", "03" ~ "Fundamental II Incompleto",
        "04" ~ "Fundamental II Completo", "05" ~  "Ensino Médio Incompleto", 
        "06" ~ "Ensino Médio Completo", "07" ~ "Superior Incompleto",
        "08" ~ "Superior Completo", "09" ~ "Ignorado", 
        "10" ~ "Fundamental I Incompleto ou Inespecífico", 
        "11" ~ "Fundamental II Incompleto ou Inespecífico",
        "12" ~  "Ensino Médio Incompleto ou Inespecífico", 
        NA ~ "Missing", .default = "Ignorado",
        #Transformando em factor e atribuindo ordem. 
        .ptype = factor(
        #Ordem dos levels  
        levels = c("Sem escolaridade", "Fundamental I Incompleto",
                   "Fundamental I Completo",  "Fundamental II Incompleto",
                   "Fundamental II Completo", "Ensino Médio Incompleto", 
                   "Ensino Médio Completo", "Superior Incompleto",
                   "Superior Completo",  
                   "Fundamental I Incompleto ou Inespecífico", 
                   "Fundamental II Incompleto ou Inespecífico",
                   "Ensino Médio Incompleto ou Inespecífico", 
                   "Missing", "Ignorado"), ordered = TRUE) ),
       #Atribuição de nome na variável (def_nome_da_variável)
       .names = "def_{str_sub(.col)}"),

      #Local do incidente - Variável criada
      local_incd = case_match(
                           .x = local_incd,
                           0 ~ "Residencial", 1 ~ "Hab. Coletiva", 2 ~ "Área de administração pública*", 
                           3 ~ "Esportiva", 4 ~ "Rua/Estrada", 5 ~ "Comercial", 6 ~ "Industrial",  
                           7 ~ "Fazenda", 8 ~ "Outros", NA ~ "Missing", .default = "Ignorado") |> as_factor(),
      
      #Local de óbito de intervenção legal é rua/estrada
      local_incd = case_when(
        #Óbitos por causa natural não indicam local do incidente
        intencao == "Natural" ~ "Natural",
        #Em intervenção legal o terceiro dígito não é o local. Vou assumir rua\estrada.
        intencao == "h_legal" ~ "Rua/Estrada",
        #Local de óbito de acidente de transporte V01-V99. O terceiro dígito não é local do incidente.
        intencao == "Acidente" & instrumento == "Veículo" ~ "Rua/Estrada", 
        #Y06.- Negligência e abandono ou Y07.- Outras síndromes de maus tratos. Residência?
        TRUE ~ local_incd),
  #Local de óbito de Y06 e Y07 Negligência, Abandono e maus tratos. Olhei as idades e não parece ser abandono de criança.
  #Olhar o keep. Autoria conhecida pode ser residência. desconhecida rua\estrada
  #Instrumento missing. Ao excluir intencao outros não deve existir instrumento NA.
  #instrumento = replace_na(instrumento,"Desconhecido").
     
    #Fazer dicionário + função?

     #Sexo
    def_sexo = case_match(.x = sexo, "1" ~ "Homem", "2" ~ "Mulher",
                        NA ~ "Missing", .default = "Ignorado") |> as_factor(),
      
    #Raça\cor
    def_racacor = case_match(.x = racacor, "1" ~ "Branca", "2" ~ "Preta", "3" ~ "Amarela",
                       "4" ~ "Parda", "5" ~ "Indigena", NA ~ "Missing", .default = "Ignorado" ) |> as_factor(),
    #Estado Civil
    def_estciv = case_match(.x = estciv, "1" ~"Solteiro", 
                          "2" ~ "Casado", "3" ~ "Viúvo", "4" ~ "Divorciado", "5" ~ "União Estável", 
                          NA ~ "Missing", .default = "Ignorado" ) |> as_factor(),

    #Local de ocorrência do óbito
    def_local_ocor = case_match(.x = lococor, "1" ~ "Hospital", 
    "2" ~  "Outros Estabelecimentos de Saúde", "3" ~ "Domicílio",
    "4" ~ "Via Pública", "5" ~  "Outros", "6" ~ "Aldeia Indígena", 
    NA ~ "Missing", .default = "Ignorado") |> as_factor(),


  #1 - Tipo de óbito
  def_tipobito = case_match(.x = tipobito, 
                            "1" ~ "Fetal",
                            "2" ~ "Não Fetal",
                            NA ~ "Missing",
                            .default = "Ignorada") |> as_factor(),
   
   #32 - Tipo de Gravidez
   def_gravidez = case_match(.x = gravidez,
                              "1" ~ "Única",
                              "2" ~ "Dupla",
                              "3" ~ "Tripla",
                              NA ~ "Missing",
                              .default = "Ignorada") |> as_factor(),
  #33 - Tipo de Parto 
  def_parto = case_match(.x = parto,
                         "1" ~ "Vaginal",
                         "2" ~ "Cesáreo",
                         NA ~ "Missing", 
                         .default = "Ignorado") |> as_factor(),
    
  #34- Morte em relação ao Parto. Momento do óbito em relação ao parto 
  def_obitoparto = case_match(.x = obitoparto,
                              "1" ~ "Antes",
                              "2" ~ "Durante",
                              "3" ~ "Depois",
                              NA ~ "Missing",
                              .default = "Ignorado") |> as_factor(),
  
  #37 - A morte ocorreu. Situação gestacional ou pósgestacional em que ocorreu o óbito  
  def_tpmorteoco = 
  if ("tpmorteoco" %in% names(data) ) {
  case_match(.x = tpmorteoco,
                              "1" ~ "Na Gravidez",
                              "2" ~ "No Parto",
                              "3" ~ "No Abortamento",
                              "4" ~ "Até 42 dias após o término do parto",
                              "5" ~ "De 43 dias a 1 ano após o término da gestação",
                              "8" ~ "Não ocorreu nestes períodos",
                              NA ~ "Missing",
                              .default = "Ignorado") |> as_factor() 
    } else {
      factor("Missing")
    },

  #38- Recebeu assist. médica durante a doença que ocasionou a morte?
  #Se refere ao atendimento médico continuado que o paciente recebeu, ou não, 
  #durante a enfermidade que ocasionou o óbito  
  def_assistmed = 
  if ("assistmed" %in% names(data) ) {
  case_match(.x = assistmed,
                             "1" ~ "Sim",
                             "2" ~ "Não",
                             NA ~ "Missing",
                             .default = "Ignorado") |> as_factor() 
    } else {
      factor("Missing")
    },

  #39 - Necrópsia
  #Refere-se a execução ou não de necropsia para confirmação do diagnóstico  
  def_necropsia = 
  if ("necropsia" %in% names(data) ) {
  
  case_match(.x = necropsia,
                             "1" ~ "Sim",
                             "2" ~ "Não",
                             NA ~ "Missing",
                             .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #43 - Óbito atestado por Médico
  def_atestante = 
  if ("atestante" %in% names(data) ) {
  
  case_match(.x = atestante,
                             "1" ~ "Assistente",
                             "2" ~ "Substituto",
                             "3" ~ "IML",
                             "4" ~ "SVO",
                             "5" ~ "Outro", 
                             NA ~ "Missing",
                             .default = "Ignorado") |> as_factor()  
    } else {
      factor("Missing")
    },

  #48 - Tipo. Tipo de morte violenta ou circunstâncias em que se deu a morte não natural
  def_circobito = 
  if ("circobito" %in% names(data) ) {
  
  case_match(.x = circobito,
                       "1" ~ "Acidente",
                       "2" ~ "Suicídio",
                       "3" ~ "Homicídio",
                       "4" ~ "Outros",
                       NA ~ "Missing", 
                       .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #49 - Acidente do trabalho. Indica se o evento que desencadeou o óbito 
  #está relacionado ao processo de trabalho 
  def_acidtrab = 
  if ("acidtrab" %in% names(data) ) {
  
  case_match(.x = acidtrab,
                            "1" ~ "Sim",
                            "2" ~ "Não",
                            NA ~ "Missing",
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },
  
  #50 - Fonte da Informação. fonte de informação utilizada para o preenchimento dos campos 48 e 49 
  def_fonte = 
  if ("fonte" %in% names(data) ) {
  
  case_match(.x = fonte,
                         "1" ~ "Ocorrência Policial",
                         "2" ~ "Hospital",
                         "3" ~ "Família",
                         "4" ~ "Outra", 
                         NA ~ "Missing", 
                         .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Origem 
  def_origem = 
  if ("origem" %in% names(data) ) {
    
  case_match(.x = origem,
                          "1" ~ "Oracle",
                          "2" ~ "Banco estadual diponibilizado via FTP",
                          "3" ~ "Banco SEADE", 
                          NA ~ "Missing", 
                          .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Óbito na gravidez
  def_obitograv = 
  if ("obitograv" %in% names(data) ) {
  
  case_match(.x = obitograv,
                             "1" ~ "Sim",
                             "2" ~ "Não",
                             NA ~ "Missing",
                             .default = "Ignorado") |> as_factor()
    
  } else {
    factor("Missing")
  },


  #Óbito no puerpério
  def_obitopuerp = 
  if ("obitopuerp" %in% names(data) ) {
  
  case_match(.x = obitopuerp,
                              "1" ~ "Até 42 dias após o parto",
                              "2" ~ "De 43 dias a 1 ano após o parto",
                              "3" ~ "Não",
                              NA ~ "Missing", 
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Exame. Realização de exame 
  def_exame = 
  if ("exame" %in% names(data) ) {
  
  case_match(.x = exame,
                         "1" ~ "Sim",
                         "2" ~ "Não", 
                         NA ~ "Missing",
                         .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Cirurgia. Realização de Cirurgia
  def_cirurgia = 
  if ("cirurgia" %in% names(data) ) {
  
  case_match(.x = cirurgia,
                            "1" ~ "Sim",
                            "2" ~ "Não",
                            NA ~ "Missing",
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Fonte de informação.
  def_fonteinv = 
  if ("fonteinv" %in% names(data) ) {
  
  case_match(.x = fonteinv,
                            "1" ~ "Comitê de Morte Materna e/ou Infantil",
                            "2" ~ "Visita domiciliar / Entrevista família",
                            "3" ~ "Estabelecimento de Saúde / Prontuário", 
                            "4" ~ "Relacionado com outros bancos de dados",
                            "5" ~ "SVO", 
                            "6" ~ "IML", 
                            "7" ~ "Outra Fonte",
                            "8" ~ "Múltiplas Fontes",
                            NA ~ "Missing", 
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Status de DO Epidemiológica  
  def_stdoepidem = 
  if ("stdoepidem" %in% names(data) ) {
  
  case_match(.x = stdoepidem,
                              "1" ~ "Sim",
                              "0" ~ "Não",
                              NA ~  "Missing",
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Status de DO Nova 
  def_stdonova = 
  if ("stdoepidem" %in% names(data) ) {
  
  case_match(.x = stdonova,
                            "1" ~ "Sim",
                            "0" ~ "Não",
                            NA ~  "Missing",
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Momento da ocorrência do óbito.
  def_tpobitocor = 
  if ("tpobitocor" %in% names(data) ) {
    
  case_match(.x = tpobitocor,
                              "1" ~ "Durante a gestação",
                              "2" ~ "Durante o abortamento",
                              "3" ~ "Após o abortamento",
                              "4" ~ "No parto ou até 1 hora após o parto",
                              "5" ~ "No puerpério  - até 42 dias após o parto",
                              "6" ~ "Entre 43 dias e até 1 ano após o parto",
                              "7" ~ "A investigação não identificou o momento do óbito",
                              "8" ~ "Mais de um ano após o parto",
                              "9" ~  "óbito não ocorreu nas circunstancias anteriores",
                              NA ~  "Missing",
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Informa se a investigação permitiu o 
  #resgate de alguma causa de óbito não informado, ou a 
  #correção de alguma antes informada 
  def_tpresginfo = 
  if ("tpresginfo" %in% names(data) ) {
  
  
  case_match(.x = tpresginfo,
                              "1" ~ "Não acrescentou nem corrigiu informação",
                              "2" ~ "Sim, permitiu o resgate de novas informações",
                              "3" ~ "Sim, permitiu a correção de alguma das causas informadas originalmente",
                              NA ~  "Missing",
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Tipo de nível investigador 
  def_tpnivelinv = 
  if ("tpnivelinv" %in% names(data) ) {
  
  case_match(.x = tpnivelinv, 
                              "E" ~ "Estadual",
                              "R" ~ "Regional",
                              "M" ~ "Municipal",
                              NA ~ "Missing",
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Momento do óbito em relação ao parto após investigação  
  def_morteparto = 
  if ("morteparto" %in% names(data) ) {
  
  case_match(.x = morteparto,
                              "1" ~ "Antes",
                              "2" ~ "Durante",
                              "3" ~ "Após",
                              NA ~ "Missing",
                              .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },
    
  
  #Indica se houve correção ou  alteração da  causa do óbito após investigação 
  def_altcausa = 
  if ("altcausa" %in% names(data) ) {
  
  case_match(.x = altcausa,
                            "1" ~ "Sim",
                            "0" ~ "Não",
                            NA ~  "Missing",
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },
  
  #Óbito investigado 
  def_tppos = 
  if ("tppos" %in% names(data) ) {
  
  case_match(.x = tppos,
                         "1" ~ "Sim",
                         "0" ~ "Não",
                         NA ~  "Missing",
                         .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },

  #Semanas de gestação (formulário antigo) Faixas de semanas de gestação 
  def_gestacao = 
  if ("gestacao" %in% names(data) ) {
  
  case_match(.x = gestacao,
                            "1" ~ "Menos de 22 semanas",
                            "2" ~ "22 a 27 semanas",
                            "3" ~ "28 a 31 semanas",
                            "4" ~ "32 a 36 semanas",
                            "5" ~ "37 a 41 semanas",
                            "6" ~ "42 e + semanas",
                            NA ~  "Missing",
                            .default = "Ignorado") |> as_factor()
  } else {
    factor("Missing")
  },
     
  #Internações por Condições Sensíveis à Atenção Primária #ICSAP: Identificação por CID
  def_icsap = 
  if ("causabas" %in% names(data) ) {
  
  case_match(.x = causabas, 
       
      c("A370", "A378", "A379", "A360", "A361", "A363", "A369", "A33", "A34", "A35", "B06", "B052", "A950", "A959", "B161", "B162", "B169", "G000",
       "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A190", "A191", "A192", "A198", "A199",
       "A150", "A151", "A152", "A153", "A160", "A161", "A162", "A154", "A155", "A156", "A157", "A158", "A159", "A163", "A164", "A165",
       "A166", "A167", "A168", "A169", "A180", "A181", "A182", "A183", "A184", "A185", "A186", "A187", "A188", "I00", "I010", "I011",
       "I012", "I018", "I019", "I020", "I029", "A511", "A512", "A513", "A514", "A515", "A519", "A520", "A521", "A522", "A523",
       "A527", "A528", "A529", "A530", "A539", "B500", "B509", "B518", "B519", "B520", "B528", "B53", "B54", "B260", "B268", "B269",
       "B770", "B778", "B779") ~ "cidgrupo1",

       c("E86", "A00", "A010", "A020", "A021", "A029", "A039", "A040",
                           "A041", "A042", "A044", "A046", "A047", "A048", "A049",
                           "A050", "A054", "A058", "A059", "A060", "A064", "A068", "A069",
                           "A071", "A073", "A078", "A079", "A080", "A083", "A084", "A085", "A09") ~ "cidgrupo2",

        c("D500", "D501", "D508", "D509") ~ "cidgrupo3",

        c("E40", "E41", "E42", "E43", "E440", "E441", "E45", "E46", "E500", "E508", "E51", "E52",
                           "E539", "E54", "E55", "E56", "E57", "E58", "E59", "E60", "E61", "E62", "E638", "E64") ~ "cidgrupo4",

        c("H660", "H661", "H662", "H663", "H664", "H669", "J00",
                           "J010", "J018", "J019", "J029", "J030", "J038", "J039", "J060", "J068", "J069", "J31") ~ "cidgrupo5",

        c("J13", "J14", "J153", "J154", "J158", "J159", "J181") ~ "cidgrupo6",

        c("J45", "J46") ~ "cidgrupo7",

        c("J200", "J201", "J203", "J205", "J209", "J210", "J218", "J219", "J40", "J410", "J411",
                           "J418", "J42", "J43", "J47", "J44", "J450", "J451", "J458", "J459") ~ "cidgrupo8",

        c("I10", "I110", "I119") ~ "cidgrupo9",

        c("I200", "I201", "I208", "I209") ~ "cidgrupo10",

        c("I500", "I501", "I509", "J81") ~ "cidgrupo11",

        c("I63", "I64", "I65", "I66", "I67", "I69") ~  "cidgrupo12",

        c("E100", "E101", "E110", "E111", "E140", "E141", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109",
                           "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E121", "E125", "E126", "E127", "E128", "E129", "E130", "E131",
                           "E132", "E133", "E135", "E136", "E137", "E138", "E139", "E142", "E143", "E144", "E145", "E146", "E147", "E148", "E149") ~ "cidgrupo13",

        c("G400", "G401", "G402", "G403", "G404", "G405", "G406", "G407", "G408", "G409", "G41") ~ "cidgrupo14",

        c("N10", "N11", "N12", "N300", "N301", "N302", "N303", "N304", "N308", "N309", "N340", "N341", "N342", "N343", "N390") ~ "cidgrupo15",

        c("A446", "L01", "L020", "L021", "L022", "L023", "L024", "L028", "L029", "L030", "L031",
                           "L032", "L033", "L038", "L039", "L040", "L041", "L042", "L043", "L048", "L049", "L080", "L088", "L089") ~ "cidgrupo16",

        c("N700", "N701", "N709", "N710", "N719", "N72", "N730", "N732", "N733", "N734", "N735",
                           "N736", "N738", "N739", "N750", "N751", "N758", "N760", "N762", "N764", "N766", "N768") ~ "cidgrupo17",

        c("K25", "K26", "K27", "K28", "K920", "K921", "K922") ~ "cidgrupo18",

        c("O23", "A500", "A501", "A502", "A503", "A504", "A505", "A506", "A507", "A509", "P35") ~ "cidgrupo19", 
       
       #Cids que não são ICSAP
       .default = "Outros") |> as_factor() 
    
  } else {
    factor("Missing")
  },

#Naturalidade
#Código iniciando com 8 para indicar naturalidade Brasileira. Neste caso, os dois dígitos 
#seguintes paracem indicar a UF. Caso contrário, parece indicar estrangeiro. 
    def_natural =  
  if ("natural" %in% names(data) ) {
  
  case_when(
    
  #Començando com 8, então Brasil 
  str_starts(natural, "8") ~ "Brasil",
  #Se não for Brasileiro, então segue os códigos ou NA então missing.
  TRUE ~ case_match(natural,
                               "170" ~ "Abissinia",  
                               "171" ~ "Acores",
                               "172" ~ "Afar frances",  
                               "241" ~ "Afeganistao",
                               "093" ~  "Albania",   
                               "030" ~ "Alemanha",
                               "174" ~ "Alto volta",  
                               "094" ~ "Andorra",
                               "175" ~ "Angola", 
                               "334" ~ "Antartica francesa",
                               "337" ~ "Antartico argentino", 
                               "333" ~ "Antartico britanico, territorio",
                               "336" ~ "Antartico chileno",  
                               "338" ~ "Antartico noruegues",
                               "028" ~  "Antigua e. dep. barbuda",  
                               "029" ~ "Antilhas holandesas",
                               "339" ~ "Apatrida",  
                               "242" ~ "Arabia saudita",
                               "176" ~ "Argelia",  
                               "021" ~ "Argentina",
                               "347" ~ "Armenia",  
                               "289" ~ "Arquipelago de bismark",
                               "175" ~ "Angola",  
                               "285" ~ "Arquipelago manahiki",
                               "286" ~ "Arquipelago midway",  
                               "033" ~ "Aruba",
                               "175" ~ "Angola",  
                               "198" ~ "Ascensao e tristao da cunha,is",
                               "287" ~ "Ashmore e cartier",  
                               "288" ~ "Australia",
                               "095" ~  "Austria",  
                               "138" ~ "Azerbaijao",
                               "243" ~ "Bahrein",  
                               "342" ~ "Bangladesh",
                               "044" ~  "Barbados",  
                               "139" ~ "Bashkista",
                               "177" ~ "Bechuanalandia",  
                               "031" ~ "Belgica",
                               "046" ~  "Belize",  
                               "178" ~ "Benin",
                               "083" ~  "Bermudas",  
                               "246" ~ "Bhutan",
                               "244" ~ "Birmania",  
                               "022" ~  "Bolivia",  
                               "134" ~ "Bosnia herzegovina",
                               "179" ~ "Botsuana", 
                               "010" ~ "Brasil",
                               "245" ~ "Brunei",  
                               "096" ~ "Bulgaria",
                               "238" ~ "Burkina fasso",  
                               "180" ~ "Burundi",
                               "141" ~ "Buryat",  
                               "343" ~ "Cabo verde",  
                               "181" ~ "Camaroes",
                               "034" ~  "Canada",  
                               "142" ~ "Carelia",   
                               "247" ~ "Catar",
                               "143" ~ "Cazaquistao",   
                               "248" ~ "Ceilao",
                               "182" ~ "Ceuta e melilla",  
                               "183" ~ "Chade",
                               "144" ~ "Chechen ingusth",  
                               "023" ~ "Chile",
                               "042" ~  "China", 
                               "249" ~ "China (taiwan)",
                               "097" ~  "Chipre",  
                               "145" ~ "Chuvash",  
                               "275" ~ "Cingapura",
                               "026" ~  "Colombia", 
                               "040" ~ "Comunidade das bahamas",
                               "054" ~  "Comunidade dominicana",  
                               "185" ~ "Congo",
                               "043" ~  "Coreia", 
                               "186" ~ "Costa do marfim",
                               "051" ~  "Costa rica", 
                               "250" ~ "Coveite",
                               "130" ~ "Croacia", 
                               "052" ~ "Cuba",  
                               "053" ~ "Curacao",
                               "146" ~ "Dagesta", 
                               "187" ~ "Daome",
                               "340" ~ "Dependencia de ross", 
                               "098" ~ "Dinamarca",
                               "188" ~ "Djibuti",  
                               "099" ~ "Eire",
                               "251" ~ "Emirados arabes unidos", 
                               "027" ~ "Equador",
                               "100" ~ "Escocia",  
                               "136" ~ "Eslovaquia",
                               "132" ~ "Eslovenia",  
                               "035" ~ "Espanha",
                               "129" ~ "Estado da cidade do vaticano",
                               "057" ~  "Estados assoc. das antilhas",
                               "036" ~  "Estados unidos da america (eua)",
                               "147" ~ "Estonia",  
                               "190" ~ "Etiopia",
                               "252" ~ "Filipinas",  
                               "102" ~ "Finlandia", 
                               "037" ~ "Franca",
                               "192" ~ "Gambia",  
                               "193" ~ "Gana",  
                               "194" ~ "Gaza",
                               "148" ~ "Georgia", 
                               "103" ~ "Gibraltar",
                               "149" ~ "Gorno altai",  
                               "032" ~ "Gra-bretanha",
                               "059" ~ "Granada",  
                               "104" ~ "Grecia", 
                               "084" ~ "Groenlandia",
                               "292" ~ "Guam",  
                               "061" ~ "Guatemala",
                               "087" ~ "Guiana francesa", 
                               "195" ~ "Guine",
                               "344" ~ "Guine bissau",  
                               "196" ~ "Guine equatorial",
                               "105" ~ "Holanda",  
                               "064" ~ "Honduras",
                               "063" ~ "Honduras britanicas", 
                               "253" ~ "Hong-kong",
                               "106" ~ "Hungria", 
                               "254" ~ "Iemen",
                               "345" ~ "Iemen do sul", 
                               "197" ~ "Ifni",
                               "300" ~ "Ilha johnston e sand", 
                               "069" ~ "Ilha milhos",
                               "293" ~ "Ilhas baker", 
                               "107" ~ "Ilhas baleares",
                               "199" ~ "Ilhas canarias", 
                               "294" ~ "Ilhas cantao e enderburg",
                               "295" ~ "Ilhas carolinas",  
                               "297" ~ "Ilhas christmas",
                               "184" ~ "Ilhas comores", 
                               "290" ~ "Ilhas cook",
                               "108" ~ "Ilhas cosmoledo (lomores)",
                               "117" ~ "Ilhas de man",  
                               "109" ~ "Ilhas do canal",
                               "296" ~ "Ilhas do pacifico", 
                               "058" ~ "Ilhas falklands",
                               "101" ~ "Ilhas faroes",
                               "298" ~ "Ilhas gilbert",
                               "060" ~ "Ilhas guadalupe",  
                               "299" ~ "Ilhas howland e jarvis",
                               "301" ~ "Ilhas kingman reef",  
                               "305" ~ "Ilhas macdonal e heard",
                               "302" ~ "Ilhas macquaire", 
                               "067" ~ "Ilhas malvinas",
                               "303" ~ "Ilhas marianas", 
                               "304" ~ "Ilhas marshall",
                               "306" ~ "Ilhas niue",  
                               "307" ~ "Ilhas norfolk",
                               "315" ~ "Ilhas nova caledonia", 
                               "318" ~ "Ilhas novas hebridas",
                               "308" ~ "Ilhas palau",  
                               "320" ~ "Ilhas pascoa",
                               "321" ~ "Ilhas pitcairin", 
                               "309" ~ "Ilhas salomao",
                               "326" ~ "Ilhas santa cruz",  
                               "065" ~ "Ilhas serranas",
                               "310" ~ "Ilhas tokelau",  
                               "080" ~ "Ilhas turca",
                               "047" ~ "Ilhas turks e caicos", 
                               "082" ~ "Ilhas virgens americanas",
                               "081" ~ "Ilhas virgens britanicas",
                               "311" ~ "Ilhas wake",  
                               "332" ~ "Ilhas wallis e futuna",
                               "255" ~ "India",   
                               "256" ~ "Indonesia",
                               "110" ~ "Inglaterra", 
                               "257" ~ "Ira",
                               "258" ~ "Iraque", 
                               "112" ~ "Irlanda",
                               "111" ~ "Irlanda do norte",
                               "113" ~ "Islandia", 
                               "259" ~ "Israel",
                               "039" ~ "Italia", 
                               "114" ~ "Iugoslavia",
                               "066" ~ "Jamaica", 
                               "041" ~ "Japao",
                               "260" ~ "Jordania",
                               "150" ~ "Kabardino balkar",  
                               "312" ~ "Kalimatan",
                               "151" ~ "Kalmir", 
                               "346" ~ "Kara kalpak",
                               "152" ~ "Karachaevocherkess", 
                               "153" ~ "Khakass",
                               "261" ~ "Kmer/camboja", 
                               "154" ~ "Komi",
                               "262" ~ "Kuwait",  
                               "263" ~ "Laos",
                               "200" ~ "Lesoto",  
                               "155" ~ "Letonia",
                               "264" ~ "Libano",
                               "201" ~ "Liberia", 
                               "202" ~ "Libia",
                               "115" ~ "Liechtenstein",
                               "156" ~ "Lituania",  
                               "116" ~ "Luxemburgo",
                               "265" ~ "Macau",  
                               "205" ~ "Madagascar",
                               "203" ~ "Madeira",  
                               "266" ~ "Malasia",
                               "204" ~ "Malawi",
                               "267" ~ "Maldivas",  
                               "206" ~ "Mali",
                               "157" ~ "Mari",   
                               "207" ~ "Marrocos",
                               "068" ~ "Martinica",
                               "268" ~ "Mascate", 
                               "208" ~ "Mauricio",
                               "209" ~ "Mauritania",  
                               "085" ~ "Mexico",
                               "284" ~ "Mianma", 
                               "210" ~ "Mocambique",
                               "158" ~ "Moldavia", 
                               "118" ~ "Monaco",  
                               "269" ~ "Mongolia",
                               "070" ~ "Monte serrat",  
                               "137" ~ "Montenegro",
                               "240" ~ "Namibia",  
                               "314" ~ "Nauru",
                               "270" ~  "Nepal",  
                               "211" ~ "Nguane",
                               "071" ~ "Nicaragua",
                               "213" ~ "Nigeria",  
                               "119" ~ "Noruega",
                               "316" ~ "Nova guine",
                               "317" ~ "Nova zelandia",  
                               "271" ~ "Oman",
                               "159" ~  "Ossetia setentrional", 
                               "121" ~ "Pais de gales",
                               "122" ~ "Paises baixos", 
                               "272" ~ "Palestina",
                               "072" ~ "Panama", 
                               "073" ~ "Panama(zona do canal)",
                               "214" ~ "Papua nova guine", 
                               "273" ~ "Paquistao",
                               "024" ~ "Paraguai", 
                               "089" ~ "Peru",
                               "322" ~ "Polinesia francesa",  
                               "123" ~ "Polonia",
                               "074" ~ "Porto rico", 
                               "045" ~ "Portugal",
                               "215" ~ "Pracas norte africanas", 
                               "216" ~ "Protetor do sudoeste africano",
                               "217" ~ "Quenia", 
                               "160" ~ "Quirguistao",
                               "075" ~ "Quitasueno", 
                               "189" ~ "Republica arabe do egito",
                               "218" ~ "Republica centro africana",
                               "173" ~ "Republica da africa do sul", 
                               "140" ~ "Republica da bielorrussia",
                               "133" ~ "Republica da macedonia", 
                               "56"  ~ "Republica de el salvador",
                               "291" ~ "Republica de fiji",  
                               "120" ~ "Republica de malta",
                               "191" ~ "Republica do gabao",  
                               "062" ~ "Republica do haiti",
                               "212" ~ "Republica do niger",  
                               "055" ~ "Republica dominicana",
                               "088" ~ "Republica guiana", 
                               "135" ~ "Republica tcheca",
                               "020" ~ "Reservado",  
                               "048" ~ "Reservado",
                               "049" ~ "Reservado",  
                               "050" ~ "Reservado",
                               "219" ~ "Reuniao",  
                               "220" ~ "Rodesia (zimbabwe)",
                               "124" ~ "Romenia",  
                               "076" ~ "Roncador",
                               "221" ~ "Ruanda",  
                               "274" ~ "Ruiquiu,is",
                               "348" ~ "Russia",  
                               "222" ~ "Saara espanhol",
                               "323" ~ "Sabah",  
                               "324" ~ "Samoa americana",
                               "325" ~ "Samoa ocidental",  
                               "125" ~ "San marino",
                               "223" ~ "Santa helena",  
                               "077" ~ "Santa lucia",
                               "078" ~ "Sao cristovao", 
                               "224" ~ "Sao tome e principe",
                               "079" ~ "Sao vicente",  
                               "327" ~ "Sarawak",
                               "349" ~ "Senegal", 
                               "276" ~ "Sequin",
                               "226" ~ "Serra leoa",  
                               "131" ~ "Servia",
                               "225" ~ "Seychelles",
                               "277" ~ "Siria",  
                               "227" ~ "Somalia, republica",
                               "278" ~ "Sri-lanka",  
                               "086" ~ "St. pierre et miquelon",
                               "228" ~ "Suazilandia", 
                               "229" ~ "Sudao",
                               "126" ~ "Suecia",  
                               "038" ~ "Suica",
                               "090" ~ "Suriname", 
                               "127" ~ "Svalbard e Jan Maye",
                               "161" ~ "Tadjiquistao",  
                               "279" ~ "Tailandia",
                               "230" ~ "Tanganica",  
                               "350" ~ "Tanzania",
                               "162" ~ "Tartaria",  
                               "128" ~ "Tchecoslovaquia",
                               "335" ~ "Terr. antartico da australia",
                               "341" ~ "Terras austrais",  
                               "231" ~ "Territ. britanico do oceano indico",
                               "328" ~ "Territorio de cocos", 
                               "319" ~ "Territorio de papua",
                               "329" ~ "Timor",  
                               "233" ~ "Togo", 
                               "330" ~ "Tonga",
                               "232" ~ "Transkei",  
                               "280" ~ "Tregua, estado",
                               "091" ~ "Trinidad e tobago",  
                               "234" ~ "Tunisia",
                               "163" ~ "Turcomenistao",   
                               "281" ~ "Turquia",
                               "331" ~ "Tuvalu",  
                               "164" ~ "Tuvin",
                               "165" ~ "Ucrania",  
                               "166" ~ "Udmurt", 
                               "235" ~ "Uganda",
                               "167" ~ "Uniao sovietica",  
                               "025" ~ "Uruguai",
                               "168" ~ "Uzbequistao", 
                               "092" ~ "Venezuela",
                               "282" ~ "Vietna do norte", 
                               "283" ~ "Vietna do sul",
                               "169" ~ "Yakut",  
                               "236" ~ "Zaire",
                               "237" ~ "Zambia",  
                               "239" ~ "Zimbabwe", 
                                NA ~ "Missing",       
                               .default = natural) |> as_factor() ) } ) |>

# Ocupações ---------------------------------------------------------------
   
  ### Ocupações da mae ocupmae
  left_join(x = _, 
              y = select(ocupacao, !c(versao_cod_proc) ) |> rename(def_ocup_mae = def_ocup), 
              join_by("ocupmae" == "cod") ) |>
  #### Ocupações do falecido
  left_join(x = _, 
              y = select(ocupacao, !c(versao_cod_proc) ), 
              join_by("ocup" == "cod") ) |>
    #Cbos de 5 dígitos são cbos antigas. Estou procurando a tabela de correspondência.
    #Outras cbos apresentam valores incorretos.
    #Estou considerando cbos com contagens de dígitos inferior a 4 como erro de 
    #preenchimento
    mutate(
      #Cbos com preenchimento inadequado.
      def_ocup = case_when(
        #CBO com 5 dígitos parece ser cbo 1994
        #O iconv acontece por causa de 1999
        nchar(iconv(as.character(ocup), from = "latin1", to = "UTF-8", sub = "")) == 5 ~ "CBO 1994?",
        #O iconv acontece por causa de 1999
        #CBo com 4 dígitos ou menos
        nchar(iconv(as.character(ocup), from = "latin1", to = "UTF-8", sub = "")) <= 4 ~ "Erro Preenchimento",
        .default = def_ocup),
      
      #Cbos com preenchimento inadequado.
      def_ocup_mae = case_when(
        nchar(as.character(ocupmae)) == 5 ~ "CBO 1994?",
        
        #CBo com 4 dígitos ou menos
        nchar(as.character(ocupmae)) <= 4 ~ "Erro Preenchimento",
        
        .default =  def_ocup_mae ),
# Municípios --------------------------------------------------------------      
     ### Refazer essa seção de municípios. Está horrível.


#Correção de ids com código de regiões administrativas do Distrito Federal.
#Vou assumir que ids começando em 53 são do Distrito Federal
#Isso acontece no SINAN. Trouxe do SINAN para o SIM
  across(.cols =  any_of(
    
          c("codmunresd", "codmunocor", 
            "codmuncart", "codmunnatu", 
            "codmunsvoi") ), 
         
          #Caso id comece em 53, então substituir pelo cod do Distrito Federal 530010
          .fns = ~ case_when(str_sub(., 1, 2) == "53" ~ "530010",
                          .default = .) |> as_factor() ), 

#Nos primeiros anos da série. 
#Alguns códigos de municípios são na realidade o bairro.
#Nestes casos, estou substituindo o código do bairro pelo código da capital da UF
  across(.cols = any_of(
    
    c("codmunresd", "codmunocor", 
    "codmuncart", "codmunnatu", 
    "codmunsvoi") ),
    
    .fns = ~ case_when(
    #Rio de Janeiro
    str_sub(., 1, 3) == "334" ~ "330455",
    #São Paulo
    str_sub(., 1, 3) == "358" ~ "355030",
    .default = .) |> as_factor() ) )  |> 
    
    #Adicionando o nome dos municípios.
    #Fiz uma função para isso. join_munic
    
    #Município de residência
    join_munic("codmunresd", "def_munic_resd", munics) |>
    #Ócorrencia
    join_munic("codmunocor", "def_munic_ocor", munics) |>
    #Município do cartório
    join_munic("codmuncart", "def_munic_cart", munics) |>
    #Município de naturalidade
    join_munic("codmunnatu", "def_munic_natu", munics) |>
    #Município do svo\iml
    join_munic("codmunsvoi", "def_munic_svoi", munics) |>
    
    
#Exclusão de variáveis não utilizadas ------------------------------------
      select(!c(causa_letra,causa_num) )
   
# Código do estabelecimento -----------------------------------------------

  data <- tibble::as_tibble(data)
  data <- droplevels(data.table::as.data.table(data))
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data,
                                                    FUN = stringi::stri_unescape_unicode)))
  
}




#Função auxiliar: tenta fazer join, ou cria a coluna derivada como NA
join_munic <- function(df, col_base, novo_nome, munics) {
  if (col_base %in% names(df)) {
    df |>
      left_join(
        munics |> 
          select(code_muni, name_muni) |> 
          rename(!!novo_nome := name_muni),
        by = setNames("code_muni", col_base)
      )
  } else {
    df |> mutate(!!novo_nome := NA_character_)
  }
}






# Função utilizada para empilhar o SIH ------------------------------------

#Passei essa função para dentro da função de criação.

# importar_empilhar_dbc <- function(pasta_dbc,
#                          var_select = NULL, # variáveis que desejo manter
#                          excluir = c("tpassina", "numerodn", "estabdescr")) { #Variáveis que serão excluidas.
#   
#   #Trata - se de lista com os arquivos dbcs que estão na pasta dbc.
#   arquivos <- list.files(
#     path = pasta_dbc, #Pasta onde estão os dbcs
#     pattern = "\\.dbc$",
#     full.names = TRUE)
#   
#   
#   if (length(arquivos) == 0) {
#     stop("Nenhum arquivo .dbc encontrado na pasta.")
#   }
#   
#   message("Encontrados ", length(arquivos), " arquivos .dbc")
#   
#   #Dados é o arquivo com os dbcs importados e empilhados.
#   dados <- 
#     
#     data.table::rbindlist(
#       
#     future_lapply(arquivos,
#     
#   function(arquivo) {
#         message("Importando: ", arquivo)
#         
#         dados <- read.dbc::read.dbc(arquivo) |>
#           
#           #Transforma em data.table
#           data.table::setDT() |>
#           
#           janitor::clean_names()
#         
#         #Exclusão de variáveis zeradas
#         vars_excluir <- intersect(excluir, names(dados))
#         if (length(vars_excluir) > 0) {
#           dados[, (vars_excluir) := NULL]
#         }
#         
#         
#         #Seleção de variáveis de interesse.
#         if (!is.null(var_select)) {
#           vars_sel <- intersect(var_select, names(dados))
#           dados <- dados[, vars_sel, with = FALSE]
#         }
#         
#         return(dados)
#       }
#     ),
#     use.names = TRUE, fill = TRUE
#   )
#   
#   message("Importação e tratamento de variáveis concluído! \n As variáveis tpassina, numerodn, estabdescr estão vazias e foram excluídas.")
#   return(dados)
# }
