
# Função de tratamento do SIH ---------------------------------------------
tratar_sih <- function(data) {
  
  #Transformando variáveis categóricas em character.
  #Vou padronizar a classe da variável, permitindo empilhar.
  # #Variáveis que deseja converter para character
  vars_char <- c("UF_ZI","SEXO","RACA_COR", "INSTRU", "MARCA_UTI", "CAR_INT", "IDENT", "COBRANCA", "NATUREZA", "NAT_JUR",
                 "GESTAO", "IND_VDRL", "HOMONIMO", "CONTRACEP1", "CONTRACEP2", "GESTRISCO", "SEQ_AIH5", "VINCPREV", "GESTOR_COD",
                 "COMPLEX", "FINANC", "FAEC_TP", "REGT", "ETNIA", "MARCA_UCI", "ESPEC", "NACIONAL")

  #Transformando variáveis de interesse para character.
  cols_existentes <- intersect(vars_char, names(data))
  if (length(cols_existentes) > 0) {
    data[, (cols_existentes) := lapply(.SD, as.character), .SDcols = cols_existentes]
  }

  # Tipo de diagnóstico secundário 
  tpdisec_vars <- paste0("TPDISEC", 1:9)
  
  # Verifica quais colunas existem na base
  cols_existentes <- intersect(tpdisec_vars, names(data))
  
  # Executa somente se houver colunas válidas
  if (length(cols_existentes) > 0) {
    data[, (cols_existentes) := lapply(.SD, function(x) {
      as_factor(fcase(
        x == "1", "Pré-existente",
        x == "2", "Adquirido",
        default = as.character(x)
      ))
    }), .SDcols = cols_existentes]
  }

  #Idade e data de internação.
  data[, `:=`(
    #Regras de IDADE
    IDADE = fifelse(COD_IDADE %in% c(2, 3), 0,
                    fifelse(COD_IDADE == 5, IDADE + 100, IDADE) ),
    #Delete do código de IDADE.
    COD_IDADE = NULL,

    # Datas principais
    NASC = ymd(NASC),
    DT_INTER = ymd(DT_INTER),
    DT_SAIDA = ymd(DT_SAIDA),
    ANO_MES_CMPT = as.Date(paste(ANO_CMPT, MES_CMPT, "01", sep = "-") ) ) ]

  ######################################################################################################################
  #Segunda parte da alteração das variáveis.
  data[, `:=`(

    #Variáveis derivadas da internação
    ANO_INTER = as.factor(lubridate::year(DT_INTER)),
    MES_INTER = as.factor(lubridate::month(DT_INTER)),
    DWK_INTER = lubridate::wday(DT_INTER, label = TRUE),
    ANO_MES_INTER = zoo::as.yearmon(DT_INTER, "%Y%b"),

    #Variáveis derivadas da saída
    ANO_SAIDA = as.factor(lubridate::year(DT_SAIDA)),
    MES_SAIDA = as.factor(lubridate::month(DT_SAIDA)),
    DWK_SAIDA = lubridate::wday(DT_SAIDA, label = TRUE),
    DTM_SAIDA = zoo::as.yearmon(DT_SAIDA, "%Y%b"),

    #Correções nos códigos do DF. Existem códigos das regiões administrativas. Conserta para código do DF.
    MUNIC_MOV = fifelse(startsWith(as.character(MUNIC_MOV), "53"), "530010", as.character(MUNIC_MOV)),
    MUNIC_RES = fifelse(startsWith(as.character(MUNIC_RES), "53"), "530010", as.character(MUNIC_RES)),


    #Correção do código de especialização do leito. Na base sih, cod_espec contém zero a esquerda.
    #Vou retirar o zero a esquerda.
    # COD_ESPEC = as.integer( as.character( COD_ESPEC ) ),

    # #Correção do código de procedimentos solicitados e realizados.
    # #Os microdados contém zero a esquerda. Estou retirando o zero a esquerda. Necessário para realizar o join com tabela de procedimentos.
    PROC_SOLIC = as.integer( as.character( PROC_SOLIC ) ),
    PROC_REA  = as.integer( as.character( PROC_REA ) ) ) ]

  #Idade
  if ("SEXO" %in% names(data)) {
    data[,  def_SEXO := as_factor( fcase(
      SEXO == "1", "Homem",
      SEXO %in% c("2", "3"), "Mulher",
      SEXO %in% c("0","9"), "Label não definido", default = as.character(SEXO) ) ) ]
    
  }
  

  #Raça\COR
  if("RACA_COR" %in% names(data)) {
   #Raça\cor
   data[, def_RACA_COR := as_factor( fcase(
      RACA_COR == "01", "Branca",
      RACA_COR == "02", "Preta",
      RACA_COR == "03", "Parda",
      RACA_COR == "04", "Amarela",
      RACA_COR == "05", "Indígena",
      RACA_COR %in% c("0","99"), "Label não definido", default = as.character(RACA_COR) ) ) ]
  }
    
    #EscolarIDADE
    if("ESC" %in% names(data) ) {
    data[, def_ESC := as_factor( fcase(
      INSTRU == "1", "Analfabeto",
      INSTRU == "2", "1º Grau",
      INSTRU == "3", "2º Grau",
      INSTRU == "4", "3º Grau",
      INSTRU %in% c(0, 9),  "Label não definido", default = as.character(INSTRU) ) ) ] 

    } 
     
  #Houve morte  
  if("MORTE" %in% names(data) ) {
  data[,
     def_MORTE := as_factor( fcase(
     MORTE == 0, "Não",
     MORTE == 1, "Sim", default = as.character(MORTE) ) ) ]
  }
  
  
  #Marca UTI. Tipo de UCI utilizada pelo paciente.  
  if("MARCA_UTI" %in% names(data) ) {
  data[,  
  def_MARCA_UTI := as_factor( fcase(
    MARCA_UTI == "00", "Não utilizou UTI",
    MARCA_UTI == "01", "Utilizou mais de um tipo de UTI",
    MARCA_UTI == "51", "UTI adulto - tipo II COVID 19",
    MARCA_UTI == "52", "UTI pediátrica - tipo II COVID 19",
    MARCA_UTI == "74", "UTI adulto - tipo I",
    MARCA_UTI == "75", "UTI adulto - tipo II",
    MARCA_UTI == "76", "UTI adulto - tipo III",
    MARCA_UTI == "77", "UTI infantil - tipo I",
    MARCA_UTI == "78", "UTI infantil - tipo II",
    MARCA_UTI == "79", "UTI infantil - tipo III",
    MARCA_UTI == "80", "UTI neonatal - tipo I",
    MARCA_UTI == "81", "UTI neonatal - tipo II",
    MARCA_UTI == "82", "UTI neonatal - tipo III",
    MARCA_UTI == "83", "UTI de queimados",
    MARCA_UTI == "85", "UTI coronariana tipo II - UCO tipo II",
    MARCA_UTI == "86", "UTI coronariana tipo III - UCO tipo III",
    MARCA_UTI == "99", "UTI Doador", default = as.character(MARCA_UTI) ) ) ]

  }
  
  
  #Caráter da internação  
  if("CAR_INT" %in% names(data) ) {
  data[,  
  def_CAR_INT := as_factor( fcase(
    CAR_INT == "01", "Eletivo",
    CAR_INT == "02", "Urgência",
    CAR_INT == "03", "Acidente no local trabalho ou a serv da empresa",
    CAR_INT == "04", "Acidente no trajeto para o trabalho",
    CAR_INT == "05", "Outros tipo de acidente de trânsito",
    CAR_INT == "06", "Out tp lesões e envenen por agent quím físicos", default = as.character(CAR_INT) ) ) ]
  }
  
  
  #Identificação do tipo da AIH  
  if("IDENT" %in% names(data) ) {
  data[,  
  def_IDENT := as_factor( fcase(IDENT == "1" , "Principal", 
                                IDENT == "3" , "Continuação", 
                                IDENT == "5" , "Longa permanência", 
                                default = as.character(IDENT) ) ) ]
  }
  
  
  # #Motivo de Saída/Permanência 
  if("COBRANCA" %in% names(data) ){
  data[,  
  def_COBRANCA := as_factor( fcase(
    #Em caso de dúvida sobre os códigos, olhar o manual da Paloma.
    #Alguns códigos foram incluidos\retirados ao longo do tempo.
    COBRANCA == "11" , "Alta curado",
    COBRANCA == "12" , "Alta melhorado",
    COBRANCA == "14" , "Alta a pedido",
    COBRANCA == "15" , "Alta com previsão de retorno p/acomp do paciente",
    COBRANCA == "16" , "Alta por evasão",
    COBRANCA == "18" , "Alta por outros motivos",
    COBRANCA == "19" , "Alta de paciente agudo em psiquiatria",
    COBRANCA == "21" , "Permanência por características próprias da doença",
    COBRANCA == "22" , "Permanência por intercorrência",
    COBRANCA == "23" , "Permanência por impossibilidade sócio-familiar",
    COBRANCA == "24" , "Permanência proc doação órg, tec, cél-doador vivo",
    COBRANCA == "25" , "Permanência proc doação órg, tec, cél-doador morto",
    COBRANCA == "26" , "Permanência por mudança de procedimento",
    COBRANCA == "27" , "Permanência por reoperação",
    COBRANCA == "28" , "Permanência por outros motivos",
    COBRANCA == "29" , "Transferência para internação domiciliar",
    COBRANCA == "32" , "Transferência para internação domiciliar",
    COBRANCA == "31" , "Transferência para outro estabelecimento",
    COBRANCA == "41" , "Óbito com DO fornecida pelo médico assistente",
    COBRANCA == "42" , "Óbito com DO fornecida pelo IML",
    COBRANCA == "43" , "Óbito com DO fornecida pelo SVO",
    COBRANCA == "51" , "Encerramento administrativo",
    COBRANCA == "61" , "Alta da mãe/puérpera e do recém-nascido",
    COBRANCA == "17" , "Alta da mãe/puérpera e do recém-nascido",
    COBRANCA == "62" , "Alta da mãe/puérpera e permanência recém-nascido",
    COBRANCA == "13" , "Alta da mãe/puérpera e permanência recém-nascido",
    COBRANCA == "63" , "Alta da mãe/puérpera e óbito do recém-nascido",
    COBRANCA == "64" , "Alta da mãe/puérpera com óbito fetal",
    COBRANCA == "65" , "Óbito da gestante e do concepto",
    COBRANCA == "66" , "Óbito da mãe/puérpera e alta do recém-nascido",
    COBRANCA == "67" , "Óbito da mãe/puérpera e permanência recém-nascido", default = as.character(COBRANCA) ) ) ]
 }
  
  #Natureza jurídica do hospital (com conteúdo até maio/12). Era utilizada a classificação de Regime e Natureza.  
  if("NATUREZA" %in% names(data) ) {
  data[,
  def_NATUREZA := as_factor( fcase(
    NATUREZA %in% c("00", "99" ),  "Label não definido",
    NATUREZA == "10" , "Próprio",
    NATUREZA == "20" , "Contratado",
    NATUREZA == "22" , "Contratado optante SIMPLES",
    NATUREZA == "30" , "Federal",
    NATUREZA == "31" , "Federal Verba Própria",
    NATUREZA == "40" , "Estadual",
    NATUREZA == "41" , "Estadual Verba Própria",
    NATUREZA == "50" , "Municipal",
    NATUREZA == "60" , "Filantrópico",
    NATUREZA == "61" , "Filantrópico isento tributos e contr.sociais",
    NATUREZA == "63" , "Filantrópico isento IR e contr.s/lucro líquido",
    NATUREZA == "70" , "Universitário Ensino",
    NATUREZA == "80" , "Sindicato",
    NATUREZA == "90" , "Universitário Pesquisas",
    NATUREZA == "91" , "Univ. Pesquisas isento tributos e contr.sociais",
    NATUREZA == "93" , "Univ. Pesquisas isento IR e contr.s/lucro líquido",
    NATUREZA == "94" , "Universitário de ensino e pesquisa privado",
    NATUREZA == "92" , "Universitário de ensino e pesquisa privado", default = as.character(NATUREZA) ) ) ]
}
  
  
  #Natureza Jurídica. Conforme concla. 
  #Os códigos da concla sofrem atualização. É interessante ficar de olho.  
  if("NAT_JUR" %in% names(data) ) {
  data[,  
  def_NAT_JUR := as_factor( fcase(
    NAT_JUR == "1015" , "Órgão Público do Poder Executivo Federal",
    NAT_JUR == "1023" , "Órgão Público do Poder Exec Estadual ou Distr Fed",
    NAT_JUR == "1031" , "Órgão Público do Poder Executivo Municipal",
    NAT_JUR == "1040" , "Órgão Público do Poder Legislativo Federal",
    NAT_JUR == "1058" , "Órgão Público do Poder Legisl Estadual ou Dist Fed",
    NAT_JUR == "1066" , "Órgão Público do Poder Legislativo Municipal",
    NAT_JUR == "1074" , "Órgão Público do Poder Judiciário Federal",
    NAT_JUR == "1082" , "Órgão Público do Poder Judiciário Estadual",
    NAT_JUR == "1104" , "Autarquia Federal",
    NAT_JUR == "1112" , "Autarquia Estadual ou do Distrito Federal",
    NAT_JUR == "1120" , "Autarquia Municipal",
    NAT_JUR == "1139" , "Fundação Federal",
    NAT_JUR == "1147" , "Fundação Estadual ou do Distrito Federal",
    NAT_JUR == "1155" , "Fundação Municipal",
    NAT_JUR == "1163" , "Órgão Público Autônomo Federal",
    NAT_JUR == "1171" , "Órgão Público Autônomo Estadual ou Distr Federal",
    NAT_JUR == "1180" , "Órgão Público Autônomo Estadual ou Distr Federal",
    NAT_JUR == "1198" , "Comissão Polinacional",
    NAT_JUR == "1201" , "Fundo Público",
    NAT_JUR == "1210" , "Associação Pública",
    NAT_JUR == "2011" , "Empresa Pública",
    NAT_JUR == "2038" , "Sociedade de Economia Mista",
    NAT_JUR == "2046" , "Sociedade Anônima Aberta",
    NAT_JUR == "2054" , "Sociedade Anônima Fechada",
    NAT_JUR == "2062" , "Sociedade Empresária Limitada",
    NAT_JUR == "2070" , "Sociedade Empresária em Nome Coletivo",
    NAT_JUR == "2089" , "Sociedade Empresária em Comandita Simples",
    NAT_JUR == "2097" , "Sociedade Empresária em Comandita por Ações",
    NAT_JUR == "2127" , "Sociedade em Conta de Participação",
    NAT_JUR == "2135" , "Empresário (Individual)",
    NAT_JUR == "2143" , "Cooperativa",
    NAT_JUR == "2151" , "Consórcio de Sociedades",
    NAT_JUR == "2160" , "Grupo de Sociedades",
    NAT_JUR == "2178" , "Estabelecimento no Brasil de Sociedade Estrangeira",
    NAT_JUR == "2194" , "Estab no Brasil Empr Binacional Argentina-Brasil",
    NAT_JUR == "2216" , "Empresa Domiciliada no Exterior",
    NAT_JUR == "2224" , "Clube/Fundo de Investimento",
    NAT_JUR == "2232" , "Sociedade Simples Pura",
    NAT_JUR == "2240" , "Sociedade Simples Limitada",
    NAT_JUR == "2259" , "Sociedade Simples em Nome Coletivo",
    NAT_JUR == "2267" , "Sociedade Simples em Comandita Simples",
    NAT_JUR == "2275" , "Empresa Binacional",
    NAT_JUR == "2283" , "Consórcio de Empregadores",
    NAT_JUR == "2291" , "Consórcio Simples",
    NAT_JUR == "2305" , "Empr Individ Responsab Limitada (Natur Empresária)",
    NAT_JUR == "2313" , "Empr Individ Responsab Limitada (Natureza Simples)",
    NAT_JUR == "3034" , "Serviço Notarial e Registral (Cartório)",
    NAT_JUR == "3069" , "Fundação Privada",
    NAT_JUR == "3077" , "Serviço Social Autônomo",
    NAT_JUR == "3085" , "Condomínio Edilício",
    NAT_JUR == "3107" , "Comissão de Conciliação Prévia",
    NAT_JUR == "3115" , "Entidade de Mediação e Arbitragem",
    NAT_JUR == "3123" , "Partido Político",
    NAT_JUR == "3131" , "Entidade Sindical",
    NAT_JUR == "3204" , "Estab no Brasil de Fundação ou Associação Estrang",
    NAT_JUR == "3212" , "Fundação ou Associação Domiciliada no Exterior",
    NAT_JUR == "3220" , "Organização Religiosa",
    NAT_JUR == "3239" , "Comunidade Indígena",
    NAT_JUR == "3247" , "Fundo Privado",
    NAT_JUR == "3999" , "Associação Privada",
    NAT_JUR == "4014" , "Empresa Individual Imobiliária",
    NAT_JUR == "4022" , "Segurado Especial",
    NAT_JUR == "4081" , "Contribuinte Individual",
    NAT_JUR == "4090" , "Candidato a Cargo Político Eletivo",
    NAT_JUR == "4111" , "Leiloeiro",
    NAT_JUR == "5010" , "Organização Internacional",
    NAT_JUR == "5029" , "Representação Diplomática Estrangeira",
    NAT_JUR == "5037" , "Outras Instituições Extraterritoriais",
    NAT_JUR == "0000" , "Label não definido", default = as.character(NAT_JUR) ) ) ]
  }
  
  
  #Indica o tipo de gestão do hospital.
  if("GESTAO" %in% names(data) ) {
  data[,
  def_GESTAO := as_factor( fcase(
    GESTAO == "0", "Estadual",
    GESTAO == "2", "Estadual plena",
    GESTAO == "1", "Municipal plena assist",
    GESTAO %in% c("3","9"),  "Label não definido", default = as.character(GESTAO) ) ) ]
  }
  
  #Indica a execução do exame VDRL
  if("IND_VDRL" %in% names(data) ) {
  data[,
  def_IND_VDRL := as_factor( fcase(
    IND_VDRL == "0", "Não",
    IND_VDRL == "1",  "Sim", default = as.character(IND_VDRL) ) ) ]
}

  #Indica se o paciente da AIH é homônimo de paciente de outra AIH.
  if("HOMONIMO" %in% names(data) ) {
 data[, 
  def_HOMONIMO := as_factor( fcase(
    HOMONIMO == "0", "Não",
    HOMONIMO == "2", "Sim", default = as.character(HOMONIMO) ) ) ]
}

  
  # #Tipo de contraceptivo utilizado 1
  if("CONTRACEP1" %in% names(data) ) {
  data[,
  def_CONTRACEP1 := as_factor(fcase(
    CONTRACEP1 == "01", "LAM",
    CONTRACEP1 == "02" , "Ogino Kaus",
    CONTRACEP1 == "03" , "Temperatura basal",
    CONTRACEP1 == "04" , "Billings",
    CONTRACEP1 == "05" , "Cinto térmico",
    CONTRACEP1 == "06" , "DIU",
    CONTRACEP1 == "07" , "Diafragma",
    CONTRACEP1 == "08" , "Preservativo",
    CONTRACEP1 == "09" , "Espermicida",
    CONTRACEP1 == "10" , "Hormônio oral",
    CONTRACEP1 == "11" , "Hormônio injetável",
    CONTRACEP1 == "12" , "Coito interrompido",
    CONTRACEP1 %in% c("00","13", "99"), "Label não definido", default = as.character(CONTRACEP1) ) ) ]
  }
   
  # #Tipo de contraceptivo utilizado 2
  if("CONTRACEP2" %in% names(data) ) {
  data[,
  def_CONTRACEP2 := as_factor(fcase(
    CONTRACEP2 == "01", "LAM",
    CONTRACEP2 == "02" , "Ogino Kaus",
    CONTRACEP2 == "03" , "Temperatura basal",
    CONTRACEP2 == "04" , "Billings",
    CONTRACEP2 == "05" , "Cinto térmico",
    CONTRACEP2 == "06" , "DIU",
    CONTRACEP2 == "07" , "Diafragma",
    CONTRACEP2 == "08" , "Preservativo",
    CONTRACEP2 == "09" , "Espermicida",
    CONTRACEP2 == "10" , "Hormônio oral",
    CONTRACEP2 == "11" , "Hormônio injetável",
    CONTRACEP2 == "12" , "Coito interrompido",
    CONTRACEP2 %in% c("00","13", "99"), "Label não definido", default = as.character(CONTRACEP2) ) ) ]
  }
   
  
  #Indica se é gestante de risco.
  if("GESTRISCO" %in% names(data) ) {
  data[,  
  def_GESTRISCO := as_factor( fcase(
    GESTRISCO == "0", "Não",
    GESTRISCO == "1", "Sim", default = as.character(GESTRISCO) ) ) ]
  } 
  
  
  # #Sequencial de longa permanência (AIH tipo 5) 
  if("SEQ_AIH5" %in% names(data) ) {
  data[,
  def_SEQ_AIH5 := as_factor(fcase(
    SEQ_AIH5 == "000", "Sequencial zerado",
    SEQ_AIH5 == "001", "Seq 1",
    SEQ_AIH5 == "002", "Seq 2",
    SEQ_AIH5 == "003", "Seq 3",
    SEQ_AIH5 %in% c("004","999"),"Outros", default = as.character(SEQ_AIH5) ) ) ]
}
   
  # #Vínculo com a Previdência em relação à atividade formal.
  if("VINCPREV" %in% names(data) ) {
  data[,
  def_VINCPREV := as_factor( fcase(
    VINCPREV == "1" , "Autônomo",
    VINCPREV == "2" , "Desempregado",
    VINCPREV == "3" , "Aposentado",
    VINCPREV == "4" , "Não segurado",
    VINCPREV == "5" , "Empregado",
    VINCPREV == "6" , "Empregador",
    VINCPREV %in% c("0","9"), "Label não definido", default = as.character(VINCPREV) ) ) ]
  } 
  
  
  #Motivo de autorização da AIH pelo gestor
  if("GESTOR_COD" %in% names(data) ) {
  data[,
  def_GESTOR_COD := as_factor(fcase(
    GESTOR_COD == "1" , "TEMPO DE PERMANENCIA",
    GESTOR_COD == "2" , "IDADE MENOR",
    GESTOR_COD == "3" , "IDADE MAIOR",
    GESTOR_COD == "4" , "TEMPO DE PERMANENCIA E IDADE",
    GESTOR_COD == "5" , "QUANTIDADE MAXIMA",
    GESTOR_COD == "7" , "PERM.MENOR",
    GESTOR_COD == "8" , "ID.MENOR",
    GESTOR_COD == "9" , "ID.MENOR E PERM.MENOR",
    GESTOR_COD == "10" , "ID.MAIOR",
    GESTOR_COD == "11" , "ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "14" , "QTD",
    GESTOR_COD == "15" , "QTD E PERM.MENOR",
    GESTOR_COD == "16" , "QTD E ID.MENOR",
    GESTOR_COD == "17" , "QTD E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "18" , "QTD E ID.MAIOR",
    GESTOR_COD == "19" , "QTD E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "38" , "CBO",
    GESTOR_COD == "39" , "CBO E PERM.MENOR",
    GESTOR_COD == "40" , "CBO E ID.MENOR",
    GESTOR_COD == "41" , "CBO E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "42" , "CBO E ID.MAIOR",
    GESTOR_COD == "43" , "CBO E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "46" , "CBO E QTD",
    GESTOR_COD == "47" , "CBO E QTD E PERM.MENOR",
    GESTOR_COD == "48" , "CBO E QTD E ID.MENOR",
    GESTOR_COD == "49" , "CBO E QTD E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "50" , "CBO E QTD E ID.MAIOR",
    GESTOR_COD == "51" , "CBO E QTD E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "70" , "TELEFONE",
    GESTOR_COD == "71" , "TELEFONE E PERM.MENOR",
    GESTOR_COD == "72" , "TELEFONE E ID.MENOR",
    GESTOR_COD == "73" , "TELEFONE E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "74" , "TELEFONE E ID.MAIOR",
    GESTOR_COD == "75" , "TELEFONE E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "78" , "TELEFONE E QTD",
    GESTOR_COD == "79" , "TELEFONE E QTD E PERM.MENOR",
    GESTOR_COD == "80" , "TELEFONE E QTD E ID.MENOR",
    GESTOR_COD == "81" , "TELEFONE E QTD E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "82" , "TELEFONE E QTD E ID.MAIOR",
    GESTOR_COD == "83" , "TELEFONE E QTD E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "102" , "TELEFONE E CBO",
    GESTOR_COD == "103" , "TELEFONE E CBO E PERM.MENOR",
    GESTOR_COD == "104" , "TELEFONE E CBO E ID.MENOR",
    GESTOR_COD == "105" , "TELEFONE E CBO E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "106" , "TELEFONE E CBO E ID.MAIOR",
    GESTOR_COD == "107" , "TELEFONE E CBO E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "110" , "TELEFONE E CBO E QTD",
    GESTOR_COD == "111" , "TELEFONE E CBO E QTD E PERM.MENOR",
    GESTOR_COD == "112" , "TELEFONE E CBO E QTD E ID.MENOR",
    GESTOR_COD == "113" , "TELEFONE E CBO E QTD E ID.MENOR E PERM.MENOR",
    GESTOR_COD == "114" , "TELEFONE E CBO E QTD E ID.MAIOR",
    GESTOR_COD == "115" , "TELEFONE E CBO E QTD E ID.MAIOR E PERM.MENOR",
    GESTOR_COD == "134" , "CNS",
    GESTOR_COD == "136" , "CNS E ID. MENOR",
    GESTOR_COD == "137" , "CNS E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "138" , "CNS E ID. MAIOR",
    GESTOR_COD == "139" , "CNS E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "142" , "CNS E QTD",
    GESTOR_COD == "143" , "CNS E QTD E PERM. MENOR",
    GESTOR_COD == "144" , "CNS E QTD E ID. MENOR",
    GESTOR_COD == "145" , "CNS E QTD E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "146" , "CNS E QTD E ID. MAIOR",
    GESTOR_COD == "147" , "CNS E QTD E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "166" , "CNS E CBO",
    GESTOR_COD == "167" , "CNS E CBO E PERM. MENOR",
    GESTOR_COD == "168" , "CNS E CBO E ID. MENOR",
    GESTOR_COD == "169" , "CNS E CBO E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "170" , "CNS E CBO E ID. MAIOR",
    GESTOR_COD == "171" , "CNS E CBO E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "174" , "CNS E CBO E QTD",
    GESTOR_COD == "175" , "CNS E CBO E QTD E PERM. MENOR",
    GESTOR_COD == "176" , "CNS E CBO E QTD E ID. MENOR",
    GESTOR_COD == "177" , "CNS E CBO E QTD E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "178" , "CNS E CBO E QTD E ID. MAIOR",
    GESTOR_COD == "179" , "CNS E CBO E QTD E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "198" , "CNS E TELEFONE",
    GESTOR_COD == "199" , "CNS E TELEFONE E PERM. MENOR",
    GESTOR_COD == "200" , "CNS E TELEFONE E ID. MENOR",
    GESTOR_COD == "201" , "CNS E TELEFONE E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "202" , "CNS E TELEFONE E ID. MAIOR",
    GESTOR_COD == "203" , "CNS E TELEFONE E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "206" , "CNS E TELEFONE E QTD",
    GESTOR_COD == "207" , "CNS E TELEFONE E QTD E PERM. MENOR",
    GESTOR_COD == "208" , "CNS E TELEFONE E QTD E ID. MENOR",
    GESTOR_COD == "209" , "CNS E TELEFONE E QTD E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "210" , "CNS E TELEFONE E QTD E ID. MAIOR",
    GESTOR_COD == "211" , "CNS E TELEFONE E QTD E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "230" , "CNS E TELEFONE E CBO",
    GESTOR_COD == "231" , "CNS E TELEFONE E CBO E PERM. MENOR",
    GESTOR_COD == "232" , "CNS E TELEFONE E CBO E ID. MENOR",
    GESTOR_COD == "233" , "CNS E TELEFONE E CBO E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "234" , "CNS E TELEFONE E CBO E ID. MAIOR",
    GESTOR_COD == "235" , "CNS E TELEFONE E CBO E ID. MAIOR E PERM. MENOR",
    GESTOR_COD == "238" , "CNS E TELEFONE E CBO E QTD",
    GESTOR_COD == "239" , "CNS E TELEFONE E CBO E QTD E PERM. MENOR",
    GESTOR_COD == "240" , "CNS E TELEFONE E CBO E QTD E ID. MENOR",
    GESTOR_COD == "241" , "CNS E TELEFONE E CBO E QTD E ID. MENOR E PERM. MENOR",
    GESTOR_COD == "242" , "CNS E TELEFONE E CBO E QTD E ID. MAIOR",
    GESTOR_COD == "243" , "CNS E TELEFONE E CBO E QTD E ID. MAIOR E PERM. MENOR", default = as.character(GESTOR_COD) ) ) ]
  }
  
  
  # #Complexidade
  if("COMPLEX" %in% names(data) ) {
  data[,
  def_COMPLEX := as_factor( fcase(
    COMPLEX == "01" , "Atenção Básica",
    COMPLEX == "02" , "Média complexidade",
    COMPLEX == "03" , "Alta complexidade",
    COMPLEX %in% c("00","99"), "Label não definido" , default = as.character(COMPLEX) ) ) ]
   
  }
  
  
  #Tipo de financiamento
  if("FINANC" %in% names(data) ) {
  data[,
  def_FINANC := as_factor( fcase(
    FINANC == "01" , "Atenção Básica (PAB)",
    FINANC == "02" , "Assistência Farmacêutica",
    FINANC == "04" , "Fundo de Ações Estratégicas e Compensações FAEC",
    FINANC == "05" , "Incentivo - MAC",
    FINANC == "06" , "Média e Alta Complexidade (MAC)",
    FINANC == "07" , "Vigilância em Saúde",
    FINANC %in% c("0", "99"), "Label não definido", default = as.character(FINANC) ) ) ]
  }
   
  #Subtipo de financiamento FAEC
  if("FAEC_TP" %in% names(data) ) {
  data[,    
  def_FAEC_TP := as_factor( fcase(
    FAEC_TP == "010000" , "Atenção Básica (PAB)",
    FAEC_TP == "020000" , "Assistência Farmacêutica",
    FAEC_TP == "040000" , "FAEC - Subtipo de financiamento ignorado",
    FAEC_TP == "040001" , "Coleta de material",
    FAEC_TP == "040002" , "Diagnóstico em laboratório clínico",
    FAEC_TP == "040003" , "Coleta/exame anátomo-patológico colo uterino",
    FAEC_TP == "040004" , "Diagnóstico em neurologia",
    FAEC_TP == "040005" , "Diagnóstico em otorrinolaringologia/fonoaudiologia",
    FAEC_TP == "040006" , "Diagnóstico em psicologia/psiquiatria",
    FAEC_TP == "040007" , "Consultas médicas/outros profissionais de nível superior",
    FAEC_TP == "040008" , "Atenção domiciliar",
    FAEC_TP == "040009" , "Atendimento/acompanhamento em reabilitação física, mental, visual, auditiva e múltiplas defic",
    FAEC_TP == "040010" , "Atendimento/acompanhamento psicossocial",
    FAEC_TP == "040011" , "Atendimento/acompanhamento em saúde do idoso",
    FAEC_TP == "040012" , "Atendimento/acompanhamento de queimados",
    FAEC_TP == "040013" , "Atendimento/acompanhamento de diagnóstico de doenças endocrinas/metabólicas e nutricionais",
    FAEC_TP == "040014" , "Tratamento de doenças do sistema nervoso central e periférico",
    FAEC_TP == "040015" , "Tratamento de doenças do aparelho da visão",
    FAEC_TP == "040016" , "Tratamento em oncologia",
    FAEC_TP == "040017" , "Nefrologia",
    FAEC_TP == "040018" , "Tratamentos odontológicos",
    FAEC_TP == "040019" , "Cirurgia do sistema nervoso central e periférico",
    FAEC_TP == "040020" , "Cirurgias de ouvido, nariz e garganta",
    FAEC_TP == "040021" , "Deformidade labio-palatal e crânio-facial",
    FAEC_TP == "040022" , "Cirurgia do aparelho da visão",
    FAEC_TP == "040023" , "Cirurgia do aparelho circulatório",
    FAEC_TP == "040024" , "Cirurgia do aparelho digestivo, orgãos anexos e parede abdominal(inclui pré e pós operatório)",
    FAEC_TP == "040025" , "Cirurgia do aparelho geniturinário",
    FAEC_TP == "040026" , "Tratamento de queimados",
    FAEC_TP == "040027" , "Cirurgia reparadora para lipodistrofia",
    FAEC_TP == "040028" , "Outras cirurgias plásticas/reparadoras",
    FAEC_TP == "040029" , "Cirurgia orofacial",
    FAEC_TP == "040030" , "Sequenciais",
    FAEC_TP == "040031" , "Cirurgias em nefrologia",
    FAEC_TP == "040032" , "Transplantes de orgãos, tecidos e células",
    FAEC_TP == "040033" , "Medicamentos para transplante",
    FAEC_TP == "040034" , "OPM auditivas",
    FAEC_TP == "040035" , "OPM em odontologia",
    FAEC_TP == "040036" , "OPM em queimados",
    FAEC_TP == "040037" , "OPM em nefrologia",
    FAEC_TP == "040038" , "OPM para transplantes",
    FAEC_TP == "040039" , "Incentivos ao pré-natal e nascimento",
    FAEC_TP == "040040" , "Incentivo ao registro cívil de nascimento",
    FAEC_TP == "040041" , "Central Nacional de Regulação de Alta Complexidade (CNRAC)",
    FAEC_TP == "040042" , "Reguladores de Atividade hormonal - Inibidores de prolactina",
    FAEC_TP == "040043" , "Política Nacional de Cirurgias Eletivas",
    FAEC_TP == "040044" , "Redesignação e Acompanhamento",
    FAEC_TP == "040045" , "Projeto Olhar Brasil",
    FAEC_TP == "040046" , "Mamografia para Rastreamento",
    FAEC_TP == "040047" , "Projeto Olhar Brasil - Consulta",
    FAEC_TP == "040048" , "Projeto Olhar Brasil - Óculos",
    FAEC_TP == "040049" , "Implementar Cirg. CV Pediátrica",
    FAEC_TP == "040050" , "Cirurgias Eletivas - Componente I",
    FAEC_TP == "040051" , "Cirurgias Eletivas - Componente II",
    FAEC_TP == "040052" , "Cirurgias Eletivas - Componente III",
    FAEC_TP == "040053" , "Prótese Mamária - Exames",
    FAEC_TP == "040054" , "Prótese Mamária - Cirurgia",
    FAEC_TP == "040055" , "Transplante - Histocompatibilidade",
    FAEC_TP == "040056" , "Triagem Neonatal",
    FAEC_TP == "040057" , "Controle de qualidade do exame citopatológico do colo de útero",
    FAEC_TP == "040058" , "Exames do Leite Materno",
    FAEC_TP == "040059" , "Atenção as Pessoas em Situação de Violência Sexual",
    FAEC_TP == "040060" , "Sangue e Hemoderivados",
    FAEC_TP == "040061" , "Mamografia para rastreamento em faixa etária recomendada",
    FAEC_TP == "040062" , "Doenças Raras",
    FAEC_TP == "040063" , "Cadeiras de Rodas",
    FAEC_TP == "040064" , "Sistema de Frequencia Modulada Pessoal-FM",
    FAEC_TP == "040065" , "Medicamentos em Urgência",
    FAEC_TP == "040066" , "Cirurgias Eletivas - Componente Único",
    FAEC_TP == "040067" , "Atenção Especializada em Saúde Auditiva",
    FAEC_TP == "040068" , "Terapias Especializadas em Angiologia",
    FAEC_TP == "040069" , "Tratamento de Doença Macular", #Fonte: SIGTAP
    FAEC_TP == "040070" , "OPME Não Relacionados ao Ato Cirúrgico", #Fonte: SIGTAP
    FAEC_TP == "040071" , "Diagnóstico/tratamento em oncologia", #Fonte: SIGTAP
    FAEC_TP == "040072" , "Diagnóstico de trombofilia em gestante", #Fonte: SIGTAP
    FAEC_TP == "040073" , "Reabilitação Pós-COVID-19", #Fonte: SIGTAP
    FAEC_TP == "040074" , "Telemedicina em Urgência	", #Fonte: SIGTAP
    FAEC_TP == "040075" , "Fisioterapia Cardiovascular", #Fonte: SIGTAP
    FAEC_TP == "040076" , "Hemodinâmica em atendimento de urgência",
    FAEC_TP == "040077" , "Exames Sorológicos e Imunológicos", #Fonte: SIGTAP
    FAEC_TP == "040078" , "QualiSUS Cardio", #Fonte: SIGTAP
    FAEC_TP == "040079" , "Pré-cirúrgico em Cirurgias Prioritárias", #Fonte: SIGTAP
    FAEC_TP == "040080" , "Redução das Filas de Procedimentos Eletivos",
    FAEC_TP == "040081" , "Síndrome Respiratória Aguda Grave - SRAG", #Fonte: SIGTAP
    FAEC_TP == "040082" , "Alta Complexidade em Cardiologia", #Fonte: SIGTAP
    FAEC_TP == "040083" , "Cirurgia da Face e do Sistema Estomatognático", #Fonte: SIGTAP
    FAEC_TP == "040084" , "Neurocirurgias", #Fonte: SIGTAP
    FAEC_TP == "040085" , "Exames coprológicos", #Fonte: SIGTAP
    FAEC_TP == "040086" , "Programa Mais Acesso a Especialistas (PMAE)", #Fonte: SIGTAP
    FAEC_TP == "021012" , "FAEC CNRAC (21012-cód ant à tab unif-vál p/2008-01)",
    FAEC_TP == "021014" , "FAEC Eletiv(21014-cód ant à tab unif-vál p/2008-01)",
    FAEC_TP == "050000" , "Incentivo - MAC",
    FAEC_TP == "060000" , "Média e Alta Complexidade (MAC)",
    FAEC_TP == "070000" , "Vigilância em Saúde",
    FAEC_TP == "080000" , "Gestão do SUS", default = as.character(FAEC_TP) ) ) ]
  }
  
  
  #Tabela de Regras Contratuais dos Sistemas de Informações do SUS
  if("REGCT" %in% names(data) ) {
  data[,  
    
  def_REGCT := as_factor(fcase(

    REGCT == "7100" , "TABELA DE NAO GERAÇÃO DE CREDITO POR PRODUCAO NA INTERNACAO E/OU AMBULATORIO",

    REGCT == "7101" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA MÉDIA COMPLEXIDADE AMBULATORIAL, exceto Fundo de Ações Estratégicas e Compensação (FAEC)",

    REGCT == "7102" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA MÉDIA COMPLEXIDADE HOSPITALAR, incluindo OPM e demais procedimentos especiais, exceto os financiados pelo FAEC",

    REGCT == "7103" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA ALTA COMPLEXIDADE AMBULATORIAL, exceto FAEC",

    REGCT == "7104" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA ALTA COMPLEXIDADE HOSPITALAR, incluindo OPM e demais procedimentos especiais, exceto os financiados pelo FAEC",

    REGCT == "7105" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO PARA OS PROCEDIMENTOS FINANCIADOS COM O FAEC",

    REGCT == "7106" , "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - incluindo FAEC",

    REGCT == "7107" , "Estabelecimento de saúde sem geração de crédito nas ações especializadas de odontologia, exceto FAEC: incentivo Centros de Especialidades Odontológicas (CEOs) I, II e III",

    REGCT == "7108" , "Estabelecimento de saúde sem geração de crédito, exceto FAEC: incentivo saúde do trabalhador",

    REGCT == "7109" , "Estabelecimento de saúde sem geração de crédito total: Ministério da Educação (MEC)",

    REGCT == "7110" , "Estabelecimento de saúde da estrutura do MS sem geração de crédito total.",

    REGCT == "7111" , "Estabelecimento de saúde sem geração de crédito Núcleo de Apoio à Saúde da Família (NASF), exceto FAEC",

    REGCT == "7112" , "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC  - EXCLUSIVO PARA REDE SARAH",

    REGCT == "7113" , "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC - OUTROS ESTABELECIMENTOS FEDERAIS",

    REGCT == "7114" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO TOTAL, INCLUSIVE FAEC - PRONTO ATENDIMENTO",

    REGCT == "7115" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - hospitais universitários/MEC",

    REGCT == "7116" , "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - Laboratório Regional de Prótese Dentária (LRPD)",
    #Origem:
    #https://www.gov.br/saude/pt-br/centrais-de-conteudo/publicacoes/estudos-e-notas-informativas/2025/nota-informativa-no-1-2025-cgspd-daet-saes-ms.pdf
    REGCT == "7117" , "Estabelecimento de saúde sem geração de crédito na média complexidade (exceto OPM e FAEC) – RCPD",

    REGCT == "0000" , "Sem regra contratual", default = as.character(REGT) ) ) ]
  }
  
  
  #ETNIA
  if("ETNIA" %in% names(data) ) {
  data[,  
  def_ETNIA := as_factor( fcase(
    ETNIA == "0001" , "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)", ETNIA == "0002" , "AIKANA (AIKANA, MAS SAKA,TUBARAO)",
    ETNIA == "0003" , "AJURU",  ETNIA == "0004" , "AKUNSU (AKUNT'SU)",
    ETNIA == "0005" , "AMANAYE",  ETNIA == "0006" , "AMONDAWA",
    ETNIA == "0007" , "ANAMBE", ETNIA == "0008" , "APARAI (APALAI)",
    ETNIA == "0009" , "APIAKA (APIACA)",  ETNIA == "0010" , "APINAYE (APINAJE/APINAIE/APINAGE)",
    ETNIA == "0011" , "APURINA (APORINA, IPURINA, IPURINA, IPURINAN)",  ETNIA == "0012" , "ARANA (ARACUAI DO VALE DO JEQUITINHONHA)",
    ETNIA == "0013" , "ARAPASO (ARAPACO)", ETNIA == "0014" , "ARARA DE RONDONIA (KARO, URUCU, URUKU)",
    ETNIA == "0015" , "ARARA DO ACRE (SHAWANAUA, AMAWAKA)", ETNIA == "0016" , "ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARI-PUANA)",
    ETNIA == "0017" , "ARARA DO PARA (UKARAGMA, UKARAMMA)", ETNIA == "0018" , "ARAWETE (ARAUETE)",
    ETNIA == "0019" , "ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)", ETNIA == "0020" , "ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)",
    ETNIA == "0021" , "ARIKOSE (ARICOBE)",ETNIA == "0022" , "ARUA",
    ETNIA == "0023" , "ARUAK (ARAWAK)", ETNIA == "0024" , "ASHANINKA (KAMPA)",
    ETNIA == "0025" , "ASURINI DO TOCANTINS (AKUAWA/AKWAWA)", ETNIA == "0026" , "ASURINI DO XINGU (AWAETE)",
    ETNIA == "0027" , "ATIKUM (ATICUM)",  ETNIA == "0028" , "AVA - CANOEIRO",
    ETNIA == "0029" , "AWETI (AUETI/AUETO)",  ETNIA == "0030" , "BAKAIRI (KURA, BACAIRI)",
    ETNIA == "0031" , "BANAWA YAFI (BANAWA, BANAWA-JAFI)",  ETNIA == "0032" , "BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)",
    ETNIA == "0033" , "BARA (WAIPINOMAKA)", ETNIA == "0034" , "BARASANA (HANERA)",
    ETNIA == "0035" , "BARE", ETNIA == "0036" , "BORORO (BOE)",
    ETNIA == "0037" , "BOTOCUDO (GEREN)", ETNIA == "0038" , "CANOE",
    ETNIA == "0039" , "CASSUPA",  ETNIA == "0040" , "CHAMACOCO",
    ETNIA == "0041" , "CHIQUITANO (XIQUITANO)", ETNIA == "0042" , "CIKIYANA (SIKIANA)",
    ETNIA == "0043" , "CINTA LARGA (MATETAMAE)",  ETNIA == "0044" , "COLUMBIARA (CORUMBIARA)",
    ETNIA == "0045" , "DENI", ETNIA == "0046" , "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)",
    ETNIA == "0047" , "DIAHUI (JAHOI, JAHUI, DIARROI)", ETNIA == "0048" , "ENAWENE-NAWE (SALUMA)",
    ETNIA == "0049" , "FULNI-O",  ETNIA == "0050" , "GALIBI (GALIBI DO OIAPOQUE, KARINHA)",
    ETNIA == "0051" , "GALIBI MARWORNO (GALIBI DO UACA, ARUA)", ETNIA == "0052" , "GAVIAO DE RONDONIA (DIGUT)",
    ETNIA == "0053" , "GAVIAO KRIKATEJE", ETNIA == "0054" , "GAVIAO PARKATEJE (PARKATEJE)",
    ETNIA == "0055" , "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)",
    ETNIA == "0056" , "GUAJA (AWA, AVA)", ETNIA == "0057" , "GUAJAJARA (TENETEHARA)",
    ETNIA == "0058" , "GUARANI KAIOWA (PAI TAVYTERA)",  ETNIA == "0059" , "GUARANI M'BYA",
    ETNIA == "0060" , "GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)",
    ETNIA == "0061" , "GUATO",  ETNIA == "0062" , "HIMARIMA (HIMERIMA)",
    ETNIA == "0063" , "INGARIKO (INGARICO, AKAWAIO, KAPON)", ETNIA == "0064" , "IRANXE (IRANTXE)",
    ETNIA == "0065" , "ISSE", ETNIA == "0066" , "JABOTI (JABUTI, KIPIU, YABYTI)",
    ETNIA == "0067" , "JAMAMADI (YAMAMADI, DJEOROMITXI)", ETNIA == "0068" , "JARAWARA",
    ETNIA == "0069" , "JIRIPANCO (JERIPANCO, GERIPANCO)", ETNIA == "0070" , "JUMA (YUMA)",
    ETNIA == "0071" , "JURUNA", ETNIA == "0072" , "JURUTI (YURITI)",
    ETNIA == "0073" , "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)", ETNIA == "0074" , "KADIWEU (CADUVEO, CADIUEU)",
    ETNIA == "0075" , "KAIABI (CAIABI, KAYABI)",  ETNIA == "0076" , "KAIMBE (CAIMBE)",
    ETNIA == "0077" , "KAINGANG (CAINGANGUE)",  ETNIA == "0078" , "KAIXANA (CAIXANA)",
    ETNIA == "0079" , "KALABASSA (CALABASSA, CALABACAS)", ETNIA == "0080" , "KALANCO",
    ETNIA == "0081" , "KALAPALO (CALAPALO)",  ETNIA == "0082" , "KAMAYURA (CAMAIURA, KAMAIURA)",
    ETNIA == "0083" , "KAMBA (CAMBA)",  ETNIA == "0084" , "KAMBEBA (CAMBEBA, OMAGUA)",
    ETNIA == "0085" , "KAMBIWA (CAMBIUA)", ETNIA == "0086" , "KAMBIWA PIPIPA (PIPIPA)",
    ETNIA == "0087" , "KAMPE",  ETNIA == "0088" , "KANAMANTI (KANAMATI, CANAMANTI)",
    ETNIA == "0089" , "KANAMARI (CANAMARI, KANAMARY, TUKUNA)",  ETNIA == "0090" , "KANELA APANIEKRA (CANELA)",
    ETNIA == "0091" , "KANELA RANKOKAMEKRA (CANELA)", ETNIA == "0092" , "KANINDE",
    ETNIA == "0093" , "KANOE (CANOE)",  ETNIA == "0094" , "KANTARURE (CANTARURE)",
    ETNIA == "0095" , "KAPINAWA (CAPINAUA)",  ETNIA == "0096" , "KARAJA (CARAJA)",
    ETNIA == "0097" , "KARAJA/JAVAE (JAVAE)", ETNIA == "0098" , "KARAJA/XAMBIOA (KARAJA DO NORTE)",
    ETNIA == "0099" , "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)",  ETNIA == "0100" , "KARAPOTO (CARAPOTO)",
    ETNIA == "0101" , "KARIPUNA (CARIPUNA)",  ETNIA == "0102" , "KARIPUNA DO AMAPA (CARIPUNA)",
    ETNIA == "0103" , "KARIRI (CARIRI)",  ETNIA == "0104" , "KARIRI-XOCO (CARIRI-CHOCO)",
    ETNIA == "0105" , "KARITIANA (CARITIANA)", ETNIA == "0106" , "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)",
    ETNIA == "0107" , "KATUENA (CATUENA, KATWENA)", ETNIA == "0108" , "KATUKINA (PEDA DJAPA)",
    ETNIA == "0109" , "KATUKINA DO ACRE", ETNIA == "0110" , "KAXARARI (CAXARARI)",
    ETNIA == "0111" , "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)",  ETNIA == "0112" , "KAXIXO",
    ETNIA == "0113" , "KAXUYANA (CAXUIANA)",  ETNIA == "0114" , "KAYAPO (CAIAPO)",
    ETNIA == "0115" , "KAYAPO KARARAO (KARARAO)", ETNIA == "0116" , "KAYAPO TXUKAHAMAE (TXUKAHAMAE)",
    ETNIA == "0117" , "KAYAPO XICRIM (XIKRIN)", ETNIA == "0118" , "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)",
    ETNIA == "0119" , "KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)",  ETNIA == "0120" , "KIRIRI",
    ETNIA == "0121" , "KOCAMA (COCAMA, KOKAMA)",  ETNIA == "0122" , "KOKUIREGATEJE",
    ETNIA == "0123" , "KORUBO", ETNIA == "0124" , "KRAHO (CRAO, KRAO)",
    ETNIA == "0125" , "KREJE (KRENYE)", ETNIA == "0126" , "KRENAK (BORUN, CRENAQUE)",
    ETNIA == "0127" , "KRIKATI (KRINKATI)", ETNIA == "0128" , "KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)",
    ETNIA == "0129" , "KUIKURO (KUIKURU, CUICURO)", ETNIA == "0130" , "KUJUBIM (KUYUBI, CUJUBIM)",
    ETNIA == "0131" , "KULINA PANO (CULINA)", ETNIA == "0132" , "KULINA/MADIHA (CULINA, MADIJA, MADIHA)",
    ETNIA == "0133" , "KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)",
    ETNIA == "0134" , "KURUAIA (CURUAIA)", ETNIA == "0135" , "KWAZA (COAIA, KOAIA)",
    ETNIA == "0136" , "MACHINERI (MANCHINERI, MANXINERI)",  ETNIA == "0137" , "MACURAP (MAKURAP)",
    ETNIA == "0138" , "MAKU DOW (DOW)", ETNIA == "0139" , "MAKU HUPDA (HUPDA)",
    ETNIA == "0140" , "MAKU NADEB (NADEB)", ETNIA == "0141" , "MAKU YUHUPDE (YUHUPDE)",
    ETNIA == "0142" , "MAKUNA (MACUNA, YEBA-MASA)", ETNIA == "0143" , "MAKUXI (MACUXI, MACHUSI, PEMON)",
    ETNIA == "0144" , "MARIMAM (MARIMA)", ETNIA == "0145" , "MARUBO",
    ETNIA == "0146" , "MATIPU", ETNIA == "0147" , "MATIS",
    ETNIA == "0148" , "MATSE (MAYORUNA)", ETNIA == "0149" , "MAXAKALI (MAXACALI)",
    ETNIA == "0150" , "MAYA (MAYA)",  ETNIA == "0151" , "MAYTAPU",
    ETNIA == "0152" , "MEHINAKO (MEINAKU, MEINACU)",  ETNIA == "0153" , "MEKEN (MEQUEM, MEKHEM, MICHENS)",
    ETNIA == "0154" , "MENKY (MYKY, MUNKU, MENKI, MYNKY)",  ETNIA == "0155" , "MIRANHA (MIRANHA, MIRANA)",
    ETNIA == "0156" , "MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)", ETNIA == "0157" , "MUNDURUKU (MUNDURUCU)",
    ETNIA == "0158" , "MURA", ETNIA == "0159" , "NAHUKWA (NAFUQUA)",
    ETNIA == "0160" , "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)",
    ETNIA == "0161" , "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)",
    ETNIA == "0162" , "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)",
    ETNIA == "0163" , "NARAVUTE (NARUVOTO)",  ETNIA == "0164" , "NAWA (NAUA)",
    ETNIA == "0165" , "NUKINI (NUQUINI, NUKUINI)",  ETNIA == "0166" , "OFAIE (OFAYE-XAVANTE)",
    ETNIA == "0167" , "ORO WIN",  ETNIA == "0168" , "PAIAKU (JENIPAPO-KANINDE)",
    ETNIA == "0169" , "PAKAA NOVA (WARI, PACAAS NOVOS)",  ETNIA == "0170" , "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)",
    ETNIA == "0171" , "PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)",
    ETNIA == "0172" , "PANKARARE (PANCARARE)",  ETNIA == "0173" , "PANKARARU (PANCARARU)",
    ETNIA == "0174" , "PANKARARU KALANKO (KALANKO)",  ETNIA == "0175" , "PANKARARU KARUAZU (KARUAZU)",
    ETNIA == "0176" , "PANKARU (PANCARU)",  ETNIA == "0177" , "PARAKANA (PARACANA, APITEREWA, AWAETE)",
    ETNIA == "0178" , "PARECI (PARESI, HALITI)",  ETNIA == "0179" , "PARINTINTIN",
    ETNIA == "0180" , "PATAMONA (KAPON)", ETNIA == "0181" , "PATAXO",
    ETNIA == "0182" , "PATAXO HA-HA-HAE", ETNIA == "0183" , "PAUMARI (PALMARI)",
    ETNIA == "0184" , "PAUMELENHO", ETNIA == "0185" , "PIRAHA (MURA PIRAHA)",
    ETNIA == "0186" , "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)",
    ETNIA == "0187" , "PITAGUARI",  ETNIA == "0188" , "POTIGUARA",
    ETNIA == "0189" , "POYANAWA (POIANAUA)",  ETNIA == "0190" , "RIKBAKTSA (CANOEIROS, ERIGPAKTSA)",
    ETNIA == "0191" , "SAKURABIAT(MEKENS, SAKIRABIAP, SAKIRABIAR)", ETNIA == "0192" , "SATERE-MAWE (SATERE-MAUE)",
    ETNIA == "0193" , "SHANENAWA (KATUKINA)", ETNIA == "0194" , "SIRIANO (SIRIA-MASA)",
    ETNIA == "0195" , "SURIANA",  ETNIA == "0196" , "SURUI DE RONDONIA (PAITER)",
    ETNIA == "0197" , "SURUI DO PARA (AIKEWARA)", ETNIA == "0198" , "SUYA (SUIA/KISEDJE)",
    ETNIA == "0199" , "TAPAYUNA (BEICO-DE-PAU)",  ETNIA == "0200" , "TAPEBA",
    ETNIA == "0201" , "TAPIRAPE (TAPI'IRAPE)",  ETNIA == "0202" , "TAPUIA (TAPUIA-XAVANTE, TAPUIO)",
    ETNIA == "0203" , "TARIANO (TARIANA, TALIASERI)", ETNIA == "0204" , "TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)",
    ETNIA == "0205" , "TEMBE",  ETNIA == "0206" , "TENHARIM",
    ETNIA == "0207" , "TERENA", ETNIA == "0208" , "TICUNA (TIKUNA, TUKUNA, MAGUTA)",
    ETNIA == "0209" , "TINGUI BOTO",  ETNIA == "0210" , "TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)",
    ETNIA == "0211" , "TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)", ETNIA == "0212" , "TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)",
    ETNIA == "0213" , "TORA", ETNIA == "0214" , "TREMEMBE",
    ETNIA == "0215" , "TRUKA",  ETNIA == "0216" , "TRUMAI",
    ETNIA == "0217" , "TSOHOM DJAPA (TSUNHUM-DJAPA)", ETNIA == "0218" , "TUKANO (TUCANO, YE'PA-MASA, DASEA)",
    ETNIA == "0219" , "TUMBALALA",  ETNIA == "0220" , "TUNAYANA",
    ETNIA == "0221" , "TUPARI", ETNIA == "0222" , "TUPINAMBA",
    ETNIA == "0223" , "TUPINIQUIM", ETNIA == "0224" , "TURIWARA",
    ETNIA == "0225" , "TUXA", ETNIA == "0226" , "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)",
    ETNIA == "0227" , "TXIKAO (TXICAO, IKPENG)",  ETNIA == "0228" , "UMUTINA (OMOTINA, BARBADOS)",
    ETNIA == "0229" , "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)", ETNIA == "0230" , "WAI WAI HIXKARYANA (HIXKARYANA)",
    ETNIA == "0231" , "WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)",  ETNIA == "0232" , "WAI WAI XEREU (XEREU)",
    ETNIA == "0233" , "WAI WAI KATUENA (KATUENA)",  ETNIA == "0234" , "WAI WAI MAWAYANA (MAWAYANA)",
    ETNIA == "0235" , "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )", ETNIA == "0236" , "WAIMIRI ATROARI (KINA)",
    ETNIA == "0237" , "WANANO (UANANO, WANANA)",  ETNIA == "0238" , "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)",
    ETNIA == "0239" , "WAREKENA (UAREQUENA, WEREKENA)", ETNIA == "0240" , "WASSU",
    ETNIA == "0241" , "WAURA (UAURA, WAUJA)", ETNIA == "0242" , "WAYANA (WAIANA, UAIANA)",
    ETNIA == "0243" , "WITOTO (UITOTO, HUITOTO)", ETNIA == "0244" , "XAKRIABA (XACRIABA)",
    ETNIA == "0245" , "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)", ETNIA == "0246" , "XERENTE (AKWE, AWEN, AKWEN)",
    ETNIA == "0247" , "XETA", ETNIA == "0248" , "XIPAIA (SHIPAYA, XIPAYA)",
    ETNIA == "0249" , "XOKLENG (SHOKLENG, XOCLENG)",  ETNIA == "0250" , "XOKO (XOCO, CHOCO)",
    ETNIA == "0251" , "XUKURU (XUCURU)",  ETNIA == "0252" , "XUKURU KARIRI (XUCURU-KARIRI)",
    ETNIA == "0253" , "YAIPIYANA",  ETNIA == "0254" , "YAMINAWA (JAMINAWA, IAMINAWA)",
    ETNIA == "0255" , "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)",  ETNIA == "0256" , "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)",
    ETNIA == "0257" , "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)",  ETNIA == "0258" , "YAWALAPITI (IAUALAPITI)",
    ETNIA == "0259" , "YAWANAWA (IAUANAUA)",  ETNIA == "0260" , "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)",
    ETNIA == "0261" , "YUDJA (JURUNA, YURUNA)", ETNIA == "0262" , "ZO'E (POTURU)",
    ETNIA == "0263" , "ZORO (PAGEYN)",  ETNIA == "0264" , "ZURUAHA (SOROWAHA, SURUWAHA)",
    ETNIA == "X265" , "AHANENAWA", ETNIA == "X266" , "AICABA",
    ETNIA == "X267" , "AIKAN\\u00c3-KWAS\\u00c1", ETNIA == "X268" , "AKUNTSU",
    ETNIA == "X269" , "ALANTESU",  ETNIA == "X271" , "AMAW\\u00c1KA",
    ETNIA == "X272" , "ANAC\\u00c9",  ETNIA == "X273" , "APURIN\\u00c3",
    ETNIA == "X274" , "ARAN\\u00c3",  ETNIA == "X275" , "ARAPA\\u00c7O",
    ETNIA == "X276" , "ARARA APOLIMA",  ETNIA == "X277" , "ARARA DO ARIPUANA",
    ETNIA == "X278" , "ARIPUAN\\u00c1", ETNIA == "X279" , "ASSURINI",
    ETNIA == "X280" , "AWUAR\\u00c1", ETNIA == "X281" , "BORBA",
    ETNIA == "X282" , "CABIXI", ETNIA == "X283" , "CAMARAR\\u00c9",
    ETNIA == "X284" , "CAMASURI", ETNIA == "X285" , "CARA PRETA",
    ETNIA == "X286" , "CHARRUA",  ETNIA == "X287" , "CUJUBIM",
    ETNIA == "X288" , "DAW",  ETNIA == "X289" , "GAVI\\u00c3O",
    ETNIA == "X290" , "GUARANI",  ETNIA == "X291" , "HALANTESU",
    ETNIA == "X292" , "HALOTESU", ETNIA == "X293" , "HENGAT\\u00da",
    ETNIA == "X294" , "HIXKARYANA", ETNIA == "X295" , "HUPDE",
    ETNIA == "X296" , "HUPDES", ETNIA == "X297" , "IAUANAUA",
    ETNIA == "X298" , "IAUARETE A\\u00c7U", ETNIA == "X299" , "IKPENG",
    ETNIA == "X300" , "INAMBU", ETNIA == "X301" , "INHABARANA",
    ETNIA == "X302" , "JAVAE",  ETNIA == "X303" , "JENIPAPO",
    ETNIA == "X304" , "JENIPAPO-KANINDE", ETNIA == "X305" , "JIAHOI",
    ETNIA == "X306" , "KAIOWA", ETNIA == "X307" , "KAMPA",
    ETNIA == "X308" , "KANELA", ETNIA == "X309" , "KARAFAWYANA",
    ETNIA == "X310" , "KARARAO",  ETNIA == "X311" , "KARUBO",
    ETNIA == "X312" , "KASSUP\\u00c1",  ETNIA == "X313" , "KATITH\\u00c3ULU",
    ETNIA == "X314" , "KATOKIN",  ETNIA == "X315" , "KATUKINA PANO",
    ETNIA == "X316" , "KATUKINA PEDA DJAPA",  ETNIA == "X317" , "KATUKINA SHANENAUWA",
    ETNIA == "X318" , "KAXAGO", ETNIA == "X319" , "KAYABI",
    ETNIA == "X320" , "KIN\\u00c3 (WAIMIRI-ATROARI)", ETNIA == "X321" , "KIRIRI-BARRA",
    ETNIA == "X322" , "KITH\\u00c3ULU", ETNIA == "X323" , "KOIAI\\u00c1",
    ETNIA == "X324" , "KOIUPANK\\u00c1",  ETNIA == "X325" , "KONTANAWA",
    ETNIA == "X326" , "KRAH\\u00d4 KANELA", ETNIA == "X327" , "KULINA",
    ETNIA == "X328" , "LATUND\\u00ca", ETNIA == "X329" , "MAKU",
    ETNIA == "X330" , "MAKUNAMB\\u00c9",  ETNIA == "X331" , "MAMAIND\\u00ca",
    ETNIA == "X332" , "MAMURI", ETNIA == "X333" , "MANACAPURU",
    ETNIA == "X334" , "MANAIRISSU", ETNIA == "X335" , "MANCHINERI",
    ETNIA == "X336" , "MANDUCA",  ETNIA == "X337" , "MARIBONDO",
    ETNIA == "X338" , "MASSAKA",  ETNIA == "X339" , "MAWAYANA",
    ETNIA == "X340" , "MAW\\u00c9", ETNIA == "X341" , "MAYORUNA",
    ETNIA == "X342" , "MIQUELENO", ETNIA == "X343" , "MOKURI\\u00d1",
    ETNIA == "X344" , "MON ORO WARAM", ETNIA == "X345" , "MUTUM",
    ETNIA == "X346" , "MYKY", ETNIA == "X347" , "NADEB",
    ETNIA == "X348" , "NAMBIKWARA", ETNIA == "X349" , "NEGAROT\\u00ca",
    ETNIA == "X350" , "NHENGATU", ETNIA == "X351" , "OFAIE XAVANTE",
    ETNIA == "X352" , "ON\\u00c7A", ETNIA == "X353" , "ORO AT",
    ETNIA == "X354" , "ORO EO", ETNIA == "X355" , "ORO JOWIN",
    ETNIA == "X356" , "ORO MIYLIN", ETNIA == "X357" , "ORO MON",
    ETNIA == "X358" , "ORO N\\u00c1O", ETNIA == "X359" , "ORO WAM",
    ETNIA == "X360" , "ORO WARAM",  ETNIA == "X361" , "ORO WARAM XIJEIN",
    ETNIA == "X362" , "PACA", ETNIA == "X363" , "PANKAR\\u00c1",
    ETNIA == "X364" , "PAPAGAIO", ETNIA == "X365" , "PAYAY\\u00c1",
    ETNIA == "X366" , "PIPIPAN",  ETNIA == "X367" , "PIRATA",
    ETNIA == "X368" , "PUROBOR\\u00c1", ETNIA == "X369" , "SABAN\\u00ca",
    ETNIA == "X370" , "SANUMA", ETNIA == "X371" , "SAWENTES\\u00da",
    ETNIA == "X372" , "SILCY-TAPUYA", ETNIA == "X373" , "SIUCI",
    ETNIA == "X374" , "TABAJARA", ETNIA == "X375" , "TAKUARA",
    ETNIA == "X376" , "TATU", ETNIA == "X377" , "TAWAND\\u00ca",
    ETNIA == "X378" , "TEF\\u00c9", ETNIA == "X379" , "TIMBIRA",
    ETNIA == "X380" , "TOR\\u00c1 DO BAIXO GRANDE", ETNIA == "X381" , "TSUNHUM-DJAP\\u00c1",
    ETNIA == "X382" , "TUBAR\\u00c3O",  ETNIA == "X383" , "TUPAIU",
    ETNIA == "X384" , "TUPI", ETNIA == "X385" , "TUPINAMB\\u00c1 DE BELMONTE",
    ETNIA == "X386" , "URUBU",  ETNIA == "X387" , "URUBU KAAPOR",
    ETNIA == "X388" , "URUP\\u00c1",  ETNIA == "X389" , "WAI WAI",
    ETNIA == "X390" , "WAIKISU", ETNIA == "X391" , "WAKALITES\\u00da",
    ETNIA == "X392" , "WASSUSU", ETNIA == "X393" , "XEREU",
    ETNIA == "X394" , "XI EIN", ETNIA == "X395" , "XICRIN",
    ETNIA == "X396" , "XIPAYA", ETNIA == "X397" , "XIRIANA",
    ETNIA == "X398" , "XIRUAI", ETNIA == "X399" ,"YEPAMASS\\u00c3",
    ETNIA == "X400" , "TIRIY\\u00d3", ETNIA == "X401" , "YANOMAMI",
    ETNIA == "X402" , "ARARA",  ETNIA == "X403" , "SAKIRIABAR",
    ETNIA == "X404" , "TATZ", ETNIA == "X405" , "SEM INFORMACAO", default = as.character(ETNIA) ) ) ]
 }

  # #MARCA_UCI
  if("MARCA_UCI" %in% names(data) ) {
  data[,  
  def_MARCA_UCI := as_factor(fcase(
    MARCA_UCI == "00" , "Não utilizou UCI",
    MARCA_UCI == "01" , "Unidade de cuidados intermed neonatal convencional",
    MARCA_UCI == "02" , "Unidade de cuidados intermed neonatal canguru",
    MARCA_UCI == "03" , "Unidade intermediária neonatal",
    MARCA_UCI == "88" , "Utilizou dois tipos de leitos UCI", default = as.character(MARCA_UCI) ) ) ]
  }

  #Especialidade do Leito.
  if("ESPEC" %in% names(data) ) {
  data[,  
  def_espec_leito :=  as_factor( fcase(
    ESPEC == "01" , "Cirúrgico",
    ESPEC == "02" , "Obstétricos",
    ESPEC == "03" , "Clínicos",
    ESPEC == "04" , "Crônicos",
    ESPEC == "05" , "Psiquiatria",
    ESPEC == "06" , "Pneumologia sanitária (tsiologia)",
    ESPEC == "07" , "Pediátricos",
    ESPEC == "08" , "Reabilitação",
    ESPEC == "09" , "Leito Dia / Cirúrgicos",
    ESPEC == "10" , "Leito Dia / Aids",
    ESPEC == "11" , "Leito Dia / Fibrose Cística",
    ESPEC == "12" , "Leito Dia / Intercorrência Pós-Transplante",
    ESPEC == "13" , "Leito Dia / Geriatria",
    ESPEC == "14" , "Leito Dia / Saúde Mental",
    #Não consta na lista do icit.
    ESPEC == "51" , "UTI II Adulto COVID 19",
    ESPEC == "52" , "UTI II Pediátrica COVID 19",
    ESPEC == "64" , "Unidade Intermediária",
    ESPEC == "65" , "Unidade Intermediária Neonatal",
    ESPEC == "74" , "UTI I",
    ESPEC == "75" , "UTI Adulto II",
    ESPEC == "76" , "UTI Adulto III",
    ESPEC == "77" , "UTI Infantil I",
    ESPEC == "78" , "UTI Infantil II",
    ESPEC == "79" , "UTI Infantil III",
    ESPEC == "80" , "UTI Neonatal I",
    ESPEC == "81" , "UTI Neonatal II",
    ESPEC == "82" , "UTI Neonatal III",
    ESPEC == "83" , "UTI Queimados",
    ESPEC == "84" , "Acolhimento Noturno",
    ESPEC == "85" , "UTI Coronariana-UCO tipo II",
    ESPEC == "86" , "UTI Coronariana-UCO tipo III",
    ESPEC == "87" , "Saúde Mental (Clínico)",
    ESPEC == "88" , "Queimado Adulto (Clínico)",
    ESPEC == "89" , "Queimado Pediátrico (Clínico)",
    ESPEC == "90" , "Queimado Adulto (Cirúrgico)",
    ESPEC == "91" , "Queimado Pediátrico (Cirúrgico)",
    ESPEC == "92" , "UCI Unidade de Cuidados Intermediarios Neonatal Convencional",
    ESPEC == "93" , "UCI Unidade de Cuidados Intermediarios Neonatal Canguru",
    ESPEC == "94" , "UCI Unidade de Cuidados Intermediarios Pediatrico",
    ESPEC == "95" , "UCI Unidade de Cuidados Intermediarios Adulto",
    ESPEC == "96" , "Suporte Ventilatório Pulmonar COVID-19", default =  as.character(ESPEC) ) ) ]
}
  
  
  #ICSAP: Identificação por CID
  if("DIAG_PRINC" %in% names(data) ) {
  data[,
  ICSAP := as_factor( fcase(
    DIAG_PRINC %in% c("A370", "A378", "A379", "A360", "A361", "A363", "A369", "A33", "A34", "A35", "B06", "B052", "A950", "A959", "B161", "B162", "B169", "G000",
                      "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A190", "A191", "A192", "A198", "A199",
                      "A150", "A151", "A152", "A153", "A160", "A161", "A162", "A154", "A155", "A156", "A157", "A158", "A159", "A163", "A164", "A165",
                      "A166", "A167", "A168", "A169", "A180", "A181", "A182", "A183", "A184", "A185", "A186", "A187", "A188", "I00", "I010", "I011",
                      "I012", "I018", "I019", "I020", "I029", "A511", "A512", "A513", "A514", "A515", "A519", "A520", "A521", "A522", "A523",
                      "A527", "A528", "A529", "A530", "A539", "B500", "B509", "B518", "B519", "B520", "B528", "B53", "B54", "B260", "B268", "B269",
                      "B770", "B778", "B779"), "cidgrupo1",

    DIAG_PRINC %in% c("E86", "A00", "A010", "A020", "A021", "A029", "A039", "A040",
                      "A041", "A042", "A044", "A046", "A047", "A048", "A049",
                      "A050", "A054", "A058", "A059", "A060", "A064", "A068", "A069",
                      "A071", "A073", "A078", "A079", "A080", "A083", "A084", "A085", "A09"), "cidgrupo2",

    DIAG_PRINC %in% c("D500", "D501", "D508", "D509"), "cidgrupo3",

    DIAG_PRINC %in% c("E40", "E41", "E42", "E43", "E440", "E441", "E45", "E46", "E500", "E508", "E51", "E52",
                      "E539", "E54", "E55", "E56", "E57", "E58", "E59", "E60", "E61", "E62", "E638", "E64"), "cidgrupo4",

    DIAG_PRINC %in% c("H660", "H661", "H662", "H663", "H664", "H669", "J00",
                      "J010", "J018", "J019", "J029", "J030", "J038", "J039", "J060", "J068", "J069", "J31"), "cidgrupo5",

    DIAG_PRINC %in% c("J13", "J14", "J153", "J154", "J158", "J159", "J181"), "cidgrupo6",

    DIAG_PRINC %in% c("J45", "J46"), "cidgrupo7",

    DIAG_PRINC %in% c("J200", "J201", "J203", "J205", "J209", "J210", "J218", "J219", "J40", "J410", "J411",
                      "J418", "J42", "J43", "J47", "J44", "J450", "J451", "J458", "J459"), "cidgrupo8",

    DIAG_PRINC %in% c("I10", "I110", "I119"), "cidgrupo9",

    DIAG_PRINC %in% c("I200", "I201", "I208", "I209"), "cidgrupo10",

    DIAG_PRINC %in% c("I500", "I501", "I509", "J81"), "cidgrupo11",

    DIAG_PRINC %in% c("I63", "I64", "I65", "I66", "I67", "I69"), "cidgrupo12",

    DIAG_PRINC %in% c("E100", "E101", "E110", "E111", "E140", "E141", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109",
                      "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E121", "E125", "E126", "E127", "E128", "E129", "E130", "E131",
                      "E132", "E133", "E135", "E136", "E137", "E138", "E139", "E142", "E143", "E144", "E145", "E146", "E147", "E148", "E149"), "cidgrupo13",

    DIAG_PRINC %in% c("G400", "G401", "G402", "G403", "G404", "G405", "G406", "G407", "G408", "G409", "G41"), "cidgrupo14",

    DIAG_PRINC %in% c("N10", "N11", "N12", "N300", "N301", "N302", "N303", "N304", "N308", "N309", "N340", "N341", "N342", "N343", "N390"), "cidgrupo15",

    DIAG_PRINC %in% c("A446", "L01", "L020", "L021", "L022", "L023", "L024", "L028", "L029", "L030", "L031",
                      "L032", "L033", "L038", "L039", "L040", "L041", "L042", "L043", "L048", "L049", "L080", "L088", "L089"), "cidgrupo16",

    DIAG_PRINC %in% c("N700", "N701", "N709", "N710", "N719", "N72", "N730", "N732", "N733", "N734", "N735",
                      "N736", "N738", "N739", "N750", "N751", "N758", "N760", "N762", "N764", "N766", "N768"), "cidgrupo17",

    DIAG_PRINC %in% c("K25", "K26", "K27", "K28", "K920", "K921", "K922"), "cidgrupo18",

    DIAG_PRINC %in% c("O23", "A500", "A501", "A502", "A503", "A504", "A505", "A506", "A507", "A509", "P35"), "cidgrupo19", default = "Outros") ) ]
  }
  
  #Nacionalidade
  if("NACIONAL" %in% names(data) ) {
  data[,
  def_NACIONAL := as_factor(fcase(
    NACIONAL == "170" , "Abissinia",NACIONAL == "171" , "Acores",
    NACIONAL == "172" , "Afar frances", NACIONAL == "241" , "Afeganistao",
    NACIONAL == "093" ,  "Albania",  NACIONAL == "030" , "Alemanha",
    NACIONAL == "174" , "Alto volta", NACIONAL == "094" ,  "Andorra",
    NACIONAL == "175" , "Angola", NACIONAL == "334" , "Antartica francesa",
    NACIONAL == "337" , "Antartico argentino",NACIONAL == "333" , "Antartico britanico, territorio",
    NACIONAL == "336" , "Antartico chileno", NACIONAL == "338" , "Antartico noruegues",
    NACIONAL == "028" ,  "Antigua e. dep. barbuda", NACIONAL == "029" , "Antilhas holandesas",
    NACIONAL == "339" , "Apatrida", NACIONAL == "242" , "Arabia saudita",
    NACIONAL == "176" , "Argelia", NACIONAL == "021" , "Argentina",
    NACIONAL == "347" , "Armenia", NACIONAL == "289" , "Arquipelago de bismark",
    NACIONAL == "175" , "Angola", NACIONAL == "285" , "Arquipelago manahiki",
    NACIONAL == "286" , "Arquipelago midway", NACIONAL == "033" , "Aruba",
    NACIONAL == "175" , "Angola", NACIONAL == "198" , "Ascensao e tristao da cunha,is",
    NACIONAL == "287" , "Ashmore e cartier", NACIONAL == "288" , "Australia",
    NACIONAL == "095" ,  "Austria", NACIONAL == "138" , "Azerbaijao",
    NACIONAL == "243" , "Bahrein", NACIONAL == "342" , "Bangladesh",
    NACIONAL == "044" ,  "Barbados", NACIONAL == "139" , "Bashkista",
    NACIONAL == "177" , "Bechuanalandia", NACIONAL == "031" , "Belgica",
    NACIONAL == "046" ,  "Belize", NACIONAL == "178" , "Benin",
    NACIONAL == "083" ,  "Bermudas", NACIONAL == "246" , "Bhutan",
    NACIONAL == "244" , "Birmania", NACIONAL == "022" ,  "Bolivia", NACIONAL == "134" , "Bosnia herzegovina",
    NACIONAL == "179" , "Botsuana", NACIONAL == "010" , "Brasil",
    NACIONAL == "245" , "Brunei", NACIONAL == "096" , "Bulgaria",
    NACIONAL == "238" , "Burkina fasso", NACIONAL == "180" , "Burundi",
    NACIONAL == "141" , "Buryat", NACIONAL == "343" , "Cabo verde", NACIONAL == "181" , "Camaroes",
    NACIONAL == "034" ,  "Canada", NACIONAL == "142" , "Carelia", NACIONAL ==  "247" , "Catar",
    NACIONAL == "143" , "Cazaquistao", NACIONAL ==  "248" , "Ceilao",
    NACIONAL == "182" , "Ceuta e melilla", NACIONAL == "183" , "Chade",
    NACIONAL == "144" , "Chechen ingusth", NACIONAL == "023" , "Chile",
    NACIONAL == "042" ,  "China", NACIONAL == "249" , "China (taiwan)",
    NACIONAL == "097" ,  "Chipre", NACIONAL == "145" , "Chuvash", NACIONAL == "275" , "Cingapura",
    NACIONAL == "026" ,  "Colombia", NACIONAL == "040" , "Comunidade das bahamas",
    NACIONAL == "054" ,  "Comunidade dominicana", NACIONAL == "185" , "Congo",
    NACIONAL == "043" ,  "Coreia", NACIONAL == "186" , "Costa do marfim",
    NACIONAL == "051" ,  "Costa rica", NACIONAL == "250" , "Coveite",
    NACIONAL == "130" , "Croacia", NACIONAL == "052" , "Cuba", NACIONAL == "053" , "Curacao",
    NACIONAL == "146" , "Dagesta", NACIONAL == "187" , "Daome",
    NACIONAL == "340" , "Dependencia de ross", NACIONAL == "098" , "Dinamarca",
    NACIONAL == "188" , "Djibuti", NACIONAL ==  "099" , "Eire",
    NACIONAL == "251" , "Emirados arabes unidos", NACIONAL == "027" , "Equador",
    NACIONAL == "100" , "Escocia", NACIONAL == "136" , "Eslovaquia",
    NACIONAL == "132" , "Eslovenia", NACIONAL == "035" , "Espanha",
    NACIONAL == "129" , "Estado da cidade do vaticano",
    NACIONAL == "057" ,  "Estados assoc. das antilhas",
    NACIONAL == "036" ,  "Estados unidos da america (eua)",
    NACIONAL == "147" , "Estonia", NACIONAL == "190" , "Etiopia",
    NACIONAL == "252" , "Filipinas", NACIONAL == "102" , "Finlandia", NACIONAL == "037" , "Franca",
    NACIONAL == "192" , "Gambia", NACIONAL == "193" , "Gana", NACIONAL == "194" , "Gaza",
    NACIONAL == "148" , "Georgia", NACIONAL == "103" , "Gibraltar",
    NACIONAL == "149" , "Gorno altai", NACIONAL == "032" , "Gra-bretanha",
    NACIONAL == "059"  , "Granada", NACIONAL == "104" , "Grecia", NACIONAL == "084" , "Groenlandia",
    NACIONAL == "292" , "Guam", NACIONAL == "061" , "Guatemala",
    NACIONAL == "087" , "Guiana francesa", NACIONAL == "195" , "Guine",
    NACIONAL == "344" , "Guine bissau", NACIONAL == "196" , "Guine equatorial",
    NACIONAL == "105" , "Holanda", NACIONAL == "064" , "Honduras",
    NACIONAL == "063" , "Honduras britanicas", NACIONAL == "253" , "Hong-kong",
    NACIONAL == "106" , "Hungria", NACIONAL == "254" , "Iemen",
    NACIONAL == "345" , "Iemen do sul", NACIONAL == "197" , "Ifni",
    NACIONAL == "300" , "Ilha johnston e sand", NACIONAL == "069" , "Ilha milhos",
    NACIONAL == "293" , "Ilhas baker", NACIONAL == "107" , "Ilhas baleares",
    NACIONAL == "199" , "Ilhas canarias", NACIONAL == "294" , "Ilhas cantao e enderburg",
    NACIONAL == "295" , "Ilhas carolinas", NACIONAL == "297" , "Ilhas christmas",
    NACIONAL == "184" , "Ilhas comores", NACIONAL == "290" , "Ilhas cook",
    NACIONAL == "108" , "Ilhas cosmoledo (lomores)",
    NACIONAL == "117" , "Ilhas de man", NACIONAL == "109" , "Ilhas do canal",
    NACIONAL == "296" , "Ilhas do pacifico", NACIONAL == "058" , "Ilhas falklands",
    NACIONAL == "101" , "Ilhas faroes", NACIONAL == "298" , "Ilhas gilbert",
    NACIONAL == "060" , "Ilhas guadalupe", NACIONAL == "299" , "Ilhas howland e jarvis",
    NACIONAL == "301" , "Ilhas kingman reef", NACIONAL == "305" , "Ilhas macdonal e heard",
    NACIONAL == "302" , "Ilhas macquaire", NACIONAL == "067" , "Ilhas malvinas",
    NACIONAL == "303" , "Ilhas marianas", NACIONAL == "304" , "Ilhas marshall",
    NACIONAL == "306" , "Ilhas niue", NACIONAL == "307" , "Ilhas norfolk",
    NACIONAL == "315" , "Ilhas nova caledonia", NACIONAL == "318" , "Ilhas novas hebridas",
    NACIONAL == "308" , "Ilhas palau", NACIONAL == "320" , "Ilhas pascoa",
    NACIONAL == "321" , "Ilhas pitcairin", NACIONAL == "309" , "Ilhas salomao",
    NACIONAL == "326" , "Ilhas santa cruz", NACIONAL == "065" , "Ilhas serranas",
    NACIONAL == "310" , "Ilhas tokelau", NACIONAL == "080" , "Ilhas turca",
    NACIONAL == "047" , "Ilhas turks e caicos", NACIONAL == "082" , "Ilhas virgens americanas",
    NACIONAL == "081" , "Ilhas virgens britanicas",
    NACIONAL == "311" , "Ilhas wake", NACIONAL == "332" , "Ilhas wallis e futuna",
    NACIONAL == "255" , "India", NACIONAL ==  "256" , "Indonesia",
    NACIONAL == "110" , "Inglaterra", NACIONAL == "257" , "Ira",
    NACIONAL == "258" , "Iraque", NACIONAL == "112" , "Irlanda",
    NACIONAL == "111" , "Irlanda do norte",
    NACIONAL == "113" , "Islandia", NACIONAL == "259" , "Israel",
    NACIONAL == "039" , "Italia", NACIONAL == "114" , "Iugoslavia",
    NACIONAL == "066" , "Jamaica", NACIONAL == "041" , "Japao",
    NACIONAL == "260" , "Jordania",
    NACIONAL == "150" , "Kabardino balkar", NACIONAL == "312" , "Kalimatan",
    NACIONAL == "151" , "Kalmir", NACIONAL == "346" , "Kara kalpak",
    NACIONAL == "152" , "Karachaevocherkess", NACIONAL == "153" , "Khakass",
    NACIONAL == "261" , "Kmer/camboja", NACIONAL == "154" , "Komi",
    NACIONAL == "262" , "Kuwait", NACIONAL == "263" , "Laos",
    NACIONAL == "200" , "Lesoto", NACIONAL == "155" , "Letonia",
    NACIONAL == "264" , "Libano",
    NACIONAL == "201" , "Liberia", NACIONAL == "202" , "Libia",
    NACIONAL == "115" , "Liechtenstein",
    NACIONAL == "156" , "Lituania", NACIONAL == "116" , "Luxemburgo",
    NACIONAL == "265" , "Macau", NACIONAL == "205" , "Madagascar",
    NACIONAL == "203" , "Madeira", NACIONAL == "266" , "Malasia",
    NACIONAL == "204" , "Malawi",
    NACIONAL == "267" , "Maldivas", NACIONAL == "206" , "Mali",
    NACIONAL == "157" , "Mari", NACIONAL ==  "207" , "Marrocos",
    NACIONAL == "068" , "Martinica",
    NACIONAL == "268" , "Mascate", NACIONAL == "208" , "Mauricio",
    NACIONAL == "209" , "Mauritania",NACIONAL ==  "085" , "Mexico",
    NACIONAL == "284" , "Mianma", NACIONAL == "210" , "Mocambique",
    NACIONAL == "158" , "Moldavia", NACIONAL == "118" , "Monaco", NACIONAL == "269" , "Mongolia",
    NACIONAL == "070" , "Monte serrat", NACIONAL == "137" , "Montenegro",
    NACIONAL == "240" , "Namibia", NACIONAL == "314" , "Nauru",
    NACIONAL == "270" , "Nepal", NACIONAL == "211" , "Nguane",
    NACIONAL == "071" , "Nicaragua",
    NACIONAL == "213" , "Nigeria", NACIONAL == "119" , "Noruega",
    NACIONAL == "316" , "Nova guine",
    NACIONAL == "317" , "Nova zelandia", NACIONAL == "271" , "Oman",
    NACIONAL == "159" , "Ossetia setentrional", NACIONAL == "121" , "Pais de gales",
    NACIONAL == "122" , "Paises baixos", NACIONAL == "272" , "Palestina",
    NACIONAL == "072" , "Panama", NACIONAL == "073" , "Panama(zona do canal)",
    NACIONAL == "214" , "Papua nova guine", NACIONAL == "273" , "Paquistao",
    NACIONAL == "024" , "Paraguai", NACIONAL == "089" , "Peru",
    NACIONAL == "322" , "Polinesia francesa", NACIONAL ==  "123" , "Polonia",
    NACIONAL == "074" , "Porto rico", NACIONAL == "045" , "Portugal",
    NACIONAL == "215" , "Pracas norte africanas", NACIONAL == "216" , "Protetor do sudoeste africano",
    NACIONAL == "217" , "Quenia", NACIONAL == "160" , "Quirguistao",
    NACIONAL == "075" , "Quitasueno", NACIONAL == "189" , "Republica arabe do egito",
    NACIONAL == "218" , "Republica centro africana",
    NACIONAL == "173" , "Republica da africa do sul", NACIONAL == "140" , "Republica da bielorrussia",
    NACIONAL == "133" , "Republica da macedonia", NACIONAL == "56" , "Republica de el salvador",
    NACIONAL == "291" , "Republica de fiji", NACIONAL == "120" , "Republica de malta",
    NACIONAL == "191" , "Republica do gabao", NACIONAL == "062" , "Republica do haiti",
    NACIONAL == "212" , "Republica do niger", NACIONAL == "055" , "Republica dominicana",
    NACIONAL == "088" , "Republica guiana", NACIONAL == "135" , "Republica tcheca",
    NACIONAL == "020" , "Reservado", NACIONAL == "048" , "Reservado",
    NACIONAL == "049" , "Reservado", NACIONAL == "050" , "Reservado",
    NACIONAL == "219" , "Reuniao", NACIONAL == "220" , "Rodesia (zimbabwe)",
    NACIONAL == "124" , "Romenia", NACIONAL == "076" , "Roncador",
    NACIONAL == "221" , "Ruanda", NACIONAL == "274" , "Ruiquiu,is",
    NACIONAL == "348" , "Russia", NACIONAL == "222" , "Saara espanhol",
    NACIONAL == "323" , "Sabah", NACIONAL == "324" , "Samoa americana",
    NACIONAL == "325" , "Samoa ocidental", NACIONAL == "125" , "San marino",
    NACIONAL == "223" , "Santa helena", NACIONAL == "077" , "Santa lucia",
    NACIONAL == "078" , "Sao cristovao", NACIONAL == "224" , "Sao tome e principe",
    NACIONAL == "079" , "Sao vicente", NACIONAL == "327" , "Sarawak",
    NACIONAL == "349" , "Senegal", NACIONAL == "276" , "Sequin",
    NACIONAL == "226" , "Serra leoa", NACIONAL == "131" , "Servia",
    NACIONAL == "225" , "Seychelles",
    NACIONAL == "277" , "Siria", NACIONAL == "227" , "Somalia, republica",
    NACIONAL == "278" , "Sri-lanka", NACIONAL == "086" , "St. pierre et miquelon",
    NACIONAL == "228" , "Suazilandia", NACIONAL == "229" , "Sudao",
    NACIONAL == "126" , "Suecia", NACIONAL == "038" , "Suica",
    NACIONAL == "090" , "Suriname", NACIONAL == "127" , "Svalbard e Jan Maye",
    NACIONAL == "161" , "Tadjiquistao", NACIONAL == "279" , "Tailandia",
    NACIONAL == "230" , "Tanganica", NACIONAL == "350" , "Tanzania",
    NACIONAL == "162" , "Tartaria", NACIONAL == "128" , "Tchecoslovaquia",
    NACIONAL == "335" , "Terr. antartico da australia",
    NACIONAL == "341" , "Terras austrais", NACIONAL == "231" , "Territ. britanico do oceano indico",
    NACIONAL == "328" , "Territorio de cocos", NACIONAL == "319" , "Territorio de papua",
    NACIONAL == "329" , "Timor", NACIONAL == "233" , "Togo", NACIONAL == "330" , "Tonga",
    NACIONAL == "232" , "Transkei", NACIONAL == "280" , "Tregua, estado",
    NACIONAL == "091" , "Trinidad e tobago", NACIONAL == "234" , "Tunisia",
    NACIONAL == "163" , "Turcomenistao", NACIONAL ==  "281" , "Turquia",
    NACIONAL == "331" , "Tuvalu", NACIONAL == "164" , "Tuvin",
    NACIONAL == "165" , "Ucrania", NACIONAL == "166" , "Udmurt", NACIONAL == "235" , "Uganda",
    NACIONAL == "167" , "Uniao sovietica", NACIONAL == "025" , "Uruguai",
    NACIONAL == "168" , "Uzbequistao", NACIONAL == "092" , "Venezuela",
    NACIONAL == "282" , "Vietna do norte",NACIONAL ==  "283" , "Vietna do sul",
    NACIONAL == "169" , "Yakut", NACIONAL == "236" , "Zaire",
    NACIONAL == "237" , "Zambia", NACIONAL == "239" , "Zimbabwe", default = as.character(NACIONAL) ) ) ]
  }
  

#Aplicando label em procedimentos -----------------------------------------------------------
# procedimentos <- fread("C:/Users/gabli/Desktop/r/SIH/bases_auxiliares/TB_SIGTAB.csv", sep = ";") |>
#     rename(proc = value) |>
#     mutate(proc = proc |> str_to_title() |> as_factor() )
# data <- read.dbc::read.dbc("C:/Users/gabli/Desktop/r/SIH/dbc/RDTO2502.dbc") |>
#     data.table::setDT()
  
#Importação de função com código de    
# source("https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/SIH/funcao_cod_procedimentos_SUS.R")
 
  #A importação da tabela com procediemtnos é feita através de source na rotina de criação da base.
  
  
  
   #left_joit label de procedimento solicitado
  data[procedimentos, def_proc_solic := i.proc, on = .(PROC_SOLIC = cod)] 
  #left_joit label de procedimento realizado
  data[procedimentos, def_proc_rea   := i.proc, on = .(PROC_REA = cod)]
  #rm(list = setdiff( c("ftp_base", "destino", "arquivos", "procedimentos") , c("data","tabela_criada") ))   
 
  
   
# #Municípios --------------------------------------------------------------
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


  #left_Join com município de residência
  data <- merge(
    x = data,
    y = munics,
    by.x = "MUNIC_RES",
    by.y = "code_muni",
    all.x = TRUE,
    suffixes = c("", "_resd")
  )

  #Renomear colunas de residência
  setnames(data,
           old = c("name_muni", "MUNIC_RES", "code_state", "abbrev_state", "name_state", "name_region"),
           new = c("munic_resd", "cod_munic_resd", "code_state_resd", "abbrev_state_resd", "uf_resd", "region_resd"))

  #left_join com município de internação
  data <- merge(
    x = data,
    y = munics,
    by.x = "MUNIC_MOV",
    by.y = "code_muni",
    all.x = TRUE,
    suffixes = c("", "_int")
  )


  #Renomear variáveis
  setnames(data,
           old = c("NASC", "ESPEC", "PROC_SOLIC", "PROC_REA", "INSTRU"),
           new = c("DT_NASC", "COD_ESPEC", "COD_PROC_SOLIC", "COD_PROC_REA", "ESC"))

  #Renomear colunas de internação
  setnames(data,
           old = c("name_muni", "MUNIC_MOV", "code_state", "abbrev_state", "name_state", "name_region"),
           new = c("munic_int", "cod_munic_int", "code_state_int", "abbrev_state_int", "uf_int", "region_int"))

  data <- tibble::as_tibble(data) 
  data <- droplevels(data.table::as.data.table(data))
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
                                                    FUN = stringi::stri_unescape_unicode)))

 }



# Função utilizada para empilhar o SIH ------------------------------------
empilhar_sih <- function(arquivo, 
                         variaveis = NULL, #Variáveis que desejo manter. NULL seleciona todas as variáveis não excluidas.
                         excluir = vars_excluir) {
  message("Importando: ", arquivo)
  dados <- read.dbc::read.dbc(arquivo) |> setDT()
  
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
  
  #Nos dados de 2013 precisa renomar variável de dado secundário
  #Se o nome do arquivo contém "13", renomeia a variável diag_secun
  if (stringr::str_detect(arquivo, "13") && "DIAG_SECUN" %in% names(dados)) {
    dados <- dados |> rename(DIAGSEC1 = DIAG_SECUN)
  }
  
  #Em 2014 a variável diagsec1 é introduzida, mas não é utilizada. 100% missing 
  #Como diagnóstico secundário ainda é utilizado diag_secun. 
  #Vou excluir diagsec, pois não é utilizada e renomear diag_secun para diagsec1. 
  if (stringr::str_detect(arquivo, "14")) {
    if ("DIAGSEC1" %in% names(dados)) dados <- dados |> select(-DIAGSEC1)
    if ("DIAG_SECUN" %in% names(dados)) dados <- dados |> rename(DIAGSEC1 = DIAG_SECUN)
  }
  
  return(dados)
}
