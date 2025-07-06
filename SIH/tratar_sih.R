library(microdatasus)

#Precisa incluir variáveis não tratadas na função.
#cid_notif

#Olhar as variáveis adicionadas pelo icit
#def_identific. Tipo de AIH.

#### POSSO USAR remove_empty() do janitor. Para excluir colunas com 100 missing.

#Variáveis para exclusão.
#GESTOR_TP. Não foi encontrado label corresponde aos levels da variável. 
#GESTOR_DT. Zerado em 2008.


#Variáveis para verificar na base completa
#infehosp.

tratar_sih <- function (data, information_system = "SIH-RD") {
  
  available_information_system <- "SIH-RD"
  
  if (!(information_system %in% available_information_system)) 
        stop("Health informaton system unknown.")
  
  #Adicionando variáveis de interesse.
  
  
  # ICSAP: Identificação por CID
  data <- data |> setDT() 
  
  data[, `:=`(
    
    #Regras de idade
    IDADE = fifelse(COD_IDADE %in% c(2, 3), 0,
                    fifelse(COD_IDADE == 5, IDADE + 100, IDADE) ),
    #Delete do código de idade.
    COD_IDADE = NULL,
    
    
    #Correções nos códigos do DF. Existem códigos das regiões administrativas. Conserta para código do DF. 
    MUNIC_MOV = fifelse(startsWith(as.character(MUNIC_MOV), "53"), "530010", as.character(MUNIC_MOV) ),
    MUNIC_RES = fifelse(startsWith(as.character(MUNIC_RES), "53"), "530010", as.character(MUNIC_RES) ) ) ]
    # 
    # mutate(icsap =  
    #          
    #          case_when(
    #            
    #            DIAG_PRINC %in% c("A370", "A378", "A379", "A360", "A361", "A363", "A369", "A33", "A34", "A35", "B06", "B052", "A950", "A959", "B161", "B162", "B169", "G000",
    #                              "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A190", "A191", "A192", "A198", "A199",
    #                              "A150", "A151", "A152", "A153", "A160", "A161", "A162", "A154", "A155", "A156", "A157", "A158", "A159", "A163", "A164", "A165",
    #                              "A166", "A167", "A168", "A169", "A180", "A181", "A182", "A183", "A184", "A185", "A186", "A187", "A188", "I00", "I010", "I011",
    #                              "I012", "I018", "I019", "I020", "I029", "A511", "A512", "A513", "A514", "A515", "A519", "A520", "A521", "A522", "A523",
    #                              "A527", "A528", "A529", "A530", "A539", "B500", "B509", "B518", "B519", "B520", "B528", "B53", "B54", "B260", "B268", "B269",
    #                              "B770", "B778", "B779") ~ "cidgrupo1",
    #            
    #            DIAG_PRINC %in% c("E86", "A00", "A010", "A020", "A021", "A029", "A039", "A040", "A041", "A042", "A044", "A046", "A047", "A048", "A049",
    #                              "A050", "A054", "A058", "A059", "A060", "A064", "A068", "A069", "A071", "A073", "A078", "A079", "A080", "A083", "A084", "A085", "A09") ~ "cidgrupo2",
    #            
    #            DIAG_PRINC %in% c("D500", "D501", "D508", "D509") ~ "cidgrupo3",
    #            
    #            DIAG_PRINC %in% c("E40", "E41", "E42", "E43", "E440", "E441", "E45", "E46", "E500", "E508", "E51", "E52", "E539", "E54", "E55", "E56", "E57", 
    #                              "E58", "E59", "E60", "E61", "E62", "E638", "E64") ~ "cidgrupo4",
    #            
    #            DIAG_PRINC %in% c("H660", "H661", "H662", "H663", "H664", "H669", "J00", "J010", "J018", "J019", "J029", "J030", "J038", "J039", "J060", "J068", "J069", "J31") ~ "cidgrupo5",
    #            
    #            DIAG_PRINC %in% c("J13", "J14", "J153", "J154", "J158", "J159", "J181") ~ "cidgrupo6",
    #            
    #            DIAG_PRINC %in% c("J45", "J46") ~ "cidgrupo7",
    #            
    #            DIAG_PRINC %in% c("J200", "J201", "J203", "J205", "J209", "J210", "J218", "J219", "J40", "J410", "J411", "J418", "J42", "J43", "J47", "J44", "J450", "J451", "J458", "J459") ~ "cidgrupo8",
    #            
    #            DIAG_PRINC %in% c("I10", "I110", "I119") ~ "cidgrupo9",
    #            
    #            DIAG_PRINC %in% c("I200", "I201", "I208", "I209") ~ "cidgrupo10",
    #            
    #            DIAG_PRINC %in% c("I500", "I501", "I509", "J81") ~ "cidgrupo11",
    #            
    #            DIAG_PRINC %in% c("I63", "I64", "I65", "I66", "I67", "I69") ~ "cidgrupo12",
    #            
    #            DIAG_PRINC %in% c("E100", "E101", "E110", "E111", "E140", "E141", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109",
    #                              "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E121", "E125", "E126", "E127", "E128", "E129", "E130", "E131",
    #                              "E132", "E133", "E135", "E136", "E137", "E138", "E139", "E142", "E143", "E144", "E145", "E146", "E147", "E148", "E149") ~ "cidgrupo13",
    #            
    #            DIAG_PRINC %in% c("G400", "G401", "G402", "G403", "G404", "G405", "G406", "G407", "G408", "G409", "G41") ~ "cidgrupo14",
    #            
    #            DIAG_PRINC %in% c("N10", "N11", "N12", "N300", "N301", "N302", "N303", "N304", "N308", "N309", "N340", "N341", "N342", "N343", "N390") ~ "cidgrupo15",
    #            
    #            DIAG_PRINC %in% c("A446", "L01", "L020", "L021", "L022", "L023", "L024", "L028", "L029", "L030", "L031", "L032", "L033", "L038", "L039",
    #                              "L040", "L041", "L042", "L043", "L048", "L049", "L080", "L088", "L089") ~ "cidgrupo16",
    #            
    #            DIAG_PRINC %in% c("N700", "N701", "N709", "N710", "N719", "N72", "N730", "N732", "N733", "N734", "N735", "N736", "N738", 
    #                              "N739", "N750", "N751", "N758", "N760", "N762", "N764", "N766", "N768") ~ "cidgrupo17",
    #            
    #            DIAG_PRINC %in% c("K25", "K26", "K27", "K28", "K920", "K921", "K922") ~ "cidgrupo18",
    #            
    #            DIAG_PRINC %in% c("O23", "A500", "A501", "A502", "A503", "A504", "A505", "A506", "A507", "A509", "P35") ~ "cidgrupo19",
    #            
    #            .default = "Outros") ) 
  
  variables_names <- names(data)
  #Pacote para usar o dplyr em data.table
  data <- dtplyr::lazy_dt(data)

  if (information_system == "SIH-RD") {

      #Especialidade do Leito
      if ("ESPEC" %in% variables_names) {
     
      data <- data %>% 
      #Renomear a variável espec para cod_espec. Facilitar a identificação.
      dplyr::rename(cod_espec = ESPEC) |>
          
      dplyr::mutate(cod_espec = as.character(.data$cod_espec)) %>% 
        
      dplyr::mutate(espec_leito = dplyr::case_match(.data$cod_espec, 
                                                   "01" ~ "Cirúrgico", 
                                                   "02" ~ "Obstétricos", 
                                                   "03" ~ "Clínicos", 
                                                   "04" ~ "Crônicos", 
                                                   "05" ~ "Psiquiatria", 
                                                   "06" ~ "Pneumologia sanitária (tsiologia)", 
                                                   "07" ~ "Pediátricos", 
                                                   "08" ~ "Reabilitação", 
                                                   "09" ~ "Leito Dia / Cirúrgicos", 
                                                   "10" ~ "Leito Dia / Aids", 
                                                   "11" ~ "Leito Dia / Fibrose Cística", 
                                                   "12" ~ "Leito Dia / Intercorrência Pós-Transplante", 
                                                   "13" ~ "Leito Dia / Geriatria", 
                                                   "14" ~ "Leito Dia / Saúde Mental", 
                                                   #Não consta na lista do icit.
                                                   "51" ~ "UTI II Adulto COVID 19", 
                                                   "52" ~ "UTI II Pediátrica COVID 19", 
                                                   "64" ~ "Unidade Intermediária", 
                                                   "65" ~ "Unidade Intermediária Neonatal", 
                                                   "74" ~ "UTI I",
                                                   "75" ~ "UTI Adulto II", 
                                                   "76" ~ "UTI Adulto III", 
                                                   "77" ~ "UTI Infantil I", 
                                                   "78" ~ "UTI Infantil II", 
                                                   "79" ~ "UTI Infantil III", 
                                                   "80" ~ "UTI Neonatal I", 
                                                   "81" ~ "UTI Neonatal II", 
                                                   "82" ~ "UTI Neonatal III",
                                                   "83" ~ "UTI Queimados", 
                                                   "84" ~ "Acolhimento Noturno", "85" ~ "UTI Coronariana-UCO tipo II", 
                                                   "86" ~ "UTI Coronariana-UCO tipo III", 
                                                   "87" ~ "Saúde Mental (Clínico)", 
                                                   "88" ~ "Queimado Adulto (Clínico)", 
                                                   "89" ~ "Queimado Pediátrico (Clínico)", 
                                                   "90" ~ "Queimado Adulto (Cirúrgico)", 
                                                   "91" ~ "Queimado Pediátrico (Cirúrgico)", 
                                                   "92" ~ "UCI Unidade de Cuidados Intermediarios Neonatal Convencional", 
                                                   "93" ~ "UCI Unidade de Cuidados Intermediarios Neonatal Canguru", 
                                                   "94" ~ "UCI Unidade de Cuidados Intermediarios Pediatrico", 
                                                   "95" ~ "UCI Unidade de Cuidados Intermediarios Adulto", 
                                                   "96" ~ "Suporte Ventilatório Pulmonar COVID-19", 
                                                   .default = .data$cod_espec)) %>% dplyr::mutate(cod_espec = as.factor(.data$cod_espec))
      }
    
    if ("IDENT" %in% variables_names) {
      data <- data %>% dplyr::mutate(IDENT = as.character(.data$IDENT)) %>% 
        dplyr::mutate(IDENT = dplyr::case_match(.data$IDENT, 
                                                "1" ~ "Principal", "3" ~ "Continuação", 
                                                "5" ~ "Longa permanência", .default = .data$IDENT)) %>% 
        dplyr::mutate(IDENT = as.factor(.data$IDENT))
    }
    
    # # if ("MUNIC_RES" %in% variables_names & municipality_data ==  TRUE) {
    #    colnames(tabMun)[1] <- "MUNIC_RES"
    #    data <- data %>% dplyr::mutate(MUNIC_RES = as.numeric(.data$MUNIC_RES)) %>% 
    #      dplyr::left_join(tabMun, by = "MUNIC_RES")
    # # }
    # 
    
    
    
    #Data de nascimento
    if ("NASC" %in% variables_names) {
         # data <- data %>% dplyr::mutate(NASC = as.Date(.data$NASC,format = "%Y%m%d")) |> rename(dt_nasc = NASC)
     
     data <- data %>% dplyr::mutate(NASC = ymd(.data$NASC) ) |> 
       rename(dt_nasc = NASC)
      
    }
    
    #Data de internação
    if ("DT_INTER" %in% variables_names) {
      
      data <- data %>% dplyr::mutate(
      DT_INTER = ymd(.data$DT_INTER),
      ano_inter = year(DT_INTER),   
      mes_inter = month(DT_INTER),
      dwk_inter = lubridate::wday(DT_INTER, label = TRUE), #Dia da semana da interenação
      ano_mes_inter = zoo::as.yearmon(DT_INTER,"%Y%b") )  #Data da internação mensal.
      
     }
    
    #Data de saída.
    if ("DT_SAIDA" %in% variables_names) {
      data <- data %>% dplyr::mutate(
      DT_SAIDA = ymd(.data$DT_SAIDA),
      ano_saida = year(DT_SAIDA) |> as_factor(),
      mes_saida = month(DT_SAIDA) |> as_factor(),
      dwk_saida = lubridate::wday(DT_SAIDA, label = TRUE),
      dtm_saida = zoo::as.yearmon(DT_SAIDA,"%Y%b") ) 
      
    }
    
    #Sexo da paciente
    if ("SEXO" %in% variables_names) {
      data <- data %>% dplyr::mutate(SEXO = as.character(.data$SEXO)) %>% 
        dplyr::mutate(SEXO = dplyr::case_match(
        .data$SEXO, "1" ~ "Masculino", "2" ~ "Feminino", 
        "3" ~ "Feminino", "0" ~ NA, "9" ~ NA, .default = .data$SEXO)) %>% 
        dplyr::mutate(SEXO = as.factor(.data$SEXO))
      
    }
    
    #Dias de UTI no mês em que se iniciou a internação em UTI. Zerado em 2008. Não é mais utilizado.
    if ("UTI_MES_IN" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_MES_IN = as.numeric(.data$UTI_MES_IN))
    }
    
    #Dias na UTI no mês anterior ao da alta. Zerado em 2008. Não é mais utilizado.
    if ("UTI_MES_AN" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_MES_AN = as.numeric(.data$UTI_MES_AN))
    }
    #Dias na UTI no mês da alta. Zerado em 2008. Não é mais utilizado.
    if ("UTI_MES_AL" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_MES_AL = as.numeric(.data$UTI_MES_AL))
    }
    
    #Total de dias de UTI durante a internação.
    if ("UTI_MES_TO" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_MES_TO = as.numeric(.data$UTI_MES_TO))
    }
    
    #Tipo de UTI utilizado pelo paciente desta AIH
    if ("MARCA_UTI" %in% variables_names) {
      data <- data %>% dplyr::mutate(MARCA_UTI = as.character(.data$MARCA_UTI)) %>% 
        dplyr::mutate(MARCA_UTI = dplyr::case_match(.data$MARCA_UTI, 
                                                    "00" ~ "Não utilizou UTI", 
                                                    "01" ~ "Utilizou mais de um tipo de UTI", 
                                                    "51" ~ "UTI adulto - tipo II COVID 19", 
                                                    "52" ~ "UTI pediátrica - tipo II COVID 19", 
                                                    "74" ~ "UTI adulto - tipo I", 
                                                    "75" ~ "UTI adulto - tipo II", 
                                                    "76" ~ "UTI adulto - tipo III", 
                                                    "77" ~ "UTI infantil - tipo I", 
                                                    "78" ~ "UTI infantil - tipo II",
                                                    "79" ~ "UTI infantil - tipo III", 
                                                    "80" ~ "UTI neonatal - tipo I", 
                                                    "81" ~ "UTI neonatal - tipo II", 
                                                    "82" ~ "UTI neonatal - tipo III", 
                                                    "83" ~ "UTI de queimados", 
                                                    "85" ~ "UTI coronariana tipo II - UCO tipo II", 
                                                    "86" ~ "UTI coronariana tipo III - UCO tipo III", 
                                                    "99" ~ "UTI Doador", 
                                                    .default = .data$MARCA_UTI)) %>% dplyr::mutate(MARCA_UTI = as.factor(.data$MARCA_UTI))
    }
    
    #Dias de UTI no mês em que se iniciou a internação em UTI intermediária. Zerada em 2008
    if ("UTI_INT_IN" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_INT_IN = as.numeric(.data$UTI_INT_IN))
    }
    #Dias na UTI no mês anterior ao da alta. Zerada em 2008.
    if ("UTI_INT_AN" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_INT_AN = as.numeric(.data$UTI_INT_AN))
    }
    #Dias na UTI intermediária no mês da alta. Zerada em 2008.
    if ("UTI_INT_AL" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_INT_AL = as.numeric(.data$UTI_INT_AL))
    }
    
    #Quantidade de diárias em UTI intermediária.
    if ("UTI_INT_TO" %in% variables_names) {
      data <- data %>% dplyr::mutate(UTI_INT_TO = as.numeric(.data$UTI_INT_TO))
    }
    
    #Quantidade de diárias de acompanhante
    if ("DIAR_ACOM" %in% variables_names) {
      data <- data %>% dplyr::mutate(DIAR_ACOM = as.numeric(.data$DIAR_ACOM))
    }
    
    #Quantidade de diárias do paciente
    if ("QT_DIARIAS" %in% variables_names) {
      data <- data %>% dplyr::mutate(QT_DIARIAS = as.numeric(.data$QT_DIARIAS))
    }
    
    #Valor de serviços hospitalares.
    if ("VAL_SH" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SH = as.numeric(.data$VAL_SH))
    }
    
    #Valor de serviços prestados por terceiros.
    if ("VAL_SP" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SP = as.numeric(.data$VAL_SP))
    }
    
    #Valor de SADT. Zerada em 2008.
    if ("VAL_SADT" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SADT = as.numeric(.data$VAL_SADT))
    }
    
    #Valor de recém-nato. Zerada em 2008.
    if ("VAL_RN" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_RN = as.numeric(.data$VAL_RN))
    }
    
    #Valor das diárias de acompanhante. Zerada em 2008.
    if ("VAL_ACOMP" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_ACOMP = as.numeric(.data$VAL_ACOMP))
    }
    
    #Valor de órtese e prótese. Zerada em 2008.
    if ("VAL_ORTP" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_ORTP = as.numeric(.data$VAL_ORTP))
    }
    
    #Valor de sangue. Zerada em 2008.
    if ("VAL_SANGUE" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SANGUE = as.numeric(.data$VAL_SANGUE))
    }
    
    #Valor referente a tomografias e ressonância nuclear magnética pagas diretamente a terceiros, sem rateio. Zerado em 2008.
    if ("VAL_SADTSR" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SADTSR = as.numeric(.data$VAL_SADTSR))
    }
    
    #Valor referente a transplantes (retirada de órgãos). Zerado em 2008.
    if ("VAL_TRANSP" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_TRANSP = as.numeric(.data$VAL_TRANSP))
    }
    
    #Valor de analgesia obstétrica. Zerado em 2008.
    if ("VAL_OBSANG" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_OBSANG = as.numeric(.data$VAL_OBSANG))
    }
    
    #Valor de pediatria para primeira consulta. Zerado em 2008.
    if ("VAL_PED1AC" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_PED1AC = as.numeric(.data$VAL_PED1AC))
    }
    
    #Valor total da AIH
    if ("VAL_TOT" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_TOT = as.numeric(.data$VAL_TOT))
    }
    
    #Valor referente aos gastos em UTI
    if ("VAL_UTI" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_UTI = as.numeric(.data$VAL_UTI))
    }
    
    #Valor total da AIH convertido para dólares
    if ("US_TOT" %in% variables_names) {
      data <- data %>% dplyr::mutate(US_TOT = as.numeric(.data$US_TOT))
    }
    
    #Motivo de Saída/Permanência  
    if ("COBRANCA" %in% variables_names) {
      data <- data %>% dplyr::mutate(COBRANCA = as.character(.data$COBRANCA)) %>% 
        dplyr::mutate(COBRANCA = dplyr::case_match(.data$COBRANCA,
                                                   #Em caso de dúvida sobre os códigos, olhar o manual da Paloma.
                                                   #Alguns códigos foram incluidos\retirados ao longo do tempo.
                                                   "11" ~ "Alta curado", "12" ~ "Alta melhorado", 
                                                   "14" ~ "Alta a pedido", "15" ~ "Alta com previsão de retorno p/acomp do paciente", 
                                                   "16" ~ "Alta por evasão", "18" ~ "Alta por outros motivos", 
                                                   "19" ~ "Alta de paciente agudo em psiquiatria", 
                                                   "21" ~ "Permanência por características próprias da doença", 
                                                   "22" ~ "Permanência por intercorrência", 
                                                   "23" ~ "Permanência por impossibilidade sócio-familiar", 
                                                   "24" ~ "Permanência proc doação órg, tec, cél-doador vivo", 
                                                   "25" ~ "Permanência proc doação órg, tec, cél-doador morto", 
                                                   "26" ~ "Permanência por mudança de procedimento", 
                                                   "27" ~ "Permanência por reoperação", 
                                                   "28" ~ "Permanência por outros motivos", 
                                                   "29" ~ "Transferência para internação domiciliar", 
                                                   "32" ~ "Transferência para internação domiciliar", 
                                                   "31" ~ "Transferência para outro estabelecimento", 
                                                   "41" ~ "Óbito com DO fornecida pelo médico assistente", 
                                                   "42" ~ "Óbito com DO fornecida pelo IML", 
                                                   "43" ~ "Óbito com DO fornecida pelo SVO", 
                                                   "51" ~ "Encerramento administrativo", 
                                                   "61" ~ "Alta da mãe/puérpera e do recém-nascido", 
                                                   "17" ~ "Alta da mãe/puérpera e do recém-nascido", 
                                                   "62" ~ "Alta da mãe/puérpera e permanência recém-nascido", 
                                                   "13" ~ "Alta da mãe/puérpera e permanência recém-nascido", 
                                                   "63" ~ "Alta da mãe/puérpera e óbito do recém-nascido", 
                                                   "64" ~ "Alta da mãe/puérpera com óbito fetal", 
                                                   "65" ~ "Óbito da gestante e do concepto", 
                                                   "66" ~ "Óbito da mãe/puérpera e alta do recém-nascido", 
                                                   "67" ~ "Óbito da mãe/puérpera e permanência recém-nascido", 
                                                   .default = .data$COBRANCA)) %>% dplyr::mutate(COBRANCA = as.factor(.data$COBRANCA))
    }
    
    #Natureza jurídica do hospital (com conteúdo até maio/12). 
    if ("NATUREZA" %in% variables_names) {
      data <- data %>% dplyr::mutate(NATUREZA = as.character(.data$NATUREZA)) %>% 
        dplyr::mutate(NATUREZA = dplyr::case_match(.data$NATUREZA, 
                                                   "0" ~ NA, "99" ~ NA, "10" ~ "Próprio", 
                                                   "20" ~ "Contratado", "22" ~ "Contratado optante SIMPLES", 
                                                   "30" ~ "Federal", "31" ~ "Federal Verba Própria", 
                                                   "40" ~ "Estadual", "41" ~ "Estadual Verba Própria", 
                                                   "50" ~ "Municipal", "60" ~ "Filantrópico", 
                                                   "61" ~ "Filantrópico isento tributos e contr.sociais", 
                                                   "63" ~ "Filantrópico isento IR e contr.s/lucro líquido", 
                                                   "70" ~ "Universitário Ensino", "80" ~ "Sindicato", 
                                                   "90" ~ "Universitário Pesquisas", 
                                                   "91" ~ "Univ. Pesquisas isento tributos e contr.sociais", 
                                                   "93" ~ "Univ. Pesquisas isento IR e contr.s/lucro líquido", 
                                                   "94" ~ "Universitário de ensino e pesquisa privado", 
                                                   "92" ~ "Universitário de ensino e pesquisa privado", 
                                                   .default = .data$NATUREZA)) %>% dplyr::mutate(NATUREZA = as.factor(.data$NATUREZA))
    }
    
    #Natureza Jurídica. Conforme concla. Os códigos da concla sofrem atualização. É interessante ficar de olho.
    if ("NAT_JUR" %in% variables_names) {
      data <- data %>% dplyr::mutate(NAT_JUR = as.character(.data$NAT_JUR)) %>% 
        dplyr::mutate(NAT_JUR = dplyr::case_match(.data$NAT_JUR, 
                                                  "1015" ~ "Órgão Público do Poder Executivo Federal", 
                                                  "1023" ~ "Órgão Público do Poder Exec Estadual ou Distr Fed", 
                                                  "1031" ~ "Órgão Público do Poder Executivo Municipal", 
                                                  "1040" ~ "Órgão Público do Poder Legislativo Federal", 
                                                  "1058" ~ "Órgão Público do Poder Legisl Estadual ou Dist Fed", 
                                                  "1066" ~ "Órgão Público do Poder Legislativo Municipal", 
                                                  "1074" ~ "Órgão Público do Poder Judiciário Federal", 
                                                  "1082" ~ "Órgão Público do Poder Judiciário Estadual", 
                                                  "1104" ~ "Autarquia Federal", 
                                                  "1112" ~ "Autarquia Estadual ou do Distrito Federal", 
                                                  "1120" ~ "Autarquia Municipal", 
                                                  "1139" ~ "Fundação Federal", 
                                                  "1147" ~ "Fundação Estadual ou do Distrito Federal", 
                                                  "1155" ~ "Fundação Municipal", 
                                                  "1163" ~ "Órgão Público Autônomo Federal",
                                                  "1171" ~ "Órgão Público Autônomo Estadual ou Distr Federal", 
                                                  "1180" ~ "Órgão Público Autônomo Estadual ou Distr Federal", 
                                                  "1198" ~ "Comissão Polinacional",
                                                  "1201" ~ "Fundo Público", 
                                                  "1210" ~ "Associação Pública", 
                                                  "2011" ~ "Empresa Pública", 
                                                  "2038" ~ "Sociedade de Economia Mista", 
                                                  "2046" ~ "Sociedade Anônima Aberta", 
                                                  "2054" ~ "Sociedade Anônima Fechada", 
                                                  "2062" ~ "Sociedade Empresária Limitada", 
                                                  "2070" ~ "Sociedade Empresária em Nome Coletivo", 
                                                  "2089" ~ "Sociedade Empresária em Comandita Simples", 
                                                  "2097" ~ "Sociedade Empresária em Comandita por Ações", 
                                                  "2127" ~ "Sociedade em Conta de Participação", 
                                                  "2135" ~ "Empresário (Individual)", 
                                                  "2143" ~ "Cooperativa", 
                                                  "2151" ~ "Consórcio de Sociedades", 
                                                  "2160" ~ "Grupo de Sociedades",
                                                  "2178" ~ "Estabelecimento no Brasil de Sociedade Estrangeira", 
                                                  "2194" ~ "Estab no Brasil Empr Binacional Argentina-Brasil", 
                                                  "2216" ~ "Empresa Domiciliada no Exterior", 
                                                  "2224" ~ "Clube/Fundo de Investimento", 
                                                  "2232" ~ "Sociedade Simples Pura", 
                                                  "2240" ~ "Sociedade Simples Limitada", 
                                                  "2259" ~ "Sociedade Simples em Nome Coletivo", 
                                                  "2267" ~ "Sociedade Simples em Comandita Simples", 
                                                  "2275" ~ "Empresa Binacional", 
                                                  "2283" ~ "Consórcio de Empregadores", 
                                                  "2291" ~ "Consórcio Simples",
                                                  "2305" ~ "Empr Individ Responsab Limitada (Natur Empresária)", 
                                                  "2313" ~ "Empr Individ Responsab Limitada (Natureza Simples)", 
                                                  "3034" ~ "Serviço Notarial e Registral (Cartório)", 
                                                  "3069" ~ "Fundação Privada", 
                                                  "3077" ~ "Serviço Social Autônomo", 
                                                  "3085" ~ "Condomínio Edilício", 
                                                  "3107" ~ "Comissão de Conciliação Prévia", 
                                                  "3115" ~ "Entidade de Mediação e Arbitragem", 
                                                  "3123" ~ "Partido Político", 
                                                  "3131" ~ "Entidade Sindical", 
                                                  "3204" ~ "Estab no Brasil de Fundação ou Associação Estrang", 
                                                  "3212" ~ "Fundação ou Associação Domiciliada no Exterior", 
                                                  "3220" ~ "Organização Religiosa",
                                                  "3239" ~ "Comunidade Indígena", 
                                                  "3247" ~ "Fundo Privado", 
                                                  "3999" ~ "Associação Privada", 
                                                  "4014" ~ "Empresa Individual Imobiliária", 
                                                  "4022" ~ "Segurado Especial", 
                                                  "4081" ~ "Contribuinte Individual", 
                                                  "4090" ~ "Candidato a Cargo Político Eletivo", 
                                                  "4111" ~ "Leiloeiro", 
                                                  "5010" ~ "Organização Internacional", 
                                                  "5029" ~ "Representação Diplomática Estrangeira", 
                                                  "5037" ~ "Outras Instituições Extraterritoriais", 
                                                  "0" ~ NA, .default = .data$NAT_JUR)) %>% dplyr::mutate(NAT_JUR = as.factor(.data$NAT_JUR))
    }
    
    #Indica o tipo de gestão do hospital.
    if ("GESTAO" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTAO = as.character(.data$GESTAO)) %>% 
        dplyr::mutate(GESTAO = dplyr::case_match(.data$GESTAO, 
                                                 "0" ~ "Estadual", "2" ~ "Estadual plena", 
                                                 "1" ~ "Municipal plena assist", "3" ~ NA, 
                                                 "9" ~ NA, .default = .data$GESTAO)) %>% dplyr::mutate(GESTAO = as.factor(.data$GESTAO))
    }
    
    #Rubrica referente à AIH. Zerado em 2008
    if ("RUBRICA" %in% variables_names) {
      data <- data %>% dplyr::mutate(RUBRICA = as.numeric(.data$RUBRICA))
    }
    
    
    #Indica a execução do exame VDRL
    if ("IND_VDRL" %in% variables_names) {
      data <- data %>% dplyr::mutate(IND_VDRL = as.character(.data$IND_VDRL)) %>% 
        dplyr::mutate(IND_VDRL = dplyr::case_match(.data$IND_VDRL, 
                                                   "0" ~ "Não", "1" ~ "Sim", .default = .data$IND_VDRL)) %>% 
        dplyr::mutate(IND_VDRL = as.factor(.data$IND_VDRL))
    }
    
    
    #Código do município onde se localiza o hospital
    if ("MUNIC_MOV" %in% variables_names) {
      data$MUNIC_MOV <- as.numeric(data$MUNIC_MOV)
    }
    
    
    #Código da idade
    if ("COD_IDADE" %in% variables_names) {
      data <- data %>% dplyr::mutate(COD_IDADE = as.character(.data$COD_IDADE)) %>% 
        dplyr::mutate(COD_IDADE = dplyr::case_match(.data$COD_IDADE, 
                                                    "0" ~ NA, "2" ~ "Dias", "3" ~ "Meses", "4" ~ 
                                                      "Anos", "5" ~ "Centena de anos (100 + idade)", 
                                                    .default = .data$COD_IDADE)) %>% dplyr::mutate(COD_IDADE = as.factor(.data$COD_IDADE))
    }
    
    #Idade.
    if ("IDADE" %in% variables_names) {
      data <- data %>% dplyr::mutate(IDADE = as.numeric(.data$IDADE))
    }
    
    #Dias de permanência
    if ("DIAS_PERM" %in% variables_names) {
      data <- data %>% dplyr::mutate(DIAS_PERM = as.numeric(.data$DIAS_PERM))
    }
    
    #Indica se o paciente teve saída com morte 
    if ("MORTE" %in% variables_names) {
      data <- data %>% dplyr::mutate(MORTE = as.character(.data$MORTE)) %>% 
        dplyr::mutate(MORTE = dplyr::case_match(.data$MORTE, 
                                                "0" ~ "Não", "1" ~ "Sim", .default = .data$MORTE)) %>% 
        dplyr::mutate(MORTE = as.factor(.data$MORTE))
    }
    
    #Código da nacionalidade do paciente
    if ("NACIONAL" %in% variables_names) {
      data <- data %>% dplyr::mutate(NACIONAL = as.character(.data$NACIONAL)) %>% 
        dplyr::mutate(NACIONAL = dplyr::case_match(.data$NACIONAL, 
                                                   "170" ~ "Abissinia", "171" ~ "Acores", "172" ~ 
                                                     "Afar frances", "241" ~ "Afeganistao", "93" ~ 
                                                     "Albania", "30" ~ "Alemanha", "174" ~ "Alto volta", 
                                                   "94" ~ "Andorra", "175" ~ "Angola", "334" ~ 
                                                     "Antartica francesa", "337" ~ "Antartico argentino", 
                                                   "333" ~ "Antartico britanico, territorio", 
                                                   "336" ~ "Antartico chileno", "338" ~ "Antartico noruegues", 
                                                   "28" ~ "Antigua e. dep. barbuda", "29" ~ "Antilhas holandesas", 
                                                   "339" ~ "Apatrida", "242" ~ "Arabia saudita", 
                                                   "176" ~ "Argelia", "21" ~ "Argentina", "347" ~ 
                                                     "Armenia", "289" ~ "Arquipelago de bismark", 
                                                   "175" ~ "Angola", "285" ~ "Arquipelago manahiki", 
                                                   "286" ~ "Arquipelago midway", "33" ~ "Aruba", 
                                                   "175" ~ "Angola", "198" ~ "Ascensao e tristao da cunha,is", 
                                                   "287" ~ "Ashmore e cartier", "288" ~ "Australia", 
                                                   "95" ~ "Austria", "138" ~ "Azerbaijao", "243" ~ 
                                                     "Bahrein", "342" ~ "Bangladesh", "44" ~ 
                                                     "Barbados", "139" ~ "Bashkista", "177" ~ 
                                                     "Bechuanalandia", "31" ~ "Belgica", "46" ~ 
                                                     "Belize", "178" ~ "Benin", "83" ~ "Bermudas", 
                                                   "246" ~ "Bhutan", "244" ~ "Birmania", "22" ~ 
                                                     "Bolivia", "134" ~ "Bosnia herzegovina", 
                                                   "179" ~ "Botsuana", "10" ~ "Brasil", "245" ~ 
                                                     "Brunei", "96" ~ "Bulgaria", "238" ~ "Burkina fasso", 
                                                   "180" ~ "Burundi", "141" ~ "Buryat", "343" ~ 
                                                     "Cabo verde", "181" ~ "Camaroes", "34" ~ 
                                                     "Canada", "142" ~ "Carelia", "247" ~ "Catar", 
                                                   "143" ~ "Cazaquistao", "248" ~ "Ceilao", "182" ~ 
                                                     "Ceuta e melilla", "183" ~ "Chade", "144" ~ 
                                                     "Chechen ingusth", "23" ~ "Chile", "42" ~ 
                                                     "China", "249" ~ "China (taiwan)", "97" ~ 
                                                     "Chipre", "145" ~ "Chuvash", "275" ~ "Cingapura", 
                                                   "26" ~ "Colombia", "40" ~ "Comunidade das bahamas", 
                                                   "54" ~ "Comunidade dominicana", "185" ~ "Congo", 
                                                   "43" ~ "Coreia", "186" ~ "Costa do marfim", 
                                                   "51" ~ "Costa rica", "250" ~ "Coveite", "130" ~ 
                                                     "Croacia", "52" ~ "Cuba", "53" ~ "Curacao", 
                                                   "146" ~ "Dagesta", "187" ~ "Daome", "340" ~ 
                                                     "Dependencia de ross", "98" ~ "Dinamarca", 
                                                   "188" ~ "Djibuti", "99" ~ "Eire", "251" ~ 
                                                     "Emirados arabes unidos", "27" ~ "Equador", 
                                                   "100" ~ "Escocia", "136" ~ "Eslovaquia", "132" ~ 
                                                     "Eslovenia", "35" ~ "Espanha", "129" ~ "Estado da cidade do vaticano", 
                                                   "57" ~ "Estados assoc. das antilhas", "36" ~ 
                                                     "Estados unidos da america (eua)", "147" ~ 
                                                     "Estonia", "190" ~ "Etiopia", "252" ~ "Filipinas", 
                                                   "102" ~ "Finlandia", "37" ~ "Franca", "192" ~ 
                                                     "Gambia", "193" ~ "Gana", "194" ~ "Gaza", 
                                                   "148" ~ "Georgia", "103" ~ "Gibraltar", "149" ~ 
                                                     "Gorno altai", "32" ~ "Gra-bretanha", "59" ~ 
                                                     "Granada", "104" ~ "Grecia", "84" ~ "Groenlandia", 
                                                   "292" ~ "Guam", "61" ~ "Guatemala", "87" ~ 
                                                     "Guiana francesa", "195" ~ "Guine", "344" ~ 
                                                     "Guine bissau", "196" ~ "Guine equatorial", 
                                                   "105" ~ "Holanda", "64" ~ "Honduras", "63" ~ 
                                                     "Honduras britanicas", "253" ~ "Hong-kong", 
                                                   "106" ~ "Hungria", "254" ~ "Iemen", "345" ~ 
                                                     "Iemen do sul", "197" ~ "Ifni", "300" ~ 
                                                     "Ilha johnston e sand", "69" ~ "Ilha milhos", 
                                                   "293" ~ "Ilhas baker", "107" ~ "Ilhas baleares", 
                                                   "199" ~ "Ilhas canarias", "294" ~ "Ilhas cantao e enderburg", 
                                                   "295" ~ "Ilhas carolinas", "297" ~ "Ilhas christmas", 
                                                   "184" ~ "Ilhas comores", "290" ~ "Ilhas cook", 
                                                   "108" ~ "Ilhas cosmoledo (lomores)", "117" ~ 
                                                     "Ilhas de man", "109" ~ "Ilhas do canal", 
                                                   "296" ~ "Ilhas do pacifico", "58" ~ "Ilhas falklands", 
                                                   "101" ~ "Ilhas faroes", "298" ~ "Ilhas gilbert", 
                                                   "60" ~ "Ilhas guadalupe", "299" ~ "Ilhas howland e jarvis", 
                                                   "301" ~ "Ilhas kingman reef", "305" ~ "Ilhas macdonal e heard", 
                                                   "302" ~ "Ilhas macquaire", "67" ~ "Ilhas malvinas", 
                                                   "303" ~ "Ilhas marianas", "304" ~ "Ilhas marshall", 
                                                   "306" ~ "Ilhas niue", "307" ~ "Ilhas norfolk", 
                                                   "315" ~ "Ilhas nova caledonia", "318" ~ "Ilhas novas hebridas", 
                                                   "308" ~ "Ilhas palau", "320" ~ "Ilhas pascoa", 
                                                   "321" ~ "Ilhas pitcairin", "309" ~ "Ilhas salomao", 
                                                   "326" ~ "Ilhas santa cruz", "65" ~ "Ilhas serranas", 
                                                   "310" ~ "Ilhas tokelau", "80" ~ "Ilhas turca", 
                                                   "47" ~ "Ilhas turks e caicos", "82" ~ "Ilhas virgens americanas", 
                                                   "81" ~ "Ilhas virgens britanicas", "311" ~ 
                                                     "Ilhas wake", "332" ~ "Ilhas wallis e futuna", 
                                                   "255" ~ "India", "256" ~ "Indonesia", "110" ~ 
                                                     "Inglaterra", "257" ~ "Ira", "258" ~ "Iraque", 
                                                   "112" ~ "Irlanda", "111" ~ "Irlanda do norte", 
                                                   "113" ~ "Islandia", "259" ~ "Israel", "39" ~ 
                                                     "Italia", "114" ~ "Iugoslavia", "66" ~ "Jamaica", 
                                                   "41" ~ "Japao", "260" ~ "Jordania", "150" ~ 
                                                     "Kabardino balkar", "312" ~ "Kalimatan", 
                                                   "151" ~ "Kalmir", "346" ~ "Kara kalpak", "152" ~ 
                                                     "Karachaevocherkess", "153" ~ "Khakass", 
                                                   "261" ~ "Kmer/camboja", "154" ~ "Komi", "262" ~ 
                                                     "Kuwait", "263" ~ "Laos", "200" ~ "Lesoto", 
                                                   "155" ~ "Letonia", "264" ~ "Libano", "201" ~ 
                                                     "Liberia", "202" ~ "Libia", "115" ~ "Liechtenstein", 
                                                   "156" ~ "Lituania", "116" ~ "Luxemburgo", 
                                                   "265" ~ "Macau", "205" ~ "Madagascar", "203" ~ 
                                                     "Madeira", "266" ~ "Malasia", "204" ~ "Malawi", 
                                                   "267" ~ "Maldivas,is", "206" ~ "Mali", "157" ~ 
                                                     "Mari", "207" ~ "Marrocos", "68" ~ "Martinica", 
                                                   "268" ~ "Mascate", "208" ~ "Mauricio", "209" ~ 
                                                     "Mauritania", "85" ~ "Mexico", "284" ~ "Mianma", 
                                                   "210" ~ "Mocambique", "158" ~ "Moldavia", 
                                                   "118" ~ "Monaco", "269" ~ "Mongolia", "70" ~ 
                                                     "Monte serrat", "137" ~ "Montenegro", "240" ~ 
                                                     "Namibia", "314" ~ "Nauru", "270" ~ "Nepal", 
                                                   "211" ~ "Nguane", "71" ~ "Nicaragua", "213" ~ 
                                                     "Nigeria", "119" ~ "Noruega", "316" ~ "Nova guine", 
                                                   "317" ~ "Nova zelandia", "271" ~ "Oman", "159" ~ 
                                                     "Ossetia setentrional", "121" ~ "Pais de gales", 
                                                   "122" ~ "Paises baixos", "272" ~ "Palestina", 
                                                   "72" ~ "Panama", "73" ~ "Panama(zona do canal)", 
                                                   "214" ~ "Papua nova guine", "273" ~ "Paquistao", 
                                                   "24" ~ "Paraguai", "89" ~ "Peru", "322" ~ 
                                                     "Polinesia francesa", "123" ~ "Polonia", 
                                                   "74" ~ "Porto rico", "45" ~ "Portugal", "215" ~ 
                                                     "Pracas norte africanas", "216" ~ "Protetor do sudoeste africano", 
                                                   "217" ~ "Quenia", "160" ~ "Quirguistao", "75" ~ 
                                                     "Quitasueno", "189" ~ "Republica arabe do egito", 
                                                   "218" ~ "Republica centro africana", "173" ~ 
                                                     "Republica da africa do sul", "140" ~ "Republica da bielorrussia", 
                                                   "133" ~ "Republica da macedonia", "56" ~ "Republica de el salvador", 
                                                   "291" ~ "Republica de fiji", "120" ~ "Republica de malta", 
                                                   "191" ~ "Republica do gabao", "62" ~ "Republica do haiti", 
                                                   "212" ~ "Republica do niger", "55" ~ "Republica dominicana", 
                                                   "88" ~ "Republica guiana", "135" ~ "Republica tcheca", 
                                                   "20" ~ "Reservado", "48" ~ "Reservado", "49" ~ 
                                                     "Reservado", "50" ~ "Reservado", "219" ~ 
                                                     "Reuniao", "220" ~ "Rodesia (zimbabwe)", 
                                                   "124" ~ "Romenia", "76" ~ "Roncador", "221" ~ 
                                                     "Ruanda", "274" ~ "Ruiquiu,is", "348" ~ 
                                                     "Russia", "222" ~ "Saara espanhol", "323" ~ 
                                                     "Sabah", "324" ~ "Samoa americana", "325" ~ 
                                                     "Samoa ocidental", "125" ~ "San marino", 
                                                   "223" ~ "Santa helena", "77" ~ "Santa lucia", 
                                                   "78" ~ "Sao cristovao", "224" ~ "Sao tome e principe", 
                                                   "79" ~ "Sao vicente", "327" ~ "Sarawak", "349" ~ 
                                                     "Senegal", "276" ~ "Sequin", "226" ~ "Serra leoa", 
                                                   "131" ~ "Servia", "225" ~ "Seychelles", "277" ~ 
                                                     "Siria", "227" ~ "Somalia, republica", "278" ~ 
                                                     "Sri-lanka", "86" ~ "St. pierre et miquelon", 
                                                   "228" ~ "Suazilandia", "229" ~ "Sudao", "126" ~ 
                                                     "Suecia", "38" ~ "Suica", "90" ~ "Suriname", 
                                                   "127" ~ "Svalbard e jan mayer,is", "161" ~ 
                                                     "Tadjiquistao", "279" ~ "Tailandia", "230" ~ 
                                                     "Tanganica", "350" ~ "Tanzania", "162" ~ 
                                                     "Tartaria", "128" ~ "Tchecoslovaquia", "335" ~ 
                                                     "Terr. antartico da australia", "341" ~ 
                                                     "Terras austrais", "231" ~ "Territ. britanico do oceano indico", 
                                                   "328" ~ "Territorio de cocos", "319" ~ "Territorio de papua", 
                                                   "329" ~ "Timor", "233" ~ "Togo", "330" ~ "Tonga", 
                                                   "232" ~ "Transkei", "280" ~ "Tregua, estado", 
                                                   "91" ~ "Trinidad e tobago", "234" ~ "Tunisia", 
                                                   "163" ~ "Turcomenistao", "281" ~ "Turquia", 
                                                   "331" ~ "Tuvalu", "164" ~ "Tuvin", "165" ~ 
                                                     "Ucrania", "166" ~ "Udmurt", "235" ~ "Uganda", 
                                                   "167" ~ "Uniao sovietica", "25" ~ "Uruguai", 
                                                   "168" ~ "Uzbequistao", "92" ~ "Venezuela", 
                                                   "282" ~ "Vietna do norte", "283" ~ "Vietna do sul", 
                                                   "169" ~ "Yakut", "236" ~ "Zaire", "237" ~ 
                                                     "Zambia", "239" ~ "Zimbabwe", .default = .data$NACIONAL)) %>% 
        dplyr::mutate(NACIONAL = as.factor(.data$NACIONAL))
    }
    
    #Número do processamento. Zerado
    if ("NUM_PROC" %in% variables_names) {
      data <- data %>% dplyr::mutate(NUM_PROC = as.numeric(.data$NUM_PROC))
    }
    
    #Caratér da internação.
    if ("CAR_INT" %in% variables_names) {
      data <- data %>% dplyr::mutate(CAR_INT = as.character(.data$CAR_INT)) %>% 
        dplyr::mutate(CAR_INT = dplyr::case_match(.data$CAR_INT, 
                                                  "1" ~ "Eletivo", "2" ~ "Urgência", 
                                                  "3" ~ "Acidente no local trabalho ou a serv da empresa", 
                                                  "4" ~ "Acidente no trajeto para o trabalho", 
                                                  "5" ~ "Outros tipo de acidente de trânsito", 
                                                  "6" ~ "Out tp lesões e envenen por agent quím físicos", 
                                                  .default = .data$CAR_INT)) %>% dplyr::mutate(CAR_INT = as.factor(.data$CAR_INT))
    }
    
    #Número de pontos de serviços profissionais nesta AIH. Zerada em 2008.
    if ("TOT_PT_SP" %in% variables_names) {
      data <- data %>% dplyr::mutate(TOT_PT_SP = as.numeric(.data$TOT_PT_SP))
    }
    
    #CPF do auditor que autorizou o pagamento da AIH, em caso de homônimos.
    if ("CPF_AUT" %in% variables_names) {
      data <- data %>% dplyr::mutate(CPF_AUT = as.numeric(.data$CPF_AUT))
    }
    
    #Indica se o paciente da AIH é homônimo de paciente de outra AIH.
    if ("HOMONIMO" %in% variables_names) {
      data <- data %>% dplyr::mutate(HOMONIMO = as.character(.data$HOMONIMO)) %>% 
        dplyr::mutate(HOMONIMO = dplyr::case_match(.data$HOMONIMO, 
                                                   "0" ~ "Não", "1" ~ "Sim", .default = .data$HOMONIMO)) %>% 
        dplyr::mutate(HOMONIMO = as.factor(.data$HOMONIMO))
    }
    
    #Número de filhos do paciente
    if ("NUM_FILHOS" %in% variables_names) {
      data <- data %>% dplyr::mutate(NUM_FILHOS = as.numeric(.data$NUM_FILHOS))
    }
    
    #Escolaridade
    if ("INSTRU" %in% variables_names) {
      data <- data %>% dplyr::mutate(INSTRU = as.character(.data$INSTRU)) %>% 
        dplyr::mutate(INSTRU = dplyr::case_match(.data$INSTRU, 
                                                 "1" ~ "Analfabeto", "2" ~ "1º grau", 
                                                 "3" ~ "2º grau", "4" ~ "3º grau", 
                                                 "0" ~ NA, "9" ~ NA, 
                                                 .default = .data$INSTRU)) %>% dplyr::mutate(INSTRU = as.factor(.data$INSTRU))
    }
    
    #Tipo de contraceptivo utilizado
    if ("CONTRACEP1" %in% variables_names) {
      data <- data %>% dplyr::mutate(CONTRACEP1 = as.character(.data$CONTRACEP1)) %>% 
        dplyr::mutate(CONTRACEP1 = dplyr::case_match(.data$CONTRACEP1, 
                                                     "01" ~ "LAM", "02" ~ "Ogino Kaus", "03" ~ "Temperatura basal", 
                                                     "04" ~ "Billings", "05" ~ "Cinto térmico", 
                                                     "06" ~ "DIU", "07" ~ "Diafragma", "08" ~ "Preservativo", 
                                                     "09" ~ "Espermicida", "10" ~ "Hormônio oral", 
                                                     "11" ~ "Hormônio injetável", "12" ~ "Coito interrompido",
                                                     "13" ~ "Label não definido",
                                                     "0" ~ NA, "99" ~ NA, .default = .data$CONTRACEP1)) %>% 
        dplyr::mutate(CONTRACEP1 = as.factor(.data$CONTRACEP1))
    }
    
    #Segundo tipo de contraceptivo utilizado 
    if ("CONTRACEP2" %in% variables_names) {
      data <- data %>% dplyr::mutate(CONTRACEP2 = as.character(.data$CONTRACEP2)) %>% 
        dplyr::mutate(CONTRACEP2 = dplyr::case_match(.data$CONTRACEP2, 
                                                     "01" ~ "LAM", "02" ~ "Ogino Kaus", "03" ~ "Temperatura basal", 
                                                     "04" ~ "Billings", "05" ~ "Cinto térmico", 
                                                     "06" ~ "DIU", "07" ~ "Diafragma", "08" ~ "Preservativo", 
                                                     "09" ~ "Espermicida", "10" ~ "Hormônio oral", 
                                                     "11" ~ "Hormônio injetável", "12" ~ "Coito interrompido",
                                                     "13" ~ "Label não definido",
                                                     "0" ~ NA, "99" ~ NA, .default = .data$CONTRACEP2)) %>% 
        dplyr::mutate(CONTRACEP2 = as.factor(.data$CONTRACEP2))
    }
    
    #Indica se é gestante de risco.
    if ("GESTRISCO" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTRISCO = as.character(.data$GESTRISCO)) %>% 
        dplyr::mutate(GESTRISCO = dplyr::case_match(.data$GESTRISCO, 
                                                    "0" ~ "Não", "1" ~ "Sim", .default = .data$GESTRISCO)) %>% 
        dplyr::mutate(GESTRISCO = as.factor(.data$GESTRISCO))
    }
    
    
    #Sequencial de longa permanência (AIH tipo 5) 
    if ("SEQ_AIH5" %in% variables_names) {
      data <- data %>% dplyr::mutate(SEQ_AIH5 = as.character(.data$SEQ_AIH5)) %>% 
        dplyr::mutate(SEQ_AIH5 = dplyr::case_match(.data$SEQ_AIH5, 
                                                   "0" ~ "Sequencial zerado", "1" ~ "Seq 1", 
                                                   "2" ~ "Seq 2", "3" ~ "Seq 3", "4" ~ NA, "999" ~ 
                                                     NA, .default = .data$SEQ_AIH5)) %>% dplyr::mutate(SEQ_AIH5 = as.factor(.data$SEQ_AIH5))
    }
    
    #Ocupação do paciente, segundo a tabela da CBO.
    if ("CBOR" %in% variables_names) {
      colnames(tabCBO)[1] <- "CBOR"
      data$CBOR <- factor(dplyr::left_join(data, tabCBO, 
                                           by = "CBOR")$nome)
    }
    
    #Vínculo com a Previdência em relação à atividade formal.
    if ("VINCPREV" %in% variables_names) {
      data <- data %>% dplyr::mutate(VINCPREV = as.character(.data$VINCPREV)) %>% 
        dplyr::mutate(VINCPREV = dplyr::case_match(.data$VINCPREV, 
                                                   "1" ~ "Autônomo", "2" ~ "Desempregado",
                                                   "3" ~ "Aposentado", "4" ~ "Não segurado", 
                                                   "5" ~ "Empregado", "6" ~ "Empregador", "0" ~ "Label não definido", 
                                                   "9" ~ "Label não definido", .default = .data$VINCPREV)) %>% 
        dplyr::mutate(VINCPREV = as.factor(.data$VINCPREV))
    }
    
    
    #Motivo de autorização da AIH pelo gestor
    if ("GESTOR_COD" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTOR_COD = as.character(.data$GESTOR_COD)) %>% 
        dplyr::mutate(GESTOR_COD = dplyr::case_match(.data$GESTOR_COD, 
                                                     "1" ~ "TEMPO DE PERMANENCIA", "2" ~ "IDADE MENOR", 
                                                     "3" ~ "IDADE MAIOR", "4" ~ "TEMPO DE PERMANENCIA E IDADE", 
                                                     "5" ~ "QUANTIDADE MAXIMA", "7" ~ "PERM.MENOR", 
                                                     "8" ~ "ID.MENOR", "9" ~ "ID.MENOR E PERM.MENOR", 
                                                     "10" ~ "ID.MAIOR", "11" ~ "ID.MAIOR E PERM.MENOR", 
                                                     "14" ~ "QTD", "15" ~ "QTD E PERM.MENOR", 
                                                     "16" ~ "QTD E ID.MENOR", "17" ~ "QTD E ID.MENOR E PERM.MENOR", 
                                                     "18" ~ "QTD E ID.MAIOR", "19" ~ "QTD E ID.MAIOR E PERM.MENOR", 
                                                     "38" ~ "CBO",
                                                     "39" ~ "CBO E PERM.MENOR", 
                                                     "40" ~ "CBO E ID.MENOR", 
                                                     "41" ~ "CBO E ID.MENOR E PERM.MENOR", 
                                                     "42" ~ "CBO E ID.MAIOR", 
                                                     "43" ~ "CBO E ID.MAIOR E PERM.MENOR", 
                                                     "46" ~ "CBO E QTD", 
                                                     "47" ~ "CBO E QTD E PERM.MENOR", 
                                                     "48" ~ "CBO E QTD E ID.MENOR", 
                                                     "49" ~ "CBO E QTD E ID.MENOR E PERM.MENOR", 
                                                     "50" ~ "CBO E QTD E ID.MAIOR", 
                                                     "51" ~ "CBO E QTD E ID.MAIOR E PERM.MENOR", 
                                                     "70" ~ "TELEFONE", 
                                                     "71" ~ "TELEFONE E PERM.MENOR", 
                                                     "72" ~ "TELEFONE E ID.MENOR", 
                                                     "73" ~ "TELEFONE E ID.MENOR E PERM.MENOR", 
                                                     "74" ~ "TELEFONE E ID.MAIOR", 
                                                     "75" ~ "TELEFONE E ID.MAIOR E PERM.MENOR", 
                                                     "78" ~ "TELEFONE E QTD",
                                                     "79" ~ "TELEFONE E QTD E PERM.MENOR", 
                                                     "80" ~ "TELEFONE E QTD E ID.MENOR", 
                                                     "81" ~ "TELEFONE E QTD E ID.MENOR E PERM.MENOR", 
                                                     "82" ~ "TELEFONE E QTD E ID.MAIOR", 
                                                     "83" ~ "TELEFONE E QTD E ID.MAIOR E PERM.MENOR", 
                                                     "102" ~ "TELEFONE E CBO", 
                                                     "103" ~ "TELEFONE E CBO E PERM.MENOR", 
                                                     "104" ~ "TELEFONE E CBO E ID.MENOR", 
                                                     "105" ~ "TELEFONE E CBO E ID.MENOR E PERM.MENOR", 
                                                     "106" ~ "TELEFONE E CBO E ID.MAIOR", 
                                                     "107" ~ "TELEFONE E CBO E ID.MAIOR E PERM.MENOR", 
                                                     "110" ~ "TELEFONE E CBO E QTD",
                                                     "111" ~ "TELEFONE E CBO E QTD E PERM.MENOR", 
                                                     "112" ~ "TELEFONE E CBO E QTD E ID.MENOR", 
                                                     "113" ~ "TELEFONE E CBO E QTD E ID.MENOR E PERM.MENOR", 
                                                     "114" ~ "TELEFONE E CBO E QTD E ID.MAIOR", 
                                                     "115" ~ "TELEFONE E CBO E QTD E ID.MAIOR E PERM.MENOR", 
                                                     "134" ~ "CNS",
                                                     "136" ~ "CNS E ID. MENOR", 
                                                     "137" ~ "CNS E ID. MENOR E PERM. MENOR", 
                                                     "138" ~ "CNS E ID. MAIOR", 
                                                     "139" ~ "CNS E ID. MAIOR E PERM. MENOR", 
                                                     "142" ~ "CNS E QTD",
                                                     "143" ~ "CNS E QTD E PERM. MENOR", 
                                                     "144" ~ "CNS E QTD E ID. MENOR", 
                                                     "145" ~ "CNS E QTD E ID. MENOR E PERM. MENOR", 
                                                     "146" ~ "CNS E QTD E ID. MAIOR", 
                                                     "147" ~ "CNS E QTD E ID. MAIOR E PERM. MENOR", 
                                                     "166" ~ "CNS E CBO", 
                                                     "167" ~ "CNS E CBO E PERM. MENOR", 
                                                     "168" ~ "CNS E CBO E ID. MENOR", 
                                                     "169" ~ "CNS E CBO E ID. MENOR E PERM. MENOR", 
                                                     "170" ~ "CNS E CBO E ID. MAIOR", 
                                                     "171" ~ "CNS E CBO E ID. MAIOR E PERM. MENOR", 
                                                     "174" ~ "CNS E CBO E QTD", 
                                                     "175" ~ "CNS E CBO E QTD E PERM. MENOR", 
                                                     "176" ~ "CNS E CBO E QTD E ID. MENOR", 
                                                     "177" ~ "CNS E CBO E QTD E ID. MENOR E PERM. MENOR", 
                                                     "178" ~ "CNS E CBO E QTD E ID. MAIOR", 
                                                     "179" ~ "CNS E CBO E QTD E ID. MAIOR E PERM. MENOR", 
                                                     "198" ~ "CNS E TELEFONE", 
                                                     "199" ~ "CNS E TELEFONE E PERM. MENOR", 
                                                     "200" ~ "CNS E TELEFONE E ID. MENOR", 
                                                     "201" ~ "CNS E TELEFONE E ID. MENOR E PERM. MENOR", 
                                                     "202" ~ "CNS E TELEFONE E ID. MAIOR", 
                                                     "203" ~ "CNS E TELEFONE E ID. MAIOR E PERM. MENOR", 
                                                     "206" ~ "CNS E TELEFONE E QTD",
                                                     "207" ~ "CNS E TELEFONE E QTD E PERM. MENOR", 
                                                     "208" ~ "CNS E TELEFONE E QTD E ID. MENOR", 
                                                     "209" ~ "CNS E TELEFONE E QTD E ID. MENOR E PERM. MENOR", 
                                                     "210" ~ "CNS E TELEFONE E QTD E ID. MAIOR", 
                                                     "211" ~ "CNS E TELEFONE E QTD E ID. MAIOR E PERM. MENOR", 
                                                     "230" ~ "CNS E TELEFONE E CBO", 
                                                     "231" ~ "CNS E TELEFONE E CBO E PERM. MENOR", 
                                                     "232" ~ "CNS E TELEFONE E CBO E ID. MENOR", 
                                                     "233" ~ "CNS E TELEFONE E CBO E ID. MENOR E PERM. MENOR", 
                                                     "234" ~ "CNS E TELEFONE E CBO E ID. MAIOR", 
                                                     "235" ~ "CNS E TELEFONE E CBO E ID. MAIOR E PERM. MENOR", 
                                                     "238" ~ "CNS E TELEFONE E CBO E QTD", 
                                                     "239" ~ "CNS E TELEFONE E CBO E QTD E PERM. MENOR", 
                                                     "240" ~ "CNS E TELEFONE E CBO E QTD E ID. MENOR", 
                                                     "241" ~ "CNS E TELEFONE E CBO E QTD E ID. MENOR E PERM. MENOR", 
                                                     "242" ~ "CNS E TELEFONE E CBO E QTD E ID. MAIOR", 
                                                     "243" ~ "CNS E TELEFONE E CBO E QTD E ID. MAIOR E PERM. MENOR", 
                                                     .default = .data$GESTOR_COD)) %>% dplyr::mutate(GESTOR_COD = as.factor(.data$GESTOR_COD))
      
    #Tipo de Gestor.  
    }
    if ("GESTOR_TP" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTOR_TP = as.numeric(.data$GESTOR_TP))
    }
    
    
    if ("GESTOR_CPF" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTOR_CPF = as.numeric(.data$GESTOR_CPF))
    }
    
    #Data da autorização dada pelo gestor  no formato aaaammdd. Zero em 2008
    if ("GESTOR_DT" %in% variables_names) {
      data <- data %>% dplyr::mutate(GESTOR_DT = as.numeric(.data$GESTOR_DT))
    }
    
    #Status de infecção hospitalar. Zerado em 2008
    if ("INFEHOSP" %in% variables_names) {
      data <- data %>% dplyr::mutate(INFEHOSP = as.character(.data$INFEHOSP)) %>% 
        dplyr::mutate(INFEHOSP = dplyr::case_match(.data$INFEHOSP, 
                                                   "0" ~ "Não", "1" ~ "Sim", .default = .data$INFEHOSP)) %>% 
        dplyr::mutate(INFEHOSP = as.factor(.data$INFEHOSP))
    }
    
    
    #Complexidade.
    if ("COMPLEX" %in% variables_names) {
      data <- data %>% dplyr::mutate(COMPLEX = as.character(.data$COMPLEX)) %>% 
        dplyr::mutate(COMPLEX = dplyr::case_match(.data$COMPLEX, 
                                                  "01" ~ "Atenção Básica", "02" ~ "Média complexidade", 
                                                  "03" ~ "Alta complexidade", "0" ~ "Label não definido",
                                                  "99" ~ "Label não definido", .default = .data$COMPLEX)) %>% 
        dplyr::mutate(COMPLEX = as.factor(.data$COMPLEX))
    }
    
    #Tipo de financiamento
    if ("FINANC" %in% variables_names) {
      data <- data %>% dplyr::mutate(FINANC = as.character(.data$FINANC)) %>% 
        dplyr::mutate(FINANC = dplyr::case_match(.data$FINANC, 
                                                 "01" ~ "Atenção Básica (PAB)", 
                                                 "02" ~ "Assistência Farmacêutica", 
                                                 "04" ~ "Fundo de Ações Estratégicas e Compensações FAEC", 
                                                 "05" ~ "Incentivo - MAC", 
                                                 "06" ~ "Média e Alta Complexidade (MAC)", 
                                                 "07" ~ "Vigilância em Saúde", 
                                                 "0" ~ "Label não definido", "99" ~ "Label não definido",
                                                 .default = .data$FINANC)) %>% dplyr::mutate(FINANC = as.factor(.data$FINANC))
    }
    
    #Subtipo de financiamento FAEC
    if ("FAEC_TP" %in% variables_names) {
      data <- data %>% dplyr::mutate(FAEC_TP = as.character(.data$FAEC_TP)) %>% 
        dplyr::mutate(FAEC_TP = dplyr::case_match(.data$FAEC_TP, 
                                                  "010000" ~ "Atenção Básica (PAB)", 
                                                  "020000" ~ "Assistência Farmacêutica",
                                                  "040000" ~  "FAEC - Subtipo de financiamento ignorado",
                                                  "040001" ~ "Coleta de material", 
                                                  "040002" ~ "Diagnóstico em laboratório clínico", 
                                                  "040003" ~ "Coleta/exame anátomo-patológico colo uterino", 
                                                  "040004" ~ "Diagnóstico em neurologia", 
                                                  "040005" ~ "Diagnóstico em otorrinolaringologia/fonoaudiologia", 
                                                  "040006" ~ "Diagnóstico em psicologia/psiquiatria", 
                                                  "040007" ~ "Consultas médicas/outros profissionais de nível superior", 
                                                  "040008" ~ "Atenção domiciliar", 
                                                  "040009" ~ "Atendimento/acompanhamento em reabilitação física, mental, visual, auditiva e múltiplas defic", 
                                                  "040010" ~ "Atendimento/acompanhamento psicossocial", 
                                                  "040011" ~ "Atendimento/acompanhamento em saúde do idoso", 
                                                  "040012" ~ "Atendimento/acompanhamento de queimados", 
                                                  "040013" ~ "Atendimento/acompanhamento de diagnóstico de doenças endocrinas/metabólicas e nutricionais", 
                                                  "040014" ~ "Tratamento de doenças do sistema nervoso central e periférico", 
                                                  "040015" ~ "Tratamento de doenças do aparelho da visão", 
                                                  "040016" ~ "Tratamento em oncologia", 
                                                  "040017" ~ "Nefrologia",
                                                  "040018" ~ "Tratamentos odontológicos", 
                                                  "040019" ~ "Cirurgia do sistema nervoso central e periférico", 
                                                  "040020" ~ "Cirurgias de ouvido, nariz e garganta", 
                                                  "040021" ~ "Deformidade labio-palatal e crânio-facial", 
                                                  "040022" ~ "Cirurgia do aparelho da visão", 
                                                  "040023" ~ "Cirurgia do aparelho circulatório", 
                                                  "040024" ~ "Cirurgia do aparelho digestivo, orgãos anexos e parede abdominal(inclui pré e pós operatório)", 
                                                  "040025" ~ "Cirurgia do aparelho geniturinário", 
                                                  "040026" ~ "Tratamento de queimados", 
                                                  "040027" ~ "Cirurgia reparadora para lipodistrofia", 
                                                  "040028" ~ "Outras cirurgias plásticas/reparadoras", 
                                                  "040029" ~ "Cirurgia orofacial", 
                                                  "040030" ~ "Sequenciais", 
                                                  "040031" ~ "Cirurgias em nefrologia", 
                                                  "040032" ~ "Transplantes de orgãos, tecidos e células", 
                                                  "040033" ~ "Medicamentos para transplante", 
                                                  "040034" ~ "OPM auditivas", 
                                                  "040035" ~ "OPM em odontologia", 
                                                  "040036" ~ "OPM em queimados", 
                                                  "040037" ~ "OPM em nefrologia", 
                                                  "040038" ~ "OPM para transplantes",
                                                  "040039" ~ "Incentivos ao pré-natal e nascimento", 
                                                  "040040" ~ "Incentivo ao registro cívil de nascimento", 
                                                  "040041" ~ "Central Nacional de Regulação de Alta Complexidade (CNRAC)", 
                                                  "040042" ~ "Reguladores de Atividade hormonal - Inibidores de prolactina", 
                                                  "040043" ~ "Política Nacional de Cirurgias Eletivas", 
                                                  "040044" ~ "Redesignação e Acompanhamento", 
                                                  "040045" ~ "Projeto Olhar Brasil", 
                                                  "040046" ~ "Mamografia para Rastreamento", 
                                                  "040047" ~ "Projeto Olhar Brasil - Consulta", 
                                                  "040048" ~ "Projeto Olhar Brasil - Óculos",
                                                  "040049" ~ "Implementar Cirg. CV Pediátrica", 
                                                  "040050" ~ "Cirurgias Eletivas - Componente I", 
                                                  "040051" ~ "Cirurgias Eletivas - Componente II",
                                                  "040052" ~ "Cirurgias Eletivas - Componente III", 
                                                  "040053" ~ "Prótese Mamária - Exames", 
                                                  "040054" ~ "Prótese Mamária - Cirurgia", 
                                                  "040055" ~ "Transplante - Histocompatibilidade", 
                                                  "040056" ~ "Triagem Neonatal",
                                                  "040057" ~ "Controle de qualidade do exame citopatológico do colo de útero", 
                                                  "040058" ~ "Exames do Leite Materno", 
                                                  "040059" ~ "Atenção as Pessoas em Situação de Violência Sexual", 
                                                  "040060" ~ "Sangue e Hemoderivados",
                                                  "040061" ~ "Mamografia para rastreamento em faixa etária recomendada", 
                                                  "040062" ~ "Doenças Raras", "40063" ~ "Cadeiras de Rodas", 
                                                  "040064" ~ "Sistema de Frequencia Modulada Pessoal-FM", 
                                                  "040065" ~ "Medicamentos em Urgência", 
                                                  "040066" ~ "Cirurgias Eletivas - Componente Único", 
                                                  "040067" ~ "Atenção Especializada em Saúde Auditiva", 
                                                  "040068" ~ "Terapias Especializadas em Angiologia", 
                                                  "021012" ~ "FAEC CNRAC (21012-cód ant à tab unif-vál p/2008-01)", 
                                                  "021014" ~ "FAEC Eletiv(21014-cód ant à tab unif-vál p/2008-01)", 
                                                  "050000" ~ "Incentivo - MAC", 
                                                  "060000" ~ "Média e Alta Complexidade (MAC)", 
                                                  "070000" ~ "Vigilância em Saúde", 
                                                  "080000" ~ "Gestão do SUS", 
                                                  .default = .data$FAEC_TP)) %>% 
        dplyr::mutate(FAEC_TP = as.factor(.data$FAEC_TP))
    }
    
    #Tabela de Regras Contratuais dos Sistemas de Informações do SUS
    if ("REGCT" %in% variables_names) {
      data <- data %>% dplyr::mutate(REGCT = as.character(.data$REGCT)) %>% 
        dplyr::mutate(REGCT = dplyr::case_match(.data$REGCT, 
                                                
                                                
                                                "7100" ~ "TABELA DE NAO GERAÇÃO DE CREDITO POR PRODUCAO NA INTERNACAO E/OU AMBULATORIO", 
                                                
                                                "7101" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA MÉDIA COMPLEXIDADE AMBULATORIAL,
                                                exceto Fundo de Ações Estratégicas e Compensação (FAEC)", 
                                                
                                                "7102" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA MÉDIA COMPLEXIDADE HOSPITALAR,
                                                 incluindo OPM e demais procedimentos especiais, exceto os financiados pelo FAEC", 
                                                
                                                "7103" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA ALTA COMPLEXIDADE AMBULATORIAL, exceto FAEC", 
                                                
                                                "7104" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO NA ALTA COMPLEXIDADE HOSPITALAR,
                                                incluindo OPM e demais procedimentos especiais, exceto os financiados pelo FAEC",
                                                
                                                "7105" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CREDITO PARA OS PROCEDIMENTOS FINANCIADOS COM O FAEC", 
                                                
                                                "7106" ~ "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - incluindo FAEC", 
                                                
                                                "7107" ~ "Estabelecimento de saúde sem geração de crédito nas ações especializadas de odontologia, 
                                                exceto FAEC: incentivo Centros de Especialidades Odontológicas (CEOs) I, II e III", 
                                                
                                                "7108" ~ "Estabelecimento de saúde sem geração de crédito, exceto FAEC: incentivo saúde do trabalhador",
                                                
                                                "7109" ~ "Estabelecimento de saúde sem geração de crédito total: Ministério da Educação (MEC)", 
                                                
                                                "7110" ~ "Estabelecimento de saúde da estrutura do MS sem geração de crédito total.", 
                                                
                                                "7111" ~ "Estabelecimento de saúde sem geração de crédito Núcleo de Apoio à Saúde da Família (NASF), exceto FAEC", 
                                                
                                                "7112" ~ "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC  - EXCLUSIVO PARA REDE SARAH", 
                                                
                                                "7113" ~ "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC - OUTROS ESTABELECIMENTOS FEDERAIS", 
                                                
                                                "7114" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO TOTAL, INCLUSIVE FAEC - PRONTO ATENDIMENTO", 
                                                
                                                "7115" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - hospitais universitários/MEC", 
                                                
                                                "7116" ~ "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - Laboratório Regional de Prótese Dentária (LRPD)", 
                                                #Origem:
                                                #https://www.gov.br/saude/pt-br/centrais-de-conteudo/publicacoes/estudos-e-notas-informativas/2025/nota-informativa-no-1-2025-cgspd-daet-saes-ms.pdf
                                                "7117" ~ "Estabelecimento de saúde sem geração de crédito na média complexidade (exceto OPM e FAEC) – RCPD", 
                                                
                                                "0000" ~ "Sem regra contratual", .default = .data$REGCT)) %>% 
        dplyr::mutate(REGCT = as.factor(.data$REGCT))
    }
    
    #Raça\Cor
    if ("RACA_COR" %in% variables_names) {
      data <- data %>% dplyr::mutate(RACA_COR = as.character(.data$RACA_COR)) %>% 
        dplyr::mutate(RACA_COR = dplyr::case_match(.data$RACA_COR, 
                                                   "01" ~ "Branca", "02" ~ "Preta", "03" ~ "Parda", 
                                                   "04" ~ "Amarela", "05" ~ "Indígena", 
                                                   "0" ~ "Label Não definido", "99" ~ "Sem Informação", .default = .data$RACA_COR)) %>% 
        dplyr::mutate(RACA_COR = as.factor(.data$RACA_COR))
    }
    
    #ETnia.
    if ("ETNIA" %in% variables_names) {
      data <- data %>% dplyr::mutate(ETNIA = as.character(.data$ETNIA)) %>% 
        dplyr::mutate(ETNIA = dplyr::case_match(.data$ETNIA, 
                                                "0001" ~ "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)", 
                                                "0002" ~ "AIKANA (AIKANA, MAS SAKA,TUBARAO)", 
                                                "0003" ~ "AJURU", "0004" ~ "AKUNSU (AKUNT'SU)", 
                                                "0005" ~ "AMANAYE", "0006" ~ "AMONDAWA", "0007" ~ 
                                                  "ANAMBE", "0008" ~ "APARAI (APALAI)", "0009" ~ 
                                                  "APIAKA (APIACA)", "0010" ~ "APINAYE (APINAJE/APINAIE/APINAGE)", 
                                                "0011" ~ "APURINA (APORINA, IPURINA, IPURINA, IPURINAN)", 
                                                "0012" ~ "ARANA (ARACUAI DO VALE DO JEQUITINHONHA)", 
                                                "0013" ~ "ARAPASO (ARAPACO)", "0014" ~ "ARARA DE RONDONIA (KARO, URUCU, URUKU)", 
                                                "0015" ~ "ARARA DO ACRE (SHAWANAUA, AMAWAKA)", 
                                                "0016" ~ "ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARI-PUANA)", 
                                                "0017" ~ "ARARA DO PARA (UKARAGMA, UKARAMMA)", 
                                                "0018" ~ "ARAWETE (ARAUETE)", "0019" ~ "ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)", 
                                                "0020" ~ "ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)", 
                                                "0021" ~ "ARIKOSE (ARICOBE)", "0022" ~ "ARUA", 
                                                "0023" ~ "ARUAK (ARAWAK)", "0024" ~ "ASHANINKA (KAMPA)", 
                                                "0025" ~ "ASURINI DO TOCANTINS (AKUAWA/AKWAWA)", 
                                                "0026" ~ "ASURINI DO XINGU (AWAETE)", "0027" ~ 
                                                  "ATIKUM (ATICUM)", "0028" ~ "AVA - CANOEIRO", 
                                                "0029" ~ "AWETI (AUETI/AUETO)", "0030" ~ "BAKAIRI (KURA, BACAIRI)", 
                                                "0031" ~ "BANAWA YAFI (BANAWA, BANAWA-JAFI)", 
                                                "0032" ~ "BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)", 
                                                "0033" ~ "BARA (WAIPINOMAKA)", "0034" ~ "BARASANA (HANERA)", 
                                                "0035" ~ "BARE", "0036" ~ "BORORO (BOE)", 
                                                "0037" ~ "BOTOCUDO (GEREN)", "0038" ~ "CANOE", 
                                                "0039" ~ "CASSUPA", "0040" ~ "CHAMACOCO", 
                                                "0041" ~ "CHIQUITANO (XIQUITANO)", "0042" ~ 
                                                  "CIKIYANA (SIKIANA)", "0043" ~ "CINTA LARGA (MATETAMAE)", 
                                                "0044" ~ "COLUMBIARA (CORUMBIARA)", "0045" ~ 
                                                  "DENI", "0046" ~ "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)", 
                                                "0047" ~ "DIAHUI (JAHOI, JAHUI, DIARROI)", 
                                                "0048" ~ "ENAWENE-NAWE (SALUMA)", "0049" ~ 
                                                  "FULNI-O", "0050" ~ "GALIBI (GALIBI DO OIAPOQUE, KARINHA)", 
                                                "0051" ~ "GALIBI MARWORNO (GALIBI DO UACA, ARUA)", 
                                                "0052" ~ "GAVIAO DE RONDONIA (DIGUT)", "0053" ~ 
                                                  "GAVIAO KRIKATEJE", "0054" ~ "GAVIAO PARKATEJE (PARKATEJE)", 
                                                "0055" ~ "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)", 
                                                "0056" ~ "GUAJA (AWA, AVA)", "0057" ~ "GUAJAJARA (TENETEHARA)", 
                                                "0058" ~ "GUARANI KAIOWA (PAI TAVYTERA)", 
                                                "0059" ~ "GUARANI M'BYA", "0060" ~ "GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)", 
                                                "0061" ~ "GUATO", "0062" ~ "HIMARIMA (HIMERIMA)", 
                                                "0063" ~ "INGARIKO (INGARICO, AKAWAIO, KAPON)", 
                                                "0064" ~ "IRANXE (IRANTXE)", "0065" ~ "ISSE", 
                                                "0066" ~ "JABOTI (JABUTI, KIPIU, YABYTI)", 
                                                "0067" ~ "JAMAMADI (YAMAMADI, DJEOROMITXI)", 
                                                "0068" ~ "JARAWARA", "0069" ~ "JIRIPANCO (JERIPANCO, GERIPANCO)", 
                                                "0070" ~ "JUMA (YUMA)", "0071" ~ "JURUNA", 
                                                "0072" ~ "JURUTI (YURITI)", "0073" ~ "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)", 
                                                "0074" ~ "KADIWEU (CADUVEO, CADIUEU)", "0075" ~ 
                                                  "KAIABI (CAIABI, KAYABI)", "0076" ~ "KAIMBE (CAIMBE)", 
                                                "0077" ~ "KAINGANG (CAINGANGUE)", "0078" ~ 
                                                  "KAIXANA (CAIXANA)", "0079" ~ "KALABASSA (CALABASSA, CALABACAS)", 
                                                "0080" ~ "KALANCO", "0081" ~ "KALAPALO (CALAPALO)", 
                                                "0082" ~ "KAMAYURA (CAMAIURA, KAMAIURA)", 
                                                "0083" ~ "KAMBA (CAMBA)", "0084" ~ "KAMBEBA (CAMBEBA, OMAGUA)", 
                                                "0085" ~ "KAMBIWA (CAMBIUA)", "0086" ~ "KAMBIWA PIPIPA (PIPIPA)", 
                                                "0087" ~ "KAMPE", "0088" ~ "KANAMANTI (KANAMATI, CANAMANTI)", 
                                                "0089" ~ "KANAMARI (CANAMARI, KANAMARY, TUKUNA)", 
                                                "0090" ~ "KANELA APANIEKRA (CANELA)", "0091" ~ 
                                                  "KANELA RANKOKAMEKRA (CANELA)", "0092" ~ 
                                                  "KANINDE", "0093" ~ "KANOE (CANOE)", "0094" ~ 
                                                  "KANTARURE (CANTARURE)", "0095" ~ "KAPINAWA (CAPINAUA)", 
                                                "0096" ~ "KARAJA (CARAJA)", "0097" ~ "KARAJA/JAVAE (JAVAE)", 
                                                "0098" ~ "KARAJA/XAMBIOA (KARAJA DO NORTE)", 
                                                "0099" ~ "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)", 
                                                "0100" ~ "KARAPOTO (CARAPOTO)", "0101" ~ "KARIPUNA (CARIPUNA)", 
                                                "0102" ~ "KARIPUNA DO AMAPA (CARIPUNA)", "0103" ~ 
                                                  "KARIRI (CARIRI)", "0104" ~ "KARIRI-XOCO (CARIRI-CHOCO)", 
                                                "0105" ~ "KARITIANA (CARITIANA)", "0106" ~ 
                                                  "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)", 
                                                "0107" ~ "KATUENA (CATUENA, KATWENA)", "0108" ~ 
                                                  "KATUKINA (PEDA DJAPA)", "0109" ~ "KATUKINA DO ACRE", 
                                                "0110" ~ "KAXARARI (CAXARARI)", "0111" ~ "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)", 
                                                "0112" ~ "KAXIXO", "0113" ~ "KAXUYANA (CAXUIANA)", 
                                                "0114" ~ "KAYAPO (CAIAPO)", "0115" ~ "KAYAPO KARARAO (KARARAO)", 
                                                "0116" ~ "KAYAPO TXUKAHAMAE (TXUKAHAMAE)", 
                                                "0117" ~ "KAYAPO XICRIM (XIKRIN)", "0118" ~ 
                                                  "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)", 
                                                "0119" ~ "KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)", 
                                                "0120" ~ "KIRIRI", "0121" ~ "KOCAMA (COCAMA, KOKAMA)", 
                                                "0122" ~ "KOKUIREGATEJE", "0123" ~ "KORUBO", 
                                                "0124" ~ "KRAHO (CRAO, KRAO)", "0125" ~ "KREJE (KRENYE)", 
                                                "0126" ~ "KRENAK (BORUN, CRENAQUE)", "0127" ~ 
                                                  "KRIKATI (KRINKATI)", "0128" ~ "KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)", 
                                                "0129" ~ "KUIKURO (KUIKURU, CUICURO)", "0130" ~ 
                                                  "KUJUBIM (KUYUBI, CUJUBIM)", "0131" ~ "KULINA PANO (CULINA)", 
                                                "0132" ~ "KULINA/MADIHA (CULINA, MADIJA, MADIHA)", 
                                                "0133" ~ "KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)", 
                                                "0134" ~ "KURUAIA (CURUAIA)", "0135" ~ "KWAZA (COAIA, KOAIA)", 
                                                "0136" ~ "MACHINERI (MANCHINERI, MANXINERI)", 
                                                "0137" ~ "MACURAP (MAKURAP)", "0138" ~ "MAKU DOW (DOW)", 
                                                "0139" ~ "MAKU HUPDA (HUPDA)", "0140" ~ "MAKU NADEB (NADEB)", 
                                                "0141" ~ "MAKU YUHUPDE (YUHUPDE)", "0142" ~ 
                                                  "MAKUNA (MACUNA, YEBA-MASA)", "0143" ~ "MAKUXI (MACUXI, MACHUSI, PEMON)", 
                                                "0144" ~ "MARIMAM (MARIMA)", "0145" ~ "MARUBO", 
                                                "0146" ~ "MATIPU", "0147" ~ "MATIS", "0148" ~ 
                                                  "MATSE (MAYORUNA)", "0149" ~ "MAXAKALI (MAXACALI)", 
                                                "0150" ~ "MAYA (MAYA)", "0151" ~ "MAYTAPU", 
                                                "0152" ~ "MEHINAKO (MEINAKU, MEINACU)", "0153" ~ 
                                                  "MEKEN (MEQUEM, MEKHEM, MICHENS)", "0154" ~ 
                                                  "MENKY (MYKY, MUNKU, MENKI, MYNKY)", "0155" ~ 
                                                  "MIRANHA (MIRANHA, MIRANA)", "0156" ~ "MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)", 
                                                "0157" ~ "MUNDURUKU (MUNDURUCU)", "0158" ~ 
                                                  "MURA", "0159" ~ "NAHUKWA (NAFUQUA)", "0160" ~ 
                                                  "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)", 
                                                "0161" ~ "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)", 
                                                "0162" ~ "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)", 
                                                "0163" ~ "NARAVUTE (NARUVOTO)", "0164" ~ "NAWA (NAUA)", 
                                                "0165" ~ "NUKINI (NUQUINI, NUKUINI)", "0166" ~ 
                                                  "OFAIE (OFAYE-XAVANTE)", "0167" ~ "ORO WIN", 
                                                "0168" ~ "PAIAKU (JENIPAPO-KANINDE)", "0169" ~ 
                                                  "PAKAA NOVA (WARI, PACAAS NOVOS)", "0170" ~ 
                                                  "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)", 
                                                "0171" ~ "PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)", 
                                                "0172" ~ "PANKARARE (PANCARARE)", "0173" ~ 
                                                  "PANKARARU (PANCARARU)", "0174" ~ "PANKARARU KALANKO (KALANKO)", 
                                                "0175" ~ "PANKARARU KARUAZU (KARUAZU)", "0176" ~ 
                                                  "PANKARU (PANCARU)", "0177" ~ "PARAKANA (PARACANA, APITEREWA, AWAETE)", 
                                                "0178" ~ "PARECI (PARESI, HALITI)", "0179" ~ 
                                                  "PARINTINTIN", "0180" ~ "PATAMONA (KAPON)", 
                                                "0181" ~ "PATAXO", "0182" ~ "PATAXO HA-HA-HAE", 
                                                "0183" ~ "PAUMARI (PALMARI)", "0184" ~ "PAUMELENHO", 
                                                "0185" ~ "PIRAHA (MURA PIRAHA)", "0186" ~ 
                                                  "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)", 
                                                "0187" ~ "PITAGUARI", "0188" ~ "POTIGUARA", 
                                                "0189" ~ "POYANAWA (POIANAUA)", "0190" ~ "RIKBAKTSA (CANOEIROS, ERIGPAKTSA)", 
                                                "0191" ~ "SAKURABIAT(MEKENS, SAKIRABIAP, SAKIRABIAR)", 
                                                "0192" ~ "SATERE-MAWE (SATERE-MAUE)", "0193" ~ 
                                                  "SHANENAWA (KATUKINA)", "0194" ~ "SIRIANO (SIRIA-MASA)", 
                                                "0195" ~ "SURIANA", "0196" ~ "SURUI DE RONDONIA (PAITER)", 
                                                "0197" ~ "SURUI DO PARA (AIKEWARA)", "0198" ~ 
                                                  "SUYA (SUIA/KISEDJE)", "0199" ~ "TAPAYUNA (BEICO-DE-PAU)", 
                                                "0200" ~ "TAPEBA", "0201" ~ "TAPIRAPE (TAPI'IRAPE)", 
                                                "0202" ~ "TAPUIA (TAPUIA-XAVANTE, TAPUIO)", 
                                                "0203" ~ "TARIANO (TARIANA, TALIASERI)", "0204" ~ 
                                                  "TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)", 
                                                "0205" ~ "TEMBE", "0206" ~ "TENHARIM", "0207" ~ 
                                                  "TERENA", "0208" ~ "TICUNA (TIKUNA, TUKUNA, MAGUTA)", 
                                                "0209" ~ "TINGUI BOTO", "0210" ~ "TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)", 
                                                "0211" ~ "TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)", 
                                                "0212" ~ "TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)", 
                                                "0213" ~ "TORA", "0214" ~ "TREMEMBE", "0215" ~ 
                                                  "TRUKA", "0216" ~ "TRUMAI", "0217" ~ "TSOHOM DJAPA (TSUNHUM-DJAPA)", 
                                                "0218" ~ "TUKANO (TUCANO, YE'PA-MASA, DASEA)", 
                                                "0219" ~ "TUMBALALA", "0220" ~ "TUNAYANA", 
                                                "0221" ~ "TUPARI", "0222" ~ "TUPINAMBA", "0223" ~ 
                                                  "TUPINIQUIM", "0224" ~ "TURIWARA", "0225" ~ 
                                                  "TUXA", "0226" ~ "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)", 
                                                "0227" ~ "TXIKAO (TXICAO, IKPENG)", "0228" ~ 
                                                  "UMUTINA (OMOTINA, BARBADOS)", "0229" ~ 
                                                  "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)", 
                                                "0230" ~ "WAI WAI HIXKARYANA (HIXKARYANA)", 
                                                "0231" ~ "WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)", 
                                                "0232" ~ "WAI WAI XEREU (XEREU)", "0233" ~ 
                                                  "WAI WAI KATUENA (KATUENA)", "0234" ~ "WAI WAI MAWAYANA (MAWAYANA)", 
                                                "0235" ~ "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )", 
                                                "0236" ~ "WAIMIRI ATROARI (KINA)", "0237" ~ 
                                                  "WANANO (UANANO, WANANA)", "0238" ~ "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)", 
                                                "0239" ~ "WAREKENA (UAREQUENA, WEREKENA)", 
                                                "0240" ~ "WASSU", "0241" ~ "WAURA (UAURA, WAUJA)", 
                                                "0242" ~ "WAYANA (WAIANA, UAIANA)", "0243" ~ 
                                                  "WITOTO (UITOTO, HUITOTO)", "0244" ~ "XAKRIABA (XACRIABA)", 
                                                "0245" ~ "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)", 
                                                "0246" ~ "XERENTE (AKWE, AWEN, AKWEN)", "0247" ~ 
                                                  "XETA", "0248" ~ "XIPAIA (SHIPAYA, XIPAYA)", 
                                                "0249" ~ "XOKLENG (SHOKLENG, XOCLENG)", "0250" ~ 
                                                  "XOKO (XOCO, CHOCO)", "0251" ~ "XUKURU (XUCURU)", 
                                                "0252" ~ "XUKURU KARIRI (XUCURU-KARIRI)", 
                                                "0253" ~ "YAIPIYANA", "0254" ~ "YAMINAWA (JAMINAWA, IAMINAWA)", 
                                                "0255" ~ "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)", 
                                                "0256" ~ "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)", 
                                                "0257" ~ "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)", 
                                                "0258" ~ "YAWALAPITI (IAUALAPITI)", "0259" ~ 
                                                  "YAWANAWA (IAUANAUA)", "0260" ~ "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)", 
                                                "0261" ~ "YUDJA (JURUNA, YURUNA)", "0262" ~ 
                                                  "ZO'E (POTURU)", "0263" ~ "ZORO (PAGEYN)", 
                                                "0264" ~ "ZURUAHA (SOROWAHA, SURUWAHA)", "X265" ~ 
                                                  "AHANENAWA", "X266" ~ "AICABA", "X267" ~ 
                                                  "AIKAN\\u00c3-KWAS\\u00c1", "X268" ~ "AKUNTSU", 
                                                "X269" ~ "ALANTESU", "X271" ~ "AMAW\\u00c1KA", 
                                                "X272" ~ "ANAC\\u00c9", "X273" ~ "APURIN\\u00c3", 
                                                "X274" ~ "ARAN\\u00c3", "X275" ~ "ARAPA\\u00c7O", 
                                                "X276" ~ "ARARA APOLIMA", "X277" ~ "ARARA DO ARIPUANA", 
                                                "X278" ~ "ARIPUAN\\u00c1", "X279" ~ "ASSURINI", 
                                                "X280" ~ "AWUAR\\u00c1", "X281" ~ "BORBA", 
                                                "X282" ~ "CABIXI", "X283" ~ "CAMARAR\\u00c9", 
                                                "X284" ~ "CAMASURI", "X285" ~ "CARA PRETA", 
                                                "X286" ~ "CHARRUA", "X287" ~ "CUJUBIM", "X288" ~ 
                                                  "DAW", "X289" ~ "GAVI\\u00c3O", "X290" ~ 
                                                  "GUARANI", "X291" ~ "HALANTESU", "X292" ~ 
                                                  "HALOTESU", "X293" ~ "HENGAT\\u00da", "X294" ~ 
                                                  "HIXKARYANA", "X295" ~ "HUPDE", "X296" ~ 
                                                  "HUPDES", "X297" ~ "IAUANAUA", "X298" ~ 
                                                  "IAUARETE A\\u00c7U", "X299" ~ "IKPENG", 
                                                "X300" ~ "INAMBU", "X301" ~ "INHABARANA", 
                                                "X302" ~ "JAVAE", "X303" ~ "JENIPAPO", "X304" ~ 
                                                  "JENIPAPO-KANINDE", "X305" ~ "JIAHOI", "X306" ~ 
                                                  "KAIOWA", "X307" ~ "KAMPA", "X308" ~ "KANELA", 
                                                "X309" ~ "KARAFAWYANA", "X310" ~ "KARARAO", 
                                                "X311" ~ "KARUBO", "X312" ~ "KASSUP\\u00c1", 
                                                "X313" ~ "KATITH\\u00c3ULU", "X314" ~ "KATOKIN", 
                                                "X315" ~ "KATUKINA PANO", "X316" ~ "KATUKINA PEDA DJAPA", 
                                                "X317" ~ "KATUKINA SHANENAUWA", "X318" ~ "KAXAGO", 
                                                "X319" ~ "KAYABI", "X320" ~ "KIN\\u00c3 (WAIMIRI-ATROARI)", 
                                                "X321" ~ "KIRIRI-BARRA", "X322" ~ "KITH\\u00c3ULU", 
                                                "X323" ~ "KOIAI\\u00c1", "X324" ~ "KOIUPANK\\u00c1", 
                                                "X325" ~ "KONTANAWA", "X326" ~ "KRAH\\u00d4 KANELA", 
                                                "X327" ~ "KULINA", "X328" ~ "LATUND\\u00ca", 
                                                "X329" ~ "MAKU", "X330" ~ "MAKUNAMB\\u00c9", 
                                                "X331" ~ "MAMAIND\\u00ca", "X332" ~ "MAMURI", 
                                                "X333" ~ "MANACAPURU", "X334" ~ "MANAIRISSU", 
                                                "X335" ~ "MANCHINERI", "X336" ~ "MANDUCA", 
                                                "X337" ~ "MARIBONDO", "X338" ~ "MASSAKA", 
                                                "X339" ~ "MAWAYANA", "X340" ~ "MAW\\u00c9", 
                                                "X341" ~ "MAYORUNA", "X342" ~ "MIQUELENO", 
                                                "X343" ~ "MOKURI\\u00d1", "X344" ~ "MON ORO WARAM", 
                                                "X345" ~ "MUTUM", "X346" ~ "MYKY", "X347" ~ 
                                                  "NADEB", "X348" ~ "NAMBIKWARA", "X349" ~ 
                                                  "NEGAROT\\u00ca", "X350" ~ "NHENGATU", "X351" ~ 
                                                  "OFAIE XAVANTE", "X352" ~ "ON\\u00c7A", 
                                                "X353" ~ "ORO AT", "X354" ~ "ORO EO", "X355" ~ 
                                                  "ORO JOWIN", "X356" ~ "ORO MIYLIN", "X357" ~ 
                                                  "ORO MON", "X358" ~ "ORO N\\u00c1O", "X359" ~ 
                                                  "ORO WAM", "X360" ~ "ORO WARAM", "X361" ~ 
                                                  "ORO WARAM XIJEIN", "X362" ~ "PACA", "X363" ~ 
                                                  "PANKAR\\u00c1", "X364" ~ "PAPAGAIO", "X365" ~ 
                                                  "PAYAY\\u00c1", "X366" ~ "PIPIPAN", "X367" ~ 
                                                  "PIRATA", "X368" ~ "PUROBOR\\u00c1", "X369" ~ 
                                                  "SABAN\\u00ca", "X370" ~ "SANUMA", "X371" ~ 
                                                  "SAWENTES\\u00da", "X372" ~ "SILCY-TAPUYA", 
                                                "X373" ~ "SIUCI", "X374" ~ "TABAJARA", "X375" ~ 
                                                  "TAKUARA", "X376" ~ "TATU", "X377" ~ "TAWAND\\u00ca", 
                                                "X378" ~ "TEF\\u00c9", "X379" ~ "TIMBIRA", 
                                                "X380" ~ "TOR\\u00c1 DO BAIXO GRANDE", "X381" ~ 
                                                  "TSUNHUM-DJAP\\u00c1", "X382" ~ "TUBAR\\u00c3O", 
                                                "X383" ~ "TUPAIU", "X384" ~ "TUPI", "X385" ~ 
                                                  "TUPINAMB\\u00c1 DE BELMONTE", "X386" ~ 
                                                  "URUBU", "X387" ~ "URUBU KAAPOR", "X388" ~ 
                                                  "URUP\\u00c1", "X389" ~ "WAI WAI", "X390" ~ 
                                                  "WAIKISU", "X391" ~ "WAKALITES\\u00da", 
                                                "X392" ~ "WASSUSU", "X393" ~ "XEREU", "X394" ~ 
                                                  "XI EIN", "X395" ~ "XICRIN", "X396" ~ "XIPAYA", 
                                                "X397" ~ "XIRIANA", "X398" ~ "XIRUAI", "X399" ~ 
                                                  "YEPAMASS\\u00c3", "X400" ~ "TIRIY\\u00d3", 
                                                "X401" ~ "YANOMAMI", "X402" ~ "ARARA", "X403" ~ 
                                                  "SAKIRIABAR", "X404" ~ "TATZ", "X405" ~ 
                                                  "SEM INFORMACAO", .default = .data$ETNIA)) %>% 
        dplyr::mutate(ETNIA = as.factor(.data$ETNIA))
    }
    
    #Valor do complemento federal de serviços hospitalares – está incluído no valor total da AIH
    if ("VAL_SH_FED" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SH_FED = as.numeric(.data$VAL_SH_FED))
    }
    
    #Valor do complemento federal de serviços profissionais – está incluído no valor total da AIH
    if ("VAL_SP_FED" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SP_FED = as.numeric(.data$VAL_SP_FED))
    }
    
    #Valor do complemento do gestor (estadual ou municipal) de serviços hospitalares – está incluído no valor total da AIH
    if ("VAL_SH_GES" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SH_GES = as.numeric(.data$VAL_SH_GES))
    }
    
    #Valor do complemento do gestor (estadual ou municipal) de serviços profissionais – está incluído no valor total da AIH
    if ("VAL_SP_GES" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_SP_GES = as.numeric(.data$VAL_SP_GES))
    }
    
    #Valor de UCI
    if ("VAL_UCI" %in% variables_names) {
      data <- data %>% dplyr::mutate(VAL_UCI = as.numeric(.data$VAL_UCI))
    }
    
    #Tipo de UCI utilizada pelo paciente. Diferente de marca_uTi
    if ("MARCA_UCI" %in% variables_names) {
      data <- data %>% dplyr::mutate(MARCA_UCI = as.character(.data$MARCA_UCI)) %>% 
        dplyr::mutate(MARCA_UCI = dplyr::case_match(.data$MARCA_UCI, 
                                                    "00" ~ "Não utilizou UCI", 
                                                    "01" ~ "Unidade de cuidados intermed neonatal convencional", 
                                                    "02" ~ "Unidade de cuidados intermed neonatal canguru", 
                                                    "03" ~ "Unidade intermediária neonatal", 
                                                    "88" ~ "Utilizou dois tipos de leitos UCI", .default = .data$MARCA_UCI)) %>% 
        dplyr::mutate(MARCA_UCI = as.factor(.data$MARCA_UCI))
    }
    
    #Tipo de diagnóstico secundário 1
    if ("TPDISEC1" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC1 = as.character(.data$TPDISEC1)) %>% 
        dplyr::mutate(TPDISEC1 = dplyr::case_match(.data$TPDISEC1, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC1)) %>% dplyr::mutate(TPDISEC1 = as.factor(.data$TPDISEC1))
    }
    
    #Tipo de diagnóstico secundário 2
    if ("TPDISEC2" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC2 = as.character(.data$TPDISEC2)) %>% 
        dplyr::mutate(TPDISEC2 = dplyr::case_match(.data$TPDISEC2, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC2)) %>% dplyr::mutate(TPDISEC2 = as.factor(.data$TPDISEC2))
    }
    
    #Tipo de diagnóstico secundário 3
    if ("TPDISEC3" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC3 = as.character(.data$TPDISEC3)) %>% 
        dplyr::mutate(TPDISEC3 = dplyr::case_match(.data$TPDISEC3, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC3)) %>% dplyr::mutate(TPDISEC3 = as.factor(.data$TPDISEC3))
    }
    
    #Tipo de diagnóstico secundário 4
    if ("TPDISEC4" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC4 = as.character(.data$TPDISEC4)) %>% 
        dplyr::mutate(TPDISEC4 = dplyr::case_match(.data$TPDISEC4, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC4)) %>% dplyr::mutate(TPDISEC4 = as.factor(.data$TPDISEC4))
      data$TPDISEC4 <- factor(data$TPDISEC4)
    }
    
    #Tipo de diagnóstico secundário 5
    if ("TPDISEC5" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC5 = as.character(.data$TPDISEC5)) %>% 
        dplyr::mutate(TPDISEC5 = dplyr::case_match(.data$TPDISEC5, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC5)) %>% dplyr::mutate(TPDISEC5 = as.factor(.data$TPDISEC5))
    }
    
    #Tipo de diagnóstico secundário 6
    if ("TPDISEC6" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC6 = as.character(.data$TPDISEC6)) %>% 
        dplyr::mutate(TPDISEC6 = dplyr::case_match(.data$TPDISEC6, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC6)) %>% dplyr::mutate(TPDISEC6 = as.factor(.data$TPDISEC6))
    }
    
    #Tipo de diagnóstico secundário 7
    if ("TPDISEC7" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC7 = as.character(.data$TPDISEC7)) %>% 
        dplyr::mutate(TPDISEC7 = dplyr::case_match(.data$TPDISEC7, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC7)) %>% dplyr::mutate(TPDISEC7 = as.factor(.data$TPDISEC7))
    }
    
    #Tipo de diagnóstico secundário 8
    if ("TPDISEC8" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC8 = as.character(.data$TPDISEC8)) %>% 
        dplyr::mutate(TPDISEC8 = dplyr::case_match(.data$TPDISEC8, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC8)) %>% dplyr::mutate(TPDISEC8 = as.factor(.data$TPDISEC8))
    }
    
    #Tipo de diagnóstico secundário 9
    if ("TPDISEC9" %in% variables_names) {
      data <- data %>% dplyr::mutate(TPDISEC9 = as.character(.data$TPDISEC9)) %>% 
        dplyr::mutate(TPDISEC9 = dplyr::case_match(.data$TPDISEC9, 
                                                   "0" ~ NA, "1" ~ "Pré-existente", "2" ~ "Adquirido", 
                                                   .default = .data$TPDISEC9)) %>% dplyr::mutate(TPDISEC9 = as.factor(.data$TPDISEC9))
    }
  }
  
  data <- tibble::as_tibble(data)
  data <- droplevels(data.table::as.data.table(data))
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
                                                     FUN = stringi::stri_unescape_unicode)))
  return(data)
  
}

