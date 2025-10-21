
#Função utilizada para enriquecimento da base CNES

tratar_cnes <- 
  function(data) {
    
    #Vou padronizar a classe das variáveis, permitindo empilhar.
    #Algumas variáveis trocam de classe, transformar todas em character permite
    #empilhar. 
    
    #Captura o nome  das variáveis.
    variables_names <- names(data) 
    
    #Transformando variáveis de interesse para character.
    cols_existentes <- intersect(variables_names, names(data))
    if (length(cols_existentes) > 0) {
      data[, (cols_existentes) := lapply(.SD, as.character), .SDcols = cols_existentes]
    }
    
    data <- dtplyr::lazy_dt(data)

# Tratamento de Estabelecimentos ------------------------------------------

    if(cnes == "Estabelecimentos") { 
      
      if ("CNES" %in% variables_names) {
        data <- data %>% dplyr::mutate(CNES = as.character(.data$CNES))
      }
      if (nomes == TRUE) {
        cadger_temp <- microdatasus::fetch_cadger()
        data <- data %>% dplyr::left_join(cadger_temp, by = "CNES")
      }
      if ("CODUFMUN" %in% variables_names & municipality_data == 
          TRUE) {
        colnames(tabMun)[1] <- "CODUFMUN"
        tabMun$CODUFMUN <- as.character(tabMun$CODUFMUN)
        data <- data %>% dplyr::left_join(tabMun, by = "CODUFMUN")
      }
      if ("COD_CEP" %in% variables_names) {
        data <- data %>% dplyr::mutate(COD_CEP = as.integer(.data$COD_CEP))
      }
      if ("CPF_CNPJ" %in% variables_names) {
        data <- data %>% dplyr::mutate(CPF_CNPJ = as.character(.data$CPF_CNPJ))
      }
      if ("PF_PJ" %in% variables_names) {
        data <- data %>% dplyr::mutate(PF_PJ = dplyr::case_match(.data$PF_PJ, 
                                                                 "1" ~ "Pessoa física", "3" ~ "Pessoa jurídica", 
                                                                 .default = .data$PF_PJ)) %>% dplyr::mutate(PF_PJ = as.factor(.data$PF_PJ))
      }
      if ("NIV_DEP" %in% variables_names) {
        data <- data %>% dplyr::mutate(NIV_DEP = dplyr::case_match(.data$NIV_DEP, 
                                                                   "1" ~ "Individual", "3" ~ "Mantida", .default = .data$NIV_DEP)) %>% 
          dplyr::mutate(NIV_DEP = as.factor(.data$NIV_DEP))
      }
      if ("CNPJ_MAN" %in% variables_names) {
        data <- data %>% dplyr::mutate(CNPJ_MAN = as.numeric(.data$CNPJ_MAN))
      }
      if ("COD_IR" %in% variables_names) {
        data <- data %>% dplyr::mutate(COD_IR = dplyr::case_match(.data$COD_IR, 
                                                                  "0" ~ NA, "10" ~ "Estabelecimento público", 
                                                                  "11" ~ "Estabelecimento filantrópico", "12" ~ 
                                                                    "Estabelecimento sem fins lucrativos", "13" ~ 
                                                                    "Estabelecimento privado luvrativa simples", 
                                                                  "14" ~ "Estabelecimento privado luvrativa", 
                                                                  "15" ~ "Estabelecimento sindical", "16" ~ "Estabelecimento pessoa física", 
                                                                  "19" ~ "Estabelecimento Ret.Manten.código 19", 
                                                                  .default = .data$COD_IR)) %>% dplyr::mutate(COD_IR = as.factor(.data$COD_IR))
      }
      if ("REGSAUDE" %in% variables_names) {
        data <- data %>% dplyr::mutate(REGSAUDE = as.character(.data$REGSAUDE))
      }
      if ("MICR_REG" %in% variables_names) {
        data <- data %>% dplyr::mutate(MICR_REG = as.integer(.data$MICR_REG))
      }
      if ("DISTRSAN" %in% variables_names) {
        data <- data %>% dplyr::mutate(DISTRSAN = as.integer(.data$DISTRSAN))
      }
      if ("VINC_SUS" %in% variables_names) {
        data <- data %>% dplyr::mutate(VINC_SUS = dplyr::case_match(.data$VINC_SUS, 
                                                                    "0" ~ "Não", "1" ~ "Sim", "2" ~ "Não", .default = .data$VINC_SUS)) %>% 
          dplyr::mutate(VINC_SUS = as.factor(.data$VINC_SUS))
      }
      if ("TPGESTAO" %in% variables_names) {
        data <- data %>% dplyr::mutate(TPGESTAO = dplyr::case_match(.data$TPGESTAO, 
                                                                    "D" ~ "Dupla", "E" ~ "Estadual", "M" ~ "Municipal", 
                                                                    "Z" ~ "Sem gestão", "S" ~ "Sem gestão", .default = .data$TPGESTAO)) %>% 
          dplyr::mutate(TPGESTAO = as.factor(.data$TPGESTAO))
      }
      if ("ESFERA_A" %in% variables_names) {
        data <- data %>% dplyr::mutate(ESFERA_A = dplyr::case_match(.data$ESFERA_A, 
                                                                    "1" ~ "Federal", "2" ~ "Estadual", "3" ~ "Municipal", 
                                                                    "4" ~ "Privada", "-99" ~ NA, .default = .data$ESFERA_A)) %>% 
          dplyr::mutate(ESFERA_A = as.factor(.data$ESFERA_A))
      }
      if ("RETENCAO" %in% variables_names) {
        data <- data %>% dplyr::mutate(RETENCAO = dplyr::case_match(.data$RETENCAO, 
                                                                    "0" ~ NA, "10" ~ "Estabelecimento público", 
                                                                    "11" ~ "Estabelecimento filantrópico", "12" ~ 
                                                                      "Estabelecimento sem fins lucrativos", "13" ~ 
                                                                      "Estabelecimento privado luvrativa simples", 
                                                                    "14" ~ "Estabelecimento privado luvrativa", 
                                                                    "15" ~ "Estabelecimento sindical", "16" ~ "Estabelecimento pessoa física", 
                                                                    .default = .data$RETENCAO)) %>% dplyr::mutate(RETENCAO = as.factor(.data$RETENCAO))
      }
      if ("ATIVIDAD" %in% variables_names) {
        data <- data %>% dplyr::mutate(ATIVIDAD = dplyr::case_match(.data$ATIVIDAD, 
                                                                    "-99" ~ NA, "1" ~ "Unidade Universitária", 
                                                                    "2" ~ "Unidade Escola Superior Isolada", "3" ~ 
                                                                      "Unidade Auxiliar de Ensino", "4" ~ "Unidade sem atividade de Ensino", 
                                                                    "5" ~ "Hospital de ensino", .default = .data$ATIVIDAD)) %>% 
          dplyr::mutate(ATIVIDAD = as.factor(.data$ATIVIDAD))
      }
      if ("NATUREZA" %in% variables_names) {
        data <- data %>% dplyr::mutate(NATUREZA = dplyr::case_match(.data$NATUREZA, 
                                                                    "-99" ~ NA, "1" ~ "Administração Direta da Saúde (MS, SES, e SMS)", 
                                                                    "2" ~ "Adm Direta outros orgãos (MEX, MEx, Marinha,...)", 
                                                                    "3" ~ "Adm Indireta - Autarquias", "4" ~ "Adm Indireta - Fundação Pública", 
                                                                    "5" ~ "Adm Indireta - Empresa Pública", "6" ~ 
                                                                      "Adm Indireta - Organização Social Pública", 
                                                                    "7" ~ "Empresa Privada", "8" ~ "Fundação Privada", 
                                                                    "9" ~ "Cooperativa", "10" ~ "Serviço Social Autônomo", 
                                                                    "11" ~ "Entidade Beneficente sem fins lucrativos", 
                                                                    "12" ~ "Economia Mista", "13" ~ "Sindicato", 
                                                                    "0" ~ "Natureza inexistente", .default = .data$NATUREZA)) %>% 
          dplyr::mutate(NATUREZA = as.factor(.data$NATUREZA))
      }
      if ("NAT_JUR" %in% variables_names) {
        data <- data %>% dplyr::mutate(NAT_JUR = dplyr::case_match(.data$NAT_JUR, 
                                                                   "1015" ~ "Órgão Público do Poder Executivo Federal", 
                                                                   "1023" ~ "Órgão Público do Poder Executivo Estadual ou do Distrito Federal", 
                                                                   "1031" ~ "Órgão Público do Poder Executivo Municipal", 
                                                                   "1040" ~ "Órgão Público do Poder Legislativo Federal", 
                                                                   "1058" ~ "Órgão Público do Poder Legislativo Estadual ou do Distrito Federal", 
                                                                   "1066" ~ "Órgão Público do Poder Legislativo Municipal", 
                                                                   "1074" ~ "Órgão Público do Poder Judiciário Federal", 
                                                                   "1082" ~ "Órgão Público do Poder Judiciário Estadual", 
                                                                   "1104" ~ "Autarquia Federal", "1112" ~ "Autarquia Estadual ou do Distrito Federal", 
                                                                   "1120" ~ "Autarquia Municipal", "1139" ~ "Fundação Pública de Direito Público Federal", 
                                                                   "1147" ~ "Fundação Pública de Direito Público Estadual ou do Distrito Federal", 
                                                                   "1155" ~ "Fundação Pública de Direito Público Municipal", 
                                                                   "1163" ~ "Órgão Público Autônomo Federal", 
                                                                   "1171" ~ "Órgão Público Autônomo Estadual ou do Distrito Federal", 
                                                                   "1180" ~ "Órgão Público Autônomo Municipal", 
                                                                   "1198" ~ "Comissão Polinacional", "1201" ~ 
                                                                     "Fundo Público", "1210" ~ "Consórcio Público de Direito Público (Associação Pública)", 
                                                                   "1228" ~ "Consórcio Público de Direito Privado", 
                                                                   "1236" ~ "Estado ou Distrito Federal", "1244" ~ 
                                                                     "Município", "1252" ~ "Fundação Pública de Direito Privado Federal", 
                                                                   "1260" ~ "Fundação Pública de Direito Privado Estadual ou do Distrito Federal", 
                                                                   "1279" ~ "Fundação Pública de Direito Privado Municipal", 
                                                                   "2011" ~ "Empresa Pública", "2038" ~ "Sociedade de Economia Mista", 
                                                                   "2046" ~ "Sociedade Anônima Aberta", "2054" ~ 
                                                                     "Sociedade Anônima Fechada", "2062" ~ "Sociedade Empresária Limitada", 
                                                                   "2070" ~ "Sociedade Empresária em Nome Coletivo", 
                                                                   "2089" ~ "Sociedade Empresária em Comandita Simples", 
                                                                   "2097" ~ "Sociedade Empresária em Comandita por Ações", 
                                                                   "2127" ~ "Sociedade em Conta de Participação", 
                                                                   "2135" ~ "Empresário (Individual)", "2143" ~ 
                                                                     "Cooperativa", "2151" ~ "Consórcio de Sociedades", 
                                                                   "2160" ~ "Grupo de Sociedades", "2178" ~ "Estabelecimento, no Brasil, de Sociedade Estrangeira", 
                                                                   "2194" ~ "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira", 
                                                                   "2216" ~ "Empresa Domiciliada no Exterior", 
                                                                   "2224" ~ "Clube/Fundo de Investimento", "2232" ~ 
                                                                     "Sociedade Simples Pura", "2240" ~ "Sociedade Simples Limitada", 
                                                                   "2259" ~ "Sociedade Simples em Nome Coletivo", 
                                                                   "2267" ~ "Sociedade Simples em Comandita Simples", 
                                                                   "2275" ~ "Empresa Binacional", "2283" ~ "Consórcio de Empregadores", 
                                                                   "2291" ~ "Consórcio Simples", "2305" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Empresária)", 
                                                                   "2313" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)", 
                                                                   "2321" ~ "Sociedade Unipessoal de Advogados", 
                                                                   "2330" ~ "Cooperativas de Consumo", "3034" ~ 
                                                                     "Serviço Notarial e Registral (Cartório)", 
                                                                   "3069" ~ "Fundação Privada", "3077" ~ "Serviço Social Autônomo", 
                                                                   "3085" ~ "Condomínio Edilício", "3107" ~ "Comissão de Conciliação Prévia", 
                                                                   "3115" ~ "Entidade de Mediação e Arbitragem", 
                                                                   "3131" ~ "Entidade Sindical", "3204" ~ "Estabelecimento, no Brasil, de Fundação ou Associação Estrangeiras", 
                                                                   "3212" ~ "Fundação ou Associação Domiciliada no Exterior", 
                                                                   "3220" ~ "Organização Religiosa", "3239" ~ 
                                                                     "Comunidade Indígena", "3247" ~ "Fundo Privado", 
                                                                   "3255" ~ "Órgão de Direção Nacional de Partido Político", 
                                                                   "3263" ~ "Órgão de Direção Regional de Partido Político", 
                                                                   "3271" ~ "Órgão de Direção Local de Partido Político", 
                                                                   "3280" ~ "Comitê Financeiro de Partido Político", 
                                                                   "3298" ~ "Frente Plebiscitária ou Referendária", 
                                                                   "3306" ~ "Organização Social (OS)", "3310" ~ 
                                                                     "Demais Condomínios", "3999" ~ "Associação Privada", 
                                                                   "4014" ~ "Empresa Individual Imobiliária", 
                                                                   "4022" ~ "Segurado Especial", "4081" ~ "Contribuinte individual", 
                                                                   "4090" ~ "Candidato a Cargo Político Eletivo", 
                                                                   "4111" ~ "Leiloeiro", "4124" ~ "Produtor Rural (Pessoa Física)", 
                                                                   "5010" ~ "Organização Internacional", "5029" ~ 
                                                                     "Representação Diplomática Estrangeira", 
                                                                   "5037" ~ "Outras Instituições Extraterritoriais", 
                                                                   "0" ~ "Não especificado ou ignorado", .default = .data$NAT_JUR)) %>% 
          dplyr::mutate(NAT_JUR = as.factor(.data$NAT_JUR))
      }
      if ("CLIENTEL" %in% variables_names) {
        data <- data %>% dplyr::mutate(CLIENTEL = dplyr::case_match(.data$CLIENTEL, 
                                                                    "-99" ~ NA, "1" ~ "Atendimento de demanda espontânea", 
                                                                    "2" ~ "Atendimento de demanda referenciada", 
                                                                    "3" ~ "Atendimento de demanda espontânea e referenciada", 
                                                                    "0" ~ "Fluxo de Clientela não exigido", .default = .data$CLIENTEL)) %>% 
          dplyr::mutate(CLIENTEL = as.factor(.data$CLIENTEL))
      }
      if ("TP_UNID" %in% variables_names) {
        data <- data %>% dplyr::mutate(TP_UNID = dplyr::case_match(.data$TP_UNID, 
                                                                   "1" ~ "Posto de saúde", "01" ~ "Posto de saúde", 
                                                                   "2" ~ "Centro de saúde / Unidade básica", 
                                                                   "02" ~ "Centro de saúde / Unidade básica", 
                                                                   "4" ~ "Policlínica", "04" ~ "Policlínica", 
                                                                   "5" ~ "Hospital geral", "05" ~ "Hospital geral", 
                                                                   "7" ~ "Hospital Especializado", "07" ~ "Hospital Especializado", 
                                                                   "9" ~ "Pronto socorro de hospital geral (antigo)", 
                                                                   "09" ~ "Pronto socorro de hospital geral (antigo)", 
                                                                   "12" ~ "Pronto socorro traumato-ortopédico (antigo)", 
                                                                   "15" ~ "Unidade mista", "20" ~ "Pronto socorro geral", 
                                                                   "21" ~ "Pronto socorro especializado", "22" ~ 
                                                                     "Consultório isolado", "32" ~ "Unidade móvel fluvial", 
                                                                   "36" ~ "Clínica / Centro de saúde de especialidade", 
                                                                   "39" ~ "Unidade de apoio diagnose e terapia (SADT isolado)", 
                                                                   "40" ~ "Unidade móvel terrestre", "42" ~ "Unidade móvel de nível pré-hospitalar na área de urgência", 
                                                                   "43" ~ "Farmácia", "45" ~ "Unidade de saúde da família", 
                                                                   "50" ~ "Unidade de vigilância em saúde", "60" ~ 
                                                                     "Cooperativa ou empresa de cessão de trabalhadores na saúde", 
                                                                   "61" ~ "Centro de parto normal - isolado", "62" ~ 
                                                                     "Hospital / Dia - Isolado", "63" ~ "Unidade autorizadora", 
                                                                   "64" ~ "Central de regulação de serviços de saúde", 
                                                                   "65" ~ "Unidade de vigilância epidemiológica (antigo)", 
                                                                   "66" ~ "Unidade de vigilância sanitária (antigo)", 
                                                                   "67" ~ "Laboratório central de saúde pública LACEN", 
                                                                   "68" ~ "Central de gestão em saúde", "69" ~ 
                                                                     "Centro de atenção hemoterapia e/ou hematologica", 
                                                                   "70" ~ "Centro de atenção psicosocial", "71" ~ 
                                                                     "Centro de apoio a saúde da família", "72" ~ 
                                                                     "Unidade de atenção a saúde indígena", 
                                                                   "73" ~ "Pronto atendimento", "74" ~ "Pólo academia da saúde", 
                                                                   "75" ~ "Telessaúde", "76" ~ "Central de regulação médica das urgências", 
                                                                   "77" ~ "Serviço de atenção domiciliar isolado (Home care)", 
                                                                   "78" ~ "Unidade de atenção em regime residencial", 
                                                                   "79" ~ "Oficina ortopédica", "80" ~ "Laboratório de saúde pública", 
                                                                   "81" ~ "Central de regulação do acesso", "82" ~ 
                                                                     "Central de notificação, captação e distribuição de órgãos estadual", 
                                                                   "83" ~ "Pólo de prevenção de doenças e agravos e promoção da saúde", 
                                                                   "84" ~ "Central de abastecimento", "85" ~ "Centro de imunização", 
                                                                   .default = .data$TP_UNID)) %>% dplyr::mutate(TP_UNID = as.factor(.data$TP_UNID))
      }
      if ("TURNO_AT" %in% variables_names) {
        data <- data %>% dplyr::mutate(TURNO_AT = dplyr::case_match(.data$TURNO_AT, 
                                                                    "-99" ~ NA, "1" ~ "Turnos intermitentes", "2" ~ 
                                                                      "Contínuo 24h/dia (Pl Sab Dom Fer)", "3" ~ 
                                                                      "Manhã / Tarde / Noite", "4" ~ "Manhã", 
                                                                    "5" ~ "Tarde", "6" ~ "Manhã / Tarde", "7" ~ 
                                                                      "Noite", .default = .data$TURNO_AT)) %>% dplyr::mutate(TURNO_AT = as.factor(.data$TURNO_AT))
      }
      if ("NIV_HIER" %in% variables_names) {
        data <- data %>% dplyr::mutate(NIV_HIER = dplyr::case_match(.data$NIV_HIER, 
                                                                    "0" ~ NA, "-99" ~ NA, "1" ~ "PAB-PABA", "2" ~ 
                                                                      "Média M1", "3" ~ "Média M2 e M3", "4" ~ 
                                                                      "Alta complexidade ambulatorial", "5" ~ "Baixa M1 e M2", 
                                                                    "6" ~ "Média M2 e M3", "7" ~ "Média M3", "8" ~ 
                                                                      "Alta complexidade hospitalar / ambulatorial", 
                                                                    .default = .data$NIV_HIER)) %>% dplyr::mutate(NIV_HIER = as.factor(.data$NIV_HIER))
      }
      if ("TP_PREST" %in% variables_names) {
        data <- data %>% dplyr::mutate(TP_PREST = dplyr::case_match(.data$TP_PREST, 
                                                                    "-99" ~ NA, "30" ~ "Público federal", "40" ~ 
                                                                      "Público estadual", "50" ~ "Público municipal", 
                                                                    "61" ~ "Filantrópico com CNAS válido", "80" ~ 
                                                                      "Sindicato", "20" ~ "Privado com fins lucrativos", 
                                                                    "22" ~ "Privado optantes pelo simples", "60" ~ 
                                                                      "Privado sem fins lucrativos", .default = .data$TP_PREST)) %>% 
          dplyr::mutate(TP_PREST = as.factor(.data$TP_PREST))
      }
      if ("CO_BANCO" %in% variables_names) {
        data <- data %>% dplyr::mutate(CO_BANCO = as.character(.data$CO_BANCO))
      }
      if ("CO_AGENC" %in% variables_names) {
        data <- data %>% dplyr::mutate(CO_AGENC = as.character(.data$CO_AGENC))
      }
      if ("C_CORREN" %in% variables_names) {
        data <- data %>% dplyr::mutate(C_CORREN = as.character(.data$C_CORREN))
      }
      if ("CONTRATM" %in% variables_names) {
        data <- data %>% dplyr::mutate(CONTRATM = as.character(.data$CONTRATM))
      }
      if ("DT_PUBLM" %in% variables_names) {
        data <- data %>% dplyr::mutate(DT_PUBLM = as.character(.data$DT_PUBLM))
      }
      if ("CONTRATE" %in% variables_names) {
        data <- data %>% dplyr::mutate(CONTRATE = as.character(.data$CONTRATE))
      }
      if ("DT_PUBLE" %in% variables_names) {
        data <- data %>% dplyr::mutate(DT_PUBLE = as.character(.data$DT_PUBLE))
      }
      if ("ALVARA" %in% variables_names) {
        data <- data %>% dplyr::mutate(ALVARA = as.character(.data$ALVARA))
      }
      if ("DT_EXPED" %in% variables_names) {
        data <- data %>% dplyr::mutate(DT_EXPED = as.character(.data$DT_EXPED))
      }
      if ("ORGEXPED" %in% variables_names) {
        data <- data %>% dplyr::mutate(ORGEXPED = dplyr::case_match(.data$ORGEXPED, 
                                                                    "1" ~ "SES", "2" ~ "SMS", .default = .data$ORGEXPED)) %>% 
          dplyr::mutate(ORGEXPED = as.factor(.data$ORGEXPED))
      }
      if ("AV_ACRED" %in% variables_names) {
        data <- data %>% dplyr::mutate(AV_ACRED = dplyr::case_match(.data$AV_ACRED, 
                                                                    "1" ~ "Sim", "2" ~ "Não", "0" ~ "Não", .default = .data$AV_ACRED)) %>% 
          dplyr::mutate(AV_ACRED = as.factor(.data$AV_ACRED))
      }
      if ("CLASAVAL" %in% variables_names) {
        data <- data %>% dplyr::mutate(CLASAVAL = dplyr::case_match(.data$CLASAVAL, 
                                                                    "-9" ~ NA, "1" ~ "Acreditado no nível 1", "2" ~ 
                                                                      "Acreditado no nível 2", "3" ~ "Acreditado no nível 3", 
                                                                    "0" ~ "Não atendeu aos padrões mínimos", 
                                                                    .default = .data$CLASAVAL)) %>% dplyr::mutate(CLASAVAL = as.factor(.data$CLASAVAL))
      }
      
      
      
      if ("DT_ACRED" %in% variables_names) {
        data <- data %>% dplyr::mutate(DT_ACRED = as.integer(.data$DT_ACRED))
      }
      
      
      if ("AV_PNASS" %in% variables_names) {
        data <- data %>% dplyr::mutate(AV_PNASS = dplyr::case_match(.data$AV_PNASS, 
                                                                    "1" ~ "Sim", "2" ~ "Não", "0" ~ "Não", .default = .data$AV_PNASS)) %>% 
          dplyr::mutate(AV_PNASS = as.factor(.data$AV_PNASS))
      }
      
      
      if ("DT_PNASS" %in% variables_names) {
        data <- data %>% dplyr::mutate(DT_PNASS = as.integer(.data$DT_PNASS))
      }
      
      
      
      if ("GESPRG1E" %in% variables_names) {
        data$GESPRG1E <- as.numeric(levels(data$GESPRG1E))[data$GESPRG1E]
        data$GESPRG1E[data$GESPRG1E == 1] <- "Sim"
        data$GESPRG1E[data$GESPRG1E == 0] <- "Não"
        data$GESPRG1E <- factor(data$GESPRG1E)
      }
      
      
      
      if ("GESPRG1M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG1M = dplyr::case_match(.data$GESPRG1M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG1M)) %>% 
          dplyr::mutate(GESPRG1M = as.factor(.data$GESPRG1M))
      }
      
      
      
      if ("GESPRG2E" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG2E = dplyr::case_match(.data$GESPRG2E, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG2E)) %>% 
          dplyr::mutate(GESPRG2E = as.factor(.data$GESPRG2E))
      }
      
      
      
      if ("GESPRG2M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG2M = dplyr::case_match(.data$GESPRG2M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG2M)) %>% 
          dplyr::mutate(GESPRG2M = as.factor(.data$GESPRG2M))
      }
      
      
      
      if ("GESPRG4E" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG4E = dplyr::case_match(.data$GESPRG4E, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG4E)) %>% 
          dplyr::mutate(GESPRG4E = as.factor(.data$GESPRG4E))
      }
      
      
      
      if ("GESPRG4M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG4M = dplyr::case_match(.data$GESPRG4M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG4M)) %>% 
          dplyr::mutate(GESPRG4M = as.factor(.data$GESPRG4M))
      }
      
      
      
      if ("NIVATE_A" %in% variables_names) {
        data <- data %>% dplyr::mutate(NIVATE_A = dplyr::case_match(.data$NIVATE_A, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$NIVATE_A)) %>% 
          dplyr::mutate(NIVATE_A = as.factor(.data$NIVATE_A))
      }
      
      
      
      if ("GESPRG3E" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG3E = dplyr::case_match(.data$GESPRG3E, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG3E)) %>% 
          dplyr::mutate(GESPRG3E = as.factor(.data$GESPRG3E))
      }
      
      
      
      if ("GESPRG3M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG3M = dplyr::case_match(.data$GESPRG3M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG3M)) %>% 
          dplyr::mutate(GESPRG3M = as.factor(.data$GESPRG3M))
      }
      
      
      
      if ("GESPRG5E" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG5E = dplyr::case_match(.data$GESPRG5E, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG5E)) %>% 
          dplyr::mutate(GESPRG5E = as.factor(.data$GESPRG5E))
      }
      
      
      
      if ("GESPRG5M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG5M = dplyr::case_match(.data$GESPRG5M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG5M)) %>% 
          dplyr::mutate(GESPRG5M = as.factor(.data$GESPRG5M))
      }
      
      
      
      if ("GESPRG6E" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG6E = dplyr::case_match(.data$GESPRG6E, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG6E)) %>% 
          dplyr::mutate(GESPRG6E = as.factor(.data$GESPRG6E))
      }
      
      
      
      if ("GESPRG6M" %in% variables_names) {
        data <- data %>% dplyr::mutate(GESPRG6M = dplyr::case_match(.data$GESPRG6M, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$GESPRG6M)) %>% 
          dplyr::mutate(GESPRG6M = as.factor(.data$GESPRG6M))
      }
      
      
      if ("NIVATE_H" %in% variables_names) {
        data <- data %>% dplyr::mutate(NIVATE_H = dplyr::case_match(.data$NIVATE_H, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$NIVATE_H)) %>% 
          dplyr::mutate(NIVATE_H = as.factor(.data$NIVATE_H))
      }
      
      
      
      if ("URGEMERG" %in% variables_names) {
        data <- data %>% dplyr::mutate(URGEMERG = dplyr::case_match(.data$URGEMERG, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$URGEMERG)) %>% 
          dplyr::mutate(URGEMERG = as.factor(.data$URGEMERG))
      }
      
      
      
      if ("ATENDAMB" %in% variables_names) {
        data <- data %>% dplyr::mutate(ATENDAMB = dplyr::case_match(.data$ATENDAMB, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$ATENDAMB)) %>% 
          dplyr::mutate(ATENDAMB = as.factor(.data$ATENDAMB))
      }
      
      
      
      if ("CENTROBS" %in% variables_names) {
        data <- data %>% dplyr::mutate(CENTROBS = dplyr::case_match(.data$CENTROBS, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$CENTROBS)) %>% 
          dplyr::mutate(CENTROBS = as.factor(.data$CENTROBS))
      }
      
      
      
      if ("CENTRNEO" %in% variables_names) {
        data <- data %>% dplyr::mutate(CENTRNEO = dplyr::case_match(.data$CENTRNEO, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$CENTRNEO)) %>% 
          dplyr::mutate(CENTRNEO = as.factor(.data$CENTRNEO))
      }
      
      
      
      if ("ATENDHOS" %in% variables_names) {
        data <- data %>% dplyr::mutate(ATENDHOS = dplyr::case_match(.data$ATENDHOS, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$ATENDHOS)) %>% 
          dplyr::mutate(ATENDHOS = as.factor(.data$ATENDHOS))
      }
      
      
      
      if ("SERAP01P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP01P = dplyr::case_match(.data$SERAP01P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP01P)) %>% 
          dplyr::mutate(SERAP01P = as.factor(.data$SERAP01P))
      }
      
      
      
      if ("SERAP01T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP01T = dplyr::case_match(.data$SERAP01T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP01T)) %>% 
          dplyr::mutate(SERAP01T = as.factor(.data$SERAP01T))
      }
      
      
      
      if ("SERAP02P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP02P = dplyr::case_match(.data$SERAP02P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP02P)) %>% 
          dplyr::mutate(SERAP02P = as.factor(.data$SERAP02P))
      }
      
      
      
      if ("SERAP02T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP02T = dplyr::case_match(.data$SERAP02T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP02T)) %>% 
          dplyr::mutate(SERAP02T = as.factor(.data$SERAP02T))
      }
      
      
      
      if ("SERAP03P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP03P = dplyr::case_match(.data$SERAP03P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP03P)) %>% 
          dplyr::mutate(SERAP03P = as.factor(.data$SERAP03P))
      }
      
      
      
      if ("SERAP03T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP03T = dplyr::case_match(.data$SERAP03T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP03T)) %>% 
          dplyr::mutate(SERAP03T = as.factor(.data$SERAP03T))
      }
      
      
      
      if ("SERAP04P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP04P = dplyr::case_match(.data$SERAP04P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP04P)) %>% 
          dplyr::mutate(SERAP04P = as.factor(.data$SERAP04P))
      }
      
      
      
      if ("SERAP04T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP04T = dplyr::case_match(.data$SERAP04T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP04T)) %>% 
          dplyr::mutate(SERAP04T = as.factor(.data$SERAP04T))
      }
      
      
      
      if ("SERAP05P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP05P = dplyr::case_match(.data$SERAP05P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP05P)) %>% 
          dplyr::mutate(SERAP05P = as.factor(.data$SERAP05P))
      }
      
      
      
      
      if ("SERAP05T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP05T = dplyr::case_match(.data$SERAP05T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP05T)) %>% 
          dplyr::mutate(SERAP05T = as.factor(.data$SERAP05T))
      }
      
      
      
      if ("SERAP06P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP06P = dplyr::case_match(.data$SERAP06P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP06P)) %>% 
          dplyr::mutate(SERAP06P = as.factor(.data$SERAP06P))
      }
      
      
      
      if ("SERAP06T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP06T = dplyr::case_match(.data$SERAP06T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP06T)) %>% 
          dplyr::mutate(SERAP06T = as.factor(.data$SERAP06T))
      }
      
      
      
      if ("SERAP07P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP07P = dplyr::case_match(.data$SERAP07P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP07P)) %>% 
          dplyr::mutate(SERAP07P = as.factor(.data$SERAP07P))
      }
      
      
      
      if ("SERAP07T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP07T = dplyr::case_match(.data$SERAP07T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP07T)) %>% 
          dplyr::mutate(SERAP07T = as.factor(.data$SERAP07T))
      }
      
      
      
      if ("SERAP08P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP08P = dplyr::case_match(.data$SERAP08P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP08P)) %>% 
          dplyr::mutate(SERAP08P = as.factor(.data$SERAP08P))
      }
      
      
      
      if ("SERAP08T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP08T = dplyr::case_match(.data$SERAP08T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP08T)) %>% 
          dplyr::mutate(SERAP08T = as.factor(.data$SERAP08T))
      }
      
      
      
      if ("SERAP09P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP09P = dplyr::case_match(.data$SERAP09P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP09P)) %>% 
          dplyr::mutate(SERAP09P = as.factor(.data$SERAP09P))
      }
      
      
      
      if ("SERAP09T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP09T = dplyr::case_match(.data$SERAP09T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP09T)) %>% 
          dplyr::mutate(SERAP09T = as.factor(.data$SERAP09T))
      }
      
      
      
      if ("SERAP10P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP10P = dplyr::case_match(.data$SERAP10P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP10P)) %>% 
          dplyr::mutate(SERAP10P = as.factor(.data$SERAP10P))
      }
      
      
      
      if ("SERAP10T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP10T = dplyr::case_match(.data$SERAP10T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP10T)) %>% 
          dplyr::mutate(SERAP10T = as.factor(.data$SERAP10T))
      }
      
      
      
      if ("SERAP11P" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP11P = dplyr::case_match(.data$SERAP11P, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP11P)) %>% 
          dplyr::mutate(SERAP11P = as.factor(.data$SERAP11P))
      }
      
      
      if ("SERAP11T" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAP11T = dplyr::case_match(.data$SERAP11T, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAP11T)) %>% 
          dplyr::mutate(SERAP11T = as.factor(.data$SERAP11T))
      }
      
      
      if ("SERAPOIO" %in% variables_names) {
        data <- data %>% dplyr::mutate(SERAPOIO = dplyr::case_match(.data$SERAPOIO, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$SERAPOIO)) %>% 
          dplyr::mutate(SERAPOIO = as.factor(.data$SERAPOIO))
      }
      
      
      if ("RES_BIOL" %in% variables_names) {
        data <- data %>% dplyr::mutate(RES_BIOL = dplyr::case_match(.data$RES_BIOL, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$RES_BIOL)) %>% 
          dplyr::mutate(RES_BIOL = as.factor(.data$RES_BIOL))
      }
      
      
      if ("RES_QUIM" %in% variables_names) {
        data <- data %>% dplyr::mutate(RES_QUIM = dplyr::case_match(.data$RES_QUIM, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$RES_QUIM)) %>% 
          dplyr::mutate(RES_QUIM = as.factor(.data$RES_QUIM))
      }
      
      
      if ("RES_RADI" %in% variables_names) {
        data <- data %>% dplyr::mutate(RES_RADI = dplyr::case_match(.data$RES_RADI, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$RES_RADI)) %>% 
          dplyr::mutate(RES_RADI = as.factor(.data$RES_RADI))
      }
      
      
      if ("RES_COMU" %in% variables_names) {
        data <- data %>% dplyr::mutate(RES_COMU = dplyr::case_match(.data$RES_COMU, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$RES_COMU)) %>% 
          dplyr::mutate(RES_COMU = as.factor(.data$RES_COMU))
      }
      
      
      if ("COLETRES" %in% variables_names) {
        data <- data %>% dplyr::mutate(COLETRES = dplyr::case_match(.data$COLETRES, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COLETRES)) %>% 
          dplyr::mutate(COLETRES = as.factor(.data$COLETRES))
      }
      
      
      if ("COMISS01" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS01 = dplyr::case_match(.data$COMISS01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS01)) %>% 
          dplyr::mutate(COMISS01 = as.factor(.data$COMISS01))
      }
      
      
      if ("COMISS02" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS02 = dplyr::case_match(.data$COMISS02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS02)) %>% 
          dplyr::mutate(COMISS02 = as.factor(.data$COMISS02))
      }
      
      
      if ("COMISS03" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS03 = dplyr::case_match(.data$COMISS03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS03)) %>% 
          dplyr::mutate(COMISS03 = as.factor(.data$COMISS03))
      }
      
      
      if ("COMISS04" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS04 = dplyr::case_match(.data$COMISS04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS04)) %>% 
          dplyr::mutate(COMISS04 = as.factor(.data$COMISS04))
      }
      
      
      if ("COMISS05" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS05 = dplyr::case_match(.data$COMISS05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS05)) %>% 
          dplyr::mutate(COMISS05 = as.factor(.data$COMISS05))
      }
      
      
      if ("COMISS06" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS06 = dplyr::case_match(.data$COMISS06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS06)) %>% 
          dplyr::mutate(COMISS06 = as.factor(.data$COMISS06))
      }
      
      
      if ("COMISS07" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS07 = dplyr::case_match(.data$COMISS07, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS07)) %>% 
          dplyr::mutate(COMISS07 = as.factor(.data$COMISS07))
      }
      
      
      if ("COMISS08" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS08 = dplyr::case_match(.data$COMISS08, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS08)) %>% 
          dplyr::mutate(COMISS08 = as.factor(.data$COMISS08))
      }
      
      
      if ("COMISS09" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS09 = dplyr::case_match(.data$COMISS09, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS09)) %>% 
          dplyr::mutate(COMISS09 = as.factor(.data$COMISS09))
      }
      
      
      if ("COMISS10" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS10 = dplyr::case_match(.data$COMISS10, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS10)) %>% 
          dplyr::mutate(COMISS10 = as.factor(.data$COMISS10))
      }
      
      
      if ("COMISS11" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS11 = dplyr::case_match(.data$COMISS11, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS11)) %>% 
          dplyr::mutate(COMISS11 = as.factor(.data$COMISS11))
      }
      
      
      if ("COMISS12" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISS12 = dplyr::case_match(.data$COMISS12, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISS12)) %>% 
          dplyr::mutate(COMISS12 = as.factor(.data$COMISS12))
      }
      
      
      if ("COMISSAO" %in% variables_names) {
        data <- data %>% dplyr::mutate(COMISSAO = dplyr::case_match(.data$COMISSAO, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$COMISSAO)) %>% 
          dplyr::mutate(COMISSAO = as.factor(.data$COMISSAO))
      }
      
      
      if ("AP01CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV01 = dplyr::case_match(.data$AP01CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV01)) %>% 
          dplyr::mutate(AP01CV01 = as.factor(.data$AP01CV01))
      }
      
      
      if ("AP01CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV02 = dplyr::case_match(.data$AP01CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV02)) %>% 
          dplyr::mutate(AP01CV02 = as.factor(.data$AP01CV02))
      }
      
      
      if ("AP01CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV05 = dplyr::case_match(.data$AP01CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV05)) %>% 
          dplyr::mutate(AP01CV05 = as.factor(.data$AP01CV05))
      }
      
      
      if ("AP01CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV06 = dplyr::case_match(.data$AP01CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV06)) %>% 
          dplyr::mutate(AP01CV06 = as.factor(.data$AP01CV06))
      }
      
      
      if ("AP01CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV03 = dplyr::case_match(.data$AP01CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV03)) %>% 
          dplyr::mutate(AP01CV03 = as.factor(.data$AP01CV03))
      }
      
      
      if ("AP01CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP01CV04 = dplyr::case_match(.data$AP01CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP01CV04)) %>% 
          dplyr::mutate(AP01CV04 = as.factor(.data$AP01CV04))
      }
      
      
      if ("AP02CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV01 = dplyr::case_match(.data$AP02CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV01)) %>% 
          dplyr::mutate(AP02CV01 = as.factor(.data$AP02CV01))
      }
      
      
      if ("AP02CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV02 = dplyr::case_match(.data$AP02CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV02)) %>% 
          dplyr::mutate(AP02CV02 = as.factor(.data$AP02CV02))
      }
      
      
      if ("AP02CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV05 = dplyr::case_match(.data$AP02CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV05)) %>% 
          dplyr::mutate(AP02CV05 = as.factor(.data$AP02CV05))
      }
      
      
      if ("AP02CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV06 = dplyr::case_match(.data$AP02CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV06)) %>% 
          dplyr::mutate(AP02CV06 = as.factor(.data$AP02CV06))
      }
      
      
      if ("AP02CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV03 = dplyr::case_match(.data$AP02CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV03)) %>% 
          dplyr::mutate(AP02CV03 = as.factor(.data$AP02CV03))
      }
      
      
      if ("AP02CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP02CV04 = dplyr::case_match(.data$AP02CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP02CV04)) %>% 
          dplyr::mutate(AP02CV04 = as.factor(.data$AP02CV04))
      }
      
      
      if ("AP03CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV01 = dplyr::case_match(.data$AP03CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV01)) %>% 
          dplyr::mutate(AP03CV01 = as.factor(.data$AP03CV01))
      }
      
      
      if ("AP03CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV02 = dplyr::case_match(.data$AP03CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV02)) %>% 
          dplyr::mutate(AP03CV02 = as.factor(.data$AP03CV02))
      }
      
      
      if ("AP03CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV05 = dplyr::case_match(.data$AP03CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV05)) %>% 
          dplyr::mutate(AP03CV05 = as.factor(.data$AP03CV05))
      }
      
      
      if ("AP03CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV06 = dplyr::case_match(.data$AP03CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV06)) %>% 
          dplyr::mutate(AP03CV06 = as.factor(.data$AP03CV06))
      }
      
      
      if ("AP03CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV03 = dplyr::case_match(.data$AP03CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV03)) %>% 
          dplyr::mutate(AP03CV03 = as.factor(.data$AP03CV03))
      }
      
      
      if ("AP03CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP03CV04 = dplyr::case_match(.data$AP03CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP03CV04)) %>% 
          dplyr::mutate(AP03CV04 = as.factor(.data$AP03CV04))
      }
      
      
      if ("AP04CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV01 = dplyr::case_match(.data$AP04CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV01)) %>% 
          dplyr::mutate(AP04CV01 = as.factor(.data$AP04CV01))
      }
      
      
      if ("AP04CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV02 = dplyr::case_match(.data$AP04CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV02)) %>% 
          dplyr::mutate(AP04CV02 = as.factor(.data$AP04CV02))
      }
      
      
      if ("AP04CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV05 = dplyr::case_match(.data$AP04CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV05)) %>% 
          dplyr::mutate(AP04CV05 = as.factor(.data$AP04CV05))
      }
      
      
      if ("AP04CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV06 = dplyr::case_match(.data$AP04CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV06)) %>% 
          dplyr::mutate(AP04CV06 = as.factor(.data$AP04CV06))
      }
      
      
      if ("AP04CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV03 = dplyr::case_match(.data$AP04CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV03)) %>% 
          dplyr::mutate(AP04CV03 = as.factor(.data$AP04CV03))
      }
      
      
      if ("AP04CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP04CV04 = dplyr::case_match(.data$AP04CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP04CV04)) %>% 
          dplyr::mutate(AP04CV04 = as.factor(.data$AP04CV04))
      }
      
      
      if ("AP05CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV01 = dplyr::case_match(.data$AP05CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV01)) %>% 
          dplyr::mutate(AP05CV01 = as.factor(.data$AP05CV01))
      }
      
      
      if ("AP05CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV02 = dplyr::case_match(.data$AP05CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV02)) %>% 
          dplyr::mutate(AP05CV02 = as.factor(.data$AP05CV02))
      }
      
      
      if ("AP05CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV05 = dplyr::case_match(.data$AP05CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV05)) %>% 
          dplyr::mutate(AP05CV05 = as.factor(.data$AP05CV05))
      }
      
      
      if ("AP05CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV06 = dplyr::case_match(.data$AP05CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV06)) %>% 
          dplyr::mutate(AP05CV06 = as.factor(.data$AP05CV06))
      }
      
      
      if ("AP05CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV03 = dplyr::case_match(.data$AP05CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV03)) %>% 
          dplyr::mutate(AP05CV03 = as.factor(.data$AP05CV03))
      }
      
      
      if ("AP05CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP05CV04 = dplyr::case_match(.data$AP05CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP05CV04)) %>% 
          dplyr::mutate(AP05CV04 = as.factor(.data$AP05CV04))
      }
      
      
      if ("AP06CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV01 = dplyr::case_match(.data$AP06CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV01)) %>% 
          dplyr::mutate(AP06CV01 = as.factor(.data$AP06CV01))
      }
      
      
      if ("AP06CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV02 = dplyr::case_match(.data$AP06CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV02)) %>% 
          dplyr::mutate(AP06CV02 = as.factor(.data$AP06CV02))
      }
      
      
      if ("AP06CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV05 = dplyr::case_match(.data$AP06CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV05)) %>% 
          dplyr::mutate(AP06CV05 = as.factor(.data$AP06CV05))
      }
      
      
      if ("AP06CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV06 = dplyr::case_match(.data$AP06CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV06)) %>% 
          dplyr::mutate(AP06CV06 = as.factor(.data$AP06CV06))
      }
      
      
      if ("AP06CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV03 = dplyr::case_match(.data$AP06CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV03)) %>% 
          dplyr::mutate(AP06CV03 = as.factor(.data$AP06CV03))
      }
      
      
      if ("AP06CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP06CV04 = dplyr::case_match(.data$AP06CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP06CV04)) %>% 
          dplyr::mutate(AP06CV04 = as.factor(.data$AP06CV04))
      }
      
      
      if ("AP07CV01" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV01 = dplyr::case_match(.data$AP07CV01, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV01)) %>% 
          dplyr::mutate(AP07CV01 = as.factor(.data$AP07CV01))
      }
      
      
      if ("AP07CV02" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV02 = dplyr::case_match(.data$AP07CV02, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV02)) %>% 
          dplyr::mutate(AP07CV02 = as.factor(.data$AP07CV02))
      }
      
      
      if ("AP07CV05" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV05 = dplyr::case_match(.data$AP07CV05, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV05)) %>% 
          dplyr::mutate(AP07CV05 = as.factor(.data$AP07CV05))
      }
      
      
      if ("AP07CV06" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV06 = dplyr::case_match(.data$AP07CV06, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV06)) %>% 
          dplyr::mutate(AP07CV06 = as.factor(.data$AP07CV06))
      }
      
      if ("AP07CV03" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV03 = dplyr::case_match(.data$AP07CV03, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV03)) %>% 
          dplyr::mutate(AP07CV03 = as.factor(.data$AP07CV03))
      }
      
      if ("AP07CV04" %in% variables_names) {
        data <- data %>% dplyr::mutate(AP07CV04 = dplyr::case_match(.data$AP07CV04, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$AP07CV04)) %>% 
          dplyr::mutate(AP07CV04 = as.factor(.data$AP07CV04))
      }
      
      if ("ATEND_PR" %in% variables_names) {
        data <- data %>% dplyr::mutate(ATEND_PR = dplyr::case_match(.data$ATEND_PR, 
                                                                    "1" ~ "Sim", "0" ~ "Não", "2" ~ "Não", .default = .data$ATEND_PR)) %>% 
          dplyr::mutate(ATEND_PR = as.factor(.data$ATEND_PR))
      }
    }
     
      
      }
    
    
    
    
    
    
    
    
    
    
    data <- tibble::as_tibble(data)
    data <- droplevels(data.table::as.data.table(data))
    data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
                                                      FUN = stringi::stri_unescape_unicode)))
    return(data)  
    
  }








