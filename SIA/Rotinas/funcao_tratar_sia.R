
#Função utilizada para enriquecimento da base SIA Produção ambulatorial

tratar_sia <- 
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
    
    
    data <- dtplyr::lazy_dt(data)
       
      #Código da Regra Contratual
      if ("PA_REGCT" %in% variables_names) {
        
        data <- data %>% dplyr::mutate(def_PA_REGCT = dplyr::case_match(.data$PA_REGCT, 
                                                                    "7100" ~ "TAB.DE NÃO GERAÇÃO CRÉDITO P/PROD.INTERN./AMBULAT.", 
                                                                    "7101" ~ "ESTAB.S/CRÉDITO NA MEDIA COMPLEXIDADE AMBULATORIAL", 
                                                                    "7102" ~ "ESTAB.S/CRÉDITO NA MEDIA COMPLEXIDADE HOSPITALAR", 
                                                                    "7103" ~ "ESTAB.S/CRÉDITO NA ALTA COMPLEXIDADE AMBULATORIAL", 
                                                                    "7104" ~ "ESTAB.S/CRÉDITO NA ALTA COMPLEXIDADE HOSPITALAR", 
                                                                    "7105" ~ "ESTAB.S/CRED.PROCED.FINANC.FD.AÇÕES ESTRAT/COMPENS.", 
                                                                    "7106" ~ "ESTABELECIM. DE SAÚDE SEM GERAÇÃO DE CRÉDITO TOTAL", 
                                                                    "7107" ~ "ESTAB.S/CRÉDITO ACOES ESPEC.ODONT(INC.CEO I,II,III)", 
                                                                    "7108" ~ "ESTAB.S/GER.CRÉDITO(INCENTIVO SAUDE DO TRABALHADOR)", 
                                                                    "7109" ~ "ESTAB.SAÚDE (HU/MEC) SEM GERAÇÃO DE CRÉDITO TOTAL", 
                                                                    "7110" ~ "ESTAB.SAUDE (MIN.SAUDE)SEM GERAÇÃO DE CRÉDITO TOTAL", 
                                                                    "7111" ~ "ESTAB.SAUDE SEM GERAÇÃO DE CRÉDITO-NASF,exceto FAEC", 
                                                                    "7112" ~ "ESTAB.S/CRED.TOTAL-INCL.FAEC-EXCLUSIVO P/REDE SARAH", 
                                                                    "7113" ~ "ESTAB.S/CRED.TOTAL,INCL.FAEC-OUTROS ESTAB. FEDERAIS", 
                                                                    "7116" ~ "ESTAB.SAÚDE S/GER DE CRÉDITO NA MÉDIA COMPLEX-LRPD", 
                                                                    "7117" ~ "ESTAB.SAÚDE S/GER DE CRÉD. MÉD COMP(EXCETO OPM)-CER", 
                                                                    "0000" ~ "SEM REGRA CONTRATUAL", .default = .data$PA_REGCT))
      }
    
      #Incremento Outros 
      if ("PA_INCOUT" %in% variables_names) {
        data <- data %>% dplyr::mutate(
          def_PA_INCOUT = dplyr::case_when(
                .data$PA_INCOUT != "0000" ~ "Com incremento", 
                .data$PA_INCOUT == "0000" ~ "Sem incremento"))
      }
    
      #Incremento Urgência 
      if ("PA_INCURG" %in% variables_names) {
        data <- data %>% dplyr::mutate(
          def_PA_INCURG = dplyr::case_when(
            .data$PA_INCURG != "0000" ~ "Com incremento",
            .data$PA_INCURG == "0000" ~ "Sem incremento"))
      }
     
      #Tipo de Estabelecimento 
      if ("PA_TPUPS" %in% variables_names) {
        data <- data %>% dplyr::mutate(
        def_PA_TPUPS = dplyr::case_match(.data$PA_TPUPS,
          "74" ~ "ACADEMIA DA SAÚDE",
          "81" ~ "CENTRAL DE REGULAÇÃO",
          "76" ~ "CENTRAL DE REGULAÇÃO MÉDICA DAS URGÊNCIAS",
          "71" ~ "CENTRO DE APOIO A SAÚDE DA FAMÍLIA-CASF",
          "69" ~ "CENTRO DE ATENÇÃO HEMOTERÁPICA E/OU HEMATOLÓGICA",
          "70" ~ "CENTRO DE ATENÇÃO PSICOSSOCIAL-CAPS",
          "61" ~ "CENTRO DE PARTO NORMAL", 
          "02" ~ "CENTRO DE SAUDE/UNIDADE BASICA DE SAUDE",
          "64" ~ "CENTRAL DE REGULACAO DE SERVICOS DE SAUDE",
          "36" ~ "CLINICA ESPECIALIZADA/AMBULATORIO ESPECIALIZADO",
          "22" ~ "CONSULTORIO", 
          "60" ~ "COOPERATIVA",
          "43" ~ "FARMACIA", 
          "07" ~ "HOSPITAL ESPECIALIZADO",
          "05" ~ "HOSPITAL GERAL", 
          "62" ~ "HOSPITAL DIA",
          "67" ~ "LABORATORIO CENTRAL DE SAUDE PUBLICA - LACEN",
          "80" ~ "ORIO DE SAUDE PUBLICA", 
          "04" ~ "POLICLINICA",
          "79" ~ "OFICINA ORTOPEDICA", 
          "01" ~ "POSTO DE SAUDE",
          "73" ~ "PRONTO ANTEDIMENTO", 
          "21" ~ "PRONTO SOCORRO ESPECIALIZADO",
          "20" ~ "PRONTO SOCORRO GERAL", 
          "68" ~ "SECRETARIA DE SAUDE",
          "77" ~ "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)",
          "63" ~ "UNIDADE AUTORIZADORA", 
          "72" ~ "UNIDADE DE ATENÇÃO À SAÚDE INDÍGENA",
          "78" ~ "UNIDADE DE ATENCAO EM REGIME RESIDENCIAL",
          "39" ~ "UNIDADE DE SERVICO DE APOIO DE DIAGNOSE E TERAPIA",
          "45" ~ "UNIDADE DE SAUDE DA FAMILIA",
          "50" ~ "UNIDADE DE VIGILANCIA EM SAUDE", 
          "65" ~ "UNIDADE DE VIGILANCIA EPIDEMIOLOGIA (ANTIGO)",
          "66" ~ "UNIDADE DE VIGILANCIA SANITARIA (ANTIGO)",
          "15" ~ "UNIDADE MISTA", 
          "42" ~ "UNIDADE MOVEL DE NIVEL PRE-HOSP-URGENCIA/EMERGENCIA",
          "32" ~ "UNIDADE MOVEL FLUVIAL", 
          "40" ~ "UNIDADE MOVEL TERRESTRE",
          "75" ~ "TELESAÚDE", 
          "09" ~ "PRONTO SOCORRO DE HOSPITAL GERAL (ANTIGO)",
          "12" ~ "PRONTO SOCORRO TRAUMATO-ORTOPEDICO (ANTIGO)",
           .default = .data$PA_TPUPS)) #%>% dplyr::mutate(PA_TPUPS = as.factor(.data$PA_TPUPS))
      }

      #Tipo de Prestador
      if ("PA_TIPPRE" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_TIPPRE = dplyr::case_match(
        .data$PA_TIPPRE,
        "20" ~ "PRIVADO COM FINS LUCRATIVOS", 
        "22" ~ "PRIVADO OPTANTE PELO SIMPLES", "30" ~ "PUBLICO FEDERAL",
        "40" ~ "PUBLICO ESTADUAL", "50" ~ "PUBLICO MUNICIPAL",
        "60" ~ "PRIVADO SEM FINS LUCRATIVOS", 
        "61" ~"FILANTROPICO COM CNAS VALIDO", 
        "80" ~ "SINDICATO", .default = .data$PA_TIPPRE)) #%>% dplyr::mutate(PA_TIPPRE = as.factor(.data$PA_TIPPRE))
      }

      #Estabelecimento Mantido / Individual 
      if ("PA_MN_IND" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_MN_IND = dplyr::case_match(
        .data$PA_MN_IND,"M" ~ "Mantida", "I" ~ "Individual", 
        .default = .data$PA_MN_IND))
      }
    
        # # if (nome_proced == TRUE) {
      # #   sigtab_temp <- microdatasus::fetch_sigtab()
      # #   data <- dplyr::left_join(data, sigtab_temp, by = c(PA_PROC_ID = "COD"))
      # # }
      # 
    
      #Tipo de Financiamento da produção 
      if ("PA_TPFIN" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_TPFIN = dplyr::case_match(
        .data$PA_TPFIN,
        "01" ~ "Atenção Básica (PAB)", 
        "02" ~ "Assistência Farmacêutica",
        "04" ~ "Fundo de Ações Estratégicas e Compensações FAEC",
        "05" ~ "Incentivo - MAC", 
        "06" ~ "Média e Alta Complexidade (MAC)",
        "07" ~ "Vigilância em Saúde", .default = .data$PA_TPFIN)) #%>% dplyr::mutate(PA_TPFIN = as.factor(.data$PA_TPFIN))
      }

    
      #Complexidade do Procedimento
      if ("PA_NIVCPL" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_NIVCPL = dplyr::case_match(
        .data$PA_NIVCPL,
        "0" ~ "Não se Aplica", 
        "1" ~ "Atenção Básica",
        "2" ~ "Média Complexidade", 
        "3" ~ "Alta Complexidade",
        .default = .data$PA_NIVCPL)) #%>% dplyr::mutate(PA_NIVCPL = as.factor(.data$PA_NIVCPL))
      }
  
      
      #Instrumento de Registro (conforme explicado na página 2)
      if ("PA_DOCORIG" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_DOCORIG = dplyr::case_match(
        .data$PA_DOCORIG, 
        "C" ~ "BPA-C", 
        "I" ~ "BPA-I", 
        "P" ~ "APAC - Procedimento Principal",
        "S" ~ "APAC - Procedimento Secundário", 
        "A" ~ "RAAS - Atenção Domiciliar", 
        "B" ~ "RAAS - Psicossocial",.default = .data$PA_DOCORIG)) #%>% dplyr::mutate(PA_DOCORIG = as.factor(.data$PA_DOCORIG))
      }

      # # if (nome_ocupacao == TRUE) {
      # #   data <- dplyr::left_join(data, microdatasus::tabCBO, 
      # #                            by = c(PA_CBOCOD = "cod"))
      # #   data <- dplyr::rename(data, ocupacao = "nome")
      # # }
      
      #Motivo de saída ou zeros, caso não tenha  
      if ("PA_MOTSAI" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_MOTSAI = dplyr::case_match(
          .data$PA_MOTSAI, 
          "11" ~ "ALTA CURADO",
          "12" ~ "ALTA MELHORADO",
          "13" ~ "ALTA DA PUÉRPERA E PERMANÊNCIA DO RECÉM NASCIDO",
          "14" ~ "ALTA A PEDIDO", 
          "15" ~ "ALTA COM PREVISÃO DE RETORNO P/ ACOMPAN. DO PACIENT",
          "16" ~ "ALTA POR EVASÃO", 
          "17" ~ "ALTA DA PUÉRPERA E RECÉM NASCIDO",
          "18" ~ "ALTA POR OUTROS MOTIVOS", 
          "21" ~ "PERMANÊNCIA POR CARACTERÍSTICAS PRÓPRIAS DA DOENÇA",
          "22" ~ "PERMANÊNCIA POR INTERCORRÊNCIA", 
          "23" ~ "PERMANÊNCIA POR IMPOSSIBILIDADE SÓCIO-FAMILIAR",
          "24" ~ "PERMAN. POR PROCESSO-DOAÇÃO DE ÓRGÃOS-DOADOR VIVO",
          "25" ~ "PERMAN. POR PROCESSO-DOAÇÃO DE ÓRGÃOS-DOADOR MORTO",
          "26" ~ "PERMANÊNCIA POR MUDANÇA DE PROCEDIMENTO",
          "27" ~ "PERMANÊNCIA POR REOPERAÇÃO", 
          "28" ~ "PERMANÊNCIA POR OUTROS MOTIVOS", 
          "31" ~ "TRANSFERIDO PARA OUTRO ESTABELECIMENTO",
          "41" ~ "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNEC. MÉDICO ASSIST",
          "42" ~ "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNECIDA PELO I.M.L",
          "43" ~ "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNECIDA PELO I.M.L",
          "51" ~ "ENCERRAMENTO ADMINSTRATIVO", 
          "00" ~ "PRODUÇÃO SEM MOTIVO DE SAÍDA (BPA-C / BPA-I)",
          .default = .data$PA_MOTSAI)) #%>% dplyr::mutate(PA_MOTSAI = as.factor(.data$PA_MOTSAI))
      }

    
      #Indicador de Óbito (APAC)
      if ("PA_OBITO" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_OBITO = dplyr::case_match(
        .data$PA_OBITO, 
        "1" ~ "Sim (motivo de saída-ÓBITO)", 
        "0" ~"Nao houve ÓBITO", .default = .data$PA_OBITO)) # %>% dplyr::mutate(PA_OBITO = as.factor(.data$PA_OBITO))
      }
     
    
      #Indicador de Encerramento (APAC)
      if ("PA_ENCERR" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_ENCERR = dplyr::case_match(
        .data$PA_ENCERR,
        "1" ~ "Sim (motivo de saída-ENCERRAMENTO)",
        "0" ~ "Nao houve ENCERRAMENTO", .default = .data$PA_ENCERR)) #%>% dplyr::mutate(PA_ENCERR = as.factor(.data$PA_ENCERR))
      }

    
      #Indicador de Permanência (APAC)
      if ("PA_PERMAN" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_PERMAN = dplyr::case_match(
        .data$PA_PERMAN,
        "1" ~ "Sim (motivo de saída-PERMANÊNCIA)",
        "0" ~ "Nao houve a PERMANÊNCIA do paciente na unidade",
        .default = .data$PA_PERMAN)) #%>% dplyr::mutate(as.factor(.data$PA_PERMAN))
      }

    
     #Indicador de Alta (APAC)  
      if ("PA_ALTA" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_ALTA = dplyr::case_match(
        .data$PA_ALTA,
        "1" ~ "Sim (motivo de saída-PERMANÊNCIA)",
        "0" ~ "Nao houve a PERMANÊNCIA do paciente na unidade",
        .default = .data$PA_ALTA)) #%>% dplyr::mutate(as.factor(.data$PA_ALTA))
      }

      #Indicador de Transferência (APAC)
      if ("PA_TRANSF" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_TRANSF = dplyr::case_match(
        .data$PA_TRANSF,
        "1" ~ "Sim (motivo de saída-TRANSFERÊNCIA)",
        "0" ~ "Nao houve TRANSFERÊNCIA do paciente",
        .default = .data$PA_TRANSF)) #%>% dplyr::mutate(as.factor(.data$PA_TRANSF))
      }
  
      #Caráter de Atendimento (APAC ou BPA-I)
      if ("PA_CATEND" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_CATEND = dplyr::case_match(
        .data$PA_CATEND,
        "01" ~ "ELETIVO", "02" ~ "URGÊNCIA",
        "03" ~ "ACIDENTE NO LOCAL TRABALHO OU A SERViÇO DA EMPRESA",
        "04" ~ "ACIDENTE NO TRAJETO PARA O TRABALHO",
        "05" ~ "OUTROS TIPOS DE ACIDENTE DE TRÂNSITO",
        "06" ~ "OUTROS TIPOS LESÕES/ENVENENAMENTOS(AGENT.FIS./QUIM.",
        "99" ~ "INFORMAÇÃO INEXISTENTE  (BPA-C)",
        "00" ~ "CARATER DE ATENDIMENTO NÃO INFORMADO",
        "07" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "10" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "12" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "20" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "53" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "54" ~ "CARATER DE ATENDIMENTO INVALIDO", 
        "57" ~ "CARATER DE ATENDIMENTO INVALIDO", .default = .data$PA_CATEND)) #%>% dplyr::mutate(as.factor(.data$PA_CATEND))
      }

      # if ("PA_IDADE" %in% variables_names) {
      #   data <- data %>% dplyr::mutate(PA_IDADE = as.numeric(.data$PA_IDADE)) %>%
      #     dplyr::mutate(PA_IDADE = dplyr::case_match(.data$PA_IDADE,
      #                                                999 ~ 0, .default = .data$PA_IDADE))
      # }

      #Idade mínima do paciente para realização do procedimento 
      if ("IDADEMIN" %in% variables_names) {
        data <- data %>% dplyr::mutate(IDADEMIN = as.numeric(.data$IDADEMIN))
      }

      #Idade máxima do paciente para realização do procedimento
      if ("IDADEMAX" %in% variables_names) {
        data <- data %>% dplyr::mutate(IDADEMAX = as.numeric(.data$IDADEMAX))
      }

    
      #Compatibilidade com a faixa de idade do procedimento
      if ("PA_FLIDADE" %in% variables_names) {
        
        data <- data %>% dplyr::mutate(def_PA_FLIDADE = dplyr::case_match(
        .data$PA_FLIDADE,
       "0" ~ "IDADE NÃO EXIGIDA", 
       "1" ~ "IDADE COMPATIVEL COM O SIGTAP",
       "2" ~ "IDADE FORA DA FAIXA DO SIGTAP", 
       "3" ~ "IDADE INEXISTENTE", 
       "4" ~ "IDADE EM BRANCO",
       .default = .data$PA_FLIDADE)) #%>% dplyr::mutate(as.factor(.data$PA_FLIDADE))
      }

      #Sexo do paciente
      if ("PA_SEXO" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_SEXO = dplyr::case_match(
        .data$PA_SEXO,
        "0" ~ "Não exigido", 
        "M" ~ "Masculino", 
        "F" ~ "Feminino", .default = .data$PA_SEXO)) #%>% dplyr::mutate(as.factor(.data$PA_SEXO))
      }

      #Raça/Cor do paciente
      if ("PA_RACACOR" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_RACACOR = dplyr::case_match(
          .data$PA_RACACOR,
          "00" ~ "RAÇA/COR NÃO EXIGIDO", 
          "01" ~ "BRANCA",
          "02" ~ "PRETA", 
          "03" ~ "PARDA", 
          "04" ~ "AMARELA",
          "05" ~ "INDIGENA", 
          "99" ~ "SEM INFORMAÇÃO",
          "06" ~ "RAÇA/COR=06 (INDEVIDO)", 
          "09" ~ "RAÇA/COR=09 (INDEVIDO)",
          "1M" ~ "RAÇA/COR  (OUTROS INDEVIDOS)", 
          "1G" ~ "RAÇA/COR  (OUTROS INDEVIDOS)", 
          "1C" ~ "RAÇA/COR  (OUTROS INDEVIDOS)",
          "DE" ~ "RAÇA/COR  (OUTROS INDEVIDOS)", 
          "D" ~ "RAÇA/COR  (OUTROS INDEVIDOS)", 
          "87" ~ "RAÇA/COR  (OUTROS INDEVIDOS)",
          .default = .data$PA_RACACOR)) #%>% dplyr::mutate(as.factor(.data$PA_RACACOR))
      }

      # if ("PA_MUNPCN" %in% variables_names & municipality_data ==
      #     TRUE) {
      #   colnames(tabMun)[1] <- "PA_MUNPCN"
      #   data <- data %>% dplyr::mutate(PA_MUNPCN = as.integer(.data$IDADEMAX)) %>%
      #     dplyr::left_join(tabMun, by = "PA_MUNPCN")
      # }

      #Quantidade Produzida (APRESENTADA)
      if ("PA_QTDPRO" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_QTDPRO = as.integer(.data$PA_QTDPRO))
      }

      #Quantidade Aprovada do procedimento 
      if ("PA_QTDAPR" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_QTDAPR = as.numeric(.data$PA_QTDAPR))
      }
  
    
      #Valor Produzido (APRESENTADO) 
      if ("PA_VALPRO" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_VALPRO = as.numeric(.data$PA_VALPRO))
      }

      #Valor Aprovado do procedimento 
      if ("PA_VALAPR" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_VALAPR = as.numeric(.data$PA_VALAPR))
      }

      #Indica se a UF de residência do paciente é diferente da UF de localização do estabelecimento:
      if ("PA_UFDIF" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_UFDIF = dplyr::case_match(
        .data$PA_UFDIF,
        "1" ~ "Sim (houve invasão)", 
        "0" ~ "Não houve invasão", .default = .data$PA_UFDIF)) #%>% dplyr::mutate(as.factor(.data$PA_UFDIF))
      }
    
      #Indica se o município de residência do paciente é diferente do município de localização do estabelecimento:
      if ("PA_MNDIF" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_MNDIF = dplyr::case_match(
        .data$PA_MNDIF,
        "1" ~ "Sim (houve invasão)", 
        "0" ~ "Não houve invasão", .default = .data$PA_MNDIF)) #%>% dplyr::mutate(as.factor(.data$PA_MNDIF))
      }

      #Diferença do Valor Unitário do procedimento praticado na Tabela Unificada 
      #com Valor Unitário praticado pelo Gestor da Produção, multiplicado pela Quantidade Aprovada 
      if ("PA_DIF_VAL" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_DIF_VAL = as.numeric(.data$PA_DIF_VAL))
      }

      #Valor Unitário do Procedimento da Tabela VPA 
      if ("NU_VPA_TOT" %in% variables_names) {
        data <- data %>% dplyr::mutate(NU_VPA_TOT = as.numeric(.data$NU_VPA_TOT))
      }
      
      #Valor Unitário do Procedimento da Tabela SIGTAP
      if ("NU_PA_TOT" %in% variables_names) {
        data <- data %>% dplyr::mutate(NU_PA_TOT = as.numeric(.data$NU_PA_TOT))
      }

    
      #Indicativo de situação da produção produzida: 
      if ("PA_INDICA" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_INDICA = dplyr::case_match(
        .data$PA_INDICA,
        "5" ~ "Aprovado totalmente", 
        "6" ~ "Aprovado parcialmente",
        "0" ~ "Não aprovado", .default = .data$PA_INDICA)) #%>% dplyr::mutate(as.factor(.data$PA_INDICA))
      }
    
      #Etnia do paciente
      if ("PA_ETNIA" %in% variables_names) {
        data <- data %>% dplyr::mutate(def_PA_ETNIA = dplyr::case_match(
        .data$PA_ETNIA,
        "0001" ~ "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)",
        "0002" ~ "AIKANA (AIKANA, MAS SAKA,TUBARAO)",
        "0003" ~ "AJURU", "0004" ~ "AKUNSU (AKUNT'SU)",
        "0005" ~ "AMANAYE", "0006" ~ "AMONDAWA", 
        "0007" ~ "ANAMBE", "0008" ~ "APARAI (APALAI)",
        "0009" ~ "APIAKA (APIACA)", "0010" ~ "APINAYE (APINAJE/APINAIE/APINAGE)",
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
        "0035" ~ "BARE", "0036" ~ "BORORO (BOE)", "0037" ~
          "BOTOCUDO (GEREN)", "0038" ~ "CANOE", "0039" ~
          "CASSUPA", "0040" ~ "CHAMACOCO", "0041" ~
          "CHIQUITANO (XIQUITANO)", "0042" ~ "CIKIYANA (SIKIANA)",
        "0043" ~ "CINTA LARGA (MATETAMAE)", "0044" ~
          "COLUMBIARA (CORUMBIARA)", "0045" ~ "DENI",
        "0046" ~ "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)",
        "0047" ~ "DIAHUI (JAHOI, JAHUI, DIARROI)", "0048" ~
          "ENAWENE-NAWE (SALUMA)", "0049" ~ "FULNI-O",
        "0050" ~ "GALIBI (GALIBI DO OIAPOQUE, KARINHA)",
        "0051" ~ "GALIBI MARWORNO (GALIBI DO UACA, ARUA)",
        "0052" ~ "GAVIAO DE RONDONIA (DIGUT)", "0053" ~
          "GAVIAO KRIKATEJE", "0054" ~ "GAVIAO PARKATEJE (PARKATEJE)",
        "0055" ~ "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)",
        "0056" ~ "GUAJA (AWA, AVA)", "0057" ~ "GUAJAJARA (TENETEHARA)",
        "0058" ~ "GUARANI KAIOWA (PAI TAVYTERA)", "0059" ~
          "GUARANI M'BYA", "0060" ~ "GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)",
        "0061" ~ "GUATO", "0062" ~ "HIMARIMA (HIMERIMA)",
        "0063" ~ "INGARIKO (INGARICO, AKAWAIO, KAPON)",
        "0064" ~ "IRANXE (IRANTXE)", "0065" ~ "ISSE",
        "0066" ~ "JABOTI (JABUTI, KIPIU, YABYTI)", "0067" ~
          "JAMAMADI (YAMAMADI, DJEOROMITXI)", "0068" ~
          "JARAWARA", "0069" ~ "JIRIPANCO (JERIPANCO, GERIPANCO)",
        "0070" ~ "JUMA (YUMA)", "0071" ~ "JURUNA", "0072" ~
          "JURUTI (YURITI)", "0073" ~ "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)",
        "0074" ~ "KADIWEU (CADUVEO, CADIUEU)", "0075" ~
          "KAIABI (CAIABI, KAYABI)", "0076" ~ "KAIMBE (CAIMBE)",
        "0077" ~ "KAINGANG (CAINGANGUE)", "0078" ~ "KAIXANA (CAIXANA)",
        "0079" ~ "KALABASSA (CALABASSA, CALABACAS)",
        "0080" ~ "KALANCO", "0081" ~ "KALAPALO (CALAPALO)",
        "0082" ~ "KAMAYURA (CAMAIURA, KAMAIURA)", "0083" ~
          "KAMBA (CAMBA)", "0084" ~ "KAMBEBA (CAMBEBA, OMAGUA)",
        "0085" ~ "KAMBIWA (CAMBIUA)", "0086" ~ "KAMBIWA PIPIPA (PIPIPA)",
        "0087" ~ "KAMPE", "0088" ~ "KANAMANTI (KANAMATI, CANAMANTI)",
        "0089" ~ "KANAMARI (CANAMARI, KANAMARY, TUKUNA)",
        "0090" ~ "KANELA APANIEKRA (CANELA)", "0091" ~
          "KANELA RANKOKAMEKRA (CANELA)", "0092" ~ "KANINDE",
        "0093" ~ "KANOE (CANOE)", "0094" ~ "KANTARURE (CANTARURE)",
        "0095" ~ "KAPINAWA (CAPINAUA)", "0096" ~ "KARAJA (CARAJA)",
        "0097" ~ "KARAJA/JAVAE (JAVAE)", "0098" ~ "KARAJA/XAMBIOA (KARAJA DO NORTE)",
        "0099" ~ "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)",
        "0100" ~ "KARAPOTO (CARAPOTO)", "0101" ~ "KARIPUNA (CARIPUNA)",
        "0102" ~ "KARIPUNA DO AMAPA (CARIPUNA)", "0103" ~
          "KARIRI (CARIRI)", "0104" ~ "KARIRI-XOCO (CARIRI-CHOCO)",
        "0105" ~ "KARITIANA (CARITIANA)", "0106" ~ "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)",
        "0107" ~ "KATUENA (CATUENA, KATWENA)", "0108" ~
          "KATUKINA (PEDA DJAPA)", "0109" ~ "KATUKINA DO ACRE",
        "0110" ~ "KAXARARI (CAXARARI)", "0111" ~ "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)",
        "0112" ~ "KAXIXO", "0113" ~ "KAXUYANA (CAXUIANA)",
        "0114" ~ "KAYAPO (CAIAPO)", "0115" ~ "KAYAPO KARARAO (KARARAO)",
        "0116" ~ "KAYAPO TXUKAHAMAE (TXUKAHAMAE)", "0117" ~
          "KAYAPO XICRIM (XIKRIN)", "0118" ~ "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)",
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
        "0157" ~ "MUNDURUKU (MUNDURUCU)", "0158" ~ "MURA",
        "0159" ~ "NAHUKWA (NAFUQUA)", "0160" ~ "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)",
        "0161" ~ "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)",
        "0162" ~ "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)",
        "0163" ~ "NARAVUTE (NARUVOTO)", "0164" ~ "NAWA (NAUA)",
        "0165" ~ "NUKINI (NUQUINI, NUKUINI)", "0166" ~
          "OFAIE (OFAYE-XAVANTE)", "0167" ~ "ORO WIN",
        "0168" ~ "PAIAKU (JENIPAPO-KANINDE)", "0169" ~
          "PAKAA NOVA (WARI, PACAAS NOVOS)", "0170" ~
          "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)",
        "0171" ~ "PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)",
        "0172" ~ "PANKARARE (PANCARARE)", "0173" ~ "PANKARARU (PANCARARU)",
        "0174" ~ "PANKARARU KALANKO (KALANKO)", "0175" ~
          "PANKARARU KARUAZU (KARUAZU)", "0176" ~ "PANKARU (PANCARU)",
        "0177" ~ "PARAKANA (PARACANA, APITEREWA, AWAETE)",
        "0178" ~ "PARECI (PARESI, HALITI)", "0179" ~
          "PARINTINTIN", "0180" ~ "PATAMONA (KAPON)",
        "0181" ~ "PATAXO", "0182" ~ "PATAXO HA-HA-HAE",
        "0183" ~ "PAUMARI (PALMARI)", "0184" ~ "PAUMELENHO",
        "0185" ~ "PIRAHA (MURA PIRAHA)", "0186" ~ "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)",
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
        "0219" ~ "TUMBALALA", "0220" ~ "TUNAYANA", "0221" ~
          "TUPARI", "0222" ~ "TUPINAMBA", "0223" ~ "TUPINIQUIM",
        "0224" ~ "TURIWARA", "0225" ~ "TUXA", "0226" ~
          "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)",
        "0227" ~ "TXIKAO (TXICAO, IKPENG)", "0228" ~
          "UMUTINA (OMOTINA, BARBADOS)", "0229" ~ "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)",
        "0230" ~ "WAI WAI HIXKARYANA (HIXKARYANA)",
        "0231" ~ "WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)",
        "0232" ~ "WAI WAI XEREU (XEREU)", "0233" ~ "WAI WAI KATUENA (KATUENA)",
        "0234" ~ "WAI WAI MAWAYANA (MAWAYANA)", "0235" ~
          "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )", "0236" ~
          "WAIMIRI ATROARI (KINA)", "0237" ~ "WANANO (UANANO, WANANA)",
        "0238" ~ "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)",
        "0239" ~ "WAREKENA (UAREQUENA, WEREKENA)", "0240" ~
          "WASSU", "0241" ~ "WAURA (UAURA, WAUJA)",
        "0242" ~ "WAYANA (WAIANA, UAIANA)", "0243" ~
          "WITOTO (UITOTO, HUITOTO)", "0244" ~ "XAKRIABA (XACRIABA)",
        "0245" ~ "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)",
        "0246" ~ "XERENTE (AKWE, AWEN, AKWEN)", "0247" ~
          "XETA", "0248" ~ "XIPAIA (SHIPAYA, XIPAYA)",
        "0249" ~ "XOKLENG (SHOKLENG, XOCLENG)", "0250" ~
          "XOKO (XOCO, CHOCO)", "0251" ~ "XUKURU (XUCURU)",
        "0252" ~ "XUKURU KARIRI (XUCURU-KARIRI)", "0253" ~
          "YAIPIYANA", "0254" ~ "YAMINAWA (JAMINAWA, IAMINAWA)",
        "0255" ~ "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)",
        "0256" ~ "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)",
        "0257" ~ "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)",
        "0258" ~ "YAWALAPITI (IAUALAPITI)", "0259" ~
          "YAWANAWA (IAUANAUA)", "0260" ~ "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)",
        "0261" ~ "YUDJA (JURUNA, YURUNA)", "0262" ~
          "ZO'E (POTURU)", "0263" ~ "ZORO (PAGEYN)",
        "0264" ~ "ZURUAHA (SOROWAHA, SURUWAHA)", "X265" ~
          "AHANENAWA", "X266" ~ "AICABA", "X267" ~ "AIKAN\\u00c3-KWAS\\u00c1",
        "X268" ~ "AKUNTSU", "X269" ~ "ALANTESU", "X271" ~
          "AMAW\\u00c1KA", "X272" ~ "ANAC\\u00c9", "X273" ~
          "APURIN\\u00c3", "X274" ~ "ARAN\\u00c3", "X275" ~
          "ARAPA\\u00c7O", "X276" ~ "ARARA APOLIMA",
        "X277" ~ "ARARA DO ARIPUANA", "X278" ~ "ARIPUAN\\u00c1",
        "X279" ~ "ASSURINI", "X280" ~ "AWUAR\\u00c1",
        "X281" ~ "BORBA", "X282" ~ "CABIXI", "X283" ~
          "CAMARAR\\u00c9", "X284" ~ "CAMASURI", "X285" ~
          "CARA PRETA", "X286" ~ "CHARRUA", "X287" ~
          "CUJUBIM", "X288" ~ "DAW", "X289" ~ "GAVI\\u00c3O",
        "X290" ~ "GUARANI", "X291" ~ "HALANTESU", "X292" ~
          "HALOTESU", "X293" ~ "HENGAT\\u00da", "X294" ~
          "HIXKARYANA", "X295" ~ "HUPDE", "X296" ~ "HUPDES",
        "X297" ~ "IAUANAUA", "X298" ~ "IAUARETE A\\u00c7U",
        "X299" ~ "IKPENG", "X300" ~ "INAMBU", "X301" ~
          "INHABARANA", "X302" ~ "JAVAE", "X303" ~ "JENIPAPO",
        "X304" ~ "JENIPAPO-KANINDE", "X305" ~ "JIAHOI",
        "X306" ~ "KAIOWA", "X307" ~ "KAMPA", "X308" ~
          "KANELA", "X309" ~ "KARAFAWYANA", "X310" ~
          "KARARAO", "X311" ~ "KARUBO", "X312" ~ "KASSUP\\u00c1",
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
        "X335" ~ "MANCHINERI", "X336" ~ "MANDUCA", "X337" ~
          "MARIBONDO", "X338" ~ "MASSAKA", "X339" ~
          "MAWAYANA", "X340" ~ "MAW\\u00c9", "X341" ~
          "MAYORUNA", "X342" ~ "MIQUELENO", "X343" ~
          "MOKURI\\u00d1", "X344" ~ "MON ORO WARAM",
        "X345" ~ "MUTUM", "X346" ~ "MYKY", "X347" ~
          "NADEB", "X348" ~ "NAMBIKWARA", "X349" ~ "NEGAROT\\u00ca",
        "X350" ~ "NHENGATU", "X351" ~ "OFAIE XAVANTE",
        "X352" ~ "ON\\u00c7A", "X353" ~ "ORO AT", "X354" ~
          "ORO EO", "X355" ~ "ORO JOWIN", "X356" ~ "ORO MIYLIN",
        "X357" ~ "ORO MON", "X358" ~ "ORO N\\u00c1O",
        "X359" ~ "ORO WAM", "X360" ~ "ORO WARAM", "X361" ~
          "ORO WARAM XIJEIN", "X362" ~ "PACA", "X363" ~
          "PANKAR\\u00c1", "X364" ~ "PAPAGAIO", "X365" ~
          "PAYAY\\u00c1", "X366" ~ "PIPIPAN", "X367" ~
          "PIRATA", "X368" ~ "PUROBOR\\u00c1", "X369" ~
          "SABAN\\u00ca", "X370" ~ "SANUMA", "X371" ~
          "SAWENTES\\u00da", "X372" ~ "SILCY-TAPUYA",
        "X373" ~ "SIUCI", "X374" ~ "TABAJARA", "X375" ~
          "TAKUARA", "X376" ~ "TATU", "X377" ~ "TAWAND\\u00ca",
        "X378" ~ "TEF\\u00c9", "X379" ~ "TIMBIRA", "X380" ~
          "TOR\\u00c1 DO BAIXO GRANDE", "X381" ~ "TSUNHUM-DJAP\\u00c1",
        "X382" ~ "TUBAR\\u00c3O", "X383" ~ "TUPAIU",
        "X384" ~ "TUPI", "X385" ~ "TUPINAMB\\u00c1 DE BELMONTE",
        "X386" ~ "URUBU", "X387" ~ "URUBU KAAPOR", "X388" ~
          "URUP\\u00c1", "X389" ~ "WAI WAI", "X390" ~
          "WAIKISU", "X391" ~ "WAKALITES\\u00da", "X392" ~
          "WASSUSU", "X393" ~ "XEREU", "X394" ~ "XI EIN",
        "X395" ~ "XICRIN", "X396" ~ "XIPAYA", "X397" ~
          "XIRIANA", "X398" ~ "XIRUAI", "X399" ~ "YEPAMASS\\u00c3",
        "X400" ~ "TIRIY\\u00d3", "X401" ~ "YANOMAMI",
        "X402" ~ "ARARA", "X403" ~ "SAKIRIABAR", "X404" ~
          "TATZ", "X405" ~ "SEM INFORMACAO", .default = .data$PA_ETNIA)) #%>% dplyr::mutate(as.factor(.data$PA_ETNIA))
      }

      #Valor do Complemento Federal 
      if ("PA_VL_CF" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_VL_CF = as.numeric(.data$PA_VL_CF))
      }
   
      #Valor do Complemento Local
      if ("PA_VL_CL" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_VL_CL = as.numeric(.data$PA_VL_CL))
      }

    
      #Valor do Incremento
      if ("PA_VL_INC" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_VL_INC = as.numeric(.data$PA_VL_INC))
      }

    
      #Código de Identificação Nacional de Equipes,
      #para registrar a atuação das equipes na execução de ações de saúde 
      if ("PA_INE" %in% variables_names) {
        data <- data %>% dplyr::mutate(PA_INE = as.character(.data$PA_INE)) %>%
          dplyr::left_join(microdatasus::equipe, by = c(PA_INE = "COD"))
      }

    #UF e município do estabelecimento
    
    
    
    
    
    
    

    data <- tibble::as_tibble(data)
    data <- droplevels(data.table::as.data.table(data))
    data <- suppressWarnings(tibble::as_tibble(lapply(X = data, 
                                                      FUN = stringi::stri_unescape_unicode)))
    return(data)  
    
  }
  
}
  
  