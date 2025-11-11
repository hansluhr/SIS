

# Importando a base -------------------------------------------------------
importar_empilhar_dbc <- function(pasta_dbc) {
  # Carregar pacotes necessários
  requireNamespace("read.dbc", quietly = TRUE)
  requireNamespace("data.table", quietly = TRUE)
  requireNamespace("janitor", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  
  # Lista todos os arquivos .dbc na pasta (recursivamente, se desejar)
  arquivos <- list.files(
    path = pasta_dbc,
    pattern = "\\.dbc$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .dbc encontrado na pasta especificada.")
  }
  
  message("🔹 ", length(arquivos), " arquivos encontrados. Importando...")
  
  # Lê, converte e limpa cada arquivo
  lista_dados <- lapply(arquivos, function(arq) {
    message("   → Lendo: ", basename(arq))
    tmp <- read.dbc::read.dbc(arq)
    data.table::setDT(tmp)
    janitor::clean_names(tmp)
  })
  
  #Empilha todos em um único data.table
  dados_empilhados <- data.table::rbindlist(lista_dados, 
                                            use.names = TRUE, 
                                            fill = TRUE)
  
  message("✅ Importação concluída! Total de linhas: ", nrow(dados_empilhados))
  return(dados_empilhados)
  
}

#Realiza a importação e empilhamento
base <- importar_empilhar_dbc(pasta_dbc = "C:/Users/P224552695/Desktop/r/SIS/Bases/SINAN_Tuberculose")
rm(importar_empilhar_dbc)


# Tratamento base SIA tuberculose -----------------------------------------

#Variáveis sem label
trat_super, doenca_tra
situa_9m, situa_12m
test_sensi,test_molec 


REGIONET.DBF parece conter os códigos das regiões de saúde.





set.seed(787)
base |> slice_sample(n = 10000) |>
  mutate(    
    
    
    tp_not = case_when(tp_not == 2 ~ "Individual", 
                       .default = tp_not),
    
    #Criando Variável idade
    idade = case_when(nu_idade_n <= 4000 ~ 0, nu_idade_n > 4000 ~ nu_idade_n - 4000, TRUE ~ NA),
    
    #Escolaridade
    def_cs_escol_n = 
      case_when(cs_escol_n == "0" ~ "Analfabeto",
                cs_escol_n == "1" ~ "1ª a 4ª série incompleta do EF", 
                cs_escol_n == "2" ~ "4ª série completa do EF (antigo 1° grau)",
                cs_escol_n == "3" ~ "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
                cs_escol_n == "4" ~ "Ensino fundamental completo (antigo ginásio ou 1° grau)",
                cs_escol_n == "5" ~ "Ensino médio incompleto (antigo colegial ou 2° grau)", 
                cs_escol_n == "6" ~ "Ensino médio completo (antigo colegial ou 2° grau)",
                cs_escol_n == "7" ~ "Educação superior incompleta", 
                cs_escol_n == "8" ~ "Educação superior completa", 
                cs_escol_n == "9" ~ "Ignorado", 
                cs_escol_n == "10" ~ "Não se aplica",
                .default = "Missing") |> 
      #Ordem dos Levels de escolaridade
      fct_relevel(
        c("Analfabeto",
          "1ª a 4ª série incompleta do EF",
          "4ª série completa do EF (antigo 1° grau)",
          "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
          "Ensino fundamental completo (antigo ginásio ou 1° grau)",
          "Ensino médio incompleto (antigo colegial ou 2° grau)", 
          "Ensino médio completo (antigo colegial ou 2° grau)",
          "Educação superior incompleta", 
          "Educação superior completa", 
          "Missing", "Ignorado","Não se aplica") ),
    
    #Sexo do Paciente
    def_sexo = case_when(cs_sexo == "M" ~ "Homem", cs_sexo == "F" ~ "Mulher", cs_sexo == "I" ~ "Ignorado",
                         .default = "Missing") |> as_factor(), 
    #Gestante
    def_cs_gestant = case_when(cs_gestant == 1 ~ "1º Trimestre", cs_gestant ==  2 ~ "2º Trimestre",
                               cs_gestant == 3 ~ "3º Trimestre", cs_gestant == 4 ~ "Idade gestacional ignorada",
                               cs_gestant == 5 ~ "Não",cs_gestant == 6 ~ "Não se aplica", cs_gestant == 9 ~"Ignorado",
                               .default = "Missing") |> as_factor(),
    
    #Raça\Cor
    def_cs_raca = case_when(cs_raca == 1 ~ "Branco", cs_raca == 2 ~ "Preto", cs_raca == 3 ~ "Amarelo", 
                        cs_raca == 4 ~ "Pardo", cs_raca == 5 ~ "Indígena", cs_raca == 9 ~ "Ignorado", 
                        .default = "Missing") |> as_factor(),
    
    #Tratamento
    def_tratamento = case_when(tratamento == 1 ~ "Caso Novo", 
                               tratamento == 2 ~ "Recidiva",
                               tratamento == 3 ~ "Reingresso após Abandono",
                               tratamento == 4 ~ "Não sabe",
                               tratamento == 5 ~ "Transferência",
                               tratamento == 6 ~ "Pós-óbito", 
                               .default = "Missing") |> as_factor(),
    #Situação do paciente que se encontra ou não em cárcere
    #institucio
    def_institucio = case_when(institucio == 1 ~ "Não", 
                               institucio == 2 ~ "Presídio",
                               institucio == 3 ~ "Asilo",
                               institucio == 4 ~ "Orfanato",
                               institucio == 5 ~ "Hospital psiquiátrico",
                               institucio == 6 ~ "Outro",
                               institucio == 9 ~ "Ignorado",
                               .default = "Missing") |> as_factor(),
    
    #Resultado do teste tuberculínico:
    def_teste_tube = case_when(teste_tube == 1 ~ "Não reator",  #Não reator (0 - 4mm)
                               institucio == 2 ~ "Reator fraco", #Reator fraco (5 - 9mm)
                               institucio == 3 ~ "Reator forte", #Reator forte (10 mm ou mais)
                               institucio == 4 ~ "Não realizado",
                               .default = "Missing") |> as_factor(),
    
    
    #Resultado da radiografia do tórax por ocasião da notificação
    def_raiox_tora = case_when(raiox_tora == 1 ~ "Suspeito",
                               raiox_tora == 2 ~ "Normal",
                               #opção 3 diz respeito a outras
                               #alterações não compatíveis com a
                               #tuberculose
                               raiox_tora == 3 ~ "Outra Patologia",
                               raiox_tora == 4 ~ "Não realizado",
                               .default = "Missing") |> as_factor(),
    
   #Forma 
   def_forma = case_when(forma == 1 ~ "Pulmonar",
                         forma == 2 ~ "Extrapulmonar",
                         forma == 3 ~ "Pulmonar + Extrapulmonar",
                         .default = "Missing") |> as_factor(),
   
   #Localização extrapulmonar da tuberculose nos casos em que o paciente
   #apresente a forma clínica igual a 2 ou 3
   def_extrapu1_n = case_when(extrapu1_n == 1 ~ "Pleural",
                              extrapu1_n == 2 ~ "Gang. Perif.",
                              extrapu1_n == 3 ~ "Geniturinária",
                              extrapu1_n == 4 ~ "Óssea",
                              extrapu1_n == 5 ~ "Ocular",
                              extrapu1_n == 6 ~ "Miliar",
                              extrapu1_n == 7 ~ "Meningoencefálico",
                              extrapu1_n == 8 ~ "Cutânea",
                              extrapu1_n == 9 ~ "Laringea",
                              extrapu1_n == 10 ~ "Outra",
                              .default = "Missing") |> as_factor(),
   
   #Idem a variável anterior. 
   #Preenchido quando houver mais de uma forma extrapulmonar
   def_extrapu2_n = case_when(extrapu2_n == 1 ~ "Pleural",
                              extrapu2_n == 2 ~ "Gang. Perif.",
                              extrapu2_n == 3 ~ "Geniturinária",
                              extrapu2_n == 4 ~ "Óssea",
                              extrapu2_n == 5 ~ "Ocular",
                              extrapu2_n == 6 ~ "Miliar",
                              extrapu2_n == 7 ~ "Meningoencefálico",
                              extrapu2_n == 8 ~ "Cutânea",
                              extrapu2_n == 9 ~ "Laringea",
                              extrapu2_n == 10 ~ "Outra",
                              .default = "Missing") |> as_factor(), 
   
  #Informar se existem agravos associados à tuberculose por ocasião da notificação
  across(.cols =  c( starts_with("agrav") & !c(agravoutde) ), 
         .names = "def_{.col}", 
         \(x)
          case_match(x, 
                     "1" ~ "Sim", 
                     "2" ~ "Não", 
                     "9" ~ "Ignorado", .default = "Missing") |> as_factor() ), 
  
  #Baciloscopia de escarro (diagnóstico)
  across(.cols = c( starts_with("bacilo") |
  #Cultura de escarro              
                c(cultura_es, cultura_ou, hiv) ),
  .names = "def_{.col}", 
  \(x)
  case_match(as.numeric(x), 
             1 ~ "Positiva", 
             2 ~ "Negativa", 
             3 ~ "Não realizada",
             4 ~ "Não se aplica", .default = "Missing") |> as_factor() ),
  
  #Histopatologia - Resultado do exame histopatólogico para diagnóstico de TB
  def_histopatol = case_when(histopatol == 1 ~ "Baar Positivo",
                             histopatol == 2 ~ "Sugestivo de TB",
                             histopatol == 3 ~ "Não sugestivo de TB",
                             histopatol == 4 ~ "Em andamento",
                             histopatol == 5 ~ "Não realizado",
                   .default = "Missing") |> as_factor(),
  #Drogas
  #rifampicin, isoniazida, etambutol, estreptomi, pirazinami, etionamida, outras
  across(.cols = c(rifampicin, isoniazida, etambutol, 
                   estreptomi, pirazinami, etionamida,
                   outras),
         
         .names = "def_{.col}", 
         \(x)
         case_match(as.numeric(x), 
                    1 ~ "Sim", 
                    2 ~ "Não", 
                    
                    .default = "Missing") |> as_factor() ),
  
  #Situação de encerramento (situa_ence)
  def_situa_ence = case_when(situa_ence == 1 ~ "Cura",
                             situa_ence == 2 ~ "Abandono",
                             situa_ence == 3 ~ "Óbito por TB",
                             situa_ence == 4 ~ "Óbito por outras causas",
                             situa_ence == 5 ~ "Transferência",
                             situa_ence == 6 ~ "Mudança de Diagnóstico",
                             situa_ence == 7 ~ "TB-DR",
                             situa_ence == 8 ~ "Mudança de Esquema",
                             situa_ence == 9 ~ "Falência",
                             situa_ence == 10 ~ "Abandono Primário",
                             .default = "Missing") |> as_factor(),
   
   #População privada de liberdade, população de rua, profissionais de saúde, 
   #imigrantes, beneficiário
   across(.cols = c(pop_liber, pop_rua, pop_saude, pop_imig, benef_gov),
  
  .names = "def_{.col}", 
  \(x)
  case_match(as.numeric(x), 
             1 ~ "Sim", 
             2 ~ "Não", 
             9 ~ "Ignorado",
             .default = "Missing") |> as_factor() ),
  
  #Se Transferência
  def_transf = case_when(transf == 1 ~ "Mesmo município",
                         transf == 2 ~ "Município diferente (mesma UF)",
                         transf == 3 ~ "UF diferente",
                         transf == 4 ~ "País diferente",
                         transf == 9 ~ "Ignorado",
                         .default = "Missing") |> as_factor() ) |>
  
  count(def_transf)
  
  
  
  
  
  
  
  
  

