library(tidyverse)
library(janitor)

# Aplicar os mesmos labels a todas as variáveis do tipo factor
base %>% mutate(
  across( c(ag_forca:ag_outros), ~ ifelse(is.na(.), "Missing", as.character(.) ) ),
  across( c(ag_forca:ag_outros), ~ fct_recode(., "Sim" = "1", "Não" = "2", "Ignorado" = "9") ) ) |> glimpse()

#Importando base SINAN
load("C:/Users/b224552695/Desktop/r/Sinan/sinan_09_2022_preliminar_transtorno.RData")
year <- c(2022)

#Grupo\Autoria da Violência
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F"  & ano_not %in% year) |>
  tabyl(grupo_viol, show_missing_levels = FALSE, show_na = FALSE) |> arrange(desc(n)) |> adorn_totals(where = "row")  |>
  rio::export(x = _, "grupo_viol.xlsx")
#Proporção dos prováveis autores\grupo de violência

#Tipo de violência, nos grupos\autorias da violência
sinan |> filter(grupo_viol != "Autoprovocada"  & cs_sexo == "F"  & ano_not %in% year) |>
   tabyl(t_viol,grupo_viol,#cs_raca,
         show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"t_violência_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são violência física.  
  

#Local de Ocorrência da violência 
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |>
   tabyl(local_ocor,grupo_viol,#cs_raca,
         show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"local_ocor_por_grupo_autoria.xlsx")
#Das violências domésticas, x% ocorrem na residência. 


#Situação conjugal
sinan |> filter(grupo_viol != "Autoprovocada" &  cs_sexo == "F" & ano_not %in% year) |>
  tabyl(sit_conjug,grupo_viol,#cs_raca,
        show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"situacao_conjugal_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são Solteiras. 

#Ocorreu outraz vezes?
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> droplevels() |>
  tabyl(out_vezes,grupo_viol,#fxet,
        show_missing_levels = FALSE, show_na = FALSE) %>% 
  adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"ocorreu_outra_vez_por_grupo_autoria.xlsx")
#Das violências domésticas, x% a violência é repetição. 

#Instrumento da ocorrência
sinan |> filter(grupo_viol != "Autoprovocada" &  cs_sexo == "F" & ano_not %in% year) |>
  tabyl(ag_corte,grupo_viol,#cs_raca,
        show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front")
#Das violências domésticas, x% utilizam arma de fogo


# ##Horário da agressão por dia da semana
# sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" & cs_sexo == "F" & ano_not %in% year) |>
#   #Criando dia da Semana
#   mutate(wday = lubridate::wday(dt_notific, label = TRUE),
#   #Criando faixa de horários
#   # f_hora_ocor = hora_ocor |> as.character() |> as.numeric() ) |>
#   f_hora_ocor = hora_ocor |> as.POSIXct(format = "%H:%M") |> floor_date("1 hour")  ) |> 
#   #count(hora_ocor,f_hora_ocor) |> view()
# 
#   tabyl(f_hora_ocor,wday,
#   show_missing_levels = TRUE, show_na = TRUE) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |>
#   adorn_ns(position = "front")


#Criando faixa etária Mulher+
sinan %>% mutate(fxet = as.factor( 
  case_when(nu_idade_n<=4009 ~ "0 a 9",
            nu_idade_n>=4010  & nu_idade_n<=4014 ~ "10 a 14",
            nu_idade_n>=4015  & nu_idade_n<=4019 ~ "15 a 19",
            nu_idade_n>=4020  & nu_idade_n<=4024 ~ "20 a 24",
            nu_idade_n>=4025  & nu_idade_n<=4029 ~ "25 a 29",
            nu_idade_n>=4030  & nu_idade_n<=4034 ~ "30 a 34",
            nu_idade_n>=4035  & nu_idade_n<=4039 ~ "35 a 39",
            nu_idade_n>=4040  & nu_idade_n<=4044 ~ "40 a 44",
            nu_idade_n>=4045  & nu_idade_n<=4049 ~ "45 a 49",
            nu_idade_n>=4050  & nu_idade_n<=4054 ~ "50 a 54",
            nu_idade_n>=4055  & nu_idade_n<=4059 ~ "55 a 59",
            nu_idade_n>=4060  & nu_idade_n<=4064 ~ "60 a 64",
            nu_idade_n>=4065  & nu_idade_n<=4069 ~ "65 a 69",
            nu_idade_n>=4070  & nu_idade_n<=4074 ~ "70 a 74",
            nu_idade_n>=4075  & nu_idade_n<=4079 ~ "75 a 79",
            nu_idade_n>=4080  & !is.na(nu_idade_n) ~ "80 ou mais",
            is.na(nu_idade_n) ~ "Sem informação"))) -> sinan

#Faixa etária por autoria\grupo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> droplevels() |>
  tabyl(fxet,grupo_viol,#autor_sexo,
        show_missing_levels = FALSE, show_na = FALSE) %>% 
  adorn_totals(where=c("col","row") ) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"faixa_etaria_sexo_autor_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são na faixa 30 a 34. 
#Olhar sexo do provável autor da agressão.


#Faixa etária por tipo de iolência
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" &  cs_sexo == "F" & ano_not %in% year) |>
  tabyl(fxet,t_viol,# autor_sexo,
        show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"faixa_etaria_por_tipo_violência.xlsx")
#Da viol sexual, 46,8% são na faixa 10 a 14. 


#Situação conjugal, por faixa etária
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "Mulher" & ano_not %in% year) |>
  tabyl(fxet,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x=_,"sit_conju_fext_doméstica.xlsx")
#Das solteiras vítimas de violência doméstica, 18,7 % estão na faixa 10 a 14 anos. 


#Situação conjugal por faixa etária
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |>
  tabyl(fxet,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front")
#Da faixa etária 30 a 34 anos, 44,5% são Casado-União estável.


#Tipo de violência por situação conjugal.
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(t_viol,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "t_viol_x_sit_conjug_doméstica.xlsx")
#Da violência física, as solteiras são 34,2% das vítimas


#Raça\Cor por tipo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(cs_raca,t_viol , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "cs_raca_x_t_viol_doméstica.xlsx")

#Raca\cor por escolaridade
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  #Ordenando escolaridade
  mutate(cs_escol_n = fct_relevel(
    cs_escol_n,"1ª a 4ª série incompleta do EF","4ª série completa do EF (antigo 1° grau)","5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
    "Ensino fundamental completo (antigo ginásio ou 1° grau)","Ensino médio incompleto (antigo colegial ou 2° grau)","Ensino médio completo (antigo colegial ou 2° grau)",
    "Educação superior incompleta","Educação superior completa","Ignorado","Não se aplica","Missing") ) |>
  tabyl(cs_raca,cs_escol_n , show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "cs_raca_x_escol_doméstica.xlsx")
#Dos brancos, 5,3% são 1ª a 4ª série incompleta do EF

#Raça\cor por faixa etária
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(fxet,cs_raca , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "fxet_x_cs_raca_doméstica.xlsx")
#Na fxet 0 a 9, os brancos são 39,1% 


#Raça\cor por situação conjutal
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(cs_raca,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "cs_raca_x_sit_conj_doméstica.xlsx")
#Dos pardos 32,2% são Casado-União Consensual


#Suspeita de álcool
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(autor_alco) |> rio::export(x=_,"alco_doméstica.xlsx")
#Em ao menos 33,05% das notificações de violência doméstica contra mulher, o autor estava com suspeita de uso de álcool. 



# Encaminhamento ----------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base SINAN
load("C:/Users/gabli/Desktop/r/Sinan/sinan_09_2022_preliminar_transtorno.RData")
year <- c(2022)


#Houve algum encaminhamento?  
#Composição do tipo de violência, nos grupos de violência.
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & grupo_viol == "Doméstica") |>
  tabyl(atend_mulh)
#Lembrar que esse não é o único encaminhamento possível.



#Interações entre os encaminhamentos
#Até 2014
"enc_saude" "enc_tutela" "enc_vara"   "enc_abrigo" "enc_sentin" "enc_deam"   "enc_dpca"   "enc_deleg" 
"enc_mpu"    "enc_mulher" "enc_creas"  "enc_iml"    "enc_outr"   "enc_espec"

#A partide de 2014
"rede_sau" "assist_soc"  "rede_educa" "atend_mulh" "cons_tutel" "cons_ido" "deleg_idos"  "dir_human" "mpu" "deleg_cria" "deleg_mulh"
"deleg" "infan_juv" "defen_publ"



#Interalção entre delgacia da mulher, atendimento 
mosaicplot( table(subset(sinan,grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year)$deleg_mulh,
      subset(sinan,grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year)$atend_mulh) )


mosaicplot( table(sinan$deleg_mulh, sinan$atend_mulh) )

mosaicplot( ftable(sinan$deleg_mulh, sinan$atend_mulh,sinan$dir_human) )


#Interações encaminhamento violência contra mulher.
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> nrow() #Total de Observações na filtragem selecionada
#São 221.240 notificações de violência contra mulher 


sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) -> sinan_mulher
#É melhor fazer a filtragem dentro da ftable. O idela é atribuir o encaminhamento aos microdados
ftable(
  sinan_mulher$assist_soc, #Rede Assistência Social
  sinan_mulher$rede_educa, #Rede educação
  sinan_mulher$atend_mulh, #Atendimento a mulher
  sinan_mulher$cons_tutel, #Conselho tutelar
  sinan_mulher$cons_ido,   #Conselho do idoso
  sinan_mulher$deleg_idos, #Delegacia do idoso
  sinan_mulher$dir_human,  #Centro de referência dos direitos Humanos 
  sinan_mulher$mpu,        #MPU
  sinan_mulher$deleg_cria, #Delegacia Especializada de Proteção à Criança e Adolescente 
  sinan_mulher$deleg_mulh, #Delegacia de atendimento à mulher
  sinan_mulher$deleg,      #Outras Delegacias
  sinan_mulher$infan_juv,  #Justiça da infência e da juventude 
  sinan_mulher$defen_publ,  #Defensoria Pública
  exclude = c("manter NA")) |> as_tibble() -> sinan_mulher

sinan_mulher |> #head(n = 1000) |> 
  #Renomeando encaminhamentos
  rename(assist_soc = Var1, rede_educa = Var2, atend_mulh = Var3, cons_tutel = Var4,cons_ido = Var5,
         deleg_idos = Var6, dir_human = Var7, mpu = Var8, deleg_cria = Var9, deleg_mulh = Var10, deleg = Var11,
         infan_juv = Var12, defen_publ = Var13 ) |> 
  #Identificando os SIM
  mutate(houve_enca = case_when(if_any(c(rede_sau:defen_publ), ~.x == "Sim") ~ "Sim", 
                                if_any(c(rede_sau:defen_publ), is.na) ~ "Informação Incompleta", 
                                if_all(c(rede_sau:defen_publ), ~.x == "Ignorado") ~ "Ignorado",
                                if_all(c(rede_sau:defen_publ), ~.x == "Não") ~ "Não",
                                #Como Tratar os outros casos ?
                                .default = "Desconhecido") )  -> sinan_mulher
#Total e proporção de encaminhamento por tipo de encaminhamento
sinan_mulher |>  
  summarise(encaminhamento = sum(Freq), .by = c(houve_enca))  |>
  mutate(nots = sum(encaminhamento),
         p_encaminhament = round( (encaminhamento/nots)*100,2) )

#Se Houve encaminhamento, qual foi o encaminhamento
sinan_mulher |> #head(1000) |>
  filter(Freq != 0 & houve_enca == "Sim") |> tabyl(mpu)
  
#Outra maneira de identificar se houve encaminhamento 
sinan |> 
  mutate(houve_enca = case_when(if_any(c(rede_sau:defen_publ), ~.x == "Sim") ~ "Sim", 
                                if_any(c(rede_sau:defen_publ), ~.x == "Missing") ~ "Informação Incompleta", 
                                if_all(c(rede_sau:defen_publ), ~.x == "Ignorado") ~ "Ignorado",
                                if_all(c(rede_sau:defen_publ), ~.x == "Não") ~ "Não",
                                #Como Tratar os outros casos ?
                                .default = "Desconhecido") ) -> sinan

#Total e proporção dos encaminhamentos, por tipo de encaminhamento
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> 
  summarise(n = n(), .by = houve_enca) |>
  mutate(prop_enc = n/sum(n)*100) |> adorn_totals()
  
  
#Se Houve encaminhamento, qual foi o encaminhamento?
#Filtrando base para buscar interações
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim") |> nrow()
#192434 linhas   

#Interações entre variáveis de encaminhamento.
ftable(
       #rede_sau não aparece no dicionário. Parece substituir enc_saude após 2014 
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),rede_sau),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),assist_soc),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),rede_educa), 
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),atend_mulh),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),cons_tutel),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),cons_ido),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),deleg_idos),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),dir_human),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),mpu),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),deleg_cria),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),deleg_mulh),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),deleg),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),infan_juv),
       pull(sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year & houve_enca == "Sim"),defen_publ),
       exclude = c("abc") ) |> as_tibble() |>  
       #Exlcuindo interações com zero ocorrências 
       filter(Freq!=0) |>
       #Renomeando encaminhamentos
       rename(rede_sau = Var1,  assist_soc = Var2, rede_educa = Var3, atend_mulh = Var4, cons_tutel = Var5,cons_ido = Var6,
       deleg_idos = Var7, dir_human = Var8, mpu = Var9, deleg_cria = Var10, deleg_mulh = Var11, deleg = Var12,
       infan_juv = Var13, defen_publ = Var14) |> arrange(desc(Freq)) -> inter

#Função para capturar as interações com "Sim"
capturar_interacoes <- function(x) {
  if_else(x == "Sim", cur_column(), "")
}

#Interações
inter %>% 
  mutate(across(-Freq, capturar_interacoes)) %>%
  unite("interacoes", everything(), sep = ", ") %>%
  #filter(interacoes != "") %>% #select(interacoes) |>
  #ExCluindo vírgula e números
  mutate(interacoes = gsub('NA|,', '', interacoes),
         interacoes = str_squish(interacoes) ) |> 
  #Separando as interações da contagem destas interações
  separate(interacoes, into = c("interacoes", "ocorrencia"), sep = " (?=\\d)", convert = TRUE) |> 
  summarise(ocorrencia = sum(ocorrencia), .by = c(interacoes) ) |> 
  #Do total de internações 11.283 foram encaminhadas a rede_sau e deleg
  filter(str_detect(interacoes,"cons_tutel")) |> summarise(ocorrencia = sum(ocorrencia)) # filter( grepl('mpu', interacoes))
         

    
    
# Violência doméstica e autoria -------------------------------------------
library(tidyverse)
library(janitor)
  
#Importando base SINAN
load("C:/Users/gabli/Desktop/r/Sinan/sinan_09_2022_preliminar_transtorno.RData")
year <- c(2022)
  
  
varlist <- c("rel_pai","rel_mae","rel_mad","rel_pad","rel_conj","rel_excon","rel_namo","rel_exnam","rel_filho","rel_irmao","rel_cuida") 
#.data[[i]]

# Criar uma lista para armazenar as tabelas
table_list <- list()

for (i in varlist) { 
  table <- sinan |> filter(grupo_viol == "Doméstica" & cs_sexo == "F" & ano_not %in% year) |>
   tabyl(!!sym(i), t_viol,show_missing_levels = FALSE, show_na = FALSE) |> adorn_totals(where=c("col","row")) %>%
    adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
    adorn_ns(position = "front")
    
  # Armazenar a tabela na lista
  table_list[[i]] <- table
        
  }


# Escrever as tabelas em um único arquivo Excel com sheets diferentes
writexl::write_xlsx(table_list, path = "output.xlsx")
rm(i,varlist,table,table_list)


