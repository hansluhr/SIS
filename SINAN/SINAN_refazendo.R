library(tidyverse)
library(janitor)

# Importando dbc ----------------------------------------------------------
start_time <- Sys.time()
sinan <-
  list.files("C:/Users/gabli/Desktop/r/SINAN/dbc",full.names = TRUE,pattern=".dbc") |>
  map(read.dbc::read.dbc) |>
  list_rbind() |> clean_names()
end_time <- Sys.time()
end_time - start_time 
rm(start_time,end_time)
gc()


skimr::skim(sinan)

#A notificação de interpessoal e autoprovocada começa em 2006, por adesão 
#Em 2011 a notificação torna - se obrigatória.
#Viol sexual e tent suicídio é comuniação imediata.
# É  importante  ressaltar  que  a  Ficha  de  Notificação  de  Violência  Interpessoal  e 
# Autoprovacada não se aplica à violência extrafamiliar cujas vítimas sejam adultos (20 a 59 anos) 
# do sexo masculino, como, por exemplo, brigas entre gangues, brigas nos estádios de futebol e 
# outras. 


#Transformando factor em numeric
sinan |> mutate(ano_not = ano_not |> levels() |> as.numeric() ) |> 
  select(ano_not) |> count(ano_not)


# Excluir variáveis com preenchimento até 2014 ----------------------------
cons_abort


#Excluir datas não utilizadas
# sinan |> select(!c(dt_invest,dt_obito, dt_digita, dt_transus, dt_transdm, dt_transsm, dt_transrm,
#                    dt_transrs, dt_transse) ) -> sinan

# Interessante olhar ------------------------------------------------------
#A profissão (CBO) do atendido é descrita na variável id_ocupa_n
id_ocupa_n

hora_ocor

evolucao

classi_fin

lesao_nat #Parece ser 

#Existe informação nas variáveis (especificar) espec
"def_espec"  "viol_espec" "ag_espec"   "sex_espec"  "cons_espec" "rel_espec"  "enc_espec"


# Atribuindo label as variáveis categóricas -------------------------------
#Faz sentido atribuir logo de início missing em todos os NAs?
start_time <- Sys.time()
set.seed(787)
sinan |> #slice_sample(n = 10000) |>
   #Excluindo variáveis não utilizadas
  select( !c(
       #Somente missing ou valor único
         tp_not,id_agravo,id_pais,nduplic,tpuninot,
         #Preenchimento até 2014
         pen_oral,pen_anal,pen_vagina,cons_abort,
         cons_grav,  cons_dst, cons_suic, cons_ment, cons_comp, cons_estre, cons_outr, cons_espec,
         lesao_nat,lesao_espe,lesao_corp, rel_sexual, enc_saude, enc_tutela, enc_vara, enc_abrigo,
         enc_sentin, enc_deam, enc_dpca, enc_deleg, enc_mpu, enc_mulher, enc_creas, enc_iml, enc_outr, enc_espec) ) |> 
  #Tratando preenchimento errado nas variáveis com código das UFs
  mutate(across( c(sg_uf_not,sg_uf,sg_uf_ocor), ~ fct_na_value_to_level(.x, level = "Missing") ) )  |> tabyl(nu_ano,sg_uf_ocor)

  
  
  
  #Copiar variáveis com código da UF. A ideia é utilizar variáveis com código para fazer os joins e utilizar variáveis com label nas tabelas.
  mutate(uf_not = sg_uf_not, 
         uf_resd = sg_uf, 
         uf_ocor = sg_uf_ocor, 
  #Colocando label nas variáveis de interesse.
  across( c(uf_not,uf_resd,uf_ocor), ~ fct_na_value_to_level(.x, level = "Missing") ) ) |> tabyl(uf_ocor)
      
      
    #Unidades da Federação. Fazer com somente um across
    across( c(sg_uf_not,  sg_uf, sg_uf_ocor), ~ ifelse(is.na(.), "Missing", as.character(.) ) ),
    across( c(sg_uf_not,  sg_uf, sg_uf_ocor), ~ #Poderia utilizar fct_recode
    recode(., '11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá", '17'= "Tocantins", 
    '21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
    '28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
    '41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
    '52'= "Goiás", '53'= "Distrito Federal", '99' = "CNRAC", 
    #Matém o missing e coloco preenchimento errado onde o preenchimento está errado.
    "Missing" = "Missing", 
     #Algumas observações estão com a uf preenchida de maneira errada. ex:/N
    .default = "Erro Preenchimento") |> as_factor() ), 
    
    #Incluindo regiões
    #Região de residência
    reg_resd = as.factor(case_when(
    #Região desconhecida
    sg_uf == "Missing" ~ "Missing",
    #Região Norte
    sg_uf %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    sg_uf %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    sg_uf %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    sg_uf %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ) ) |>
  tabyl(sg_uf,reg_resd) |> view()





    
    
    
    #Ano da Notificação, Ano de Ocorrência, Ano primeira digitação da notificação no sistema e Ano encerramento do caso.
    across( c(dt_notific, dt_ocor, dt_digita, dt_encerra), \(x) year(x) |> as_factor(), .names = "ano_{str_sub(.col, start = 4, end = 6)}"),
    
    #Ano de notificação. nu_ano é variável do sistema, ano de notificação.
    nu_ano = nu_ano |> case_match(NA ~ "Missing", .default = nu_ano) |> as_factor(),
    
    #Criando Variável idade
    idade = case_when(nu_idade_n <= 4000 ~ 0, nu_idade_n >4000 ~ nu_idade_n - 4000, TRUE ~ NA), 
    
    #Tipo de Notificação. Usar fct_recode ou recode ou case_match
    tp_not = case_when(tp_not == 1 ~ "Negativa", 
                       tp_not == 2 ~ "Individual", tp_not == 3 ~ "Surto",tp_not == 4 ~ "Agregado",
                       .default = "Missing") |> as_factor(),
    #Gestante
    cs_gestant = case_when(cs_gestant == 1 ~ "1º Trimestre", cs_gestant ==  2 ~ "2º Trimestre",
                           cs_gestant == 3 ~ "3º Trimestre", cs_gestant == 4 ~ "Idade gestacional ignorada",
                           cs_gestant == 5 ~ "Não",cs_gestant == 6 ~ "Não se aplica", cs_gestant == 9 ~"Ignorado",
    .default = "Missing") |> as_factor(),
    
     #Escolaridade
    cs_escol_n = 
      case_when(cs_escol_n == "00" ~ "Analfabeto",
                cs_escol_n == "01" ~ "1ª a 4ª série incompleta do EF", 
                cs_escol_n == "02" ~ "4ª série completa do EF (antigo 1° grau)",
                cs_escol_n == "03" ~ "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
                cs_escol_n == "04" ~ "Ensino fundamental completo (antigo ginásio ou 1° grau)",
                cs_escol_n == "05" ~ "Ensino médio incompleto (antigo colegial ou 2° grau)", 
                cs_escol_n == "06" ~ "Ensino médio completo (antigo colegial ou 2° grau)",
                cs_escol_n == "07" ~ "Educação superior incompleta", 
                cs_escol_n == "08" ~ "Educação superior completa", 
                cs_escol_n == "09" ~ "Ignorado", 
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
    cs_sexo = case_when(cs_sexo == "M" ~ "Homem", cs_sexo == "F" ~ "Mulher", cs_sexo == "I" ~ "Ignorado",
    .default = "Missing") |> as_factor(),
    
    #Raça\Cor
    cs_raca = case_when(cs_raca == 1 ~ "Branco", cs_raca == 2 ~ "Preto", cs_raca == 3 ~ "Amarelo", 
                             cs_raca == 4 ~ "Pardo", cs_raca == 5 ~ "Indígena", cs_raca == 9 ~ "Ignorado", 
    .default = "Missing") |> as_factor(),
    
    #Situação Conjugal do Paciente
    sit_conjug = case_when(sit_conjug == 1 ~ "Solteiro", sit_conjug == 2 ~ "Casado-União consensual",
                           sit_conjug == 3 ~ "Viúvo",sit_conjug == 4 ~ "Separado", sit_conjug == 8 ~ "Não se aplica", 
                           sit_conjug == 9 ~ "Ignorado",
    .default = "Missing") |> as_factor(),
    
    #Orientação Sexual do Paciente
    orient_sex = case_when(orient_sex == "1" ~ "Heterossexual", orient_sex == "2" ~ "Homossexual (gay-lésbica)",
                           orient_sex == "3" ~ "Bissexual", orient_sex == "8" ~ "Não se aplica", orient_sex == "9" ~ "Ignorado",
    .default = "Missing") |> as_factor(),
    
    #Identidade de Gênero
    ident_gen = case_when(ident_gen == 1  ~ "Travesti", ident_gen == 2 ~ "Transexual Mulher", 
                          ident_gen == 3 ~ "Transexual Homem", ident_gen == 8 ~ "Não se aplica", ident_gen == 9 ~ "Ignorado",
    .default = "Missing") |> as_factor(), 
    
    #Deficiências e transtorno mental e transtorno comportamental (39) starts_with("def_")
    #Ocorreu violência sexual (58), Procedimento realizado (59)
    across( 
    #Deficiências, transtorno mental e transtorno comportamental
    c(def_fisica, def_mental, def_visual, def_auditi, def_out, tran_ment, tran_comp,
    #Ocorreu Violência Sexual (58)
    sex_assedi, sex_estupr, sex_porno, sex_explo, sex_outro,
    #Proceimento realizado (59)
    proc_dst, proc_hiv, proc_hepb, proc_sang, proc_semen, proc_vagin, proc_contr, proc_abort,
    #Se sim, foi emitida a comunicação de acidente de trabalho (CAT) (67)
    rel_cat), \(x)
    case_match(x, "1" ~ "Sim", "2" ~ "Não", "8" ~ "Não se Aplica", "9" ~ "Ignorado", .default = "Missing") |> as_factor() ),
    
    #Lesão autoprovocada(54)
    les_autop = case_when(les_autop == "1" ~ "Sim",les_autop == "2" ~ "Não", 
                          les_autop == "8" ~ "Não se Aplica", les_autop == "9" ~ "Ignorado", .default = "Missing") |> as_factor(),
    #Número de envolvidos
    num_envolv = case_match(num_envolv, "1" ~ "Um", "2" ~ "Dois ou mais", "9" ~ "Ignorado", .default = "Missing") |> as_factor(),

    #Local de ocorrência
    local_ocor = case_when(
                 local_ocor == "01" ~ "Residência", local_ocor == "02" ~ "Habitação coletiva",
                 local_ocor == "03" ~ "Escola", local_ocor == "04" ~ "Local de prática esportiva", local_ocor == "05" ~ "Bar ou similar",
                 local_ocor == "06" ~ "Via pública", local_ocor == "07" ~ "Comércio/Serviços", local_ocor == "08" ~ "Indústrias/Construção",
                 local_ocor == "09" ~ "Outro", local_ocor == "99" ~ "Ignorado",
    .default = "Missing") |> as_factor(), 
    
    #Motivação da Violência. Informar se violência tem relação com caractéristicas da vitima
    viol_motiv = case_when(viol_motiv == "01" ~ "Sexismo",
                                     viol_motiv == "02" ~ "Homofobia/Lesbofobia/Bifobia/Transfobia",viol_motiv == "03" ~ "Racismo",
                                     viol_motiv == "04" ~ "Intolerância religiosa", viol_motiv == "05" ~ "Xenofobia", viol_motiv == "06" ~ "Conflito geracional",
                                     viol_motiv == "07" ~ "Situação de rua", viol_motiv == "08" ~ "Deficiência", viol_motiv == "09" ~ "Outros",
                                     viol_motiv == "88" ~ "Não se aplica", viol_motiv == "99" ~ "Ignorado",
    .default = "Missing") |> as_factor(), 
    
    #Ciclo de vida do provável autor da agressão.
    cicl_vid = case_when(cicl_vid == 1 ~ "Criança",cicl_vid == 2 ~ "Adolescente",cicl_vid == 3 ~ "Jovem",
                cicl_vid == 4 ~ "Pessoa adulta",cicl_vid == 5 ~ "Pessoa idosa", cicl_vid == 9 ~ "Ignorado",
    .default = "Missing") |> as_factor(),
    
    #Sexo do provável autor da violência (62)
    autor_sexo = case_match(autor_sexo,"1" ~ "Masculino", "2" ~ "Feminino", "3" ~ "Ambos os sexos",  "9" ~ "Ignorado", .default = "Missing") |> as_factor(), 
    
    #Possui deficiência ou transtorno(38) 
    #Ocorreu outra vez, tipos de violência (56), Meio de Agressão (57), relacionamento com a vítima (61), 
    #encaminhamento (65), violência relacionada ao trabalho (66)
    across( c(
    #Possui Deficiência ou transtorno
    def_trans,  
    #Ocorreu outra vez
    out_vezes,
    #Tipo de violência # start_with(viol_)
    viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr,          
    #Meio de Agressão
    ag_forca, ag_enfor, ag_objeto, ag_corte, ag_quente, ag_enven, ag_fogo, ag_ameaca, ag_outros,
    #Relacionamento com a vítima (61)
    rel_pai, rel_mae, rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, rel_irmao, rel_conhec, 
    rel_cuida, rel_patrao, rel_inst, rel_pol,rel_propri, rel_outros, rel_mad,
    #Encaminhamento (65). Esses encaminhamentos começam em 2014.
    rede_sau, assist_soc,  rede_educa, atend_mulh, cons_tutel, cons_ido, deleg_idos,  dir_human, mpu, deleg_cria, deleg_mulh,
    deleg, infan_juv, defen_publ,
    #Provável autor estava álcoolizado (63)
    autor_alco, rel_trab), \(x)
    #Violência relacionada ao Trabalho (66) #Poderia utilizar fct_na_value_to_level(level = "Missing") e fct_recode
     case_match(x, "1" ~ "Sim", "2" ~ "Não", "9" ~ "Ignorado", .default = "Missing") |> as_factor() ) ) -> sinan
 end_time <- Sys.time()
 end_time - start_time #Minha máquia 51.33 secs
rm(start_time, end_time)    
gc()


#Excluindo variáveis não utilizadas
sinan |> select(tpuninot,id_agravo)
#tpuninot todas as observações são NA
glimpse(sinan)
sinan |> tabyl(tp_not)


#Adicionar classificação final 
#Adicionar evolução
sinan |> filter(cs_sexo == "Mulher" & (les_autop!="Sim" & rel_propri!="Sim") ) |>
  tabyl(evolucao)

sinan |> filter(cs_sexo == "Mulher" & evolucao %in% c(3,4) ) |>
  tabyl(orient_sex)

#Verificando a exclusão de violência comunitária em Homens, na faixa etária 20 a 59 anos.
sinan |> filter(grupo_viol == "Comunitária") |>
  mutate(fxet = (idade %/% 5) * 5 ) |>
  tabyl(fxet,cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |> view()

#De fato ocorre retração na participação de homens na faixa etária 20 a 59 quando o grupo_viol é comunitário

sinan |> filter(grupo_viol == "Comunitária" & orient_sex %in% c("Heterossexual", "Não se aplica") ) |>
  mutate(fxet = (idade %/% 5) * 5 ) |>
  tabyl(fxet,cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") 


# Transformações deficiências ---------------------------------------------
### Criando variável para identifcar se vítima possui deficiência.
### Para ser PCD precisar ter ao menos uma das cinco condições a seguir: 
sinan |>
  mutate(
  #Possui deficiência se possuir ao menos um das cinco condições a seguir.  
  def = case_when(
    if_any( c(tran_ment,def_fisica,def_visual,def_mental,def_auditi), ~ .x == "Sim") ~ "Sim", .default = "Não") |> as_factor() ) -> sinan

#Contagem de notificações de pessoas com alguma das cinco deficiênicas
sinan |> count(def)


# Função para capturar os nomes das variáveis de deficiênca que atendem a condição "Sim"
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {  #Colocar para pegar o preenchimento quando todas forem diferentes de Sim.
    nomes_sim <- "Não/Ignorado/Aplica/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}

# Adicionar nova variável que captura os nomes das deficiências que atendem a condição "Sim"
sinan |> 
  select(tran_ment,def_fisica,def_visual,def_mental,def_auditi) %>% 
  #Aplica a função caputrar_nomes_sim as colunas de deficiência.
  mutate(nome_defs = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_defs) |> 
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)  
gc()



#A contagem de notificações de pessoas COM  deficiência precisa bater em def
sinan |> count(nome_defs, sort = TRUE) |> filter(nome_defs!= "Não/Ignorado/Aplica/Missing") |>
  summarise(n = sum(n) )
sinan |> count(def)
#A contagem de notificações de pessoas SEM deficiência precisa bater em def
sinan |> count(nome_defs, sort = TRUE) |> filter(nome_defs == "Não/Ignorado/Aplica/Missing") |>
  summarise(n = sum(n) )
sinan |> count(def)

 
### Individualizando as deficiências principais 
sinan %>% mutate( 
  tipodef = case_when(
    
    #Quando todas as defs são não, então sem deficiência
    if_all( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~ .x == "Não") ~ "Sem Deficiência",
    
    #Quando todas as def são missing, não é possível definir a deficiência.
    if_all( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~ .x == "Missing") ~ "Missing",
    
    #Quando todas as deficiências são não aplica, então não se aplica
    if_all( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~ .x == "Não se Aplica") ~ "Não se Aplica",
    
    #Quando todas as deficiências são missing, então missing
    if_all( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~ .x == "Ignorado") ~ "Ignorado",
    
    #Interação entre missing, Não se Aplica e Ignorado
    if_all( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~ .x %in% c("Não","Missing","Não se Aplica","Ignorado") )  ~ "Sem Informação",
    
    #Individualizar significa ter certeza das deficiências. 
    #Então def_fisica == "Sim" e def_visual == "Ignorado" não é ter certeza. Não sei o que é ignorado. 
    #Somente transtorno mental
    tran_ment == "Sim" & if_all( c(def_fisica, def_visual, def_mental, def_auditi), ~ .x == "Não") ~ "Trans. Mental",
    #Somente deficiência física
    def_fisica == "Sim" & if_all( c(tran_ment , def_visual, def_mental, def_auditi), ~ .x == "Não") ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & if_all( c(tran_ment, def_visual, def_fisica, def_auditi), ~ .x == "Não") ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & if_all(c (tran_ment, def_fisica, def_mental, def_auditi), ~ .x == "Não") ~ "Visual",
    #Somente deficiência Auditiva
    def_auditi == "Sim" & if_all(c (tran_ment, def_fisica, def_mental, def_visual), ~ .x == "Não")  ~ "Auditiva", 
    
    #Ao menos um tipo de deficiência. Como fazer melhor?
    if_any( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~.x == "Sim" ) &
    if_any( c(tran_ment,def_fisica, def_visual, def_mental, def_auditi), ~.x %in% c("Não","Missing","Não se Aplica","Ignorado") )  ~ "Ao menos uma") ) |>
  
  #Em deficiências múltiplas basta duas def == Sim. Aqui vai substituir Ao menos uma erradas.
  mutate(
    tipodef = case_when(
      # Se pelo menos duas variáveis factor forem "Sim", definir como "Múltiplas"
      rowSums(sinan |> select(tran_ment,def_fisica, def_visual, def_mental, def_auditi) == "Sim") >= 2 ~ "Múltiplas",
      .default = tipodef) |> as_factor() ) -> sinan

sinan |> tabyl(tipodef)

#Contagem deve apresentar somente uma def. Não é possível determinar as outras defs
sinan |> filter(tipodef == "Ao menos uma") |> count(nome_defs) |> view()

#Contagem deve apresentar ao menos duas defs
sinan |> filter(tipodef == "Múltiplas") |> count(nome_defs) |> view()  

#Ao fazer filter por tipodef, a contagem de nome_defs deve apresentar somente a deficiência filtrada.
sinan |> filter(tipodef == "Visual") |>  count(nome_defs)

#A contagem deve apresentar somente NAs. Eu alterei a função de identificar as deficiências. Agora Na é Não/Ignorado/Aplica/Missing
sinan |> filter(tipodef %in% c("Sem Deficiência","Ignorado","Missing","Não se Aplica","Sem Informação") ) |> count(nome_defs)

sinan |> count(def)
sinan |> filter(def == "Sim") |> tabyl(nu_ano,tipodef) |> adorn_totals(where = c("col") )

sinan |> tabyl(nome_defs, tipodef) |> adorn_totals(where = c("col","row") )
sinan |> filter(def == "Sim") |>  tabyl(nome_defs, tipodef) |> adorn_totals(where = c("col","row") ) |> view()
#tipodef são as deficiências individualizadas. 
#A contagem das deficências individualizdas precisa ser inferior a contagem destas deficiências em nome_defs.
#nomes_defs captura situações de ao menos uma e múltiplas.

sinan |> tabyl(tipodef)

### Na população da PNS precisa fazer esses cruzamentos entre deficiências



# Encaminhamento da notificação -------------------------------------------
#Variável indicando se houve encaminhamento
# sinan |> 
#   mutate(houve_enca = case_when(
#     #Se algum encaminhamento for sim, então houve encaminhamento.
#     if_any(c(rede_sau:defen_publ), ~.x == "Sim") ~ "Sim", 
#     #Se Algum encaminhamento é NA, não é possível dizer concluir o encaminhamento                             
#     if_any(c(rede_sau:defen_publ), ~.x == "Missing") ~ "Informação Incompleta", 
#     #Se todos são ignorados, então Ignorado                            
#     if_all(c(rede_sau:defen_publ), ~.x == "Ignorado") ~ "Ignorado",
#     #Se todos são Não, então não houve encaminhamento
#     if_all(c(rede_sau:defen_publ), ~.x == "Não") ~ "Não",
#     #Combinação de Não e Ignorado. Ignora
#     if_any( c(rede_sau:defen_publ), ~ .x %in% c("Não","Ignorado") )  ~ "Sem Informação",
#     #Aqui vai indicar se alguma combinação não foi contemplada. Erro deve estar sempre zerado
#     .default = "Erro") |> as_factor() ) -> sinan
 
sinan |> 
  mutate(houve_enca = case_when(
    #Se algum encaminhamento for sim, então houve encaminhamento.
    if_any(c(rede_sau:defen_publ), ~.x == "Sim") ~ "Sim", 
    #Se todos são Não, então não houve encaminhamento
    if_all(c(rede_sau:defen_publ), ~.x == "Não") ~ "Não",
     #Se Algum encaminhamento é NA, não é possível dizer concluir o encaminhamento                             
    if_all(c(rede_sau:defen_publ), ~.x == "Missing") ~ "Missing", 
    #Se todos são ignorados, então Ignorado                            
    if_all(c(rede_sau:defen_publ), ~.x == "Ignorado") ~ "Ignorado",
    #Combinação de Não, Ignorado e Missing
    if_all( c(rede_sau:defen_publ), ~ .x %in% c("Não","Missing","Ignorado") )  ~ "Sem Informação",
    #Aqui vai indicar se alguma combinação não foi contemplada. Erro deve estar sempre zerado
    .default = "Erro") |> as_factor() ) -> sinan

sinan |> tabyl(houve_enca)


#Check. Não pode registar encaminhamento
sinan |> filter(houve_enca == "Sem Informação") |>
  tabyl(nome_enca) |> head()




# Função para capturar os nomes das variáveis que atendem a condição "Sim"
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- "Não/Ignorado/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}


# Adicionar nova variável que captura os nomes das deficiências que atendem a condição "Sim"
sinan |> 
  select(rede_sau:defen_publ) %>%  
  #Aplica a função caputrar_nomes_sim as colunas de deficiência.
  mutate(nome_enca = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_enca) |>
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)    
gc()


#Houve encaminhamento
sinan |> tabyl(houve_enca)
sinan |> filter(nome_enca != "Não/Ignorado/Missing") |>
  count(nome_enca, sort = TRUE) |>  summarise(n = sum(n)) #Está batendo com o houve atendiment

#Se houve encaminhamento, qual foi o encaminhamento
sinan |> filter(houve_enca == "Sim") |>
  count(nome_enca, sort = TRUE) |> view()


#Se houve encaminhamento, qual foi o encaminhamento
sinan |> filter(houve_enca == "Sim") |>
  group_by(nome_enca) |>
  filter(str_detect(nome_enca,"mpu") ) |> count(sort = TRUE) |> view()

                  ######## INDIVIDUALIZAR OS ENCAMINHAMENTOS??? #####

# Tipos de violência -----------------------------------
#Variável individualizando os tipos de deficiência
sinan |>
  mutate(t_viol = case_when(
 #Quando todas as violências estão preenchidas com Não.
  #Essa situação não deveria ocorrer.
  #Neste caso, as notificações com preenchimento de viol_espec sugerem que essas notificações são tentativas de suicídio\lesão autoprovocada
  if_all( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "Não", 
  
  #Quando todas as violências não estão preenchidas
  #Esta situação não deveria ocorrer 
  if_all( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Missing") ~ "Missing",

  #Quando todas as violências são missing, então missing
  if_all( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Ignorado") ~ "Ignorado",

 #Interação entre missing, Não se Aplica e Ignorado
 if_all( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf,
          viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x %in% c("Não","Missing","Ignorado") )  ~ "Sem Informação", 

 #Individualizar significa ter certeza das deficiências. 
 #Então viol_fisic == "Sim" e viol_tort == "Ignorado" não é ter certeza. Ignorado pode ser Sim. 
 #Somente Violência Física
 viol_fisic == "Sim" & if_all( c(viol_psico, viol_tort, viol_sexu, viol_traf, 
                                 viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "V.Física",
 #Somente tortura
 viol_tort == "Sim" & if_all( c(viol_psico, viol_fisic, viol_sexu, viol_traf, 
                                 viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "Tortura", 
 
 #Somente violência sexual
 viol_sexu == "Sim" & if_all( c(viol_psico, viol_tort, viol_fisic, viol_traf, 
                                viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "V.Sexual",
 #Somente Negligência
 viol_negli == "Sim" & if_all( c(viol_psico, viol_tort, viol_fisic, viol_traf, 
                                viol_finan, viol_sexu, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "Negligência",
 
 #Somente Viol. Psicológica inclui psicológica e financeira/econômica),
  if_any( c(viol_psico,viol_finan), ~.x == "Sim" ) &
  if_all( c(viol_tort, viol_fisic, viol_traf, 
        viol_sexu, viol_infan, viol_legal, viol_outr), ~ .x == "Não") ~ "V.Psico",
 
 #Somente intervenção legal
 viol_legal == "Sim" & if_all( c(viol_psico, viol_tort, viol_fisic, viol_traf, 
                                 viol_finan, viol_sexu, viol_infan, viol_negli, viol_outr), ~ .x == "Não") ~ "Int. Legal",
 
  #Ao menos um tipo de violência é SIM.
 if_any( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~.x == "Sim" ) &
 if_any( c(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, 
           viol_finan, viol_negli, viol_infan, viol_legal, viol_outr), ~.x %in% c("Não","Missing","Ignorado") )  ~ "Ao menos uma") ) |>
 #Em violência múltiplas basta duas viols == "Sim".Aqui vai substituir Ao menos uma erradas.
 mutate(
   t_viol = case_when(
      # Se pelo menos duas variáveis factor forem "Sim", definir como "Múltiplas"
      rowSums(sinan |> select(viol_fisic, viol_psico, viol_tort, viol_sexu,
                              viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr) == "Sim") >= 2 ~ "Múltiplas",
      .default = t_viol) |> as_factor() ) -> sinan

#Essa tabela com a variável t_viol indica o número de notificações com somente violência física ou somente tortura
sinan |> tabyl(t_viol)
#Ao menos uma 18,8% e múltiplas 23,0%, ou seja, em 41% dos casos não é violência única.
#Se não é única, qual são as violências?



### Variável com o nome das violências
# Função para capturar os nomes das variáveis que atendem a condição "Sim"
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- "Não/Ignorado/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}


# Adicionar nova variável que captura os nomes das violências que atendem a condição "Sim"
sinan |> 
  select(viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr) %>%  
  #Aplica a função caputrar_nomes_sim as colunas de violência
  mutate(nome_viol = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_viol) |>
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)    
gc()


#Nome da violêcia "Não/Ignorado/Missing#
sinan |> filter(nome_viol == "Não/Ignorado/Missing") |> 
  select(t_viol,viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr, viol_espec) |> view()
#No caso das notificações onde não houve SIM em tipos de violência, às notificações com viol_espec preenchidas indicam tentativa de suicídio\lesão autoprovocada 


#Nome da violêcia diferente de "Não/Ignorado/Missing
sinan |> filter(nome_viol != "Não/Ignorado/Missing") |> 
  select(t_viol,viol_fisic, viol_psico, viol_tort, viol_sexu, viol_traf, viol_finan, viol_negli, viol_infan, viol_legal, viol_outr, viol_espec) |> view()
#No caso das notificações onde não houve SIM em tipos de violência, às notificações com viol_espec preenchidas indicam tentativa de suicídio\lesão autoprovocada 


sinan |> tabyl(nome_viol, t_viol) |> adorn_totals(where = c("col","row") ) |> arrange(desc(Total)) |> view()
#Como agregar o nome_viol de maneira a ser representativoe conseguir apresentar na tabela t_viol?

#Vou atribuir label other aos levels que aparecem em menos de 0.0001
sinan |> mutate(nome_viol = nome_viol |> fct_lump(prop = 0.0001) ) |>
  tabyl(nome_viol, t_viol) |> adorn_totals(where = c("col","row") ) |> arrange(desc(Total)) |> view()

#Precisa ter cuidado. Na base cada linha é uma notificação.
#Cada notificação pode ter mais de uma violência. Então o número de violências será superior ao número de notificações.
#Essa distinção precisa aparecer no atlas.

#A variável t_viol indica o número de notificações com somente v.física ou somente tortura ou somente xxx ou múltiplas
sinan |> tabyl(t_viol) |> adorn_totals()
#São X notificações onde ocorre somente violência física
#São x notificações onde ocorre somente violência sexual


#O total de violências é capturado através da variável já existente na base. (viol_tort)
#Nº total de violências: Torturas 
sinan |>
  filter(str_detect(nome_viol,"viol_tort") ) |> count(sort = TRUE, .by = c(nome_viol) ) |> #head()
  summarise(n = sum(n))
sinan |> tabyl(viol_tort)
#São x notificações onde ocorreu ao menos tortura.



# Meio de Agressão --------------------------------------------------------
### Variável com o nome do instrumento utilizado na(s) violências
# Função para capturar os nomes das variáveis que atendem a condição "Sim"
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- "Não/Ignorado/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}


# Adicionar nova variável que captura os nomes das violências que atendem a condição "Sim"
sinan |> 
  select(ag_forca, ag_enfor, ag_objeto, ag_corte, ag_quente, ag_enven,  ag_fogo, ag_ameaca, ag_outros) %>%  
  #Aplica a função caputrar_nomes_sim as colunas de violência
  mutate(nome_instr = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_instr) |>
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)    
gc()


sinan |> count(nome_instr, sort = TRUE) |> head(50)

#Número de instrumentos registrados na notificação de violências contra mulher
sinan |> filter(cs_sexo == "Mulher") |> drop_na(idade) |>
  #slice_sample(n = 10000) |>
  #mutate(nome_viol = nome_viol |> fct_lump(prop = 0.0001) ) |>
  mutate(nome_instr = nome_instr |> fct_lump(prop = 0.009) ) |> #tabyl(nome_instr) |> adorn_totals()
  ggplot() +
  geom_histogram(aes(x=idade,fill = nome_instr),binwidth  = 3 ) 

sinan |> filter(cs_sexo == "Mulher" & (les_autop!="Sim" & rel_propri!="Sim") & str_detect(nome_viol,"viol_negli") ) |> drop_na(idade) |>
    mutate(nome_instr = nome_instr |> fct_lump(prop = 0.0025) ) |>
  group_by(idade = (idade %/% 10) * 10 ) |>
    tabyl(nome_instr,idade) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |> view()


sinan |> select(nome_viol) |>
  filter(str_detect(nome_viol,"viol_sexu") ) |> view()


sinan |> filter(cs_sexo == "Mulher" & (les_autop!="Sim" & rel_propri!="Sim") & str_detect(nome_viol,"viol_negli") ) |> drop_na(idade,viol_espec) |>
  select(viol_espec) |>
  filter(str_detect(viol_espec,"DROGADICAO") ) |> view()

# Grupo agressor ----------------------------------------------------------
#Vairável com o(s) provávei(s) autore(s) da agressão:
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- "Não/Ignorado/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}

# Adicionar nova variável que captura os nomes das violências que atendem a condição "Sim"
sinan |> 
  select(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
         rel_irmao, rel_conhec, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros, rel_mad ) %>%  
  #Aplica a função caputrar_nomes_sim as colunas de violência
  mutate(nome_rel = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_rel) |>
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)    
gc()


#Criando grupo agressor.
#Variável individualizando os tipos de deficiência
sinan |>
  mutate(
    grupo_viol = case_when(
    #Se pelo menos duas variáveis factor forem "Sim", definir como "Misto"
    #Misto será as combinações não convergentes na identificação dos grupos de violência.
    #Exemplo, a combinação rel_pai (doméstica) e rel_patrao (Institucional). Não está contemplada nas combianções de doméstica ou institucional.
    #Por ser difícil elaborar classificação de todas as possíveis combinações de autorias, estou considerando como misto àquelas fora dos grupos tradicionas  
    rowSums(sinan |> 
    select(rel_pai, rel_mae, rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, rel_irmao, 
    rel_conhec, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros, rel_mad) == "Sim") >= 2 ~ "Misto") ) |> 
    
    #Nesta seção cria o grupos tradicionais
    mutate(grupo_viol = case_when(
    #Quando todas os autores estão preenchidas com Não.
    #Essa situação não deveria ocorrer.
    #Neste caso, as notificações com preenchimento de viol_espec sugerem que essas notificações são tentativas de suicídio\lesão autoprovocada ou acidentes.
    if_all( c(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
              rel_irmao, rel_conhec, rel_cuida, rel_patrao, 
              rel_inst, rel_pol, rel_propri, rel_outros, rel_mad), ~ .x == "Não") ~ "Não",
  
    #Quando todas as notificações estão com autoria missing
    #Esta situação não deveria ocorrer 
    if_all( c(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
              rel_irmao, rel_conhec, rel_cuida, rel_patrao, 
              rel_inst, rel_pol, rel_propri, rel_outros, rel_mad), ~ .x == "Missing") ~ "Missing",
    
    #Quando todas as notificações estão com autoria ignorada
    #Esta situação não deveria ocorrer.
    if_all( c(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
              rel_irmao, rel_conhec, rel_cuida, rel_patrao, 
              rel_inst, rel_pol, rel_propri, rel_outros, rel_mad), ~ .x == "Ignorado") ~ "Ignorado",
    
    #Interação entre missing, Não se Aplica e Ignorado. Neste caso não existe informação sobre provável autor.
    #Esta situação não deveria ocorrer
    if_all( c(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
              rel_irmao, rel_conhec, rel_cuida, rel_patrao,
              rel_inst, rel_pol, rel_propri, rel_outros, rel_mad), ~ .x %in% c("Não","Missing","Ignorado") )  ~ "Sem Informação",
    
    #No caso de grupos de violência o individualizar os grupos considera:
    #Em doméstica. Se qualquer autoria familar ou combinação de autoria familar for SIM e outras variáveis Não, Missing ou Ignorado, então estou considerando Domésitca 
    
    #Situação de autoria doméstica sim (rel_mae) e autorias outras "Não, Missing, "Ignorado" (rel_pol) é considerado como violência doméstica e não como "Ao menos uma"
    #Estou assumindo que faz pouco sentido combinação de autorias além dos grupos conhecidos.
    
    #Portanto, em autorias não existe a categoria "Ao menos uma". 
    #Não quero fazer hipóteses sobre as categorias não preenchidas. 

    #Violência doméstica
    if_any( c(rel_pai, rel_mae, rel_mad, rel_pad, rel_conj, rel_excon,
    rel_namo, rel_exnam, rel_filho, rel_irmao, rel_cuida, rel_mad), ~.x == "Sim")  &
      
    if_all( c(rel_conhec, rel_desco, rel_patrao, rel_inst, 
              rel_pol, rel_propri, rel_outros), ~ .x %in% c("Não","Missing","Ignorado") ) ~ "Doméstica",
 
    #Violência comunitária
    #Pode conter a combinação rel_conhec e rel_desco
    if_any( c(rel_conhec, rel_desco ), ~.x == "Sim") & 
    if_all( 
      c(#Doméstica
              rel_pai, rel_mae, rel_mad, rel_pad, rel_conj, rel_excon,
              rel_namo, rel_exnam, rel_filho, rel_irmao, rel_cuida,
              #Institucional
              rel_patrao, rel_inst, rel_pol, 
              #Autoprovocada
              rel_propri,
              #Outros
              rel_outros), ~ .x %in% c("Não","Missing","Ignorado") ) ~ "Comunitária", 
    
    #Violência Institucional
    if_any( c(rel_patrao, rel_inst, rel_pol), ~.x == "Sim") &
    if_all(
      c(#Doméstica
      rel_pai, rel_mae, rel_mad, rel_pad, rel_conj, rel_excon,
      rel_namo, rel_exnam, rel_filho, rel_irmao, rel_cuida,
      #Comunitária
      rel_conhec, rel_desco,
      #Autoprovocada
      rel_propri,
      #Outros
      rel_outros ), ~ .x %in% c("Não","Missing","Ignorado") ) ~ "Institucional",
    
    #Violência Autoprovocada
    if_any( c(rel_propri), ~.x == "Sim") &
      if_all(
        c(#Doméstica
          rel_pai, rel_mae, rel_mad, rel_pad, rel_conj, rel_excon,
          rel_namo, rel_exnam, rel_filho, rel_irmao, rel_cuida,
          #Comunitária
          rel_conhec, rel_desco,
          #Institucional 
          rel_patrao, rel_inst, rel_pol,
          #Outros
          rel_outros ), ~ .x %in% c("Não","Missing","Ignorado") ) ~ "Autoprovocada",

    #Violência Outros
    if_any( c(rel_outros), ~.x == "Sim") &
  
      if_all(
        c(#Doméstica
          rel_pai, rel_mae, rel_mad, rel_pad, rel_conj, rel_excon,
          rel_namo, rel_exnam, rel_filho, rel_irmao, rel_cuida,
          #Comunitária
          rel_conhec, rel_desco,
          #Institucional 
          rel_patrao, rel_inst, rel_pol,
          #Autoprovocada
          rel_propri ), ~ .x %in% c("Não","Missing","Ignorado") ) ~ "Outros", .default = grupo_viol ) |> as_factor() ) -> sinan  
gc()
#Ao menos um tipo de violência é SIM.
#O ao menos uma está pegando o múltiplas
# grupo_viol != "Misto" & (
# if_any( c(rel_pai, rel_mae, rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, rel_irmao, 
#          rel_conhec, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros, rel_mad), ~.x == "Sim" ) &
#   
# if_any( c(rel_pai, rel_mae, rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, rel_irmao, 
#             rel_conhec, rel_cuida, rel_patrao, rel_inst, 
#             rel_pol, rel_propri, rel_outros, rel_mad), ~.x %in% c("Não","Missing","Ignorado") ) )  ~ "Ao menos uma", .default = grupo_viol ) ) -> sinan  

sinan |> tabyl(nome_rel, grupo_viol) |> view()

#Verificando se existe autor fora do grupo. Precisa ser zero.
sinan |> filter(grupo_viol == "Doméstica" & str_detect(nome_rel,"rel_pol") ) |> count()
sinan |> filter(grupo_viol == "Autoprovocada" & str_detect(nome_rel,"rel_outros") ) |> count()




# Código da ocupação ------------------------------------------------------
#Importando ocupação. Fornecido pelo sinan net
cbo <- rio::import("C:/Users/gabli/Desktop/r/Sinan/tab_sinan/TAB_SINANNET/OCUPANET.DBF") |>
  clean_names() |> mutate(nm_ocupaca = nm_ocupaca |> as_factor() )
#Tamanho do código cbo na variável id_ocupa_n
sinan |>  mutate(len = id_ocupa_n |> as.character() |> str_length() ) |>
  tabyl(len) # filter(len < 6) |> select(id_ocupa_n,len) |> view()
#O correto é código de lenght 6. 
#O tabyl indica a maioria das notificações não estão preenchidas de maneira correta.
#Exemplo:
#Existe id_ocupa_n com lenght 6, preenchida com ponto (7152.1). Não posso afirmar nada sobre este código.
left_join(x = sinan, y = cbo, by = join_by(id_ocupa_n)) |>
  select(id_ocupa_n,nm_ocupaca) |>
  mutate(len = id_ocupa_n |> as.character() |> str_length() ) |>
  filter(len == 6 & is.na(nm_ocupaca) ) |> view()


#Atribuição dos labels de ocupação.
left_join(x = sinan, y = cbo, by = join_by(id_ocupa_n)) |>
  rename(ocupa = nm_ocupaca) -> sinan
rm(cbo)  
gc()



# Municípios --------------------------------------------------------------
# sinan |> select(starts_with("id_") ) |>  
#   #Proporção de preenchimento
#   summarize(across(everything(), ~ mean(!is.na(.x) ) ) )

sinan |> select(sg_uf,id_mn_resi) |>
  filter(
  str_detect(sg_uf, "^35") ) |> view()

#   #Proporção de preenchimento
#   summarize(across(everything(), ~ mean(!is.na(.x) ) ) )


'11' = "Rondônia", '12' ="Acre", '13'= "Amazonas", '14'= "Roraima", '15'= "Pará",'16'= "Amapá", '17'= "Tocantins", 
'21'= "Maranhão", '22'= "Piauí", '23'= "Ceará", '24'= "Rio Grande do Norte", '25'= "Paraíba", '26'= "Pernambuco", '27'= "Alagoas", 
'28'= "Sergipe", '29' ="Bahia", '31'= "Minas Gerais", '32'= "Espírito Santo", '33'= "Rio de Janeiro", '35'= "São Paulo", 
'41'= "Paraná", '42'= "Santa Catarina", '43'= "Rio Grande do Sul", '50'= "Mato Grosso do Sul",'51'= "Mato Grosso", 
'52'= "Goiás", '53'= "Distrito Federal", '99' = "CNRAC", 


#Importando municípios.
mun <- rio::import("C:/Users/b224552695/Desktop/r/Sinan/tab_sinan/TAB_SINANNET/MUNICNET.DBF") |>
  clean_names() |> select(id_municip, nm_municip) |> 
  #Transformando em factor
  mutate( across(everything(), ~ as_factor(.x) ) ) 

#ID_MUNICIP. Município onde a unidade de saúde está localizado. Município de notificação
#ID_MN_RESI. Código do município de residência do caso notificado.
#ID_MN_OCOR Código e nome do município de ocorrência do evento notificado.

#Join do nome dos municípios.
#Município de notificação
left_join(x = sinan, y = mun |> rename(munic_not = nm_municip), by = join_by("id_municip" == "id_municip") ) |>
  #Municípios de residência
  left_join(x = _, y = mun |> rename(munic_resd = nm_municip), by = join_by("id_mn_resi" == "id_municip") ) |>
  #Município de ocorência
  left_join(x = _, y = mun |> rename(munic_ocor = nm_municip), by = join_by("id_mn_ocor" == "id_municip") ) |>  
  
  #Alguns municípios estão com missing, códigos inexistentes, símbolos ou letras no lugar do código de identificação.
  #No correspondente nome do município, vou colocar Erro de Preenchimento
  mutate(across( c(munic_not,munic_ocor,munic_resd), ~ fct_na_value_to_level(.x, level = "Erro de Preenchimento")  ) ) -> sinan

#Checks
sinan |> 
  filter(munic_ocor == "Erro de Preenchimento") |> select(ano_not,id_mn_ocor,munic_ocor) |>  drop_na() |> view()


# Unidade de atendimento e notificação --------------------------------------------------
uni <- rio::import("C:/Users/gabli/Desktop/r/Sinan/tab_sinan/TAB_SINANNET/UNIDTOTAL.DBF") |>
  clean_names() |> select(id_unidade, nm_unidade)
#nu_cgc_uni é o cnpj



sinan |> select(hora_ocor) |>  
  #Proporção de preenchimento
  summarize(across(everything(), ~ mean(!is.na(.x) ) ) )
