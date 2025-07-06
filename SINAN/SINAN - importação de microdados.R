#library(rio)
library(read.dbc)
library(janitor)
library(tidyverse)
#library(lubridate)
library(data.table)#Por causa do as.IDate na corre??o de classes.




###Importação de todos os arquivos - Usei na ideia de futebol e viol?ncia dom?stica###
#Pasta com os arquivos
filenames <- list.files("C:/Users/b224552695/Desktop/SINAN/DBC",full.names = TRUE,pattern=".dbc")
#Lista dos arquivos
list_data <- lapply(filenames, read.dbc::read.dbc)
#Bind da lista de dbcs
sinan <- data.table::rbindlist(list_data, use.names = TRUE,fill=TRUE ) %>% clean_names()
rm(filenames,list_data)
gc()

# DBC NACIONAL ------------------------------------------------------------
sinan_2009 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR09.dbc"))
sinan_2009 <- clean_names(sinan_2009)
sinan_2010 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR10.dbc"))
sinan_2010 <- clean_names(sinan_2010)
sinan_2011 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR11.dbc"))
sinan_2011 <- clean_names(sinan_2011)
sinan_2012 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR12.dbc"))
sinan_2012 <- clean_names(sinan_2012)
sinan_2013 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR13.dbc"))
sinan_2013 <- clean_names(sinan_2013)
sinan_2014 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR14.dbc"))
sinan_2014 <- clean_names(sinan_2014)
sinan_2015 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR15.dbc"))
sinan_2015 <- clean_names(sinan_2015)
sinan_2016 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR16.dbc"))
sinan_2016 <- clean_names(sinan_2016)
sinan_2017 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR17.dbc"))
sinan_2017 <- clean_names(sinan_2017)
sinan_2018 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR18.dbc"))
sinan_2018 <- clean_names(sinan_2018)
sinan_2019 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR19.dbc"))
sinan_2019 <- clean_names(sinan_2019)
sinan_2020 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR20.dbc"))
sinan_2020 <- clean_names(sinan_2020)
sinan_2021 <- as_tibble(read.dbc("C:/Users/gabli/Desktop/r/Sinan/dbc/VIOLBR21.dbc"))
sinan_2021 <- clean_names(sinan_2021)

#Corre??o classe sinan
sinan_2018 %>% mutate(dt_notific = ymd(dt_notific),
                      dt_ocor = ymd(dt_ocor), dt_nasc = ymd(dt_nasc),
                      dt_invest = ymd(dt_invest),dt_obito = ymd(dt_obito),
                      dt_encerra = ymd(dt_encerra),
                      tp_uni_ext = as.numeric(as.character((tp_uni_ext))),
                      nu_idade_n = as.numeric(as.character((nu_idade_n)))) -> z_2018


#Merge em base ?nica
sinan <- bind_rows(sinan_2009,sinan_2010,sinan_2011,sinan_2012,sinan_2013,sinan_2014,sinan_2015,sinan_2016,sinan_2017,z_2018,
                        sinan_2019,sinan_2020,sinan_2021)

#Excluindo
rm(sinan_2009,sinan_2010,sinan_2011,sinan_2012,sinan_2013,sinan_2014,sinan_2015,sinan_2016,sinan_2017,sinan_2018,
   sinan_2019,sinan_2020,sinan_2021)


#Exportando csv.
export(sinan,"D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/csv/sinan_br_09_21_preliminar.csv")

#Exportando xlsx
export(sinan,"D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/xlsx/sinan_br_09_21_preliminar.xlsx")

#Exportando dta
export(sinan,"D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dta/sinan_br_09_21_preliminar.dta")


#Check missing
sapply(sinan_temp, function(x) sum(is.na(x)))





# Ler DBC UFs -------------------------------------------------------------
# AC<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLAC19.dbc")
# AL<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLAL19.dbc")
# AM<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLAM19.dbc")
# AP<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLAP19.dbc")
# BA<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLBA19.dbc")
# CE<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLCE19.dbc")
# DF<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLDF19.dbc")
# ES<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLES19.dbc")
# GO<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLGO19.dbc")
# MA<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLMA19.dbc")
# MG<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLMG19.dbc")
# MS<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLMS19.dbc")
# MT<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLMT19.dbc")
# PA<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLPA19.dbc")
# PB<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLPB19.dbc")
# PE<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLPE19.dbc")
# PI<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLPI19.dbc")
# PR<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLPR19.dbc")
# RJ<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLRJ19.dbc")
# RN<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLRN19.dbc")
# RO<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLRO19.dbc")
# RR<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLRR19.dbc")
# RS<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLRS19.dbc")
# SC<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLSC19.dbc")
# SE<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLSE19.dbc")
# SP<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLSP19.dbc")
# TO<-read.dbc("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/dbc/VIOLTO19.dbc")
# 
# 
# ##UNINDO AS BASES EM UMA S?
# #sinan=rbind(AC,AL,AM,AP,BA,CE,DF,ES,GO,MA,MG,MS,MT,PA,PB,PE,PI,PR,RJ,RN,RO,RR,RS,SC,SE,SP,TO)
# sinan_temp=rbind(AC,AL,AM,AP,BA,CE,DF,ES,GO,MA,MG,MS,MT,PA,PB,PE,PI,PR,RJ,RN,RO,RR,RS,SC,SE,SP,TO)
# sinan_temp <- clean_names(sinan_temp)
# sinan <- rbind(sinan,sinan_temp)
# ##DESCARTANDO O QUE N?O VAI SER MAIS USADO
# rm(sinan_temp)
# rm(AC)
# rm(AL)
# rm(AM)
# rm(AP)
# rm(BA)
# rm(CE)
# rm(DF)
# rm(ES)
# rm(GO)
# rm(MA)
# rm(MG)
# rm(MS)
# rm(MT)
# rm(PA)
# rm(PB)
# rm(PE)
# rm(PI)
# rm(PR)
# rm(RJ)
# rm(RN)
# rm(RO)
# rm(RR)
# rm(RS)
# rm(SC)
# rm(SE)
# rm(SP)
# rm(TO)
# 
# 
# sinan <- clean_names(sinan)



#Exportando base
#export(sinan,"sinan_09_18.dta")
#export(sinan_temp,"sinan2019.csv")

#Importando base completa e Vou juntar as duas.
base <- import("D:/Users/B224552695/Desktop/Trabalhos/Sinan/sinan_09_18.csv")
#Se a base do ano estiver em arquivo ?nico dbf, utilizar o excel e converter para csv
base_2019 <- import("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/sinan_br_2019.csv") #Acertei as datas na m?o.
#base_2019 <- import("D:/Users/B224552695/Desktop/Trabalhos/Sinan/Bases/VIOLEN19_OK_nnom_18-03-2021.dbf") 

sinan2019 <- read.csv("~/sinan2019.csv") #Juntei 2019 e exportei no formato csv. Agora importo o 2019 em formto csv. 

full_join(base,sinan2019)
#Caso dta
base_2019 %>% mutate(across(c(dt_notific),as_date)) -> base_2019
#base_2019 %>% mutate(across(c(dt_ocor),as_date)) -> base_2019
#base_2019 %>% mutate(across(c(dt_nasc),as_date)) -> base_2019
#base_2019 %>% mutate(across(c(dt_encerra),as_date)) -> base_2019
base_2019 %>% mutate(across(c(id_pais),as.numeric)) -> base_2019
base_2019 %>% mutate(across(c(def_trans),as.character)) -> base_2019
base_2019 %>% mutate(across(c(id_mn_ocor),as.character)) -> base_2019
base_2019 %>% mutate(across(c(out_vezes),as.character)) -> base_2019
base_2019 %>% mutate(across(c(les_autop),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_fisic),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_psico),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_tort),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_sexu),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_traf),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_finan),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_negli),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_infan),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_legal),as.character)) -> base_2019
base_2019 %>% mutate(across(c(viol_outr),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_forca),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_enfor),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_objeto),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_corte),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_quente),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_enven),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_fogo),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_ameaca),as.character)) -> base_2019
base_2019 %>% mutate(across(c(ag_outros),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_assedi),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_estupr),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_pudor),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_porno),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_explo),as.character)) -> base_2019
base_2019 %>% mutate(across(c(sex_outro),as.character)) -> base_2019
base_2019 %>% mutate(across(c(pen_oral),as.character)) -> base_2019
base_2019 %>% mutate(across(c(pen_anal),as.character)) -> base_2019
base_2019 %>% mutate(across(c(pen_vagina),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_dst),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_hiv),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_hepb),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_sang),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_semen),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_vagin),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_contr),as.character)) -> base_2019
base_2019 %>% mutate(across(c(proc_abort),as.character)) -> base_2019
base_2019 %>% mutate(across(c(cons_abort),as.character)) -> base_2019
base_2019 %>% mutate(across(c(cons_grav),as.character)) -> base_2019
base_2019 %>% mutate(across(c(num_envolv),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_sexual),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_pai),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_mae),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_pad),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_conj),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_excon),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_namo),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_exnam),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_filho),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_desco),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_irmao),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_conhec),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_cuida),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_patrao),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_inst),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_pol),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_propri),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_outros),as.character)) -> base_2019
base_2019 %>% mutate(across(c(rel_mad),as.character)) -> base_2019


#Caso integra??o feita atrv?s de dbc
sinan_temp %>% mutate(across(c(tp_not),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dt_notific),as.IDate)) -> sinan_temp
sinan_temp %>% mutate(across(c(sem_not),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(nu_ano),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(sg_uf_not),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_municip),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_unidade),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_regiona),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_rg_resi),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dt_ocor),as.IDate)) -> sinan_temp
sinan_temp %>% mutate(across(c(sem_pri),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dt_nasc),as.IDate)) -> sinan_temp
sinan_temp %>% mutate(across(c(cs_gestant),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(cs_raca),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(cs_escol_n),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(sg_uf),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_mn_resi),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(id_pais),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(sit_conjug),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(def_fisica),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(def_mental),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(def_visual),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(def_auditi),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(tran_ment),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(tran_comp),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(def_out),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(sg_uf_ocor),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(local_ocor),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(lesao_nat),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(lesao_corp),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(autor_sexo),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(autor_alco),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_saude),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_tutela),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_vara),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_abrigo),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_sentin),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_deam),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_dpca),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_deleg),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_mpu),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_mulher),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_creas),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_iml),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(enc_outr),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(rel_trab),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(rel_cat),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(classi_fin),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(evolucao),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dt_obito),as.IDate)) -> sinan_temp
sinan_temp %>% mutate(across(c(orient_sex),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(ident_gen),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(viol_motiv),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(cicl_vid),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(rede_sau),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(assist_soc),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(rede_educa),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(atend_mulh),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(cons_tutel),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(cons_ido),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(deleg_idos),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dir_human),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(mpu),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(deleg_cria),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(deleg_mulh),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(deleg),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(infan_juv),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(defen_publ),as.integer)) -> sinan_temp
sinan_temp %>% mutate(across(c(dt_encerra),as.IDate)) -> sinan_temp


x <- full_join(base,sinan_temp)
export(x,"sinan_br_09_19.dta")

