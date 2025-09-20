library(microdatasus)
library(tidyverse)
library(janitor)

sia_ac <- fetch_datasus(information_system = "SIA-PA",
                     uf = c('AC'),
                     year_start = 2024,
                     month_start = 01,
                     year_end = 2025,
                     month_end = 06) |> process_sia() 


sia_pb <- fetch_datasus(information_system = "SIA-PA",
                        uf = c('PB'),
                        year_start = 2024,
                        month_start = 01,
                        year_end = 2025,
                        month_end = 06) |> process_sia()



sia_go <- fetch_datasus(information_system = "SIA-PA",
                        uf = c('GO'),
                        year_start = 2024,
                        month_start = 01,
                        year_end = 2025,
                        month_end = 06) |> process_sia()


sia_rs <- fetch_datasus(information_system = "SIA-PA",
                        uf = c('RS'),
                        year_start = 2024,
                        month_start = 01,
                        year_end = 2025,
                        month_end = 06) |> process_sia()


sia <- bind_rows(sia_ac,sia_go,sia_pb) |> 
  clean_names()  


rm(sia_ac,sia_go,sia_pb)



load("C:/Users/B224552695/Desktop/pa.RData")

sia |>
  filter(pa_proc_id %in% c("0901010057",
                           "0901010111",
                           "0901010120") )  



sia |>
  filter(pa_proc_id %in% c("0901010057","0201010666","0203020081",
                           "0211040029","0301010072","0301010307",
                           "0901010111","0203020022","0409060089",
                           "0901010120","0409060305") ) -> x


0901010057 OCI INVESTIGAÇÃO DIAGNÓSTICA DE CÂNCER DE COLO DO ÚTERO
0201010666 BIOPSIA DO COLO UTERINO
0203020081 EXAME ANATOMO-PATOLOGICO DO COLO UTERINO - BIOPSIA
0211040029 COLPOSCOPIA
0301010072 CONSULTA MEDICA EM ATENÇÃO ESPECIALIZADA
0301010307 TELECONSULTA MÉDICA NA ATENÇÃO ESPECIALIZADA

0901010111 OCI AVALIAÇÃO DIAGNÓSTICA E TERAPÊUTICA DE CÂNCER DE COLO DO ÚTERO-I
0203020022 EXAME ANATOMO-PATOLOGICO DO COLO UTERINO - PECA CIRURGICA
0211040029 COLPOSCOPIA
0301010072 CONSULTA MEDICA EM ATENÇÃO ESPECIALIZADA
0301010307 TELECONSULTA MÉDICA NA ATENÇÃO ESPECIALIZADA
0409060089 EXCISÃO TIPO I DO COLO UTERINO

0901010120 OCI AVALIAÇÃO DIAGNÓSTICA E TERAPÊUTICA DE CÂNCER DE COLO DO ÚTERO-II
0203020022 EXAME ANATOMO-PATOLOGICO DO COLO UTERINO - PECA CIRURGICA
0211040029 COLPOSCOPIA
0301010072 CONSULTA MEDICA EM ATENÇÃO ESPECIALIZADA
0409060305 EXCISÃO TIPO 2 DO COLO UTERINO

