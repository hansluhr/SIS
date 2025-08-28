library(tidyverse)
library(janitor)

#SIGTBA 
sigtab <- data.table::fread("C:/Users/gabli/Desktop/r/SIH/bases_auxiliares/TB_SIGTAB.csv", sep = ";") |>
    rename(proc_sigtab = value,
           cod_sigtab = cod) |>
    mutate(proc_sigtab = proc_sigtab |> str_to_title() |> as_factor() )



#Importação da tabela procedimentos
source(file = "https://raw.githubusercontent.com/hansluhr/SIS/refs/heads/main/Rotinas%20Gerais/funcao_cod_procedimentos_SUS.R")
proc_auto <- procedimentos |>
  rename(cod_auto = cod, 
         proc_auto = proc)


rm(list = setdiff(ls(), c("proc_auto","sigtab") ) )

#Somente na tabela sigtab
sigtab |>
  left_join(x = _, y = proc_auto, by = join_by("cod_sigtab" == "cod_auto"), 
            keep = TRUE) -> sig
#Somenta na tabela automática
sigtab |>
  right_join(x = _, y = proc_auto, by = join_by("cod_sigtab" == "cod_auto"), 
            keep = TRUE)  -> auto
#Códigos em comum (Inner Join)
sigtab |>
  inner_join(x = _, y = proc_auto, by = join_by("cod_sigtab" == "cod_auto"), 
             keep = TRUE) -> inner
#Full Join
sigtab |>
  full_join(x = _, y = proc_auto, by = join_by("cod_sigtab" == "cod_auto"), 
             keep = TRUE) -> full

list("sigtab" = sig,
     "auto" = auto,
     "comum" = inner,
     "full" = full)   |>
  openxlsx::write.xlsx(x = _, file = "as.xlsx")
