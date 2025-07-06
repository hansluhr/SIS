#Rotina tenste união RAIS e SINAN
library(tidyverse)
library(janitor)
library(RPostgreSQL)

#Importando base sinan
load("C:/Users/P224552695/Desktop/r/Sinan/sinan_14_22.RData")

###Criando variáveis - SINAN
sinan <- 
  sinan |>
  mutate(
        #Escolaridade. Agregando escolaridade da SINAN para realizar join compatível com RAIS
        esc_join = 
        case_when(
        cs_escol_n %in% c("1ª a 4ª série incompleta do EF", "4ª série completa do EF (antigo 1° grau)",
                                  "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)") ~ "Fundamental Incompleto",
                              .default = cs_escol_n) |> as_factor() ) 

glimpse(sinan)

#Configuração acesso dbeaver a todas as bases
source('C:/Users/P224552695/Dropbox/Ipea/Atlas/Rotinas/config_acesso_dbeaver.R')#Informa o sobre dbname,host,port
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = dbname, host = host, 
                port = port, user = user, password= password)
#Importa observações de interesse na RAIS.
tictoc::tic() 
rais <- dbGetQuery(con, "SELECT nome_trab, cpf, ano, natureza_juridica, codemun, rem_media_reais, clas_cnae20, cbo2002, data_nasc, genero, grau_instr,
                            horas_contr, idade, raca_cor
                            from vinculos_v6.tb_vinculos 
                            where ano = 2021 AND genero = 2 AND codemun = 330455 AND rem_media_reais > 0")
tictoc::toc()
rm(drv,con,dbname,host,password,port,user)

#Na rais variável tipo de deficiência está mal preenchida.

#Alterações RAIS
rais |> 
  
  #Eliminar cpfs repetidos
  distinct(cpf, .keep_all = TRUE) |> 
  
  #Rename de variáveis. 
  rename(sexo = genero,
         educ = grau_instr,
         cs_raca_rais = raca_cor,
         id_ocupa_n6 = cbo2002,
         #Confirmar que codemun é município de residência.
         id_mn_resi = codemun) |>
  
  #Alterando classe das variáveis de interesse. Necessário para o join
  mutate(id_mn_resi = id_mn_resi |> as_factor(),
  
         
  #Criando Variável ano de nascimento
  ano_nasc_rais =  year(data_nasc) |> as_factor(),
         
         
  #Alterando variáveis de interesse.
  across( c(sexo, educ, cs_raca_rais), ~  case_when(

    #Label de sexo
    . == 1 & cur_column() == "sexo" ~ "Homem",
    . == 2 & cur_column() == "sexo" ~ "Mulher",
    
    #Label de nível de escolaridade
    . == 1 & cur_column() == "educ" ~ "Analfabeto", . == 2 & cur_column() == "educ" ~ "ATE 5.A INC",
    . == 3 & cur_column() == "educ" ~ "5.A CO FUND", . == 4 & cur_column() == "educ" ~ "6. A 9. FUND",
    . == 5 & cur_column() == "educ" ~ "FUND COMPL", . == 6 & cur_column() == "educ" ~ "MEDIO INCOMP",
    . == 7 & cur_column() == "educ" ~ "MEDIO COMPL",. == 8 & cur_column() == "educ" ~ "SUP. INCOMP",
    . == 9 & cur_column() == "educ" ~ "SUP. COMP",. == 10 & cur_column() == "educ" ~ "Mestrado",
    . == 11 & cur_column() == "educ" ~ "Doutorado", . == -1 & cur_column() == "educ" ~ "Ignorado",
    
    #Label de cs_raca_rais
    . == 1 & cur_column() == "cs_raca_rais" ~ "Indígena", . == 2 & cur_column() == "cs_raca_rais" ~ "Branco",
    . == 4 & cur_column() == "cs_raca_rais" ~ "Preto", . == 6 & cur_column() == "cs_raca_rais" ~ "Amarelo",
    . == 8 & cur_column() == "cs_raca_rais" ~ "Pardo", . == 9 & cur_column() == "cs_raca_rais" ~ "Não identificado",
    . == -1 & cur_column() == "cs_raca_rais" ~ "Ignorado", 
    
    #Label de tipo de deficiência
    # . == 1 & cur_column() == "tipo_defic" ~ "Física", . == 2 & cur_column() == "tipo_defic" ~ "Auditiva",  
    # . == 3 & cur_column() == "tipo_defic" ~ "Visual", . == 4 & cur_column() == "tipo_defic" ~ "Mental",
    # . == 5 & cur_column() == "tipo_defic" ~ "Múltipla", . == 6 & cur_column() == "tipo_defic" ~ "Reabilitado",
    # . == 0 & cur_column() == "tipo_defic" ~ "Não deficiente", . == -1 & cur_column() == "tipo_defic" ~ "Ignorado",
    # 
    .default = "Missing" ) |> as_factor() ),
    
    ###Criando variáveis
    #Escolaridade. Agregando escolaridade da RAIS para realizar join compatível com SINAN
    esc_join = case_when(educ %in% c("ATE 5.A INC","5.A CO FUND","educ" ~ "6. A 9. FUND") ~ "Fundamental Incompleto",
                         .default = educ) |> as_factor() ) |> tibble() -> rais

#glimpse(rais)


# Acrecentar a RAIS Classificação Nacional das Atividades Econômicas. -----------------------
#Importar base cnae.
cnae <- readxl::read_excel("C:/Users/P224552695/Dropbox/Ipea/Atlas/Dicionário/CNAE/CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx", skip = 5) |>
  select(grupo = ...3, atv_grupo_cnae = "AGRICULTURA, PECUÁRIA E SERVIÇOS RELACIONADOS") |> 
  #Exclusão das linhas correspondetes ao grupo com valor missing.
  drop_na(grupo) |>
  #Eliminar caracteres da variável grupo
  mutate(grupo = str_replace_all(grupo, "[^0-9]", "") )

#Join entre Cnae e Rais
rais <- rais |> 
  #Cria variável grupo cnae
  mutate(cnae_grupo = str_sub(clas_cnae20, 1, 3) ) |> 
  #Join Rais e CNAE através do código grupo cnae
  left_join(x = _ , y = cnae, join_by(cnae_grupo == grupo) ) |>
  select(!c(cnae_grupo))
rm(cnae)



# Acrescentar natureza jurídica a RAIS -------------------------------------------
#Buscar excel de natreza jurídica


# Join base SINAN e RAIS --------------------------------------------------
#Pode ser através da ano de nascimento, cbo, município de residência, cor, escolaridade
tictoc::tic()
base <- 
  sinan |> 
  filter(cs_sexo == "Mulher" &  les_autop!="Sim" & grupo_viol == "Doméstica" & !is.na(idade) & idade >= 15 &
         ocupa != "Missing", #Mantém somente observações com preenchimento válido da variável id_ocupa_n6
         ano_not == 2021 & id_mn_resi == 330455) |> droplevels() |> 
  
  #Variáveis de interesse do sinan para análise
  select(idade, ano_nasc, id_mn_resi, esc_join,cs_escol_n, ocupa, id_ocupa_n6, cs_raca, ano_not, starts_with("nome_"), cs_gestant,
         sit_conjug,local_ocor, out_vezes) |>
  
  #Join Sinan e Rais
  left_join(x = _, y = rais, 
            #Variáveis utilizadas no join entre SINAN e RAIS_ID. Poderia usar ano de nascimento.
            join_by(idade, id_ocupa_n6, esc_join, id_mn_resi) ) |> 
  #Elimina quando não houver join 
  filter(!is.na(nome_trab) ) 
  #Mantém variáveis de interesse
  #select(idade,ano_nasc,ano_nasc_rais,cs_raca,cs_raca_rais) 
tictoc::toc()

#Salvar base
save.image("C:/Users/P224552695/Desktop/r/Sinan/sinan_rais.RData")

# Gráficos ----------------------------------------------------------------
library(tidyverse)
library(janitor)
#Importando base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_rais.RData")


base |>
  count(ocupa,sort = TRUE)

base |>
  count(atv_grupo_cnae,sort = TRUE)

base |>
  summarise(renda = mean(rem_media_reais),
            n = n(), .by = ocupa) |> arrange(desc(n))

base |>
  ggplot(aes(x = rem_media_reais) ) +
  stat_ecdf(pad = FALSE,na.rm=TRUE) + geom_hline(yintercept=0.5,linetype = "dotted") +
  scale_x_continuous( breaks = seq(0,3200,500) ) +
  labs(x = "Renda Média")


ecdf_func <- ecdf(base$rem_media_reais)

base |>
  distinct(rem_media_reais) |>
  arrange(rem_media_reais) |>
  mutate(prop_renda = ecdf_func(rem_media_reais) )

#Gráfico de Distribuição Cumulativa
#50% das pessoas recebem até a mediana de rendimento"
base |>
ggplot(aes(x = rem_media_reais)) +
  stat_ecdf(geom = "step", color = "blue") +  # Curva de distribuição cumulativa
  geom_vline(xintercept = median(base$rem_media_reais), color = "red", linetype = "dashed") +  # Linha vertical na mediana
  scale_x_continuous( breaks = seq(500,3250,250) ) +
  scale_y_continuous(labels = scales::label_percent() ) +
  annotate("text", x = 2100, y = 0.5, label = paste("Mediana =", median(base$rem_media_reais), "reais"), 
           color = "red", vjust = -0.5) +  # Texto indicando o valor da mediana
  labs(
    #title = "Distribuição Cumulativa dos Rendimentos",
    x = "Remuneração Média",
    y = "Proporção acumulada")

#Histograma.
base |>
  ggplot( aes(x = rem_media_reais) ) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = median(base$rem_media_reais), color = "red", linetype = "dashed") +
  annotate("text", x =  median(base$rem_media_reais), y = max(table(cut(base$rem_media_reais, breaks = 20)))*0.9, 
            label = paste("Mediana =",  median(base$rem_media_reais), "reais"), color = "red", hjust = -0.1) +
  scale_x_continuous( breaks = seq(500,3250,250) ) +
  labs(x = "Remuneração Média", y = "Frequência")








#Percentuais com texto dentro da barra.
library(ggstats)
#https://larmarange.github.io/ggstats/
base |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(cs_sexo,t_viol) |>
  #Gráfico
  ggplot( aes(x = t_viol, weight = n, fill = cs_sexo) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5) ) +
  scale_y_continuous(labels = percent) 


base |>
  ggplot() +
  geom_histogram(aes(x= rem_media_reais, fill = ocupa ) ) 






# Tabelas -----------------------------------------------------------------
library(tidyverse)
library(janitor)
#Importando base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_rais.RData")

base |>
  count(ocupa, sort = TRUE)

base |> 
  tabyl(atv_grupo_cnae,ocupa, show_missing_levels = FALSE) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") ) |> view()


#Mediana da remuneração por ocupação.
base |>
  summarise(rend = median(rem_media_reais),
            n = n(), .by = ocupa) |>
  #Acrescentar a mediana de todas as observações.
  
  bind_rows(
    tibble(ocupa = "Média Geral", rend = mean(base$rem_media_reais, na.rm = TRUE))
  )


#Mediana da remuneração por natureza jurídica
base |>
  summarise(rend = median(rem_media_reais),
            n = n(), .by = natureza_juridica) |>
  #Acrescentar a mediana de todas as observações.
  
  bind_rows(
    tibble(natureza_juridica = "Média Geral", rend = mean(base$rem_media_reais, na.rm = TRUE))
  )





base |>
  summarise(median_renda = median(rem_media_reais), .by = c(ocupa,natureza_juridica) ) |>
  
  pivot_wider(names_from = natureza_juridica, values_from = median_renda, values_fill = 0) %>%
  
  summarise(ocupa = c(ocupa, 'mean'),
            across(where(is.numeric), ~ c(., mean(.))))
  
  
  add_row(ocupa = 'mean', !!! colMeans(.[-1]))
