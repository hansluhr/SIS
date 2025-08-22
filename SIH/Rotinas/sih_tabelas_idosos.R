#Pacotes
library(tidyverse)
library(janitor)

#Importando base de internações
load("C:/Users/P224552695/Desktop/r/SIH/sih_13_23.RData")
gc()

# set.seed(787)
# sih |> filter(intencao_homic == "Homicídio") |>
#   slice_sample(n = 100000) -> sih


#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop-PNADc-60+.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Na seção de idosos, basta o último ano.
  .x = tail(readxl::excel_sheets(excel_pnadc), 1),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população homem negro, homem não negro, mulher negro e mulher não negro
  select(uf_resd=UF, h_negro = H.Negro, h_nao_negro = H.Não_Negra, m_negra = M.Negro, m_nao_negra = M.Não_Negra) |>
  
  #Excluindo as regiões. Não utilizado
  filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
  
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf_resd, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .after=uf_resd) |>
  
  #Formato Long para facilitar o join
  pivot_longer(cols = !c(uf_resd,cod_ibge), names_to = "sex_raca_cor", names_transform = list(racacor = as.factor),
               values_to = "pop_pnadc")
rm(excel_pnadc)

#Internações por agressão
year <- 2023
sih |> 
  filter(intencao_homic == "Homicídio" & idade>=60 & ano_inter == year & 
                sexo != "Missing" & !raca_cor %in%  c("Missing","Sem Informação") ) |> tabyl(sexo)
   #Contagem por sexo e raçacor
   count(code_state_resd,sexo,raca_cor) |> 
   
   mutate(code_state_resd = code_state_resd |> as.numeric() ) |>
    
  
   #Jutando sexo e raça_cor. Para o join. Acho que assim é mais rápido do que criar uma variável nova (sex_raca_cor).
   pivot_wider(names_from = c(sexo,raca_cor), values_from = n, names_sep = "_", values_fill = 0) |>
   
   summarise(h_negro = sum(Homem_Preta, Homem_Parda, na.rm = TRUE),
             h_nao_negro = sum(Homem_Branca + Homem_Indígena + Homem_Amarela, na.rm = TRUE),
             m_negra = sum(Mulher_Parda + Mulher_Preta, na.rm = TRUE),
             m_nao_negra = sum(Mulher_Branca + Mulher_Amarela + Mulher_Indígena, na.rm = TRUE), .by = code_state_resd) |> 
  
   pivot_longer(cols = !c(code_state_resd), names_to = "sex_raca_cor", values_to = "n_sih") |> 

 #Join com a tabela de população
left_join(x = pop_pnadc, y = _, join_by("cod_ibge" == "code_state_resd",sex_raca_cor) ) %>% #rio::export("teste.xlsx")
  
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop_pnadc = sum(pop_pnadc),
                        n_sih = sum(n_sih), .by=c(sex_raca_cor) ) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_sih = format(round((n_sih/pop_pnadc)*100000,digits = 1) ) |> as.double() ) -> base
  


base |>
  select(uf_resd,sex_raca_cor,n_sih,tx_sih) |>
  
  pivot_wider(names_from = sex_raca_cor, values_from = c(n_sih,tx_sih) ) |> 
  #Ordem das colunas
  select(uf_resd,n_sih_h_negro, tx_sih_h_negro, n_sih_h_nao_negro, tx_sih_h_nao_negro, n_sih_m_negra, tx_sih_m_negra,
         n_sih_m_nao_negra, tx_sih_m_nao_negra) |> 
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  rio::export(x = _, "internações_idosos_raca_cor.xlsx")







