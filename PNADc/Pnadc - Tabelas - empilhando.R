library(tidyverse)
library(janitor)


# Tabela de referência com os códigos IBGE dos estados
codigos_ibge <- data.frame(
  nome_estado = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", 
                  "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", 
                  "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", 
                  "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", 
                  "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", 
                  "Goiás", "Distrito Federal"),
  cod_uf = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 
                  24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 
                  41, 42, 43, 50, 51, 52, 53)
)

#Importando populaC'C#o - Geral
data_list <- rio::import_list("C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx", 
                              which = seq(from = 1, to = 12, by = 1), 
                              #Indicar as sheets desejadas. Aqui estou pegando a populaC'C#o Geral
                              #Com o passar dos anos precisa alterar o from, indicando a sheet com a primeira pnadc jovem desejada
                              #Com o passar dos anos precisa alterar o by, indicando a sheet com a C:ltima pnadc jovem
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela C:nica
pop_pnadc <- do.call(bind_rows, 
                     mget(setdiff(ls(pattern = "^[^.]+$"),c("codigos_ibge") ) ) ) |> 
  
  left_join(x = _, y = codigos_ibge, by = join_by("UF" == "nome_estado") ) |> 
  
  #Ordenando colunas
  relocate(cod_uf, .after = UF) |>  clean_names() |>
  #Rename de colunas
  rename(uf_resd = uf, pop_homem = po_p_homem, pop_mulher =  po_p_mulher )
  #Adicionar total Brasil

#Removendo pnadcs, exceto pop_pnadc_homem_jovem
rm(list = setdiff(ls(), c("pop_pnadc") ) )

pop_pnadc <- 
   pop_pnadc %>%
  #Acrescentando total Brasil e criando taxas
  bind_rows( . |>
            #Excluindo regiões do somatório.   
            filter(!is.na(cuf) ) |>   
            summarise(
             uf_resd = "Brasil",
             #Soma das colunas de população. 
             across(where(is.numeric) & !c(cod_uf), \(x) sum(x, na.rm = TRUE) ),
             .by = ano ) ) 


#Exportando tabela PNADc
rio::export(x = pop_pnadc, "pnadc_painel.xlsx")


# Empilhado jovens 15 à 29 anos --------------------------------------------------------
library(tidyverse)
library(janitor)

# Tabela de referência com os códigos IBGE dos estados
codigos_ibge <- data.frame(
  nome_estado = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", 
                  "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", 
                  "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", 
                  "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", 
                  "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", 
                  "Goiás", "Distrito Federal"),
  cod_uf = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 
             24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 
             41, 42, 43, 50, 51, 52, 53) )




##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop_Jovens_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Pegando todas as PNADcs
  .x = readxl::excel_sheets(excel_pnadc),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
 
  #Adicionar código da UF de residência.
 left_join(x = _, y = codigos_ibge, by = join_by("UF" == "nome_estado") ) |> 
  
  #Ordenando colunas
  relocate(cod_uf, .after = UF) |>  clean_names() |>
  #Rename de colunas
  rename(uf_resd = uf, pop_homem = po_p_homem, pop_mulher =  po_p_mulher )
#Adicionar total Brasil
  
rm(codigos_ibge)

pop_pnadc <- 
  pop_pnadc %>%
  #Acrescentando total Brasil e criando taxas
  bind_rows( . |>
               #Excluindo regiões do somatório.   
               filter(!is.na(cuf) ) |>   
               summarise(
                 uf_resd = "Brasil",
                 cuf = "BR",
                 #Soma das colunas de população. 
                 across(where(is.numeric) & !c(cod_uf), \(x) sum(x, na.rm = TRUE) ),
                 .by = ano ) ) |> view()

#Exportando tabela PNADc
rio::export(x = pop_pnadc, "pnadc_painel_jovem.xlsx")
rm(excel_pnadc)