### Necessário importar microdados sim_doext96_22
# Estimação dos homicídios ocultos. ----------------------------------
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(themis) #Subsamples
library(baguette)
load("C:/Users/b224552695/Desktop/sim_doext_96_22_test.RData")

set.seed(787)
#Trecho utilizado ao testar a rotina
base_old %>%
  filter(ano==2014 | ano==2015) %>%
  #Retira o levels de anos não utilizados 
  droplevels() %>%
  #Slice da base
  slice_sample(prop = 0.025) -> base_old

glimpse(base_old)

# #Check de proporções
base_old %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))
# base_old %>% group_by(uf) %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))

####Separação da base em training set e test set.
#Split dos dados - A estratificação também ocorre nas UFs
set.seed(747)
data_split <- initial_split(base_old,prop = 0.7,strata = intencao_homic)
# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


# #Checando estratificação
train_data %>% count(intencao_homic) %>% mutate(prop = n/sum(n))
# train_data %>% group_by(uf) %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))
# #Checando estratificação
# test_data %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))

#create resampling folds. 
set.seed(234)
folds <- vfold_cv(train_data, strata = intencao_homic,v=10)
folds

#Receita modelos de Árvore de decisão
rf_recipe <- recipe(intencao_homic ~ idade + sexo + racacor + estciv + esc + local_obito + instrumento + ano + mes + dia + uf_ocor, data=train_data) %>%
  #Imputação de idade
  step_impute_knn(c(idade),neighbors = 5,impute_with = imp_vars(all_predictors())) %>%
  #Transforma factor em indicator variable(dummy)
  step_dummy(all_nominal_predictors(),one_hot = TRUE) %>% #Aqui pode usar todas as categorias.
  #remove variables that contain only a single value.
  step_zv(all_predictors()) %>%
  #Over Sample
  step_smote(intencao_homic,skip = TRUE,seed=51)  

#Nesse caso apresenta tibbles do train_data após aplicar os passos da receita.
rf_recipe %>% prep() %>% bake(new_data = NULL) -> train_data_rf

# Modelos -----------------------------------------------------------------
#Random forest
rf_spec  <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% 
  set_engine("ranger",importance = "impurity",verbose = TRUE,seed = 747) %>% 
  set_mode("classification")

#Workflow + Receita random forest 
w_rfxgb <- workflow_set(preproc = list(n_int = rf_recipe), #O nome final do modelo ? a combina??o entre o pr?-processamento e o nome do modelo
                        models = list(RF = rf_spec))
w_rfxgb

#Removendo especificações e receitas.
rm(rf_spec,rf_recipe)

# Tunning de hiperparametros - Grid search --------------------------------
#Métricas utilizadas
grid_metrics <- yardstick::metric_set(accuracy,bal_accuracy,mcc,
                                      precision,sensitivity,specificity,recall,f_meas,j_index,pr_auc,roc_auc)
library(finetune)
grid_ctrl <-
  control_race(#Aqui não precisa o race
    save_pred = FALSE,verbose = TRUE,
    parallel_over = "resamples",
    allow_par = TRUE,save_workflow = FALSE) 


#doParallel::registerDoParallel()
library(doParallel)
detectCores(logical = TRUE)
cl <- makeCluster(parallel::detectCores()-2)
registerDoParallel(cl)
set.seed(77)

#Cross- Validation - Random forest
full_results_time <- 
  system.time(
    grid_results_all <- w_rfxgb %>% 
      workflow_map(seed = 777, resamples = folds,metrics = grid_metrics, grid = 5, 
                   control = grid_ctrl, verbose = TRUE))

#save.image("C:/Users/gabli/Desktop/r/Microdados_SIM_2018_2020_label.RData")
stopCluster(cl)
gc()

rm(grid_ctrl,grid_metrics,cl,full_results_time,folds,w_rfxgb)

#Tabela com todos os modelos do cross-validation - selecionando o modelo com melhor roc_auc
grid_results_all %>% rank_results(rank_metric = "roc_auc", select_best = TRUE) %>% select(.metric,mean,std_err,wflow_id,model,rank) %>% 
  #Fazendo tabela.
  pivot_wider(names_from = .metric, values_from = c(mean,std_err)) %>%
  mutate(wflow_id = case_when(wflow_id == "n_int_RF" ~ "RF")) %>%
  rename(Modelos = wflow_id) %>% select(!c(model,rank)) %>%
  #Removendo mean do nome das colunas.
  rename_with(~stringr::str_remove(.,"mean_")) -> cv_metrics 
#std. devation est? nas colunas. Precisa passar para linha.

rio::export(cv_metrics,"cv_metrics.xlsx")
rm(cv_metrics)
#Curva roc_auc de cada combinaçaõ.
grid_results_all %>% 
  rank_results() %>% 
  filter(.metric == "roc_auc") %>% 
  select(model, .config, roc_auc = mean, rank)

#Gráfico dos melhores modelos
autoplot(
  grid_results_all,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
  select_best = TRUE )    # <- one point per workflow

#Plot estatísticas de precisão
autoplot(grid_results_all)
autoplot(grid_results_all, metric = "mcc")
autoplot(grid_results_all, id = "n_int_RF", metric = "roc_auc")
autoplot(grid_results_all, id = "n_int_RF", metric = "roc_auc")


#Coletando m?tricas dos modelos
grid_results_all %>%  collect_metrics() %>% filter(.metric=="roc_auc") %>% arrange(desc(mean))

#Previs?es de todos os modelos
grid_results_all %>% collect_predictions()

grid_results_all %>% extract_workflow("n_int_RF") %>% summary()


# Ajuste no train_data e previsão no test_data ----------------------------
#Random forest - Previsão contra o test set
best_rf <- grid_results_all %>% 
  extract_workflow_set_result("n_int_RF") %>% 
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc") 
set.seed(777)
rf_test_fit <- grid_results_all %>%  extract_workflow("n_int_RF") %>% 
  finalize_workflow(best_rf) %>% 
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,mcc,precision,sensitivity,specificity,recall,f_meas,j_index,pr_auc,roc_auc)) 
rf_metrics_test <- collect_metrics(rf_test_fit)
rm(best_rf) #Não excluir modelo de melhor ajuste.
save.image("C:/Users/gabli/Desktop/r/Microdados_SIM_2018_2020_label.RData")
gc()

#Tabela com M?tricas test_data
bind_cols(
  tibble(models = c("RF")), 
  bind_rows(
    rf_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric))) %>%
  select(!c(.estimator,.config)) -> test_data_metrics #Aqui não tem std. deviation.
rm(rf_metrics_test)
rio::export(test_data_metrics,"test_data_metrics.xlsx")


#Seleciona melhor configuração no cross-validation de modelo especificado 
grid_results_all %>% extract_workflow_set_result("n_int_RF") %>% 
  select_best(metric = "roc_auc") -> best_model #Configuração do melhor modelo

#Rodando melhor modelo do CV no test set.
test_results <- 
  grid_results_all %>% 
  extract_workflow("n_int_RF") %>% 
  finalize_workflow(best_model) %>% 
  last_fit(split = data_split, metrics = metric_set(accuracy,precision,sensitivity,specificity,recall,f_meas,j_index,pr_auc,roc_auc)) 

collect_metrics(test_results) 

#Confusion matrix - utiliza as previs?es discretas
rf_test_fit %>% collect_predictions() %>%  conf_mat(truth = intencao_homic, estimate = .pred_class)
#Figura da Confusion matrix
rf_test_fit %>% collect_predictions() %>% conf_mat(truth = intencao_homic, estimate = .pred_class) %>% autoplot()
#Accuracy
test_results %>% collect_predictions() %>% accuracy(truth = intencao_homic, estimate = .pred_class)
#Matthews correlation coefficient
test_results %>% collect_predictions() %>% mcc(truth = intencao_homic, estimate = .pred_class)
#F1 metic
test_results %>% collect_predictions() %>% f_meas(truth = intencao_homic, estimate = .pred_class)
#ROC Curve - Utiliza probabilidades de cada classe
rf_test_fit %>% collect_predictions() %>% roc_curve(truth = intencao_homic, estimate = .pred_homic) %>%  autoplot()
#Roac
rf_test_fit %>% collect_predictions() %>% roc_auc(truth = intencao_homic, .pred_homic)

rm(test_results)

#Workflow do melhor modelo
grid_results_all %>% extract_workflow("n_int_RF") %>% finalize_workflow(best_model) -> best_work_flow
#Fit do modelo na base_old inteira. Esse modelo será utilizado na classificação.
last_model_fit <- fit(best_work_flow,base_old)


rm(grid_results_all,data_split,best_rf,best_model)

#N?mero de homic?dios ocultos. ------------------------------
#Como ficou o test set ap?s pr? processamento - COMO COLOCAR A RECEITA VENCEDORA?
best_work_flow$pre$actions$recipe$recipe %>% prep() %>% bake(new_data=base_new) -> x #Esta preenchendo o missing de idade
rm(x)

glimpse(base_new)
rm(sim_doext)

set.seed(787)
#Trecho utilizado ao testar a rotina
base_new %>%
  filter(ano==2014 | ano==2015) %>%
  #Retira o levels de anos não utilizados
  droplevels() %>%
  #Slice da base
  slice_sample(prop = 0.025) -> base_new

#Identificação de homicídios ocultos
homic_preds <- augment(last_model_fit, base_new) #Essa é a base de mortes indeterminadas idetificadas 
homic_preds %>% count(.pred_class)

#Criando base de ocultos por UF - Pegando os Indeterminados.
base_new %>% select(ano,uf_ocor) %>% group_by(ano,uf_ocor) %>%
  mutate(indeterminado = 1) %>% summarise(indeterminado = NROW(indeterminado)) -> base_oculto_uf

#Pegando e junto os ocultos - Precisa incluir ano\uf sem homic?dio oculto.
homic_preds %>% select(ano,uf_ocor,.pred_class) %>% group_by(ano,uf_ocor) %>% count(.pred_class) %>%
  pivot_wider(names_from = .pred_class,values_from = n,
              #Coloca 0 quando n?o tiver oculto.
              values_fill = 0) %>% select(!c("n_homic")) %>%
  rename(ppb2_homicidio = homic) %>%
  left_join(base_oculto_uf,by = c("ano", "uf_ocor")) -> base_oculto_uf


#Pegando e juntando os homic?dios registrados.
base_old %>% select(ano,uf_ocor,intencao_homic) %>% filter(intencao_homic == "homic") %>% group_by(ano,uf_ocor) %>% 
  count(intencao_homic) %>% select(!c(intencao_homic)) %>% rename(homicidio = n) %>%
  left_join(base_oculto_uf,by = c("ano", "uf_ocor")) %>%
  mutate(across(where(is.numeric),.fns = ~replace_na(.x,0)),
         homicidio_proj = homicidio + ppb2_homicidio,
         hom_oculto_prop = case_when(ppb2_homicidio!=0 ~ ppb2_homicidio/indeterminado, TRUE ~ 0)) -> base_oculto_uf

sapply(base_oculto_uf,function(x) sum(is.na(x)))

#Merge das Popula??es
library(readxl)
pop <- readxl::read_excel("D:/Trabalhos/Hoculto/Populacao/populacao.xlsx")
pop <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Homicidio Oculto/Populacao/populacao.xlsx")

base_oculto_uf %>%  mutate(ano = as.numeric(as.character(ano)), 
                           #Colocando siglas nas UFs.  
                           cuf = case_when(uf_ocor == "Acre" ~ "AC" , uf_ocor == "Alagoas" ~ "AL" , uf_ocor == "Amap?" ~ "AP" ,
                                           uf_ocor == "Amazonas" ~ "AM" , uf_ocor == "Bahia" ~ "BA" , uf_ocor == "Cear?" ~ "CE" , uf_ocor == "Bras?lia" ~ "DF",
                                           uf_ocor == "Esp?rito Santo" ~ "ES" , uf_ocor == "Goi?s" ~ "GO" , uf_ocor == "Maranh?o" ~ "MA" , uf_ocor == "Mato Grosso" ~ "MT" ,
                                           uf_ocor == "Mato Grosso do Sul" ~ "MS", uf_ocor == "Minas Gerais" ~ "MG" , uf_ocor == "Par?" ~ "PA" , uf_ocor == "Para?ba" ~ "PB",
                                           uf_ocor == "Paran?" ~ "PR" , uf_ocor == "Pernambuco" ~ "PE" , uf_ocor == "Piau?" ~ "PI" , uf_ocor == "Rio de Janeiro" ~ "RJ" ,
                                           uf_ocor == "Rio Grande do Norte" ~ "RN" , uf_ocor == "Rio Grande do Sul" ~ "RS" , uf_ocor == "Rond?nia" ~ "RO" , uf_ocor == "Roraima" ~ "RR", 
                                           uf_ocor == "Santa Catarina" ~ "SC" , uf_ocor == "S?o Paulo" ~ "SP" , uf_ocor == "Sergipe" ~ "SE" , uf_ocor == "Tocantins" ~"TO",
                                           TRUE ~ NA_character_)) %>%
  #Juntando Estima??es a popula??o.
  left_join(pop,by = c(c("ano", "cuf"))) %>% 
  #Aruumando a base
  #select(!c(uf.y)) %>% rename(uf=uf.x) %>% relocate(cuf,.after = uf) %>%
  #Criando Taxas
  mutate(tx_homicidio = (homicidio/pop)*100000,
         tx_indeterminado = (indeterminado/pop)*100000,
         tx_oculto = (ppb2_homicidio/pop)*100000,
         tx_hom_projetado  = (homicidio_proj/pop)*100000) -> base_oculto_uf

#Check na
sapply(base_oculto_uf,function(x) sum(is.na(x)))

rio::export(base_oculto_uf,"D:/Trabalhos/Hoculto/H.Oculto - Ufs.xlsx")
rio::export(base_oculto_uf,"C:/Users/gabli/Dropbox/Ipea/Homicidio Oculto/H.Oculto - Ufs.xlsx")

###Exporta??o base Brasil
base_oculto_uf %>% group_by(ano) %>%
  summarise(homicidio = sum(homicidio),ppb2_homicidio = sum(ppb2_homicidio),indeterminado=sum(indeterminado),
            homicidio_proj = sum(homicidio_proj),pop=sum(pop)) %>%
  #Criando Taxas
  mutate(tx_homicidio = (homicidio/pop)*100000,
         tx_indeterminado = (indeterminado/pop)*100000,
         tx_oculto = (ppb2_homicidio/pop)*100000,
         tx_hom_projetado  = (homicidio_proj/pop)*100000) -> base_oculto_br

rio::export(base_oculto_br,"D:/Trabalhos/Hoculto/Tabelas/H.Oculto - BR.xlsx")

sapply(base_oculto_br, function(x) sum(is.na(x)))


#Homic?dio oculto por munic?pio de resid?ncia
#Importando popula??o dos munic?pios
pop_munic <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas Munic 2023/pop_munic_00_21.xlsx",skip = 3) |> 
  pivot_longer(cols = where(is.numeric),names_to = "ano",values_to = "pop_munic") |> rename(munic = "Munic?pio") |>
  #Excluindo munic?pio total 
  filter(munic!="Total") |>
  #Separar c?digo munic do nome munic. 
  separate(munic, 
           into = c("codmunres", "munic"), 
           sep = "(?<=[0-9])(?=\\s?[A-Z])") |> 
  mutate(ano = as.numeric(ano))

#Homic?dios por munic?pio   
sim_doext |>
  select(ano,codmunres,intencao_homic) %>% group_by(ano,codmunres) %>% 
  #Filtrando anos com popula??o dispon?vel e mantendo somente homic
  filter(ano %in% (2000:2021) & intencao_homic == "homic") |> droplevels() |>
  count(intencao_homic) |> rename(homic = n) |> select(!intencao_homic) |>
  #Retirar 7 d?gito de codmunres. Para ser compat?vel com c?digo informado na pop.
  mutate(codmunres = str_sub(codmunres, start = 1, end = 6),
         ano = as.numeric(as.character(ano))) -> homic_munic

#Ocultos por homic?dios
homic_preds |>
  select(ano,codmunres,.pred_class) %>% group_by(ano,codmunres) %>% 
  #Filtrando anos com popula??o dispon?vel e mantendo somente homic
  filter(ano %in% (2000:2021) & .pred_class == "homic") |> droplevels() |>
  count(.pred_class) |> rename(oculto = n) |> select(!.pred_class) |>
  #Retirar 7 d?gito de codmunres. Para ser compat?vel com c?digo informado na pop.
  mutate(codmunres = str_sub(codmunres, start = 1, end = 6),
         ano = as.numeric(as.character(ano))) -> oculto_munic



#Jutando homic?dio registrado a base de popula??es
pop_munic |> left_join(homic_munic,by = c("ano", "codmunres")) %>%
  #Agluns munic?pios n?o registram homic
  mutate(homic = replace_na(homic,0),  
         tx_homic = (homic/pop_munic)*100000) |> 
  #Jutando ocutlos
  left_join(oculto_munic,by=c("ano","codmunres")) |>
  #Alguns munic?pios n?o registram oculto
  mutate(oculto = replace_na(oculto,0), 
         tx_oculto = (oculto/pop_munic)*100000,
         proj = homic+oculto, tx_proj = (proj/pop_munic)*100000) |>
  select(codmunres,munic,ano,pop_munic,homic,oculto,proj,tx_homic,tx_oculto,tx_proj) |> rio::export(x=_,"homic_ocul_proj_munic.xlsx")


rm(pop_munic,homic_munic,oculto_munic)

# Interpretação dos restulados --------------------------------------------
library(DALEXtra)
explainer_base_new  <- explain_tidymodels(model=last_model_fit, #final_trained
                                          #Quero explicar as probabilidades de homic_preds
                                          data = dplyr::select(homic_preds, -c(.pred_class,codmunocor,codmunres,.pred_homic,.pred_n_homic)),
                                          #N?o seria as novas observa??es?homic_pred
                                          y =  homic_preds$.pred_class == "homic",
                                          predict_function_target_column = "homic",
                                          label = "",
                                          verbose = T, type = "classification")

#18.2 Local explanations
#it computes how contributions attributed to individual features change the mean model's prediction 
#for a particular observation
#Shapley Additive Explanations

#Seleciona a observação desejada - Selecionando a observação mais frequente no test set.
homic_preds %>% mutate(id = 1:nrow(homic_preds)) %>%
  filter(instrumento == "Enforcamento" & idade == 0) %>% view()

local_expla_idividual <- homic_preds[379,] 
#Cálculo do Shap Value de observaçao individual 
set.seed(737)
shap_local_idividual <- 
  DALEX::predict_parts(explainer = explainer_base_new,N=NULL,
                       new_observation = local_expla_idividual, 
                       type = "shap",B = 5)

plot(shap_local_idividual)

# The box plots in Figure 18.2 display the distribution of contributions across all the orderings we 
# tried, and the bars display the average attribution for each feature:
shap_local_idividual %>% #O Mesmo gr?fico do plot anterior.
  group_by(variable) %>% mutate(mean_val = mean(contribution)) %>% ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable),  alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") + scale_fill_viridis_d() + labs(y = NULL,x="")

rm(local_expla_idividual,shap_local_idividual)


# 18.3 GLOBAL EXPLANATIONS
#Permutation feature imporantece\ Permutation based variable importance
#Criando explainer 
final_trained <- rf_test_fit$.workflow[[1]] #Precisa estar ajustado ao training_data
#Aqui quero explicar os erros contra o test_set
explainer  <- explain_tidymodels(model=final_trained, 
                                 data = dplyr::select(test_data, c(idade,sexo,racacor,estciv,esc,local_obito,instrumento,ano,mes,dia,uf_ocor)),
                                 y =  test_data$intencao_homic == "homic",
                                 predict_function_target_column = "homic",
                                 label = "",
                                 verbose = T, type = "classification")
set.seed(502)
vip_rf <- model_parts(explainer, N = NULL,  type = "difference",#pode trocar o type.
                      loss_function = loss_one_minus_auc, B = 25)
plot(vip_rf)

#Função de plot sugerida no site tmwr.
ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}
ggplot_imp(vip_rf)
rm(ggplot_imp)

#Perda média por vari?vel
vip_rf |> filter(!(variable %in% c("_baseline_","_full_model_"))) |>
  summarise(mean =  mean(dropout_loss), .by =  variable) |>
  arrange(desc(mean))


#Plot similar ao anterior
vip_rf %>% filter(!(variable %in% c("_baseline_","_full_model_","mes","dia"))) %>%
  mutate(variable = case_when(variable == "instrumento" ~ "Instrumento",
                              variable == "local_obito" ~ "Local do\n Incidente",variable == "uf_ocor" ~ "UF", variable == "idade" ~ "Idade",
                              variable == "ano" ~ "Ano", variable == "estciv" ~ "Estado\n Civil",
                              variable == "esc" ~ "Escolaridade", variable == "racacor" ~ "Raça\\Cor",
                              variable == "sexo" ~ "Sexo", TRUE ~ variable),
         variable = fct_reorder(variable,dropout_loss)) %>%
  ggplot() + geom_jitter(aes(x = dropout_loss, y = variable),alpha=0.1) +
  geom_boxplot(aes(x = dropout_loss, y = variable )) + 
  #geom_vline(xintercept = 0.1,linewidth = 1.4, lty = 2, alpha = 0.7) + 
  labs(y = "", x = "1 - AUC após permutações") +
  theme(legend.position = "none", axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(size = 7.5),axis.text.x = element_text(size=7.5)) 

rm(vip_rf,ggplot_imp)

# 18.4 BUILDING GLOBAL EXPLANATIONS FROM LOCAL EXPLANATIONS
#Partial dependece profile\plot - Varia??o no valor previsto dado varia??o na vari?vel analisada
#Sexo
set.seed(805)
pdp_sexo <- model_profile(explainer, N = 500,  variables = "sexo",variable_type = "categorical")
plot(pdp_sexo)
rm(pdp_sexo)

#Idade por sexo
#https://ema.drwhy.ai/partialDependenceProfiles.html
#https://christophm.github.io/interpretable-ml-book/pdp.html
set.seed(747)
pdp_age <- model_profile(explainer_base_new, N = NULL,  variables = "idade",groups = "sexo", type = "partial")
plot(pdp_age)

#Plot do Partil dependence profile com nº observações no eixo X. Explicando HOMIC_PREDS
ggplot() + 
  geom_rug(data = homic_preds,mapping =  aes(x=idade), sides="b",outside = TRUE) + coord_cartesian(clip = "off") +
  geom_line(data = 
              as_tibble(pdp_age$agr_profiles) %>% 
              mutate(`_label_` = stringr::str_remove(`_label_`, "Random Forest_")) %>%
              rename(idade = `_x_`,sexo = `_groups_`),
            aes(x = idade, y = `_yhat_`,color = sexo), linewidth = 1.2, alpha = 0.8) + 
  facet_wrap(~sexo,scales = "free",nrow = 3) +
  scale_y_continuous(labels = scales::label_percent()) + scale_x_continuous(breaks = seq(0, 120, 10)) +
  labs(x = "Idade",y = "Pr. Média Homicídio Oculto",color = "") + theme(legend.position ="")

#Valor da probabilidade média por idade
pdp_age[["agr_profiles"]] %>% filter(`_label_` == "_Ignorado") %>% view()
rm(pdp_age)


#Instrumento x idade
set.seed(747)
pdp_ins_age <- model_profile(explainer_base_new, N = NULL, variables = "idade",groups = "instrumento", type = "partial")
plot(pdp_ins_age)

#Partil dependence profile com n? de observa??es teste eixo x - Idade x Instrumento
ggplot() + 
  geom_rug(data = homic_preds,mapping =  aes(x=idade), sides="b",outside = TRUE) + coord_cartesian(clip = "off") +
  geom_line(data = 
              as_tibble(pdp_ins_age$agr_profiles) %>% 
              mutate(`_label_` = stringr::str_remove(`_label_`, "Random Forest_")) %>%
              rename(idade = `_x_`,instrumentos = `_groups_`),
            aes(x = idade, y = `_yhat_`,color = instrumentos), linewidth = 1.2, alpha = 0.8) + 
  facet_wrap(~instrumentos,scales = "free") +
  scale_y_continuous(labels = scales::label_percent()) + scale_x_continuous(breaks = seq(0, 120, 20)) +
  labs(x = "",y = "Pr. Média Homicídio Oculto",color = "") + theme(legend.position ="")
rm(pdp_ins_age)


# Shap value Global utilizando fastshap ------------------------------------------
#Importando pacotes
library(fastshap)
library(ranger)
set.seed(747)  # for reproducibility
(rfo <- ranger(intencao_homic ~ ., data = base_old |> filter(!is.na(idade)) |> select(c(intencao_homic,idade,sexo,racacor,estciv,esc,local_obito,instrumento,ano,mes,dia,uf_ocor)), 
               min.node.size = 32,num.trees = 100,importance =  'impurity',classification = TRUE,verbose = TRUE, 
               probability = TRUE))

#Função de previsão
pfun <- function(object, newdata) {  # prediction wrapper
  library(ranger)
  library(iterators)
  library(parallel)
  unname(predict(object, data = newdata)$predictions[, "homic"])
}

#Necessário para o fastshap::explain
base_old |> filter(!is.na(idade) & uf_ocor!="CNRAC") |> droplevels() |>
  select(c(idade,sexo,racacor,estciv,esc,local_obito,instrumento,ano,mes,dia,uf_ocor)) |> as.data.frame() -> base_old
base_new |> filter(!is.na(idade)) |> select(c(idade,sexo,racacor,estciv,esc,local_obito,instrumento,ano,mes,dia,uf_ocor)) |> as.data.frame() -> base_new

#Global explanation
library(doParallel)
# With parallelism
registerDoParallel(cores = 4)
set.seed(2224)  # for reproducibility
fast_shap <- fastshap::explain(rfo, X = base_old,
                               #Explicando a base new
                               newdata = base_new,
                               pred_wrapper = pfun, nsim = 100, adjust = TRUE,shap_only = FALSE, parallel = TRUE)
tibble::as_tibble(fast_shap$shapley_values)

library(shapviz)
shv.global <- shapviz(fast_shap)

sv_importance(shv.global, kind = "bar", show_numbers = TRUE) +
  labs(y= "",x="Média(|SHAP VALUE|)",color="Valor da Vari?vel") +
  theme(legend.position = c(0.85,0.55),
        axis.text.y = element_text(size = 13.5),
        legend.background = element_rect(fill = "transparent", colour = NA))
ggsave(filename ="shap_value.bmp",width = 7,height = 7,device='bmp', dpi=300)
ggsave(filename ="fig11.eps",width = 10,height = 15,device=cairo_ps, dpi=350)

# sv_importance(shv.global, kind = "both", show_numbers = TRUE) +
#   labs(y= "",x="M?dia(|SHAP VALUE|)",color="Valor da Vari?vel") +
#   theme(legend.position = c(0.85,0.55),
#         axis.text.y = element_text(size = 13.5),
#         legend.background = element_rect(fill = "transparent", colour = NA))


#Boxplot do shap value - instrumento
fast_shap$feature_values |> select(instrumento,sexo) |> tibble() |>
  #rename(instrumento_x = "pull(sv$Class_1$X, instrumento)") |> 
  bind_cols(
    fast_shap$shapley_values |> data.frame() |> select(instrumento_s = instrumento)) |> 
  mutate(instrumento = fct_reorder2(instrumento,sexo,instrumento_s)) |>
  #Fazendo o boxplot
  ggplot() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  #geom_violin(aes(x=instrumento_x, y = instrumento_s)) +
  geom_boxplot(aes(x=instrumento, y = instrumento_s,fill=sexo)) +
  #geom_point(aes(x=instrumento, y = instrumento_s,color=sexo),position = position_jitter(seed = 1, width = 0.2)) +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.1)) +
  labs(x="Instrumento", y = "Shap value do instrumento",fill = "") +
  theme(legend.background=element_blank(),legend.box = "horizontal",
        legend.key = element_blank(),legend.position = c(0.8,0.6))


# Shapley Regression 
homic_preds |> filter(!is.na(idade)) |> select(.pred_homic) |>
  bind_cols(fast_shap$shapley_values) %>%
  lm(.pred_homic ~ .,data = .) %>% summary()
broom::tidy() 





















































# Shap Value --------------------------------------------------------------
library(shapviz)
library(treeshap) #? poss?vel utilizar as an?lises anteriores utilizando treeshap

#Receita Random forest
recipe(intencao_homic ~ ., data=base_old) %>%
  step_rm(starts_with("cod")) %>% step_naomit(idade) %>%
  #Imputa??o de idade - Ocorre um problema no base ao fazer a imputa??oo de idade.
  #step_impute_knn(c(idade),neighbors =5,impute_with = imp_vars(all_predictors())) %>%
  #Transforma factor em indicator variable(dummy)
  step_dummy(all_nominal_predictors(),one_hot = TRUE) %>% #Aqui pode usar todas as categorias.
  step_zv(all_predictors()) %>%
  #Aplicando receita na base de treinamento.
  prep() -> train_recipe #Salvando receita para aplicar ao base new
train_recipe %>%
  bake(new_data = NULL, composition = "data.frame") %>%
  #Necess?rio ao rodar o ranger.
  mutate(intencao_homic = recode(intencao_homic, homic = 1, n_homic = 0)) -> base_old_rf

train_recipe
#Random forest   
model_shap <- ranger::ranger(intencao_homic ~ ., data = base_old_rf,mtry = 15, min.node.size = 32,
                             num.trees = 100,importance =  'impurity',classification = TRUE,
                             seed=747,#Acho que a seed est? diferente do modelo original
                             verbose = TRUE)

#C?lculo do shap value
#Aplicando receita da old na base new utilizada no c?lculo do shap
homic_preds %>% select(!c(.pred_homic,.pred_n_homic)) %>% #,codmunocor, codmunres
  #Retirando idade NA
  drop_na(idade) %>%
  rename(intencao_homic = .pred_class) %>%
  bake(object = train_recipe,  composition = "data.frame") %>%
  #Necess?rio ao rodar o ranger.
  mutate(intencao_homic = recode(intencao_homic, homic = 1, n_homic = 0)) -> base_new_rf

# Prediction wrapper
pfun <- function(object, newdata) {
  library(ranger)
  predict(object, data = newdata)$predictions
}

#Calculando o shap
library(doParallel)
detectCores()
cl <- makeCluster(10)
registerDoParallel(cl)
shap <- fastshap::explain(model_shap,X = select(base_old_rf,!intencao_homic),
                          nsim = 500, .parallel = T,
                          newdata = select(base_new_rf,!intencao_homic),
                          pred_wrapper = pfun)
stopCluster(cl)
rm(cl)
autoplot(shap) # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "idade", X = base_new_rf)
autoplot(shap, type = "contribution", row_num = 1) # explain first row of X
fastshap::force_plot(shap)

#Gr?ficos
library(shapviz)
shp <- shapviz(shap, X = base_new_rf)
#WaterFall plot
sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

#Plot force - POSSO FAZER A EXPLICA??O DAS OBSERVA??ES INDIVIDUAIS AQUI.
sv_force(shp, row_id = 502)

# A bar plot of mean absolute SHAP values
sv_importance(shp)

# A beeswarm plot
sv_importance(shp, kind = "beeswarm")

# Or both!
https://stackoverflow.com/questions/74754945/how-to-customize-x-axis-range-with-shapviz
sv_importance(shp, kind = "both", show_numbers = TRUE, bar_width = 0.5,fill="#e00025", max_display = 10, show_other = FALSE) +
  labs(y= "M?dia(|SHAP VALUE|)",color="Valor da Vari?vel") +
  theme(legend.position = c(0.85,0.35),
        axis.text.y = element_text(size = 10),
        legend.background = element_rect(fill = "transparent", colour = NA))

#Valor da vari?vel x SHAP value
sv_dependence(shp, v = "idade") +
  geom_hline(yintercept = 0.0, linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,120,6)) + 
  labs(y= "SHAP value de idade",x="idade")


###Juntar o shap value de idade, as vari?veis idade e instrumento
#Quero fazer um dependece plot. Existe melhor maneira de fazer isso? 
#valor do Shap de idade.
unlist(shp$S) %>% as_tibble() %>% select(idade) %>% rename(shap_idade = idade) -> shap_idade
#Colunas indicando o instrumento do ?bito.
unlist(shp$X["idade"]) %>% as_tibble() %>% rename(idade = value) -> idade
unlist(shp$X["instrumento_Afogamento"]) %>% as_tibble() %>% rename(afogamento = value) -> afogamento
unlist(shp$X["instrumento_Contundente"]) %>% as_tibble() %>% rename(contundente = value) -> contundente
unlist(shp$X["instrumento_Desconhecido"]) %>% as_tibble() %>% rename(desconhecido = value) -> desconhecido
unlist(shp$X["instrumento_Enforcamento"]) %>% as_tibble() %>% rename(enforcamento = value) -> enforcamento
unlist(shp$X["instrumento_Envenenamento"]) %>% as_tibble() %>% rename(envenenamento = value) -> envenenamento
unlist(shp$X["instrumento_Fogo"]) %>% as_tibble() %>% rename(fogo = value) -> fogo
unlist(shp$X["instrumento_Impacto"]) %>% as_tibble() %>% rename(impacto = value) -> impacto
unlist(shp$X["instrumento_PAF"]) %>% as_tibble() %>% rename(PAF = value) -> paf
unlist(shp$X["instrumento_Perfurante"]) %>% as_tibble() %>% rename(perfurante = value) -> perfurante
unlist(shp$X["instrumento_Ve?culo"]) %>% as_tibble() %>% rename(veiculo = value) -> veiculo

#Uni?o de shap idade e instrumento 
idade %>% cbind(shap_idade,afogamento,contundente,desconhecido,enforcamento,envenenamento,fogo,impacto,paf,perfurante,veiculo) %>% as_tibble() %>%
  mutate(instrumento = case_when(afogamento == 1 ~ "Afogamento", contundente == 1 ~ "Contundente",  desconhecido == 1 ~ "Desconhecido",
                                 enforcamento == 1 ~ "Desconhecido", envenenamento == 1 ~ "Envenenamento", fogo == 1 ~ "Fogo", 
                                 impacto == 1 ~ "Impacto", PAF == 1  ~ "PAF", perfurante == 1 ~ "Perfurante", veiculo == 1 ~ "Ve?culo",
                                 TRUE ~ "NA")) %>% select(idade,shap_idade,instrumento) -> base_shap
rm(shap_idade,idade,afogamento,contundente,desconhecido,enforcamento,paf,veiculo,envenenamento,fogo,impacto,perfurante)

base_shap %>%
  ggplot(aes(x = idade, y = shap_idade, color = instrumento)) +
  geom_point() +
  geom_hline(yintercept = 0.0, linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,120,6)) + 
  labs(y= "SHAP value de idade",x="idade",color = "") +
  theme(legend.background =  element_rect(fill = "transparent"), 
        legend.position = c(0.25,0.25)) + guides(fill = guide_legend(nrow  = 4,byrow=TRUE))
rm(base_shap)

