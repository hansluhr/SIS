library(tidymodels)
tidymodels_prefer()
library(tidyverse)
library(themis) #Subsamples
library(baguette)
library(janitor)

#Importando base
load("E:/Trabalhos/Ocultos/Base/sim_doext_96_22.RData")
glimpse(sim_doext)

#Tratando base importada.
glimpse(base_old)
# base_old %>%
#   filter(ano==2014 | ano==2015) %>%
#   #Retira o levels de anos n?o utilizados
#   droplevels() %>%
#   #Slice da base
#   slice_sample(prop = 0.025) -> base_old


glimpse(base_old)
rm(sim_doext)

base_old %>% tabyl(ano,intencao_homic)
base_old %>% tabyl(idade) %>% slice_max(n=10,order_by = n)
base_old %>% tabyl(ano)

# #Check de propor??es
base_old %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))
# base_old %>% group_by(uf) %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))


#Split dos dados - A estratifica??o tamb?m nas feita nas UFs
set.seed(747)
data_split <- initial_split(base_old,prop = 0.7,strata = intencao_homic)
# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

glimpse(train_data)

# #Checando estratifica??o
train_data %>% count(intencao_homic) %>% mutate(prop = n/sum(n))
# train_data %>% group_by(uf) %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))
# #Checando estratifica??o
# test_data %>% count(intencao_homic) %>%  mutate(prop = n/sum(n))

#create resampling folds.
set.seed(234)
folds <- vfold_cv(train_data, strata = intencao_homic,v=10)
folds

#Receitas
#receita logit e logit penalizado
lr_recipe <- recipe(intencao_homic ~ idade + sexo + racacor + estciv + esc + local_obito + instrumento + ano + mes + dia + uf_ocor, data=train_data) %>%
  #Imputa??o de idade
  step_impute_knn(c(idade),neighbors =5,impute_with = imp_vars(all_predictors())) %>%
  #Transforma factor em indicator variable(dummy)
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  # #SubSample - Igualar a propor??oo de classes na amostra - CHECAR SE APLICA SOMENTE AO TRAINING SET
  #step_rose(intencao_homic,skip = TRUE) %>% #Rose - Random over sampling Examples
  step_smote(intencao_homic,skip = TRUE,seed=51)   

#Nesse caso apresenta tibbles do train_data ap?s aplicar os passos da receita.
lr_recipe %>% prep() %>% bake(new_data = NULL) -> x


#Receita modelos de ?rvore de decis?o
rf_recipe <- recipe(intencao_homic ~ idade + sexo + racacor + estciv + esc + local_obito + instrumento + ano + mes + dia + uf_ocor, data=train_data) %>%
  #Imputa??o de idade
  step_impute_knn(c(idade),neighbors =5,impute_with = imp_vars(all_predictors())) %>%
  #Transforma factor em indicator variable(dummy)
  step_dummy(all_nominal_predictors(),one_hot = TRUE) %>% #Aqui pode usar todas as categorias.
  step_zv(all_predictors()) %>%
  # #SubSample - Igualar a propor??o de classes na amostra - CHECAR SE APLICA SOMENTE AO TRAINING SET
  #step_rose(intencao_homic,skip = TRUE) %>% #Rose - Random over sampling Examples
  step_smote(intencao_homic,skip = TRUE,seed=51)  


#Nesse caso apresenta tibbles do train_data ap?s aplicar os passos da receita.
rf_recipe %>% prep() %>% bake(new_data = NULL) -> train_data_rf

# Modelos -----------------------------------------------------------------
#Logit
logit_spec <- logistic_reg() %>% set_engine("glm")
#lasso Logit
lasso_logit_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
#ridge_logit
ridge_logit_spec <- logistic_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet")
#elastic net model
elastic_p_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")

#nearest_neighbor
# knn_spec <- nearest_neighbor(neighbors = tune()) %>%
#   set_engine("kknn") %>% set_mode("classification")

#Decision Tree
#Interessante indicar o crit?rio de divis?o da ?rvore
tree_spec <- decision_tree(cost_complexity = tune(),tree_depth = tune(),min_n = tune()) %>%   
  set_engine("rpart") %>% set_mode("classification")

#Bootstrap aggregation (bagging)
bag_spec <- bag_tree(cost_complexity = tune(),tree_depth = tune(),min_n = tune()) %>%   
  set_engine("rpart",seed = 747, times=100) %>% set_mode("classification")

#Random forest
#Especificando o modelo - dois hiperparametros
rf_spec  <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>%
  set_engine("ranger",importance = "impurity",verbose = TRUE,seed = 747) %>%
  set_mode("classification")

#xgboost
xgb_spec <- boost_tree(tree_depth = tune(), mtry = tune(),learn_rate = tune(), loss_reduction = tune(),
                       min_n = tune(), sample_size = tune(), trees = 100) %>%
  set_engine("xgboost",set.seed = 747) %>% set_mode("classification")

#Workflow
#Workflow + Receita logit e logit-p
w_logit <- workflow_set(preproc = list(int = lr_recipe),
                        models = list(logit = logit_spec, lasso_logit = lasso_logit_spec,
                                      ridge_logit = ridge_logit_spec, elastic_logit = elastic_p_spec))
w_logit

#Olhar workflow espec?fico
w_logit %>% extract_workflow(id="int_lasso_logit")

#Workflow + Receita random forest e xgb
w_rftree <- workflow_set(preproc = list(n_int = rf_recipe),
                        models = list(dtree = tree_spec,#bag = bag_spec,
                                      RF = rf_spec, XGBoosting = xgb_spec))
w_rftree

#Juntando workflows
all_workflows <- bind_rows(w_logit,w_rftree)
all_workflows

#Removendo especifica??es e receitas.
rm(w_logit,w_rftree,logit_spec,lasso_logit_spec, ridge_logit_spec, elastic_p_spec,
   tree_spec,bag_spec,rf_spec,xgb_spec,lr_recipe,rf_recipe)




# Tunning de hiperparametros - Grid search --------------------------------
#M?tricas utilizadas
grid_metrics <- yardstick::metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc)
library(finetune)
grid_ctrl <-
  control_race(
    save_pred = FALSE,verbose = TRUE,
    parallel_over = "resamples",
    allow_par = TRUE,save_workflow = FALSE)


#doParallel::registerDoParallel()
library(doParallel)
detectCores(logical = TRUE)
cl <- makeCluster(10)
registerDoParallel(cl)
set.seed(77)

#Rodando os modelos
full_results_time <- 
  system.time(
    grid_results_all <- 
      all_workflows %>% 
      workflow_map(seed = 777, resamples = folds,metrics = grid_metrics, grid = 5, 
                   control = grid_ctrl, verbose = TRUE))

stopCluster(cl)
gc()

rm(grid_ctrl,grid_metrics,cl,full_results_time,folds)

#Tabela com todos os modelos do cross-validation - selecionando o modelo com melhor roc_auc
grid_results_all %>% rank_results(rank_metric = "roc_auc", select_best = TRUE) %>% select(.metric,mean,std_err,wflow_id,model,rank) %>%
  #Fazendo tabela.
  pivot_wider(names_from = .metric, values_from = c(mean,std_err)) %>%
  mutate(wflow_id = case_when(wflow_id == "n_int_RF" ~ "RF",wflow_id == "n_int_bag" ~ "Bagging Tree",
                              wflow_id == "int_lasso_logit" ~ "Lasso Logit", wflow_id == "int_logit" ~ "Logit",
                              wflow_id == "int_elastic_logit" ~ "Elastic Logit", wflow_id == "int_ridge_logit" ~ "Ridge Logit",
                              wflow_id == "n_int_XGBoosting" ~ "XGB", wflow_id == "n_int_dtree" ~ "DTree")) %>%
  rename(Modelos = wflow_id) %>% select(!c(model,rank)) %>%
  #Removendo mean do nome das colunas.
  rename_with(~stringr::str_remove(.,"mean_")) -> cv_metrics
#std. devation est? nas colunas. Precisa passar para linha.

rio::export(cv_metrics,"cv_metrics.xlsx")

#Curva roc_auc
grid_results_all %>%
  rank_results() %>%
  filter(.metric == "roc_auc") %>%
  select(model, .config, roc_auc = mean, rank)

#Gr?fico dos melhores modelos
autoplot(
  grid_results_all,
  rank_metric = "roc_auc",  # <- how to order models
  metric = "roc_auc",       # <- which metric to visualize
  select_best = TRUE )    # <- one point per workflow

#Plot estat?sticas de precis?o
autoplot(grid_results_all)
autoplot(grid_results_all, metric = "mcc")
autoplot(grid_results_all, id = "n_int_RF", metric = "roc_auc")
autoplot(grid_results_all, id = "int_logit_p", metric = "roc_auc")


#Coletando m?tricas dos modelos
grid_results_all %>%  collect_metrics() %>% filter(.metric=="roc_auc") %>% arrange(desc(mean))

#Previs?es de todos os modelos
grid_results_all %>% collect_predictions()

grid_results_all %>% extract_workflow("int_logit") %>% summary()


# Ajuste no train_data e previs?o no test_data ----------------------------
#Logit - Previs?o contra o test set
logit_test_fit <- grid_results_all %>%
  extract_workflow("int_logit") %>%   #Aqui n?o precisa pegar o hiperpar?metro
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
logit_metrics_test <- collect_metrics(logit_test_fit)
rm(logit_test_fit)

#Lasso Logit - Previs?o contra o test set
best_logit_p <- grid_results_all %>%
  extract_workflow_set_result("int_lasso_logit") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
logit_pen_test_fit <- grid_results_all %>%  extract_workflow("int_lasso_logit") %>%
  finalize_workflow(best_logit_p) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
logit_pen_metrics_test <- collect_metrics(logit_pen_test_fit)
rm(best_logit_p,logit_pen_test_fit)


#Elastic net Logit - Previs?o contra o test set
best_elastic <- grid_results_all %>%
  extract_workflow_set_result("int_elastic_logit") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
elastic_test_fit <- grid_results_all %>% extract_workflow("int_elastic_logit") %>%
  finalize_workflow(best_elastic) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
elastic_metrics_test <- collect_metrics(elastic_test_fit)
rm(best_elastic,elastic_test_fit)


#Ridge Logit - Previs?o contra o test set
best_ridge <- grid_results_all %>%
  extract_workflow_set_result("int_ridge_logit") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
ridge_test_fit <- grid_results_all %>% extract_workflow("int_ridge_logit") %>%
  finalize_workflow(best_ridge) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
ridge_metrics_test <- collect_metrics(ridge_test_fit)
rm(best_ridge,ridge_test_fit)


#Decision tree - Previs?o contra o test set
best_dtree <- grid_results_all %>%
  extract_workflow_set_result("n_int_dtree") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
set.seed(777)
dtree_test_fit <- grid_results_all %>%  extract_workflow("n_int_dtree") %>%
  finalize_workflow(best_dtree) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
dtree_metrics_test <- collect_metrics(dtree_test_fit)
rm(best_dtree,dtree_test_fit)


# #Bagging - Previs?o contra o test set
# best_bagging <- grid_results_all %>%
#   extract_workflow_set_result("n_int_bag") %>%
#   #Selecionou o melhor hipepar?metro
#   select_best(metric = "roc_auc")
# set.seed(777)
# bdtree_test_fit <- grid_results_all %>%  extract_workflow("n_int_bag") %>%
#   finalize_workflow(best_bagging) %>%
#   last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
# bdtree_metrics_test <- collect_metrics(bdtree_test_fit)
# rm(best_bagging,bdtree_test_fit)

#Random forest - Previs?o contra o test set
best_rf <- grid_results_all %>%
  extract_workflow_set_result("n_int_RF") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
set.seed(777)
rf_test_fit <- grid_results_all %>%  extract_workflow("n_int_RF") %>%
  finalize_workflow(best_rf) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
rf_metrics_test <- collect_metrics(rf_test_fit)
#rm(best_rf) #N?o excluir modelo de melhor ajuste.
save.image("E:/Trabalhos/Ocultos/Base/sim_doext_96_22_cv.RData")


#Extreme Gradient Boosting - Previs?o contra o test set
best_xgb <- grid_results_all %>%
  extract_workflow_set_result("n_int_XGBoosting") %>%
  #Selecionou o melhor hipepar?metro
  select_best(metric = "roc_auc")
set.seed(777)
xgb_test_fit <- grid_results_all %>%  extract_workflow("n_int_XGBoosting") %>%
  finalize_workflow(best_xgb) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))
xgb_metrics_test <- collect_metrics(xgb_test_fit)
#rm(best_xgb)#N?o excluir modelo de melhor ajuste.
save.image("E:/Trabalhos/Ocultos/Base/sim_doext_96_22_cv.RData")
gc()

#Tabela com M?tricas test_data
bind_cols(
  tibble(models = c("Logit", "lasso Logit","Ridge Logit", "Elastic Net Logit",
                    "DTree","RF","XGB")), # "Bagging",
  bind_rows(
    logit_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    logit_pen_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    ridge_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    elastic_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    dtree_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    # bdtree_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    rf_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),
    xgb_metrics_test %>% pivot_wider(values_from = .estimate, names_from = .metric),)) %>%
  select(!c(.estimator,.config)) -> test_data_metrics #Aqui n?o tem std. deviation.
rm(logit_metrics_test,logit_pen_metrics_test,dtree_metrics_test,ridge_metrics_test,elastic_metrics_test,
   bdtree_metrics_test,rf_metrics_test,xgb_metrics_test,data_split,best_xgb,xgb_test_fit,all_workflows)


rio::export(test_data_metrics,"test_data_metrics.xlsx")


#Seleciona melhor configura??o no cross-validation de modelo especificado
grid_results_all %>% extract_workflow_set_result("n_int_RF") %>%
  select_best(metric = "roc_auc") -> best_model #Configura??o do melhor modelo

#Rodando melhor modelo do CV no test set.
test_results <-
  grid_results_all %>%
  extract_workflow("n_int_RF") %>%
  finalize_workflow(best_model) %>%
  last_fit(split = data_split, metrics = metric_set(accuracy,bal_accuracy,precision,sensitivity,specificity,roc_auc,f_meas,j_index,mcc))

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


#Workflow do melhor modelo
grid_results_all %>% extract_workflow("n_int_RF") %>% finalize_workflow(best_model) -> best_work_flow
#Fit do modelo na base_old inteira. Esse modelo ser? utilizado na classifica??oo.
last_model_fit <- fit(best_work_flow,base_old)

#Excluindo fit contro o test set
rm(rf_test_fit,xgb_test_fit)

# #Previs?o do n?mero de homic?dios ocultos. ------------------------------
#Importando base de indeterminados
# load("D:/Trabalhos/Hoculto/Bases/sim_doext.RData")
# glimpse(base_old)

#Tratando base importada - teste da rotina
base_new %>%
  filter(ano==2014 | ano==2015) %>%
  # Retira o levels de anos n?o utilizados
  droplevels() %>%
  # Slice da base
  slice_sample(prop = 0.095) -> base_new

glimpse(base_new)
rm(sim_doext)


#Como ficou o test set ap?s pr? processamento - COMO COLOCAR A RECEITA VENCEDORA?
best_work_flow$pre$actions$recipe$recipe %>% prep() %>% bake(new_data=base_new) -> x #Esta preenchendo o missing de idade
rm(x)

#Previs?o de homic?dios ocultos
homic_preds <-  augment(last_model_fit, base_new)
homic_preds %>% count(.pred_class)
sapply(homic_preds, function(x) sum(is.na(x)))#Check de na

#Ideal ? come?ar por homic?dio.
sim_doext %>% ....

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


###Exporta??o dos munic?pios
#Exporta??o dos microdados com os ocultos.
homic_preds %>% count(.pred_class)

Se quiser juntar as bases precisa juntar base_old + homic_preds

rm(pop,base_oculto_br,base_oculto_uf)


# Interpreta??o do modelo -------------------------------------------------
library(DALEXtra)
#Explainer das probabilidades estimadas.
explainer_base_new  <- explain_tidymodels(model=last_model_fit, #final_trained
                                          #Utilizar test ou trained data?
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
#Qual a combina??o de vari?veis mais frequente?
base_new %>% select(!c(codmunocor,codmunres)) %>% #PARA VALER FAZER NO TEST DATA!!!
  group_by_all() %>%
  summarise(count = n()) %>%
  arrange(desc(count))
#Seleciona a observa??o desejada - Selecionando a observa??o mais frequente no test set.
local_expla <- homic_preds[10,]
#C?lculo do Shap Value de observa??o individual
set.seed(737)
shap_local <-
  DALEX::predict_parts(explainer = explainer_base_new,N=100,
                       new_observation = local_expla,
                       type = "shap",B = 5)
plot(shap_local)

#Explicar idade imputada,instrumento e local de ?bito desconhecido
homic_preds %>% filter(is.na(idade) & racacor == "Ignorado" & estciv == "Ignorado" &
                         instrumento == "Desconhecido" & local_obito == "Ignorado")

# The box plots in Figure 18.2 display the distribution of contributions across all the orderings we
# tried, and the bars display the average attribution for each feature:
shap_local %>%
  group_by(variable) %>% mutate(mean_val = mean(contribution)) %>%  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val),
           aes(mean_val, variable),  alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") +
  scale_fill_viridis_d() + labs(y = NULL,x="")

rm(local_expla,shap_local)


# 18.3 GLOBAL EXPLANATIONS ------------------------------------------------
#Permutation feature imporantece\ Permutation based variable importance
###Alfere a Import?ncia no desempenho preditivo###
# One way to compute variable importance is to permute the features (Breiman 2001a).
# We can permute or shuffle the values of a feature, predict from the model, and then measure how much worse
# the model fits the data compared to before shuffling.
# If shuffling a column causes a large degradation in model performance, it is important;
# if shuffling a column's values doesn't make much difference to how the model performs,
# it must not be an important variable.
#Explainer do Partial dependece plot - Utiliza a test base
#Criando explainer
final_trained <- rf_test_fit$.workflow[[1]] #Precisa estar ajustado ao training_data
#Aqui quero explicar os erros contra o test_set
explainer  <- explain_tidymodels(model=final_trained, #last_model_fit
                                 #Utilizar test
                                 #data = dplyr::select(test_data, -c(intencao_homic,codmunocor,codmunres)),
                                 data = dplyr::select(test_data, -c(idade,sexo,racacor,estciv,esc,local_obito,instrumento,ano,mes,dia,uf_ocor)),
                                 #N?o seria as novas observa??es?homic_pred
                                 y =  test_data$intencao_homic == "homic",
                                 predict_function_target_column = "homic",
                                 label = "",
                                 verbose = T, type = "classification")


set.seed(502)
vip_rf <- model_parts(explainer, N = NULL,  type = "difference",#pode trocar o type.
                      loss_function = loss_one_minus_auc, B = 50)
plot(vip_rf)

#Fun??o de plot sugerida no site tmwr.
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
  labs(y = "", x = "1 - AUC ap?s permuta??es") +
  theme(legend.position = "none", axis.text.y = element_text(size = 7.5),
        axis.title.x = element_text(size = 7.5),axis.text.x = element_text(size=7.5)) 

ggsave(filename ="PIF.bmp",width = 6,height = 6,device='bmp', dpi=300)
ggsave(filename ="fig10.eps",width = 7,height = 7,device=cairo_ps, dpi=300)


#Outra maneira de apresentar o feature importance.
# library(vip)
# rf_test_fit %>%
#   pluck(".workflow", 1) %>%
#   extract_fit_parsnip() %>%
#   vip(geom = "point",num_features = 20)


# 18.4 BUILDING GLOBAL EXPLANATIONS FROM LOCAL EXPLANATIONS ---------------
#Partial dependece profile\plot - Varia??o no valor previsto dado varia??o na vari?vel analisada
#Sexo
set.seed(805)
pdp_sexo <- model_profile(explainer_base_new, N = NULL,  variables = "sexo",variable_type = "categorical")
plot(pdp_sexo)
rm(pdp_sexo)

#Idade por sexo
#https://ema.drwhy.ai/partialDependenceProfiles.html
#https://christophm.github.io/interpretable-ml-book/pdp.html
set.seed(747)
pdp_age <- model_profile(explainer_base_new, N = NULL,  variables = "idade",groups = "sexo", type = "partial")
plot(pdp_age)
#Plot do Partil dependence profile com n? observa??es teste eixo x.
ggplot() +
  geom_rug(data = test_data,mapping =  aes(x=idade), sides="b",outside = TRUE) + coord_cartesian(clip = "off") +
  geom_line(data =
              as_tibble(pdp_age$agr_profiles) %>%
              mutate(`_label_` = stringr::str_remove(`_label_`, "Random Forest_")) %>%
              rename(idade = `_x_`,sexo = `_groups_`),
            aes(x = idade, y = `_yhat_`,color = sexo), linewidth = 1.2, alpha = 0.8) +
  facet_wrap(~sexo,scales = "free",nrow = 3) +
  scale_y_continuous(labels = scales::label_percent()) + scale_x_continuous(breaks = seq(0, 120, 10)) +
  labs(x = "Idade",y = "Pr. M?dia Homic?dio Oculto",color = "") + theme(legend.position ="")

ggsave(filename ="PDP - Idade.bmp",width = 7,height = 7,device='bmp', dpi=300)
ggsave(filename ="fig14.eps",width = 7,height = 7,device=cairo_ps, dpi=300)

#Estat?sticas do pdf_age_sexo
view(pdp_age$agr_profiles)
rm(pdp_age)


#Instrumento x idade
set.seed(747)
pdp_ins_age <- model_profile(explainer_base_new, N = NULL, variables = "idade",groups = "instrumento", type = "partial")
plot(pdp_ins_age)

#Partil dependence profile com n? observa??es teste eixo x - Idade x Instrumento
ggplot() +
  geom_rug(data = homic_preds,mapping =  aes(x=idade), sides="b") +
  geom_line(data =
              as_tibble(pdp_ins_age$agr_profiles) %>%
              mutate(`_label_` = stringr::str_remove(`_label_`, "Random Forest_")) %>%
              rename(idade = `_x_`,instrumentos = `_groups_`),
            aes(x = idade, y = `_yhat_`,color = instrumentos), linewidth = 1.2, alpha = 0.8) +
  facet_wrap(~instrumentos,scales = "free") +
  scale_y_continuous(labels = scales::label_percent()) + scale_x_continuous(breaks = seq(0, 120, 20)) +
  labs(x = "",y = "Pr. M?dia Homic?dio Oculto",color = "") + theme(legend.position ="")

ggsave(filename ="PDP - Idade_Instrumento.bmp",width = 7,height = 7,device='bmp', dpi=300)
ggsave(filename ="fig15.eps",width = 7,height = 7,device=cairo_ps, dpi=300)


rm(pdp_ins_age)


# Utilizando o Tidymodels -------------------------------------------------
library(kernelshap)
library(shapviz)
library(tidyverse)
library(tidymodels)
library(themis) #Subsamples
library(baguette)

https://stackoverflow.com/questions/76772086/r-kernelshap-package-with-tidymodels-with-classification
https://freedium.cfd/https://towardsdatascience.com/explain-any-models-with-the-shap-values-use-the-kernelexplainer-79de9464897a

library(doFuture)
options(doFuture.rng.onMisuse = "ignore")  # To suppress some warning on random seeds
# Set up parallel backend
registerDoFuture()
plan(multisession, workers = 27)  # Windows
# plan(multicore, workers = 4)   # Linux, macOS, Solaris

#x <- c("student", "balance", "income")
system.time(
  ks <- kernelshap(
    last_model_fit, 
    X = slice_sample(select(base_new,!c(codmunocor,codmunres)), n = 5000),    # Assuming random row order
    bg_X = slice_sample(select(base_old,!c(codmunocor,codmunres)), n = 500),  # Assuming random row order
    type = "prob",              # Predictions must be numeric
    #feature_names = x,# Or use X = head(Default[x], 1000)
    parallel = TRUE,
    parallel_args = list(.packages = "tidymodels")
  ) 
)

sv <- shapviz(ks, interactions = TRUE)             # Contains one shapviz object per class

#You can now clearly see the overall contribution of this feature.
#Interpreta??o das barras do shap.
#For example, we use the mean SHAP plot in the code below. 
#Looking at Figure 5, we can use this plot to highlight important categorical features. 
#For example, we can see that odor tends to have large positive/ negative SHAP values. Instrumento!
sv_importance(sv$Class_1, kind = "both", show_numbers = TRUE) +
  labs(y= "",x="M?dia(|SHAP VALUE|)",color="Valor da Vari?vel") +
  theme(legend.position = c(0.85,0.55),
        axis.text.y = element_text(size = 13.5),
        legend.background = element_rect(fill = "transparent", colour = NA))

sv_dependence(sv$Class_1, v = "idade",color_var = "instrumento")

sv_dependence(sv$Class_1, v = "idade",interactions = TRUE)

#Boxplot do shap value - instrumento
sv$Class_1$X |> select(instrumento,sexo) |> tibble() |>
  #rename(instrumento_x = "pull(sv$Class_1$X, instrumento)") |> 
  bind_cols(
    sv$Class_1$S |> data.frame() |> select(instrumento_s = instrumento)) |> 
  #Fazendo o boxplot
  ggplot() +
  #geom_violin(aes(x=instrumento_x, y = instrumento_s)) +
  geom_boxplot(aes(x=instrumento, y = instrumento_s,fill=sexo)) +
  #geom_point(aes(x=instrumento, y = instrumento_s,color=sexo),position = position_jitter(seed = 1, width = 0.2)) +
  labs(x="Instrumento", y = "Shap value",fill = "") +
  theme(legend.background=element_blank(),legend.key = element_blank(),legend.position = c(0.8,0.6))


#Boxplot do shap value - local do incidente
sv$Class_1$X |> select(local = local_obito,sexo) |> tibble() |>
  #rename(local_x = "pull(sv$Class_1$X, local_obito)") |> 
  bind_cols(
    sv$Class_1$S |> data.frame() |> select(local_s = local_obito)) |> 
  #Fazendo o boxplot
  ggplot() +
  geom_boxplot(aes(x=local, y = local_s,fill = sexo)) +
  labs(x="Local do Incidente", y = "Shap value",fill="") +
  theme(legend.background=element_blank(),legend.key = element_blank(),legend.position = c(0.8,0.6))

# Shap value utilizando fastshap ------------------------------------------
#Importando pacotes
library(fastshap)
library(tidyverse)
library(ranger)
load("C:/Users/b224552695/Desktop/Oculto/Bases/sim_doext_96_21.RData")
set.seed(747)  # for reproducibility
(rfo <- ranger(intencao_homic ~ ., data = base_old |> filter(!is.na(idade)) |> select(!c(codmunocor,codmunres)), 
               min.node.size = 32,num.trees = 100,importance =  'impurity',classification = TRUE,verbose = TRUE, 
               probability = TRUE))
#Fun??o de previs?o
pfun <- function(object, newdata) {  # prediction wrapper
  library(ranger)
  library(iterators)
  library(parallel)
  unname(predict(object, data = newdata)$predictions[, "homic"])
}
load("C:/Users/b224552695/Desktop/Oculto/Bases/sim_doext_96_21_shap.RData")
#Necess?rio para o fastshap::explain
base_old |> filter(!is.na(idade) & uf_ocor!="CNRAC") |> droplevels() |>
  select(!c(intencao_homic,codmunocor,codmunres)) |> as.data.frame() -> base_old
base_new |> filter(!is.na(idade)) |> select(!c(codmunocor,codmunres)) |> as.data.frame() -> base_new
#"Error in { : task 1 failed -" "Missing data in columns: uf_ocor."
#Global explanation
library(doParallel)
# With parallelism
registerDoParallel(cores = 10)
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

# Utilizando TreeShap e Shapviz -------------------------------------------
#https://cran.r-project.org/web/packages/shapviz/vignettes/shapviz.html
library(shapviz)
library(treeshap) #? poss?vel utilizar as an?lises anteriores utilizando treeshap

#Se Random forest for o melhor modelo - Pegando a base utilizada no treinamento.
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
                             num.trees = 100,importance =  'impurity',classification = TRUE,seed=51,verbose = TRUE)
model_unified <- ranger.unify(model_shap, base_old_rf)#Unificando objetos.

#C?lculo do shap value
#Aplicando receita da old na base new utilizada no c?lculo do shap
homic_preds %>% select(!c(.pred_homic,.pred_n_homic)) %>% #,codmunocor, codmunres
  #Retirando idade NA
  drop_na(idade) %>%
  rename(intencao_homic = .pred_class) %>%
  bake(object = train_recipe,  composition = "data.frame") %>%
  #Necess?rio ao rodar o ranger.
  mutate(intencao_homic = recode(intencao_homic, homic = 1, n_homic = 0)) -> base_new_rf
#rm(train_recipe)

base_new %>%
  select(!c(codmunocor,codmunres)) %>%
  slice_sample(prop = 0.05) %>%
  bake(object = train_recipe,  composition = "data.frame")

#C?lculo do shap value
#Aqui entra valores a serem explicados
treeshap <- treeshap(model_unified, base_new_rf, interactions = TRUE, verbose = TRUE)

#Plots shapley value
#https://cran.r-project.org/web/packages/shapviz/vignettes/shapviz.html
shp <- shapviz(treeshap, X = base_new_rf)
#WaterFall plot
sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

#Plot force
sv_force(shp, row_id = 1L)

# A bar plot of mean absolute SHAP values
sv_importance(shp)

# A beeswarm plot
sv_importance(shp, kind = "beeswarm")

# Or both!
sv_importance(shp, kind = "both", show_numbers = TRUE) +
  labs(y= "M?dia(|SHAP VALUE|)",color="Valor da Vari?vel") +
  theme(legend.position = c(0.85,0.55),
        axis.text.y = element_text(size = 13.5),
        legend.background = element_rect(fill = "transparent", colour = NA))

Na legenda colocar Maior\Sim. Menor\N?o.

#SHAP dependence plots
sv_dependence(shp, v = "idade", color_var = "auto")
sv_dependence(shp, v = "idade", color_var = "instrumento_PAF")
#Color_var indica a vari?vel utilizada para colorir.
sv_dependence(shp, v = "idade") +
  labs(x="Idade", y = "SHAP Value")


#https://www.r-bloggers.com/2021/01/treeshap%E2%80%8A-%E2%80%8Aexplain-tree-based-models-with-shap-values/
#https://github.com/ModelOriented/treeshap
library(treeshap)

#Colocar a observa??o mais frequentes. Precisa olhar o valor da observa??o no homic_preds
plot_contribution(treeshap, obs = 14, max_vars = 10,explain_deviation=F, title = "")

#Feature Importance - Est? relacionado a varia??es no desempenho preditivo.
plot_feature_importance(treeshap, max_vars = 10) #Aqui entrega as vari?veis importantes utilizadas no summary plot.

#Feature Dependence
plot_feature_dependence(treeshap, variable = "idade")

#Interaction Plot
plot_interaction(treeshap, "idade", "instrumento_PAF")


#Summary Shap - Precisa retirar idade de coluna.
treeshap$observations %>% mutate(id = 1:nrow(treeshap$observations)) %>% select(!c(intencao_homic)) %>%
  pivot_longer(cols = !c(id),names_to = "coluna", values_to = "Valor_Var") -> x


treeshap$shaps %>% mutate(id = 1:nrow(treeshap$shaps)) %>% select(!c(intencao_homic)) %>%
  pivot_longer(cols = !c(id), names_to = "coluna", values_to = "Shap_value") -> y

x %>% left_join(y, by = c("id", "coluna")) -> base_shap

#Selecionando vari?veis Importantes. Extraidas do Feature Importance
base_shap %>% filter(coluna %in% c("idade","instrumento_PAF","instrumento_Ve?culo","instrumento_Perfurante",
                                   "local_obito_Rua.Estrada","instrumento_Impacto")) -> base_shap

rm(x,y)

#Summary plot.
base_shap %>%
  ggplot(aes(coluna,Shap_value)) +
  geom_jitter(data = base_shap %>% filter(coluna=="idade"), aes(color=Valor_Var)) +
  scale_color_viridis_c("Idade",option = "inferno",direction=-1) +
  ggnewscale::new_scale_color() +  
  geom_jitter(data = base_shap %>% filter(coluna!="idade"), aes(color=as.factor(Valor_Var))) + #Melhorar o jitter
  scale_colour_brewer("", type = "qual", palette = "Set1",labels =c("1"= "Sim","0"="N?o"))  +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(color="",x = "", y = "Shap Value")




# Calibra??o do modelo ---------------------------------------------------
https://www.tidyverse.org/blog/2022/11/model-calibration/
library(probably)

#Aqui vai olhar contra o test set. 
#As previs?es de classe e classe observada est?o em lista. Essa linha transforma lista em dataframe
as.data.frame(do.call(cbind,rf_test_fit$.predictions)) %>%
  cal_plot_breaks(intencao_homic, .pred_homic)

as.data.frame(do.call(cbind,rf_test_fit$.predictions)) %>%
  cal_plot_breaks(intencao_homic, .pred_homic, num_breaks = 50)
ggsave(filename ="Calibração do modelo.bmp",width = 7,height = 7,device='bmp', dpi=300)
ggsave(filename ="fig9.eps",width = 7,height = 7,device=cairo_ps, dpi=350)

as.data.frame(do.call(cbind,rf_test_fit$.predictions)) %>%
  cal_plot_windowed(intencao_homic, .pred_homic)

as.data.frame(do.call(cbind,rf_test_fit$.predictions)) %>%
  cal_plot_logistic(intencao_homic, .pred_homic)



# Optimal thresholds ------------------------------------------------------
#Varia??es no desempenho, dado varia??es no threshold. 
library(probably)
# https://machinelearningmastery.com/threshold-moving-for-imbalanced-classification/
# https://probably.tidymodels.org/reference/threshold_perf.html 
load("C:/Users/gabli/Desktop/r/SIM/Oculto/sim_doext_96_21_atlas_2023.RData")
threshold_perf(as.data.frame(do.call(cbind,rf_test_fit$.predictions)), intencao_homic, .pred_homic, thresholds = 0.6)


#optimal threshold em cada m?trica
threshold_perf(.data = as_tibble(do.call(cbind,rf_test_fit$.predictions)), truth = intencao_homic, 
               estimate = .pred_homic, thresholds = seq(0.1,0.95,by=0.01),
               yardstick::metric_set(accuracy,bal_accuracy,mcc,
                                     precision,sensitivity,specificity,f_meas,j_index)) %>% 
  group_by(.metric) %>% arrange(desc(.estimate)) %>% slice_max(order_by = .estimate, n=1) |> 
  #Retirando acur?cia repetida na base de teste (curto)
  distinct(.estimate, .keep_all = TRUE) -> mytable 
#rio::export(x=_,"threshold.xlsx")

mytable |> #filter(.metric %in% c("accuracy","bal_accuracy","mcc","f_meas","j_index")) |>
  select(.metric,.threshold,.estimate) |> mutate(.estimate = round(.estimate,2)) |>
  rename(optimal = .estimate) -> mytable

#Variações no desempenho, dado variações no threshold.  
threshold_perf(.data = as_tibble(do.call(cbind,rf_test_fit$.predictions)), truth = intencao_homic, 
               estimate = .pred_homic, thresholds = seq(0.1,0.95,by=0.05),
               yardstick::metric_set(accuracy,bal_accuracy,mcc,
                                     precision,sensitivity,specificity,
                                     f_meas,j_index)) |> #rio::export(x=_,"threshold.xlsx")
  ggplot() +
  geom_vline(xintercept = 0.5, linetype="dotted") +
  geom_line(aes(x=.threshold,y=.estimate,color=.metric)) +
  geom_point(aes(x=.threshold,y=.estimate,color=.metric)) +
  annotation_custom(gridExtra ::tableGrob(mytable, rows=NULL),xmin= 0.20,ymin = 0.65) +
  scale_x_continuous(breaks = seq(0.1,1,0.05)) + scale_y_continuous(breaks = seq(0.70,1,0.05)) +
  labs(color="",x="Threshold",y="Desempenho") +
  theme(legend.position = c(0.21,0.33),legend.key = element_blank(),
        legend.background = element_blank(),legend.direction = "vertical")

ggsave(filename ="Optimal thresholds.bmp",width = 8,height = 7,device='bmp', dpi=350)
ggsave(filename ="fig8.eps",width = 10,height = 10,device=cairo_ps, dpi=350)

rm(mytable)

#Variações no desempenho, dado Variações no threshold - UF
as_tibble(do.call(cbind,rf_test_fit$.predictions)) |> 
  mutate(id = row_number()) |>
  left_join(select(mutate(test_data,id = row_number()),uf_ocor,id),by = "id") |>
  group_by(uf_ocor) %>% #Agrupando para c?lculo por UF.
  threshold_perf(truth = intencao_homic, 
                 estimate = .pred_homic, thresholds = seq(0.5,0.95,by=0.05),
                 yardstick::metric_set(accuracy,bal_accuracy,mcc,
                                       precision,sensitivity,specificity,f_meas,j_index)) |> 
  ggplot() +
  geom_line(aes(x=.threshold,y=.estimate,color=.metric)) +
  facet_wrap(vars(uf_ocor),scales = "free") +  
  scale_x_continuous(labels = scales::label_percent()) +
  labs(color="",x="Threshold",y="Desempenho") +
  theme(legend.position = c(0.75,0.1),legend.background = element_blank(),
        axis.text.x = element_text(size=7),axis.text.y=element_text(size=7)) +
  guides(color = guide_legend(nrow  = 2))

ggsave(filename ="Optimal thresholds - UF.bmp",width = 11,height = 7,device='bmp', dpi=300)


# Equivocal zones ---------------------------------------------------------
#Acumulado de homic?dio oculto por threshold. 
library(probably)
#library(slider)
load("C:/Users/gabli/Desktop/r/SIM/Oculto/sim_doext_96_21_atlas_2023.RData")
#Acumulado de homic?dio oculto por threshold.    
#Quero considerando .5 como threshold tenho y ocultos. considerando 0.6 tenho y\2 ocultos.
homic_preds %>% select(.pred_class, .pred_homic) |>
  mutate(.pred_homic = round(.pred_homic,2)) |> filter(.pred_class == "homic") |>
  group_by(.pred_homic) |> 
  summarise(n = n()) |> 
  mutate(total = slider::slide_dbl(n,.f=sum,.after = Inf, .complete = TRUE)) |> 
  ggplot() +
  geom_line(aes(x=.pred_homic,y=total)) + #Linha
  #Pontos no gr?fico
  geom_point(data = . %>% filter(.pred_homic %in% seq(0.5, 1, 0.05)),aes(x =.pred_homic,y=total)) + #N?o sei o motivo do 0.85 n?o aparecer.
  #Adicionando os valores ao gr?fico
  ggrepel::geom_text_repel(data =. %>% filter(.pred_homic %in% seq(0.5, 1, 0.05)), 
                           aes(x=.pred_homic,y=total,label = total),size = 3.5,direction = "both") + #N?o sei o motivo do 0.85 n?o aparecer.
  scale_y_continuous(breaks = scales::breaks_extended(8),labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_continuous(breaks = seq(0.5, 1, 0.05),labels = scales::label_percent()) + labs(x ="Threshold", y="N? Homic?dios ocultos")

ggsave(filename ="Equivocal zones.bmp",width = 7,height = 5,device='bmp', dpi=300)


#Equivocal zones por UFs
homic_preds %>% select(.pred_class, .pred_homic,uf_ocor) |>
  mutate(.pred_homic = round(.pred_homic,2)) |> filter(.pred_class == "homic") |>
  group_by(uf_ocor,.pred_homic) |> 
  summarise(n = n()) |> 
  mutate(total = slider::slide_dbl(n,.f=sum,.after = Inf, .complete = TRUE)) |> 
  ggplot() +
  geom_line(aes(x=.pred_homic,y=total)) +
  facet_wrap(vars(uf_ocor),scales = "free") +
  #scale_y_continuous(breaks = scales::breaks_extended(8),labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(x ="Threshold", y="N? Homic?dios ocultos") +
  theme(axis.text.x = element_text(size=7),axis.text.y=element_text(size=7))

ggsave(filename ="Equivocal zones - UF.bmp",width = 10,height = 7,device='bmp', dpi=300)


# An?lise Threshold -------------------------------------------------------
library(tidyverse)
load("C:/Users/gabli/Desktop/r/SIM/Oculto/sim_doext_96_21_atlas_2023.RData")
#Empirical cumulative distribution function
homic_preds %>% select(.pred_class, .pred_homic, .pred_n_homic) |> #slice_sample(prop=0.01)|>
  ggplot() +
  stat_ecdf(aes(x=.pred_homic), pad = FALSE,geom = "point")  


#Quantidade de Homic?dio oculto por threshold
homic_preds %>% select(.pred_class, .pred_homic) |>
  mutate(.pred_homic = round(.pred_homic,2)) |> 
  group_by(.pred_homic) |> 
  count(.pred_class) |> filter(.pred_class == "homic") |> 
  ggplot() +
  geom_line(aes(x=.pred_homic,y=n,color=.pred_class)) +
  labs(color="")

#Fazer historiograma
homic_preds %>% select(.pred_class, .pred_homic) |> filter(.pred_class == "homic") |>
  ggplot() +
  geom_histogram(aes(x=.pred_homic))



# Cria??oo dos homic?dios ocultos sem cross validation ---------------------
#Importando base - Sem Indeterminados e outras intencionalidades
load("D:/Trabalhos/Hoculto/Bases/Microdados_SIM_1996_2020_label.RData")

#Tratando base importada.
glimpse(base_old)
base_old %>%
  filter(ano==2014 | ano==2015) %>%  
  #Retira o levels de anos n?o utilizados
  droplevels() -> base_old

#Logit
logit_spec <- logistic_reg() %>% set_engine("glm")

#Receitas
#receita logit e logit penalizado
lr_recipe <- recipe(intencao_homic ~ idade + sexo + racacor + estciv + esc + local_obito + instrumento + ano + mes + dia + uf_ocor,
                    data=base_old) %>%
  #Criando colunas bin?rias das datas
  #step_date(data, features = c("dow", "month", "year")) %>% #Ano n?o estava entrando como factor
  #step_rm(intencao) %>% #Retirando intencao da equa??o.
  #Imputa??o de idade e idade2
  step_impute_knn(c(idade),neighbors =5,impute_with = imp_vars(all_predictors())) %>%
  #Transforma factor em indicator variable(dummy)
  step_dummy(all_nominal_predictors()) %>%
  #Intera??es
  #step_interact(~starts_with("instrumento"):starts_with("local_obito") + starts_with("esc"):starts_with("sexo")) %>%
  step_zv(all_predictors())


#Workflow
lg_wflow <-
  workflow() %>%
  add_model(logit_spec) %>%
  add_recipe(lr_recipe)


#doParallel::registerDoParallel()
library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)
set.seed(77)


#Ajuste na base de treinamento
lg_fit <- fit(lg_wflow, base_old)


stopCluster(cl)
gc()


#Previs?o dos homic?dios ocultos
homic_preds <- predict(lg_fit, new_data = base_new)

#Previs?o de homic?dios ocultos
homic_preds <-  augment(lg_fit, base_new)
homic_preds %>% count(.pred_class)

###Tabela por munic?pio de resid?ncia
#Pegando os Indeterminados.
base_new %>% select(ano,codmunres) %>% group_by(ano,codmunres) %>%
  mutate(indeterminado = 1) %>% summarise(indeterminado = NROW(indeterminado)) -> base_oculto_mun
#Check valores
#base_oculto_mun %>% ungroup() %>% summarise(indeterminado = sum(indeterminado,na.rm = T))

#Pegando homicidios ocultos
homic_preds %>% select(ano,codmunres,.pred_class) %>%  group_by(ano,codmunres) %>% count(.pred_class) %>%
  pivot_wider(names_from = .pred_class,values_from = n) %>% select(!c("n_homic")) %>%
  rename(ppb2_homicidio = homic) %>%
  #Coloca 0 quando n?o tiver oculto.
  mutate(ppb2_homicidio = replace_na(ppb2_homicidio,0)) %>%
  #Jutando aos indeterminados
  left_join(base_oculto_mun,by = c("ano", "codmunres")) -> base_oculto_mun

#Check valores
#base_oculto_mun %>% ungroup() %>% summarise(indeterminado = sum(indeterminado,na.rm = T),oculto = sum(ppb2_homicidio,na.rm = T))

#Pegando homic?dios e criando H.Projetado e Propor??o.
base_old %>% select(ano,codmunres,intencao_homic) %>%
  group_by(ano,codmunres) %>% count(intencao_homic) %>% filter(intencao_homic == "homic") %>%
  select(!c(intencao_homic)) %>% rename(homicidio = n) %>%
  full_join(base_oculto_mun,by = c("ano", "codmunres")) %>% #Alguns munic?pios n?o registraram homic?dios - full_join
  mutate(homicidio_proj = homicidio + ppb2_homicidio,
         hom_oculto_prop = ppb2_homicidio/indeterminado) -> base_oculto_mun

##Aqui utilizar o pacote geobr para colocar os nomes dos munic?pios.
library(geobr)
munic = as.data.frame(read_municipality(code_muni="all", year=2020,showProgress=T)) %>%
  select(!c(geom,code_region,name_region)) -> munic
#Retirando o s?timo digito
munic %>% mutate(code_muni = substr(code_muni,1,6)) -> munic

#Retirando o s?timo digito dos ocultos e pegando informa??es sobre os munic?pios.
base_oculto_mun %>% mutate(
  codmunres = case_when(
    nchar(as.character(codmunres)) > 6 ~ substr(codmunres,1,6), TRUE ~ as.character(codmunres))) %>%
  left_join(munic, by = c("codmunres"="code_muni")) %>%
  #Coloca 0 nas colunas n?mericas com NA
  mutate(across(c(homicidio,hom_oculto_prop,ppb2_homicidio, indeterminado, homicidio_proj),
                .fns = ~replace_na(.x,0)),
         #Como Melhorar?
         #Colocando munic?pios Ignorados
         name_muni = case_when(codmunres == "110000" ~ "Munic?pio ignorado - RO",codmunres == "120000" ~ "Munic?pio ignorado - AC",
                               codmunres == "130000" ~ "Munic?pio ignorado - AM",codmunres == "140000" ~ "Munic?pio ignorado - RR",
                               codmunres == "150000" ~ "Munic?pio ignorado - PA",codmunres == "160000" ~ "Munic?pio ignorado - AP",
                               codmunres == "210000" ~ "Munic?pio ignorado - MA",codmunres == "220000" ~ "Munic?pio ignorado - PI",
                               codmunres == "170000" ~ "Munic?pio ignorado - TO",codmunres == "230000" ~ "Munic?pio ignorado - CE",
                               codmunres == "240000" ~ "Munic?pio ignorado - RN",codmunres == "250000" ~ "Munic?pio ignorado - PB",
                               codmunres == "260000" ~ "Munic?pio ignorado - PE",codmunres == "270000" ~ "Munic?pio ignorado - AL",
                               codmunres == "280000" ~ "Munic?pio ignorado - SE",codmunres == "290000" ~ "Munic?pio ignorado - BA",
                               codmunres == "310000" ~ "Munic?pio ignorado - MG",codmunres == "320000" ~ "Munic?pio ignorado - ES",
                               codmunres == "330000" ~ "Munic?pio ignorado - RJ",codmunres == "350000" ~ "Munic?pio ignorado - SP",
                               codmunres == "410000" ~ "Munic?pio ignorado - PR",codmunres == "420000" ~ "Munic?pio ignorado - SC",
                               codmunres == "430000" ~ "Munic?pio ignorado - RS",codmunres == "500000" ~ "Munic?pio ignorado - MS",
                               codmunres == "510000" ~ "Munic?pio ignorado - MT",
                               codmunres == "520000" ~ "Munic?pio ignorado - GO", TRUE ~ name_muni),
         #Colocando c?digo Estados de munic?pios Ignorados
         code_state = case_when(startsWith(codmunres,"12") & is.na(code_state) ~ 12, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"11") & is.na(code_state) ~ 11, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"27") & is.na(code_state) ~ 27, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"16") & is.na(code_state) ~ 16, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"13") & is.na(code_state) ~ 13, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"29") & is.na(code_state) ~ 29, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"23") & is.na(code_state) ~ 23, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"53") & is.na(code_state) ~ 53, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"32") & is.na(code_state) ~ 32, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"52") & is.na(code_state) ~ 52, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"21") & is.na(code_state) ~ 21, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"51") & is.na(code_state) ~ 51, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"50") & is.na(code_state) ~ 50, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"31") & is.na(code_state) ~ 31, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"15") & is.na(code_state) ~ 15, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"25") & is.na(code_state) ~ 25, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"41") & is.na(code_state) ~ 41, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"26") & is.na(code_state) ~ 26, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"22") & is.na(code_state) ~ 22, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"24") & is.na(code_state) ~ 24, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"43") & is.na(code_state) ~ 43, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"33") & is.na(code_state) ~ 33, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"11") & is.na(code_state) ~ 11, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"14") & is.na(code_state) ~ 14, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"42") & is.na(code_state) ~ 42, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"35") & is.na(code_state) ~ 35, TRUE ~ code_state),code_state = case_when(startsWith(codmunres,"28") & is.na(code_state) ~ 28, TRUE ~ code_state),
         code_state = case_when(startsWith(codmunres,"17") & is.na(code_state) ~ 17, TRUE ~ code_state),
         
         #Colocando nome de Estados de munic?pios Ignorados
         
         name_state = case_when(startsWith(codmunres,"12") & is.na(name_state) ~ "Acre", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"11") & is.na(name_state) ~ "Rond?nia", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"27") & is.na(name_state) ~ "Alagoas", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"16") & is.na(name_state) ~ "Amap?", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"13") & is.na(name_state) ~ "Amazonas", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"29") & is.na(name_state) ~ "Bahia", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"23") & is.na(name_state) ~ "Cear?", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"53") & is.na(name_state) ~ "Distrito Federal", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"32") & is.na(name_state) ~ "Esp?rito Santo", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"52") & is.na(name_state) ~ "Goi?s", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"21") & is.na(name_state) ~ "Maranh?o", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"51") & is.na(name_state) ~ "Mato Grosso", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"50") & is.na(name_state) ~ "Mato Grosso Sul", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"31") & is.na(name_state) ~ "Minas Gerais", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"15") & is.na(name_state) ~ "Par?", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"25") & is.na(name_state) ~ "Para?ba", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"41") & is.na(name_state) ~ "Paran?", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"26") & is.na(name_state) ~ "Pernambuco", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"22") & is.na(name_state) ~ "Piau?", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"24") & is.na(name_state) ~ "Rio Grande do Norte", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"43") & is.na(name_state) ~ "Rio Grande do Sul", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"33") & is.na(name_state) ~ "Rio de Janeiro", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"33") & is.na(name_state) ~ "Rio de Janeiro", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"14") & is.na(name_state) ~ "Roraima", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"42") & is.na(name_state) ~ "Santa Catarina", TRUE ~ name_state),name_state = case_when(startsWith(codmunres,"35") & is.na(name_state) ~ "S?o Paulo", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"28") & is.na(name_state) ~ "Sergipe", TRUE ~ name_state),
         name_state = case_when(startsWith(codmunres,"17") & is.na(name_state) ~ "Tocantins", TRUE ~ name_state)
         
         #Colocando Rio de Janeiros no munic?pios 33 e S?o Paulo nos 35. Isso cria v?rios munic?pios de mesmo nome
         name_muni = case_when(startsWith(codmunres,"33") & (ano == 1998 | ano == 1997 | ano == 1996)  ~ "Rio de Janeiro",
                               startsWith(codmunres,"35") & ano == 1998  ~ "S?o Paulo",TRUE ~ name_muni)) -> base_oculto_mun
#Precisa elimar munic?pios duplicados e preservar c?digo de RJ e SP





base_oculto_mun %>%
  filter(is.na(code_state)) -> x

sapply(base_oculto_mun, function(x) sum(is.na(x)))

rm(munic)

####Observa??es###
#Munic?pio 431453
# Por outro lado, o munic?ipio de Pinto
# Bandeira, c?odigo IBGE 431453, que tinha 2.673 habitantes em 2002, foi exclu?ido.
# Pinto Bandeira era um distrito do munic?ipio de Bento Gon?calves no estado do Rio
# Grande do Sul. Foi elevado `a categoria de munic?ipio em 2001, mas extinto por
# decis~ao do Supremo Tribunal Federal em 2002.




#Filtrando anos e check de valores.
base_oculto_mun %>% filter(ano == "2018" |  ano == "2019" | ano == "2020") %>%
  ungroup() %>% select(!c(codmunres)) %>%
  summarise(homicidio = sum(homicidio,na.rm = T),
            indeterminado = sum(indeterminado,na.rm = T))


#Filtra anos de interesse - Atlas do Campo
library(rio)
base_oculto_mun %>% filter(ano == "2018" |  ano == "2019" | ano == "2020") %>%
  export(.,"D:/Trabalhos/Hoculto/Bases/hoculto.xlsx")


#Verificando total por munic?pios
y %>% group_by(name_muni,ano) %>%
  summarise(homic = sum(homicidio)) %>%
  pivot_wider(names_from = ano,values_from = homic)

y %>% tabyl(name_muni,ano,homicidio)

#Exportando tabela do todos os munic?pios - Homic?dios
base_oculto_mun %>% ungroup() %>%  select(ano,codmunres,name_muni,homicidio) %>%
  pivot_wider(id_cols = c(codmunres,name_muni),
              names_from = ano,values_from = homicidio,values_fill = 0,
              values_fn = list(homicidio = sum)) %>% adorn_totals(where = c("row","col")) -> x


library(rio)
export(x,"munic_homic.xlsx")
