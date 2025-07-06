# Individualizando as deficiências principais -----------------------------
sinan %>% mutate( 
  tipodef = case_when(
    #Precisa ser def_fisica == 1 e def_visual!="Sim" e não pode ter missing, pois quero ter certeza que estou individualizando a deificência.
    #Somente deficiência física. 
    def_fisica == "Sim" & ((def_visual== "Não" | def_visual=="Não se aplica") & (def_mental== "Não"| def_mental=="Não se aplica") & (def_auditi== "Não" | def_auditi=="Não se aplica")) ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & ((def_visual== "Não" | def_visual=="Não se aplica") & (def_fisica== "Não" | def_fisica=="Não se aplica") & (def_auditi== "Não" | def_auditi=="Não se aplica")) ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & ((def_fisica== "Não" | def_fisica=="Não se aplica") & (def_mental== "Não" | def_mental=="Não se aplica") & (def_auditi== "Não" | def_auditi=="Não se aplica")) ~ "Visual",
    #Somente deficiência auditiva
    def_auditi == "Sim" & ((def_fisica== "Não" | def_fisica=="Não se aplica") & (def_mental== "Não" | def_mental=="Não se aplica") & (def_visual== "Não" | def_visual=="Não se aplica")) ~ "Auditiva"), 
  #Pessoa com múltiplas deficiênciais
  tipodef = replace(tipodef,def_fisica =="Sim" & (def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_mental =="Sim" & (def_visual=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_visual =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = as.factor(replace(tipodef,def_auditi =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_visual=="Sim"),"Múltipla"))) -> x



x %>% tabyl(ano_not,tipodef, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col"))


sinan %>% mutate( 
  tipodef = case_when(
    #Precisa ser def_fisica == 1 e def_visual!="Sim" e não pode ter missing, pois quero ter certeza que estou individualizando a deificência.
    #Somente deficiência física. 
    def_fisica == "Sim" & ((def_visual!= "Sim" & def_visual!="Missing" & def_visual!="Ignorado") & (def_mental!= "Sim"& def_mental!="Missing" & def_mental!= "Ignorado") & 
                          (def_auditi!= "Sim" & def_auditi!="Missing" & def_auditi!= "Ignorado")) ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & ((def_visual!= "Sim"& def_visual!="Missing" & def_visual!="Ignorado") & (def_fisica!= "Sim" & def_fisica!="Missing" & def_fisica!= "Ignorado") & 
                          (def_auditi!= "Sim" & def_auditi!="Missing" & def_auditi!="Ignorado")) ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & ((def_fisica!= "Sim" & def_fisica!="Missing" &  def_fisica!="Ignorado") & (def_mental!= "Sim" & def_mental!="Missing" & def_mental!= "Ignorado") & 
                          (def_auditi!= "Sim"  & def_auditi!="Missing" & def_auditi!= "Ignorado")) ~ "Visual",
    #Somente deficiência auditiva
    def_auditi == "Sim" & ((def_fisica!= "Sim" & def_fisica!="Missing" &  def_fisica!="Ignorado") & (def_mental!= "Sim" & def_mental!="Missing" & def_mental!="Ignorado") & 
                         (def_visual!= "Sim" & def_visual!="Missing" & def_visual!="Ignorado")) ~ "Auditiva"), 
  #Pessoa com múltiplas deficiênciais
  tipodef = replace(tipodef,def_fisica =="Sim" & (def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_mental =="Sim" & (def_visual=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_visual =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = as.factor(replace(tipodef,def_auditi =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_visual=="Sim"),"Múltipla"))) -> y



y %>% tabyl(ano_not,tipodef, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 




###ORiginal da rotina do SINAN
sinan %>% mutate( 
  tipodef = case_when(
    #Precisa ser def_fisica == 1 e def_visual!="Sim" e não pode ter missing, pois quero ter certeza que estou individualizando a deificência.
    #Somente deficiência física. 
    def_fisica == "Sim" & ((def_visual== "Não") & (def_mental== "Não" ) & (def_auditi== "Não")) ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & ((def_visual== "Não") & (def_fisica== "Não") & (def_auditi== "Não")) ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & ((def_fisica== "Não") & (def_mental== "Não") & (def_auditi== "Não")) ~ "Visual",
    #Somente deficiência auditiva
    def_auditi == "Sim" & ((def_fisica== "Não") & (def_mental== "Não") & (def_visual== "Não")) ~ "Auditiva"), 
  #Pessoa com múltiplas deficiênciais
  tipodef = replace(tipodef,def_fisica =="Sim" & (def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_mental =="Sim" & (def_visual=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_visual =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = as.factor(replace(tipodef,def_auditi =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_visual=="Sim"),"Múltipla"))) -> z


z %>% tabyl(ano_not,tipodef, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 





###ORiginal da rotina do SINAN
sinan %>% mutate( 
  tipodef = case_when(
    #Precisa ser def_fisica == 1 e def_visual!="Sim" e não pode ter missing, pois quero ter certeza que estou individualizando a deificência.
    #Somente deficiência física. 
    def_fisica == "Sim" & ((def_visual!= "Sim" & def_visual!="Missing") & (def_mental!= "Sim"& def_mental!="Missing") & (def_auditi!= "Sim" & def_auditi!="Missing")) ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & ((def_visual!= "Sim"& def_visual!="Missing") & (def_fisica!= "Sim" & def_fisica!="Missing") & (def_auditi!= "Sim" & def_auditi!="Missing")) ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & ((def_fisica!= "Sim" & def_fisica!="Missing") & (def_mental!= "Sim" & def_mental!="Missing") & (def_auditi!= "Sim"& def_auditi!="Missing")) ~ "Visual",
    #Somente deficiência auditiva
    def_auditi == "Sim" & ((def_fisica!= "Sim" & def_fisica!="Missing") & (def_mental!= "Sim" & def_mental!="Missing") & (def_visual!= "Sim" & def_visual!="Missing")) ~ "Auditiva"), 
  #Pessoa com múltiplas deficiênciais
  tipodef = replace(tipodef,def_fisica =="Sim" & (def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_mental =="Sim" & (def_visual=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_visual =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = as.factor(replace(tipodef,def_auditi =="Sim" & (def_mental=="Sim" | def_fisica=="Sim" | def_visual=="Sim"),"Múltipla"))) -> sinan









