# ORmodelrun function -----------------------------------------------------

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

ORmodelrun_chrono<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(all_of(DependentVar),Chronotype) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(Chronotype)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq) %>% 
    filter(Chronotype != "Do not know")%>% 
    filter(Chronotype != "Prefer not to answer") -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(Chronotype)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(Chronotype != "Do not know")%>%
      filter(Chronotype != "Prefer not to answer") %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:3],confint.default(mod3,2:3))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "Chronotype") %>% unlist %>% .[(1:2)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="Chronotype",by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

ORmodelrun_shift<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(all_of(DependentVar),JiNS) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(JiNS)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq) %>% 
    filter(JiNS != "Prefer not to answer")%>% 
    filter(JiNS != "Do not know") -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(JiNS != "Do not know")%>%
      filter(JiNS != "Prefer not to answer") %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:3],confint.default(mod3,2:3))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "JiNS") %>% unlist %>% .[(1:2)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="JiNS",by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

ORmodelrun_4shift<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,JiNS) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(JiNS)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq) %>% 
    filter(JiNS != "Prefer not to answer")%>% 
    filter(JiNS != "Do not know") -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(JiNS != "Do not know")%>%
      filter(JiNS != "Prefer not to answer") %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:4],confint.default(mod3,2:4))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "JiNS") %>% unlist %>% .[(1:3)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="JiNS",by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

MeansLMmodelrun_shift<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(JiNS) %>% table(useNA = "always") %>%
    as.data.frame() %>% dplyr::rename(Variable='.') %>% 
    filter(!is.na(Variable)) %>%
    filter(Variable != "Prefer not to answer")%>% 
    filter(Variable != "Do not know") %>%
    mutate(Variable=paste0("JiNS",Variable)) -> tab_1
  
 # tab_1 <- tibble(Variable=c("JiNSNo shift work","JiNSIrregular shift work","JiNSAlways"))
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(!(DependentVar %in% c(-1,-3))) %>%
      filter(JiNS != "Do not know")%>%
      filter(JiNS != "Prefer not to answer") %>%
      glm(data = .,modeli,family = gaussian) -> modi
    
    model_list[[i]] <- modi
    model_name_i <- model_names[i]
    summary(modi)$coef %>% as_tibble(rownames = "Variable") %>% 
      mutate(sig=ifelse(`Pr(>|t|)`<0.001,"***",ifelse(`Pr(>|t|)`<0.01,"**",ifelse(`Pr(>|t|)`<0.05,"*","")))) %>%
      mutate("{model_name_i}":=paste0(round(Estimate,2)," (",round(`Std. Error`,2),")",sig)) %>%
      filter(Variable %in% c("JiNSNo shift work","JiNSIrregular shift work","JiNSAlways")) %>%
      dplyr::select(Variable,all_of(model_name_i)) %>% 
      merge(tab_1,.,by="Variable",all.x=TRUE) -> tab_1
    
    
  }
  return(list(tab_1,model_list))
}

MeansLMmodelrun_chrono<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(Chronotype) %>% table(useNA = "always") %>%
    as.data.frame() %>% dplyr::rename(Variable='.') %>% 
    filter(!is.na(Variable)) %>%
    filter(Variable != "Prefer not to answer")%>% 
    filter(Variable != "Do not know") %>%
    mutate(Variable=paste0("Chronotype",Variable)) -> tab_1
  
  #tab_1 <- tibble(Variable=c("ChronotypeDefinitely a morning person","ChronotypeDefinitely an evening person"))
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(!(DependentVar %in% c(-1,-3))) %>%
      filter(Chronotype != "Do not know")%>%
      filter(Chronotype != "Prefer not to answer") %>%
      glm(data = .,modeli,family = gaussian) -> modi
    
    model_list[[i]] <- modi
    model_name_i <- model_names[i]
    summary(modi)$coef %>% as_tibble(rownames = "Variable") %>% 
      mutate(sig=ifelse(`Pr(>|t|)`<0.001,"***",ifelse(`Pr(>|t|)`<0.01,"**",ifelse(`Pr(>|t|)`<0.05,"*","")))) %>%
      mutate("{model_name_i}":=paste0(round(Estimate,2)," (",round(`Std. Error`,2),")",sig)) %>%
      filter(Variable %in% c("ChronotypeDefinitely a morning person","ChronotypeDefinitely an evening person")) %>%
      dplyr::select(Variable,all_of(model_name_i)) %>% 
      merge(tab_1,.,by="Variable",all.x=TRUE) -> tab_1
    
    
  }
  return(list(tab_1,model_list))
}

MeansLMmodelrun_4shift<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(JiNS) %>% table(useNA = "always") %>%
    as.data.frame() %>% dplyr::rename(Variable='.') %>% 
    filter(!is.na(Variable)) %>%
    filter(Variable != "Prefer not to answer")%>% 
    filter(Variable != "Do not know") %>%
    mutate(Variable=paste0("JiNS",Variable)) -> tab_1
  
  # tab_1 <- tibble(Variable=c("JiNSNo shift work","JiNSIrregular shift work","JiNSAlways"))
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(!(DependentVar %in% c(-1,-3))) %>%
      filter(JiNS != "Do not know")%>%
      filter(JiNS != "Prefer not to answer") %>%
      glm(data = .,modeli,family = gaussian) -> modi
    
    model_list[[i]] <- modi
    model_name_i <- model_names[i]
    summary(modi)$coef %>% as_tibble(rownames = "Variable") %>% 
      mutate(sig=ifelse(`Pr(>|t|)`<0.001,"***",ifelse(`Pr(>|t|)`<0.01,"**",ifelse(`Pr(>|t|)`<0.05,"*","")))) %>%
      mutate("{model_name_i}":=paste0(round(Estimate,2)," (",round(`Std. Error`,2),")",sig)) %>%
      filter(Variable %in% c("JiNSNo shift work","JiNSNever/rarely","JiNSIrregular shift work","JiNSAlways")) %>%
      dplyr::select(Variable,all_of(model_name_i)) %>% 
      merge(tab_1,.,by="Variable",all.x=TRUE) -> tab_1
    
    
  }
  return(list(tab_1,model_list))
}
