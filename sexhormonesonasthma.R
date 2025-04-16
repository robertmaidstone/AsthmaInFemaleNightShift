
source("load_data.R")
source("modelfunctions.R")
source("data_wrangling.R")
library(tidyverse)
library(officer)
library(flextable)
library(lubridate)

rm(ukb_data_processed)

# funcs -------------------------------------------------------------
ORmodelrun.hormone_group<-function(model_data,DependentVar,model_vec,model_names,hormone_group){
  model_vec %>% str_split(pattern ="[+]",n = 2) %>% unlist %>% .[c(2,4,6)] -> model_vec
  model_vec<-paste(DependentVar,"~",hormone_group,"+",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,hormone_group) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(",hormone_group,")) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(hormone_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:4],confint.default(mod3,2:4))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = hormone_group) %>% unlist %>% .[(1:3)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x=hormone_group,by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}
ORmodelrun.hormone_group2<-function(model_data,DependentVar,model_vec,model_names,hormone_group){
  model_vec %>% str_split(pattern ="[+]",n = 2) %>% unlist %>% .[c(2,4,6)] -> model_vec
  model_vec<-paste(DependentVar,"~",hormone_group,"+",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,hormone_group) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(",hormone_group,")) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq)  -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(hormone_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:5],confint.default(mod3,2:5))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = hormone_group) %>% unlist %>% .[(1:4)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x=hormone_group,by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}
trend.GRS<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  tab_1 <- c()
  str_replace(model_vec,"GRS_group","SCORE") -> model_vec
  
  
  model_list <-list()
  ptrend <- c()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(GRS_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    anova(mod3,test="Chisq")[2,5]->ptrend[i]
    
    exp(cbind(coef(mod3)[2],confint.default(mod3,2))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    
    tab_1<-rbind(tab_1,c(i,kk_temp$OR,summary(mod3)$coefficients[2,1:2]))
  }
  colnames(tab_1)<-c("model","OR","beta","SE")
  return(list(ptrend,tab_1))
}

##

model_data %>% filter(Sex==1) -> model_data_men
model_data %>% filter(Sex==0) -> model_data_women

model_data_men  %>%
  mutate(testosterone_group=ifelse(Testosterone<quantile(model_data_men$Testosterone,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                          ifelse(Testosterone<quantile(model_data_men$Testosterone,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                 ifelse(Testosterone<quantile(model_data_men$Testosterone,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                        ifelse(Testosterone>=quantile(model_data_men$Testosterone,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) %>%
  mutate(oestradiol_group=ifelse(Oestradiol<quantile(model_data_men$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                                   ifelse(Oestradiol<quantile(model_data_men$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                          ifelse(Oestradiol<quantile(model_data_men$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                                 ifelse(Oestradiol>=quantile(model_data_men$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) %>%
  mutate(shbg_group=ifelse(SHBG<quantile(model_data_men$SHBG,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                                   ifelse(SHBG<quantile(model_data_men$SHBG,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                          ifelse(SHBG<quantile(model_data_men$SHBG,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                                 ifelse(SHBG>=quantile(model_data_men$SHBG,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) -> model_data_men

model_data_women  %>%
  mutate(testosterone_group=ifelse(Testosterone<quantile(model_data_women$Testosterone,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                                   ifelse(Testosterone<quantile(model_data_women$Testosterone,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                          ifelse(Testosterone<quantile(model_data_women$Testosterone,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                                 ifelse(Testosterone>=quantile(model_data_women$Testosterone,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) %>%
  mutate(oestradiol_group=ifelse(Oestradiol<quantile(model_data_women$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                                 ifelse(Oestradiol<quantile(model_data_women$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                        ifelse(Oestradiol<quantile(model_data_women$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                               ifelse(Oestradiol>=quantile(model_data_women$Oestradiol,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) %>%
  mutate(shbg_group=ifelse(SHBG<quantile(model_data_women$SHBG,probs = c(.25,.5,.75),na.rm=T)[1],"q1",
                           ifelse(SHBG<quantile(model_data_women$SHBG,probs = c(.25,.5,.75),na.rm=T)[2],"q2",
                                  ifelse(SHBG<quantile(model_data_women$SHBG,probs = c(.25,.5,.75),na.rm=T)[3],"q3",
                                         ifelse(SHBG>=quantile(model_data_women$SHBG,probs = c(.25,.5,.75),na.rm=T)[3],"q4",NA))))) -> model_data_women


model_data_women %>% mutate(Oestradiol_group2=ifelse(is.na(oestradiol_group),oestradiolmissing==2,oestradiol_group))%>%
  mutate(Oestradiol_group2=ifelse(Oestradiol_group2==FALSE,NA,Oestradiol_group2)) %>%
  mutate(Oestradiol_group2=factor(Oestradiol_group2,levels=c("TRUE","q1","q2","q3","q4"
                                                             #,"FALSE"
                                                             ))) -> model_data_women 

model_data_men %>% mutate(Oestradiol_group2=ifelse(is.na(oestradiol_group),oestradiolmissing==2,oestradiol_group))%>%
  mutate(Oestradiol_group2=ifelse(Oestradiol_group2==FALSE,NA,Oestradiol_group2)) %>%
  mutate(Oestradiol_group2=factor(Oestradiol_group2,levels=c("TRUE","q1","q2","q3","q4"
                                                             #,"FALSE"
  ))) -> model_data_men 
save.image(file="hormonequartiledata.RData")

# running -----------------------------------------------------------------
tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(1,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}

model_vec<-c("JiNS + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS  + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS  + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired +Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates +  mediators (95% CI)")

model_data_men %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) -> model_data_men_temp
model_data_women %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                            (Asthma_def_ms==TRUE)) -> model_data_women_temp

DependentVar <- "Asthma_def_ms"
ORmodelrun.hormone_group(model_data_men_temp,DependentVar,model_vec,model_names,hormone_group="testosterone_group")[[1]] -> test_men
test_men %>% tf

#ORmodelrun.hormone_group(model_data_men_temp,DependentVar,model_vec,model_names,hormone_group="oestradiol_group") -> oes_men
ORmodelrun.hormone_group2(model_data_men_temp,DependentVar,model_vec,model_names,hormone_group="Oestradiol_group2") -> oes_men
oes_men[[1]] %>% tf

ORmodelrun.hormone_group(model_data_men_temp,DependentVar,model_vec,model_names,hormone_group="shbg_group") -> shbg_men
shbg_men[[1]] %>% tf

ORmodelrun.hormone_group(model_data_women_temp,DependentVar,model_vec,model_names,hormone_group="testosterone_group")[[1]] -> test_women
test_women %>% tf

#ORmodelrun.hormone_group(model_data_women_temp,DependentVar,model_vec,model_names,hormone_group="oestradiol_group") -> oes_women
oes_women[[1]] %>% tf
ORmodelrun.hormone_group2(model_data_women_temp,DependentVar,model_vec,model_names,hormone_group="Oestradiol_group2") -> oes_women
# oes_women[[1]] %>% tf

ORmodelrun.hormone_group(model_data_women_temp,DependentVar,model_vec,model_names,hormone_group="shbg_group") -> shbg_women
shbg_women[[1]] %>% tf


save(shbg_women,shbg_men,test_women,test_men,oes_women,oes_men,file = "hormone.RData")

###
trend.GRS<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  tab_1 <- c()
  str_replace(model_vec,"testosterone_group","testosterone") -> model_vec
  
  
  model_list <-list()
  ptrend <- c()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(GRS_group)) %>%
      filter(!is.na(DependentVar)) %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    anova(mod3,test="Chisq")[2,5]->ptrend[i]
    
    exp(cbind(coef(mod3)[2],confint.default(mod3,2))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    
    tab_1<-rbind(tab_1,c(i,kk_temp$OR,summary(mod3)$coefficients[2,1:2]))
  }
  colnames(tab_1)<-c("model","OR","beta","SE")
  return(list(ptrend,tab_1))
}
###

##

mod_name="Model 2"

rbind(
  test_women %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
  test_men %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(group=factor(group,levels=c("women","men")))-> plot_data

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pd_width <- 0.6

plot_data %>%
  #filter(Model==mod_name) %>%
  filter(group=="women") %>%
  ggplot(aes(y=OR,x=testosterone_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$testosterone_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  guides(colour="none") +
  ggtitle("A. Testosterone") -> plot_test_women

plot_data %>%
  #filter(Model==mod_name) %>%
  filter(group=="men") %>%
  ggplot(aes(y=OR,x=testosterone_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$testosterone_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  guides(colour="none") +
  ggtitle("") -> plot_test_men

###

rbind(
  oes_women[[1]] %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
  oes_men[[1]] %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(group=factor(group,levels=c("women","men")))-> plot_data

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pd_width <- 0.6

plot_data %>%
  filter(Model==mod_name) %>%
  filter(group=="women") %>%
  ggplot(aes(y=OR,x=oestradiol_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.75,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$oestradiol_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  guides(colour="none") +
  ggtitle("B. Oestradiol") -> plot_oes_women

plot_data %>%
  filter(Model==mod_name) %>%
  filter(group=="men") %>%
  ggplot(aes(y=OR,x=oestradiol_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.75,1.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$oestradiol_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  guides(colour="none") +
  ggtitle("") -> plot_oes_men

library(patchwork)
(plot_test_women + plot_oes_women) /
(plot_test_men + plot_oes_men)

##
rbind(
shbg_women[[1]] %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
shbg_men[[1]] %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
) %>%
  as_tibble %>% 
  separate(values,sep = "[ |-]",into=c("OR","LCI","UCI")) %>%
  mutate(LCI=as.numeric(str_remove(LCI,pattern = "\\("))) %>%
  mutate(UCI=as.numeric(str_remove(UCI,pattern = "\\)"))) %>%
  mutate(OR=as.numeric(OR)) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(group=factor(group,levels=c("women","men")))-> plot_data

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pd_width <- 0.6

plot_data %>%
  #filter(Model==mod_name) %>%
  filter(group=="women") %>%
  ggplot(aes(y=OR,x=shbg_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$shbg_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  ggtitle("SHBG-women") -> plot_shbg_women

plot_data %>%
  #filter(Model==mod_name) %>%
  filter(group=="men") %>%
  ggplot(aes(y=OR,x=shbg_group,colour=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.1, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.5,1.1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(plot_data$shbg_group)))+
  scale_colour_manual(#values = cbPalette[2:4],
    #values = c("transparent","transparent","black"),
    values = c(cbPalette[c(2,3)],"black"),
    name = element_blank(),guide = guide_legend(reverse=TRUE)) +
  ggtitle("SHBG-men") -> plot_shbg_men

library(patchwork)
plot_shbg_men + plot_shbg_women


#######################

model_vec<-c("JiNS + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS + Sex + Year_of_birth + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired +Smoking_n + Packyears_nn + BMI_o + SleepDur")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates +  mediators (95% CI)")

model_vec %>% strsplit(split = "[+ ]") %>% unlist -> mv
mv[mv!=""] %>% unique -> mv

sum_na <- function(x){sum(is.na(x))}
model_data %>% dplyr::select(JiNS,mv,WW="X2316.0.0","FEV1lt80","Asthma_def","Asthma_def_ms") %>% apply(2,sum_na)

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) -> model_data_temp
DependentVar <- "Asthma_def_ms"
model_data_temp %>% dplyr::select(JiNS,all_of(mv),DependentVar) -> md_small
md_small %>% filter((md_small %>% apply(1,sum_na))==0) -> md_small_filt
ORmodelrun_4shift(md_small_filt,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all

ORmodelrun_4shift(md_small_filt%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women

ORmodelrun_4shift(md_small_filt%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men
