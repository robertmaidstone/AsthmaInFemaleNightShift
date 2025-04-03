library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
library(lmtest)

source("load_data.R")
source("modelfunctions.R")
source("data_wrangling.R")

rm(ukb_data_processed)
tf<-function(table_input){
  table_input %>% mutate(samplesize=`FALSE`+`TRUE`,trueprop=round(`TRUE`/samplesize*100,2)) %>% 
    dplyr::select(JiNS,`TRUE`,trueprop,samplesize,everything()) %>% dplyr::select(-`FALSE`) %>% t
}
sex_plots <- function(table_all,table_men,table_women,title,model){
  rbind(
    table_all %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="all"),
    table_women %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="women"),
    table_men %>% pivot_longer(cols=4:6,names_to = "Model",values_to = "values") %>% dplyr::select(-`FALSE`,-`TRUE`) %>% mutate(group="men")
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
    mutate(group=factor(group,levels=c("women","men","all"))) %>%
    mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Irregular shift work","Always"),labels=c("Day workers","Shift work, but never\nor rarely nights","Irregular shift work\nincluding nights","Permanent night\nshift work"))) -> plot_data
  
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette <- c("red","black","red")
  pd_width <- 0.6
  
  plot_data %>%
    filter(group!="all") %>%
    filter(Model==model) %>%
    ggplot(aes(y=OR,x=JiNS,colour=group)) + 
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.position=c(0.8, 0.85),
          legend.background = element_blank())+
    ylab("Odds Ratio") +
    ylim(c(.6,2)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(plot_data$JiNS)))+
    scale_colour_manual(#values = cbPalette[2:4],
      #values = c("transparent","transparent","black"),
      values = c(cbPalette[c(2,3)],"black"),
      name = element_blank(),guide = guide_legend(reverse=TRUE)) +
    ggtitle(title) -> plot_ms2
  
  return(plot_ms2)
}

# JiNS -----------------------------------------------------------------

model_vec<-c("JiNS  + Year_of_birth",
             "JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur")
model_names <- c("Model 1: Age adjusted.",
                 #"Model 2: Adjusted by age, smoking status, pack years, alcohol status, daily alcohol intake, ethnicity, TDI, days exercised (walked, moderate, vigorous), chronotype, length of working week, job asthma risk, job medical required.",
                 "Model 2: Adjusted by multivariate covariates.",
                 "Model 3: Model 2 covariates + mediators")

model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  # filter(!is.na(Year_of_birth)) %>%
  # filter(!is.na(Ethnicity)) %>%
  # filter(!is.na(TDI)) %>%
  # filter(!is.na(SleepDur)) %>%
  # filter(!is.na(Smoking)) %>%
   filter(!is.na(Packyears_nn)) %>%
  # filter(!is.na(Alc_daily)) %>%
  # filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men


model_data  %>%
   filter(!is.na(JiNS_o)) %>%
   filter(!is.na(Sex)) %>%
  # filter(!is.na(Year_of_birth)) %>%
  # filter(!is.na(Ethnicity)) %>%
  # filter(!is.na(TDI)) %>%
  # filter(!is.na(SleepDur)) %>%
  # filter(!is.na(Smoking)) %>%
  filter(!is.na(Packyears_nn)) %>%
  # filter(!is.na(Alc_daily)) %>%
  # filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  #filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> asthma_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> asthma_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> asthma_men

sex_plots(asthma_all,asthma_men,asthma_women,"Asthma All","Model 1")

model_data  %>%
  # filter(!is.na(JiNS)) %>%
  # filter(!is.na(Sex)) %>%
  # filter(!is.na(Year_of_birth)) %>%
  # filter(!is.na(Ethnicity)) %>%
  # filter(!is.na(TDI)) %>%
  # filter(!is.na(SleepDur)) %>%
  # filter(!is.na(Smoking)) %>%
  filter(!is.na(Packyears_nn)) %>%
  # filter(!is.na(Alc_daily)) %>%
  # filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter(X2316.0.0 %in% c(0,1)) -> model_data_temp

DependentVar <- "X2316.0.0"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] %>% dplyr::rename('FALSE'=`0`,'TRUE'=`1`)-> WW_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]]%>% dplyr::rename('FALSE'=`0`,'TRUE'=`1`) -> WW_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]]%>% dplyr::rename('FALSE'=`0`,'TRUE'=`1`) -> WW_men

sex_plots(WW_all,WW_men,WW_women,"Wheeze Whistling","Model 2")

model_data  %>%
  # filter(!is.na(JiNS)) %>%
  # filter(!is.na(Sex)) %>%
  # filter(!is.na(Year_of_birth)) %>%
  # filter(!is.na(Ethnicity)) %>%
  # filter(!is.na(TDI)) %>%
  # filter(!is.na(SleepDur)) %>%
  # filter(!is.na(Smoking)) %>%
  filter(!is.na(Packyears_nn)) %>%
  # filter(!is.na(Alc_daily)) %>%
  # filter(!is.na(LengthofWW)) %>%
  # filter(!is.na(BMI)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter(FEV1lt80 %in% c(0,1)) -> model_data_temp

DependentVar <- "FEV1lt80"

ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> fev_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> fev_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> fev_men

sex_plots(fev_all,fev_men,fev_women,"FeV 1","Model 2")

library(patchwork)

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"A.","Model 2")+annotate("text", x=4.3, y=.75, label= "p = 0.01")  + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma") + guides(colour="none") +
sex_plots(asthma_all,asthma_men,asthma_women,"B.","Model 2") +annotate("text", x=4.3, y=.75, label= "p = 0.01") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of any asthma") +
  sex_plots(WW_all,WW_men,WW_women,"C.","Model 2") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.01") +ylab("Adjusted odds ratio of experiencing\nwheeze or whistling in chest")+ guides(colour="none") +
  sex_plots(fev_all,fev_men,fev_women,"D.","Model 2") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.61") +ylab("Adjusted odds ratio of having critical\nFEV1 predicted percentage")+ guides(colour="none") -> p_fig1

ggsave(plot= p_fig1,filename="plots/fig1.png",width=8,height=6.5)


# newplotsforchest --------------------------------------------------------

sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"A.","Model 2")+annotate("text", x=4.3, y=.75, label= "p = 0.01")  + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma") + guides(colour="none") +
  sex_plots(tab7asthmadefms_all,tab7asthmadefms_men,tab7asthmadefms_women,"","Model 3")+annotate("text", x=4.3, y=.75, label= "p = 0.01")  + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of moderate-\nsevere asthma") +
  sex_plots(asthma_all,asthma_men,asthma_women,"B.","Model 2") +annotate("text", x=4.3, y=.75, label= "p = 0.01") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of any asthma")+ guides(colour="none")+
    sex_plots(asthma_all,asthma_men,asthma_women,"","Model 3") +annotate("text", x=4.3, y=.75, label= "p = 0.01") + theme(plot.title.position = "plot")+ylab("Adjusted odds ratio of any asthma")+ guides(colour="none")-> p_fig1

ggsave(plot= p_fig1,filename="plots/fig1n.png",width=8,height=6.5)

sex_plots(WW_all,WW_men,WW_women,"A.","Model 2") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.01") +ylab("Adjusted odds ratio of experiencing\nwheeze or whistling in chest")+ guides(colour="none") +
  sex_plots(WW_all,WW_men,WW_women,"","Model 3") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.11") +ylab("Adjusted odds ratio of experiencing\nwheeze or whistling in chest")+ 
  sex_plots(fev_all,fev_men,fev_women,"B.","Model 2") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.61") +ylab("Adjusted odds ratio of having critical\nFEV1 predicted percentage")+ guides(colour="none") +
  sex_plots(fev_all,fev_men,fev_women,"","Model 3") + theme(plot.title.position = "plot")+annotate("text", x=4.3, y=.75, label= "p = 0.85") +ylab("Adjusted odds ratio of having critical\nFEV1 predicted percentage")+ guides(colour="none") -> p_fig1

ggsave(plot= p_fig1,filename="plots/fig2n.png",width=8,height=6.5)

###

fev_men %>% mutate(n=`FALSE`+`TRUE`) %>% 
  mutate(p=round(`TRUE`/n*100,2)) %>% 
  mutate(case=paste0(`TRUE`," (",p,"%)")) %>%
  dplyr::select(JiNS,case,n,everything()) %>% dplyr::select(-`FALSE`,-`TRUE`,-p) %>% t -> tab_m
fev_women %>% mutate(n=`FALSE`+`TRUE`) %>% 
  mutate(p=round(`TRUE`/n*100,2)) %>% 
  mutate(case=paste0(`TRUE`," (",p,"%)")) %>%
  dplyr::select(JiNS,case,n,everything()) %>% dplyr::select(-`FALSE`,-`TRUE`,-p) %>% t -> tab_w


###
 model_vec<-c("JiNS + Sex + Year_of_birth  + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype_o + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired",
              "JiNS + JiNS*Sex + Sex + Year_of_birth  + Alcohol + Ethnicity_o + TDI + DaysWalked + DaysModerate + DaysVigorous + Alcintake + Chronotype_o + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired")
 model_names <- c("No Int",
                  "Int")
 model_vec<-c("JiNS  + Sex + Year_of_birth",
              "JiNS  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
              "JiNS  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur",
              "JiNS + JiNS*Sex  + Sex + Year_of_birth",
              "JiNS + JiNS*Sex  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
              "JiNS + JiNS*Sex  + Sex + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur")
 model_names <- c("Model 1",
                  "Model 2",
                  "Model 3",
                  "Model 1 int",
                  "Model 2 int",
                  "Model 3 int")
 
 model_data  %>%
   filter(!is.na(Packyears_nn)) %>%
   mutate(JiNS=JiNS_o) %>%
   filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE))-> model_data_temp
 DependentVar <- "Asthma_def_ms"
 
 ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
 lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
 lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
 lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3
 
 model_data  %>%
   filter(!is.na(Packyears_nn)) %>%
   mutate(JiNS=JiNS_o) %>%
   filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def==TRUE))-> model_data_temp
 DependentVar <- "Asthma_def"
 
 ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
 lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
 lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
 lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3
 
 
 model_data  %>%
   filter(!is.na(Packyears_nn)) %>%
   mutate(JiNS=JiNS_o) %>%
   filter(FEV1lt80 %in% c(0,1)) -> model_data_temp
 
 DependentVar <- "FEV1lt80"
 
 ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
 lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
 lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
 lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3
 
 model_data  %>%
   filter(!is.na(Packyears_nn)) %>%
   mutate(JiNS=JiNS_o) %>%
   filter(X2316.0.0 %in% c(0,1)) -> model_data_temp
 
 DependentVar <- "X2316.0.0"
 
 ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names) -> temp
 lrtest(temp[[2]][[4]],temp[[2]][[1]]) #interaction model 1
 lrtest(temp[[2]][[5]],temp[[2]][[2]]) #interaction model 2
 lrtest(temp[[2]][[6]],temp[[2]][[3]]) #interaction model 3
 