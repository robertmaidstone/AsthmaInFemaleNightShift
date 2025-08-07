sex_plots <- function(table_men,table_women,title,model){
  rbind(
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

model_vec<-c("JiNS  + Year_of_birth",
             "JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o",
             "JiNS  + Year_of_birth + Ethnicity_o + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking_n + Packyears_nn + BMI_o + SleepDur + Sleep_med")
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

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_men

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
  #filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp
DependentVar <- "Asthma_def_ms"

DependentVar <- "COPD"

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7copd_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7copd_men

DependentVar <- "AllHypertension"

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7hyp_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7hyp_men

DependentVar <- "Pulmonary_Fibrosis"

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7pf_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7pf_men

DependentVar <- "Bronchiectasis"

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7bs_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7bs_men

DependentVar <- "Inter_lung_disease"

#ORmodelrun_4shift(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_all
ORmodelrun_4shift(model_data_temp%>% filter(Sex==0),DependentVar,model_vec,model_names)[[1]] -> tab7ils_women
ORmodelrun_4shift(model_data_temp%>% filter(Sex==1),DependentVar,model_vec,model_names)[[1]] -> tab7ils_men




sex_plots(tab7asthmadefms_men,tab7asthmadefms_women,"Asthma (ms)","Model 2")
sex_plots(tab7copd_men,tab7copd_women,"COPD","Model 2")+ylim(c(-1,5))
sex_plots(tab7hyp_men,tab7hyp_women,"Hypertension","Model 2")
sex_plots(tab7pf_men,tab7pf_women,"Pulmonary Fibrosis","Model 2")+ylim(c(-1,12))
sex_plots(tab7bs_men,tab7bs_women,"Bronchiectasis","Model 2")+ylim(c(-1,12))
sex_plots(tab7ils_men,tab7ils_women,"ILS","Model 2")+ylim(c(-1,12))


library(patchwork)

sex_plots(tab7asthmadefms_men,tab7asthmadefms_women,"Asthma (ms)","Model 2") +
sex_plots(tab7copd_men,tab7copd_women,"COPD","Model 2")+ylim(c(-1,5)) +
sex_plots(tab7hyp_men,tab7hyp_women,"Hypertension","Model 2")
