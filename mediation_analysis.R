

library(tidyverse)
library(officer)
library(flextable)
library(lubridate)
library(mediation)

source("load_data.R")
source("data_wrangling.R")


# medication analysis -----------------------------------------------------
# potential mediators Smoking_n , Packyears_nn , BMI_o , SleepDur ####
model_data  %>%
  filter(!is.na(JiNS)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(Packyears_nn)) %>%
  mutate(JiNS=JiNS_o) %>%
  filter((Asthma_med_all == FALSE&Asthma2==FALSE) | (Asthma_def_ms==TRUE)) %>%
  as_tibble -> model_data_temp



mod1 <- lm(BMI_o ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp%>% filter(Sex==0))
mod2 <- glm(Asthma_def_ms ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + BMI_o,model_data_temp%>% filter(Sex==0),family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS_o",mediator = "BMI_o",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_bmi_women <- results

mod1 <- lm(Packyears_nn ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp%>% filter(Sex==0))
mod2 <- glm(Asthma_def_ms ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Packyears_nn,model_data_temp%>% filter(Sex==0),family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS_o",mediator = "Packyears_nn",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_py_women <- results


mod1 <- lm(SleepDur_o ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp%>% filter(Sex==0))
mod2 <- glm(Asthma_def_ms ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + SleepDur_o,model_data_temp%>% filter(Sex==0),family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS_o",mediator = "SleepDur_o",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_sleep_women <- results


model_data_temp %>% mutate(Smoking2=ifelse((Smoking_n=="current heavy smoker")|(Smoking_n=="current occassional smoker, previously a heavy smoker")|(Smoking_n=="not a current smoker, smoked heavily previously"),1,0)) -> model_data_temp2
mod1 <- lm(Smoking2 ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp2%>% filter(Sex==0))
mod2 <- glm(Asthma_def_ms ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Smoking2,model_data_temp2%>% filter(Sex==0),family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS_o",mediator = "Smoking2",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_smoking_women <- results

model_data_temp %>% mutate(Sleep_med=as.numeric(Sleep_med)) -> model_data_temp2
mod1 <- glm(Sleep_med ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o,model_data_temp2%>% filter(Sex==0),family = binomial(link="logit"))
mod2 <- glm(Asthma_def_ms ~  JiNS_o  + Year_of_birth + Ethnicity + TDI + Alcohol + Alcintake + DaysWalked + DaysModerate + DaysVigorous  + LengthofWW_o + Job_AsthmaRisk + Job_MedRequired + Chronotype_o + Sleep_med,model_data_temp2%>% filter(Sex==0),family = binomial(link="logit"))
results <- mediate(mod1,mod2, treat = "JiNS_o",mediator = "Sleep_med",boot=T,sims=100,control.value = "No shift work", treat.value = "Always")
results %>% summary

results_sleepmed_women <- results

save(file = "data/mediationresults2.RData",results_bmi_women,results_py_women,results_sleep_women)
