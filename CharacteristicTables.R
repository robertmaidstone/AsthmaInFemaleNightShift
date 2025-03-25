library(lubridate)
library(flextable)
library(officer)
library(MASS)
library(lmtest)
library(nnet)

source("load_data.R")

rd <- 2 # digits to round to
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

source("data_wrangling.R")

model_data  %>%
  filter(!is.na(JiNS)) %>% 
  filter(!is.na(Sex)) %>% 
  filter(!is.na(Year_of_birth)) %>% 
  filter(!is.na(Ethnicity)) %>%
  filter(!is.na(TDI)) %>% 
  filter(!is.na(SleepDur)) %>% 
  #filter(!is.na(Smoking)) %>%
  #filter(!is.na(Packyears)) %>%
  filter(!is.na(Alc_daily)) %>% 
  mutate(Packyears=ifelse(Smoking==0,0,Packyears)) %>%
  filter(!is.na(LengthofWW))  -> model_data_temp

dim(model_data_temp)[1]

cbat <- function(TT,SHBG,ALB = 43){
  Kalb <- 3.6*10^4
  Kshbg <- 10^9
  N <- 1 + Kalb*ALB/69000
  a <- N*Kshbg
  b <- N + Kshbg*(SHBG - TT)/10^9
  c <- -TT/10^9
  FT <- (-b + sqrt(b^2 - 4*a*c))/(2*a)*10^9
  cbat <- N*FT
  # return(list(free.T = FT, cbat = cbat))
  return(FT)
}
cbat_v <- Vectorize(cbat)

(model_data_temp %>% dplyr::select(Age,Sex,BMI=BMI_o, 'Smoking Status'=Smoking, 'Pack-years'=Packyears, 'Drink alcohol daily'=Alcintake,
                                   'Monthly units of alcohol consumed'=Monthly_units, 'Sleep Duration'=SleepDur, Chronotype,Ethnicity,
                                   'Weekly work hours'=LengthofWW, 'Single Occupancy'=NuminHouse,
                                   'Urban area'=UrbanRural,TDI,'Maternal Smoking'=MatSmoking,
                                   'Breastfed as baby'=Breastfed, 'Birth weight'=Birthweight,
                                   'High cholesterol'=High_Cholesterol,'Type I Diabetes'=DiabetesI,'Type II Diabetes'=DiabetesII,
                                   'Hypertension'=AllHypertension,Depression=AllDepression,'Cardiovascular Disease'=CardiovascularDisease) %>%
  apply(2,function(x){sum(is.na(x))})) -> na_values

na_values/dim(model_data_temp)[1] * 100 -> na_values_pct

tibble(Names=names(na_values),'Missing Values'=as.numeric(na_values),'Percentage of total data' = round(as.numeric(na_values_pct),2)) %>%
  dplyr::filter(`Missing Values` != 0) -> tab_supp

(model_data_temp %>% dplyr::select(Sex,BMI=BMI_o, 'Birth weight'=Birthweight,'High cholesterol'=High_Cholesterol,
                                   'Sleep Apnoea'= Sleep_Apnoea, COPD, 'Emphysema/Chronic Bronchitis'=Emp_Chron_Bronchitis,
                                   Bronchiectasis, 'Interstitial Lung Disease'=Inter_lung_disease,
                                   'Other Respiratory Problems'=Other_resp_probs,'Gastro-Oesophageal Reflux'=GastroOesReflux, 
                                   'Type 1 Diabetes'=DiabetesI,'Type 2 Diabetes'=DiabetesII,
                                   'Hypertension'=AllHypertension,Depression=AllDepression,'Cardiovascular Disease'=CardiovascularDisease,
                                   #'Eosinophil Count'=Eosinophil,
                                   'Hayfever/Allergic Rhinitis/Allergy to house dust mite'=HayfeverRhinitisHouseDustMite,
                                   Eczema, 'Allergy or Anaphylactic reaction to food/drug'=Allergy,
                                   'Anxiety/panic attacks'=AnxietyPanic) %>%
    apply(2,function(x){sum(is.na(x))})) -> na_values

na_values/dim(model_data_temp)[1] * 100 -> na_values_pct

tibble(Names=names(na_values),'Missing Values'=as.numeric(na_values),'Percentage of total data' = round(as.numeric(na_values_pct),2)) %>%
  dplyr::filter(`Missing Values` != 0) -> tab_supp

model_data_temp %>% mutate(FT=cbat_v(Testosterone,SHBG)) %>% mutate(FAI=Testosterone/SHBG*100) -> model_data_temp

SD_table_function <- function(model_data_temp){
model_data_temp %>% 
  filter(!(JiNS_o %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  group_by(JiNS_o) %>%
  dplyr::summarise(N=n(),
                   `Age (years)`=paste0(round(mean(Age),rd)," (",round(sd(Age),rd),")"), 
                   `Sex (% male)`=round(sum(Sex)/n()*100,rd),
                   `Never`=round(sum(Smoking==0,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Previous`=round(sum(Smoking==1,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Current`=round(sum(Smoking==2,na.rm=T)/sum(!is.na(Smoking))*100,rd),
                   `Smoking pack-years`=paste0(round(mean(Packyears,na.rm=T),rd)," (",round(sd(Packyears,na.rm=T),rd),")"),
                   `Drink alcohol daily (%)`=round(sum(Alcintake==1,na.rm=T)/sum(!is.na(Alcintake))*100,rd),
                   `Monthly units of alcohol consumed *`=paste0(round(mean(Monthly_units,na.rm=T),rd)," (",round(sd(Monthly_units,na.rm=T),rd),")"),
                   `Sleep Duration (h)`=paste0(round(mean(SleepDur_o,na.rm=T),rd)," (",round(sd(SleepDur_o,na.rm=T),rd),")"),
                   ##
                   `Morning`=round(sum(Chronotype=="Definitely a morning person",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Intermediate`=round(sum(Chronotype=="Intermediate chronotype",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Evening`=round(sum(Chronotype=="Definitely an evening person",na.rm=T)/sum(!is.na(Chronotype))*100,rd),
                   `Do Not Know`=round(sum(Chronotype=="Do not know",na.rm=T)/sum(!is.na(Chronotype))*100,rd), ######
                   ##
                   `White British`=round(sum(Ethnicity%in%c(1001),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `White Other`=round(sum(Ethnicity%in%c(1,1002,1003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Mixed`=round(sum(Ethnicity%in%c(2,2001,2002,2003,2004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Asian`=round(sum(Ethnicity%in%c(3,3001,3002,3003,3004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Black`=round(sum(Ethnicity%in%c(4,4001,4002,4003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Chinese`=round(sum(Ethnicity%in%c(5),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   `Other`=round(sum(Ethnicity%in%c(1),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
                   ##
                   `Weekly work hours`=paste0(round(mean(LengthofWW,na.rm=T),rd)," (",round(sd(LengthofWW,na.rm=T),rd),")"),
                   ##
                   `Single Occupancy (%)`=round(sum(NuminHouse==1,na.rm=T)/sum(!is.na(NuminHouse))*100,rd),
                   `Urban area (%)`=round(sum(UrbanRural%in%c(1,5,11,12),na.rm=T)/sum(!is.na(UrbanRural))*100,rd),
                   `Townsend Index`=paste0(round(median(TDI,na.rm=T),2)," (",paste0(round(quantile(TDI,c(0.25,0.75),na.rm=T),2),collapse = " to "),")"),
  ) -> tab_SDchar
return(tab_SDchar)
}

H_table_function <- function(model_data_temp){
  model_data_temp %>% 
    filter(!(JiNS_o %in% c(NA,"Do not know","Prefer not to answer"))) %>%
    group_by(JiNS_o) %>%
    dplyr::summarise(N=n(),
                     `BMI (kg/m^2)` = paste0(round(mean(BMI_o,na.rm=T),rd)," (",round(sd(BMI_o,na.rm=T),rd),")"),
                     `Birth Weight (kg)*` = paste0(round(mean(Birthweight,na.rm=T),rd)," (",round(sd(Birthweight,na.rm=T),rd),")"),
                     `High Cholesterol (%)`=round(sum(High_Cholesterol)/n()*100,rd),
                     `Sleep Apnoea (%)`=round(sum(Sleep_Apnoea)/n()*100,rd),
                     `Chronic Obstructive Airways Disease/COPD`=round(sum(COPD)/n()*100,rd),
                     `Emphysema/Chronic Bronchitis`=round(sum(Emp_Chron_Bronchitis)/n()*100,rd),
                     `Bronchiectasis (%)`=round(sum(Bronchiectasis)/n()*100,rd),
                     `Interstitial Lung Disease (%)`=round(sum(Inter_lung_disease)/n()*100,rd),
                     `Other Respiratory Problems (%)`=round(sum(Other_resp_probs)/n()*100,rd),
                     `Gastro-Oesophageal Reflux (%)`=round(sum(GastroOesReflux)/n()*100,rd),
                     `Type 1 Diabetes (%)`=round(sum(DiabetesI)/n()*100,rd),
                     `Type 2 Diabetes (%)`=round(sum(DiabetesII)/n()*100,rd),
                     `Hypertension (%)`=round(sum(AllHypertension)/n()*100,rd),
                     `Cardiovascular Disease (%)`=round(sum(CardiovascularDisease)/n()*100,rd),
                     `Depression (%)`=round(sum(AllDepression)/n()*100,rd),
                     `Anxiety/panic attacks (%)`=round(sum(AnxietyPanic)/n()*100,rd),
                     `Hayfever/Allergic Rhinitis/Allergy to house dust mite (%)`=round(sum(HayfeverRhinitisHouseDustMite)/n()*100,rd),
                     `Eczema/Dermatitis (%)`=round(sum(Eczema)/n()*100,rd),
                     `Allergy or Anaphylactic reaction to food/drug (%)`=round(sum(Allergy)/n()*100,rd),
                     `Eosinophil Count`=NA,
                     `Testosterone (nmol/L)` =  paste0(round(mean(Testosterone,na.rm=T),rd)," (",round(sd(Testosterone,na.rm=T),rd),")"),
                     `Oestradiol (pmol/L)` =  paste0(round(mean(Oestradiol,na.rm=T),rd)," (",round(sd(Oestradiol,na.rm=T),rd),")"),
                     `SHBG (nmol/L)` =  paste0(round(mean(SHBG,na.rm=T),rd)," (",round(sd(SHBG,na.rm=T),rd),")"),
                     `Free Androgen Index` =  paste0(round(mean(FAI,na.rm=T),rd)," (",round(sd(FAI,na.rm=T),rd),")"),
                     `Free Testosterone (nmol/L)` =  paste0(round(mean(FT,na.rm=T),rd)," (",round(sd(FT,na.rm=T),rd),")")
    ) -> tab_Hchar
  return(tab_Hchar)
}

SD_table_function(model_data_temp %>% filter(Sex==0)) -> tab_SD_women
SD_table_function(model_data_temp %>% filter(Sex==1)) -> tab_SD_men

H_table_function(model_data_temp %>% filter(Sex==0)) -> tab_H_women
H_table_function(model_data_temp %>% filter(Sex==1)) -> tab_H_men
