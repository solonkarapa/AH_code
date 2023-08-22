######
# This script reads in the stph data and creates workable data frames
library(tidyverse)
library(dplyr)

# Load data 
path <- "/Users/work/IDrive-Sync/Projects/MIMAH/data"
load(paste0(path, "/stopah_plus_scores.Rdata"))
stph <- rename(data_stph_prelim3)

# Rename relevant variables
stph <- rename(stph, Bilirubin.mg.dl = Bilirubin.Merged..mg.dL..Merged..calc.)
stph <- rename(stph, Creatinine = Creatinine...Merged)
stph <- rename(stph, Albumin = Albumin...Merged)
stph <- rename(stph, WBC = WBC...Merged) # White blood count
stph <- rename(stph, INR = INR...Merged.clinical.and.calc) 
stph <- rename(stph, protime = Prothrombin.Time..patient....Merged) # Prothrombin time
stph <- rename(stph, HE = Hepatic.Encephalopathy...Merged) # Brain function
stph <- rename(stph, Sodium = Sodium...Merged)
    
# Create survival variable
stph$D90_surv <- 1 - stph$D90_DTH

# Transform creatinine into mg/dl
stph$Creatinine.mg.dl <- 0.0113*stph$Creatinine

# Time-to-event
stph <- stph %>% 
    mutate(Date_of_death = as.Date(Date_of_death, format = "%d/%m/%y"),
           Date_rand = as.Date(Treatment.start.date..randomisation.date.if.rx.start.date.missing., format = "%d/%m/%y"),
           Date_Last_day_study_contact = as.Date(Last_day_study_contact,  format = "%d-%B-%y"),
           Death_event = ifelse(is.na(Date_of_death), 0, 1), 
           Time_to_death_from_rand = ifelse(Death_event == 1, 
                                            (Date_of_death - Date_rand), (Date_Last_day_study_contact - Date_rand))) %>%
    mutate(M6_DTH = ifelse((Death_event == 1 & Time_to_death_from_rand <= 183), 1, 0), # Calculate 6-month death indicator
           M6_surv = 1 - M6_DTH) # Calculate 6-month survival indicator

#stph2$Time_to_death_from_rand
#stph2$Death_event
#stph2 %>% filter(Death_event == 0 & Time_to_death_from_rand < 90) %>% 
#    select(Time_to_death_from_rand, Death_event, Date_of_death, 
#           Time.to.death..calculated.by.MRIS.retrieved.data., Date_Last_day_study_contact, Date_rand) #%>% 
    #summarise(n())

#stph$Last_day_study_contact[1]
#as.Date(stph$Last_day_study_contact[1], format = "%d-%B-%y")
#stph2 <- stph %>% mutate(Event = ifelse(is.na(Time.to.death..calculated.by.MRIS.retrieved.data.), 0, 1))
#stph2 %>% filter(Time.to.death..calculated.by.MRIS.retrieved.data. < 90)
#range(stph$Time.to.death..calculated.by.MRIS.retrieved.data., na.rm = T)
#stph$Y1_DTH
#plot(density(na.omit(stph$Time.to.death..calculated.by.MRIS.retrieved.data.)))
#as.Date(Date_of_death, format = "%d/%m/%y")
#ind <-str_detect(colnames(stph), "rand")
#stph[,ind]

# Handle missing data and create data frames for each of the prognostic scores
stph.meld <- stph[complete.cases(stph$Bilirubin.mg.dl, stph$Creatinine, stph$INR, stph$Sodium),]
stph.clif <- stph[complete.cases(stph$Bilirubin.mg.dl, stph$Creatinine, stph$INR, stph$WBC, 
                                 stph$HE, stph$MAP),]
stph.lille <- stph[complete.cases(stph$Bilirubin.day.7, stph$Bilirubin.Merged, stph$Creatinine,
                                  stph$protime, stph$Albumin),]

#path <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"
#setwd(path)
#save(stph, file = "full_sample.Rdata")

# Create table of descriptive statistics and number of missing values per variable
library(table1)
# Some factor variables
stph$HE_f <- as.factor(stph$HE) 
stph$D90_surv_f <- as.factor(stph$D90_surv)
stph$Gender_f <- as.factor(stph$Gender)
# table stratified by outcome
tb1 <- table1::table1(~ Age.at.randomisation..calc. + Bilirubin.mg.dl + Creatinine.mg.dl + Albumin + WBC + protime + 
                          INR + HE_f + Bilirubin.day.7 + Gender_f + Sodium | factor(D90_surv_f), data = stph)

library(xtable)
xtable(as_tibble(tb1))

# 90-day death rate
mean(stph$D90_DTH)

# KM estimates and plots
library(ggsurvfit)
survfit2(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = stph) %>% 
    ggsurvfit() +
    labs(
        x = "Days",
        y = "Overall survival probability"
    ) + 
    add_confidence_interval() +
    add_risktable()

summary(survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = stph), times = 90)
summary(survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = stph), times = 183)

survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = stph) %>% 
    gtsummary::tbl_survfit(
        times = 90,
        label_header = "**1-year survival (95% CI)**"
    )


