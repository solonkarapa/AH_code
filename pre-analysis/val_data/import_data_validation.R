######
# This script reads in the Global_AlcHep data and creates workable data frames
library(readxl)
library(tidyverse)
library(dplyr)

# Load data 
path <- "/Users/work/IDrive-Sync/Projects/MIMAH/data"
Global_AlcHep <- read_excel(paste0(path, "/Global_AlcHep.xlsx"))

# Rename relevant variables
Global_AlcHep <- rename(Global_AlcHep, Bilirubin = Bili_admission) # mg/L
Global_AlcHep <- rename(Global_AlcHep, Creatinine = "Creatinine\r\nadmission") # mg/dL or mg/L
Global_AlcHep <- rename(Global_AlcHep, Albumin = Alb_admission) #g/dL
Global_AlcHep <- rename(Global_AlcHep, INR = INR_admission) 
Global_AlcHep <- rename(Global_AlcHep, protime = PT_admission) # Prothrombin time (sec)
Global_AlcHep <- rename(Global_AlcHep, Sodium = "Sodium at admission")
Global_AlcHep <- rename(Global_AlcHep, Bilirubin.day.7 = "Total bili day  7")

# to check again - the discrepancy
#diff <- (Global_AlcHep$`INR admission` - Global_AlcHep$INR_admission)
#ind <- which(diff !=0 )
#Global_AlcHep[ind, ]

# Create survival variable
Global_AlcHep <- rename(Global_AlcHep, D90_surv = "Alive at day 90\r\n")
Global_AlcHep$D90_DTH <- 1 - Global_AlcHep$D90_surv

# Time-to-event
Global_AlcHep <- Global_AlcHep %>% 
    mutate(Date_of_death = as.Date(`Date of death`, format = "%d/%m/%y"),
           Date_rand = as.Date(`Date of admission`, format = "%d/%m/%y"),
           Date_Last_day_study_contact = as.Date(`Last follow up`,  format = "%d-%B-%y"),
           Death_event = ifelse(is.na(Date_of_death), 0, 1), 
           Time_to_death_from_rand = ifelse(Death_event == 1, 
                                            (Date_of_death - Date_rand), (Date_Last_day_study_contact - Date_rand))) 

# Handle missing data and create data frames for each of the prognostic scores
Global_AlcHep.meld <- Global_AlcHep[complete.cases(Global_AlcHep$Bilirubin, 
                                                   Global_AlcHep$Creatinine, 
                                                   Global_AlcHep$INR, 
                                                   Global_AlcHep$Sodium),]

Global_AlcHep.lille <- Global_AlcHep[complete.cases(Global_AlcHep$Bilirubin.day.7, 
                                                    Global_AlcHep$Bilirubin, 
                                                    Global_AlcHep$Creatinine,
                                                    Global_AlcHep$protime, 
                                                    Global_AlcHep$Albumin),]

#path <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/val_data"
#setwd(path)
#save(Global_AlcHep.meld, Global_AlcHep.lille, file = "full_sample_Global_AlcHep.Rdata")

# Create table of descriptive statistics and number of missing values per variable
library(table1)
# Some factor variables
Global_AlcHep$D90_surv_f <- as.factor(Global_AlcHep$D90_surv)
Global_AlcHep$Gender_f <- as.factor(Global_AlcHep$Gender)
# table stratified by outcome
tb1 <- table1::table1(~ Age + Bilirubin + Creatinine + Albumin + protime + 
                          INR + Bilirubin.day.7 + Gender_f + Sodium | factor(D90_surv_f), data = Global_AlcHep)
tb1
    
    
library(xtable)
xtable(as_tibble(tb1))

# 90-day death rate
mean(Global_AlcHep$D90_DTH)

# 
unique(Global_AlcHep$Country)

# KM estimates and plots
library(ggsurvfit)
survfit2(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = Global_AlcHep) %>% 
    ggsurvfit() +
    labs(
        x = "Days",
        y = "Overall survival probability"
    ) + 
    add_confidence_interval() +
    add_risktable()

summary(survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = Global_AlcHep), times = 90)
summary(survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = Global_AlcHep), times = 183)

survfit(Surv(Time_to_death_from_rand, Death_event) ~ 1, data = Global_AlcHep) %>% 
    gtsummary::tbl_survfit(
        times = 90,
        label_header = "**1-year survival (95% CI)**"
    )


