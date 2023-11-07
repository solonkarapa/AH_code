
##################################################
###################### pkgs ######################
##################################################
library(mice)
library(dplyr)
library(purrr)
library(forcats)
library(VIM)
library(ggplot2)
library(gridExtra)

##################################################
###################### data ######################
##################################################
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"
load(paste0(path_data, "full_sample.Rdata"))

# select patients that died within 7 days of randomisation
dead_before_day7 <- stph %>% filter(Time_to_death_from_rand <= 7 & Death_event == 1)

# these are 49 in total
dead_before_day7 %>% summarise(n())

# exclude patients that died within 7 days from analysis
stph_day7 <- stph %>% filter(! (Subject %in% dead_before_day7$Subject))
    
##################################################
################## logistic ######################
##################################################
# association between MELD and missing bilirubin day 7
stph_missing_day7 <- stph_day7 %>% mutate(miss_bili_day7 = ifelse(is.na(Bilirubin.day.7), 1, 0))

fit <- glm(miss_bili_day7 ~ MELD, data = stph_missing_day7, family = binomial("logit"))
exp(coef(fit)) # odds ratio
exp(confint.default(fit)) # CIs
summary(fit)

##################################################
################ imputation ######################
##################################################
# vars to use in the imputation
vars_MELD <- c("Creatinine.mg.dl", "Bilirubin.mg.dl", "INR", "Sodium")
vars_MELD3 <- c(vars_MELD, "Albumin", "Gender")
vars_Lille_without_Bili7 <- c("Age", "Albumin", "Bilirubin.mg.dl", "protime")
vars_CLIF <- c("Bilirubin.mg.dl", "Creatinine.mg.dl", "HE", "INR", "MAP", "Age", "WBC")

vars_Lille_with_Bili7 <- c("Age", "Albumin", "Bilirubin.day.7", "Bilirubin.mg.dl", "protime")

# select dataframe for all models excluding Bilirubin.day.7
stph_short <- stph %>% 
    rename(Age = Age.at.randomisation..calc.) %>%
    select(D90_DTH, all_of(vars_MELD), all_of(vars_MELD3), all_of(vars_Lille_without_Bili7), all_of(vars_CLIF))

#select dataframe for Lille
stph_short_BiliDay7 <- stph %>% 
    filter(! (Subject %in% dead_before_day7$Subject)) %>% #exclude patients that died within 7 days from analysis
    rename(Age = Age.at.randomisation..calc.) %>%
    select(D90_DTH, all_of(vars_MELD), all_of(vars_MELD3), all_of(vars_Lille_with_Bili7), all_of(vars_CLIF))

# create df for plot of missing values
stph_plot <- stph %>% 
    rename(Age = Age.at.randomisation..calc.) %>%
    select(D90_DTH, all_of(vars_MELD), all_of(vars_MELD3), all_of(vars_Lille_with_Bili7), all_of(vars_CLIF))

# inspect the missing data pattern
#md.pattern(stph_short, rotate.names = T)

# missingness plot
a <- aggr(stph_plot, plot = F)
#plot(a, numbers = TRUE, prop = FALSE, only.miss = T, combined = T)

a$missings %>% filter(Count != 0) %>% # filter out complete obs
    mutate(Variable = 
               case_when(
                   Variable == "Bilirubin.mg.dl" ~ "Bilirubin day 0",
                   Variable == "Creatinine.mg.dl" ~ "Creatinine",
                   Variable == "Bilirubin.day.7" ~ "Bilirubin day 7", 
                   Variable == "proctime" ~ "Prothrombin time", 
                   TRUE ~ Variable)) %>%
    mutate(Variable = fct_reorder(Variable, Count)) %>%  # reorder factor
    ggplot(., aes(x = Variable, y = Count)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), hjust = -0.5, size = 3.5) +
    labs(x = "") +
    coord_flip() +
    theme_classic(12) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) 

# multiple imputation
imp <- mice(stph_short, m = 20, method = "pmm", seed = 5678)

imp_day7 <- mice(stph_short_BiliDay7, m = 20, method = "pmm", seed = 5678)

# predictor matrix
# https://www.gerkovink.com/miceVignettes/Convergence_pooling/Convergence_and_pooling.html 
imp$pred

# inspect the convergence of the algorithm
plot(imp)

# imputation method
imp$method #pmm method

# diagnostic checking
stripplot(imp, Bilirubin.day.7 ~ .imp, pch = 20, cex = 2)
stripplot(imp)

summary(stph_short$Bilirubin.day.7)
summary(with(imp, mean(Bilirubin.day.7)))

densityplot(imp, ~ Bilirubin.day.7)

# collection of the m imputed data sets 
imp_data <- complete(imp, action = "long")
imp_data_day7 <- complete(imp_day7, action = "long")

imp_data[which(is.na(imp_data)),]

#setwd("~/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis")
#save(imp_data, imp_data_day7, file = "imputed_data.Rdata")

###########################################################
################### Sensitivity analysis ##################
###########################################################
# create  vector that represent the following adjustment values 
#for Bili day 7: 0 for MAR, and -50, -100, and -200 for MNAR.
delta <- c(0, -50, -100, -200)

# perform a dry run (using maxit = 0)
ini <- mice(stph_short_BiliDay7, maxit = 0)

imp.all <- vector("list", length(delta))
post <- ini$post
for (i in 1:length(delta)){
    d <- delta[i]
    cmd <- paste("imp[[j]][,i] <- squeeze(imp[[j]][,i] +", d, ",c(0, 1061))") #squeeze values to observed ranges
    post["Bilirubin.day.7"] <- cmd
    imp <- mice(stph_short_BiliDay7, post = post, m = 20, maxit = 30, seed = i, print = FALSE)
    imp.all[[i]] <- imp
}

#bwplot(imp.all[[1]])
#bwplot(imp.all[[4]])

p1 <- densityplot(imp.all[[1]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", main = expression(paste(delta, " = 0")))
p2 <- densityplot(imp.all[[2]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", main = expression(paste(delta, " = -50")))
p3 <- densityplot(imp.all[[3]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", main = expression(paste(delta, " = -100")))
p4 <- densityplot(imp.all[[4]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", main = expression(paste(delta, " = -200")))

grid.arrange(p1, p2, p3, p4, nrow = 1, 
             bottom = textGrob("Bilirubin day 7", gp = gpar(fontsize = 13), vjust = -1.5),
             left = textGrob("Density", gp = gpar(fontsize = 13), rot = 90, vjust = 1.5, hjust = 0)
             )

#densityplot(imp.all[[1]], ~ Bilirubin.day.7, lwd = 3)
#densityplot(imp.all[[4]], ~ Bilirubin.day.7, lwd = 3)
#summary(with(imp.all[[3]], range(Bilirubin.day.7)))

## post-process
names(imp.all) <- delta # add names to list
complete_list <- lapply(imp.all, complete, action = "long")
imp_sens_df <- map_df(complete_list, ~ as.data.frame(.x), .id = "delta")

#setwd("~/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis")
#save(imp.all, imp_sens_df, file = "imputed_data_sens.Rdata")

