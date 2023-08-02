
library(mice)
library(dplyr)
library(purrr)

# data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"
load(paste0(path_data, "full_sample.Rdata"))

# vars to use in the imputation
vars_MELD <- c("Creatinine.mg.dl", "Bilirubin.mg.dl", "INR", "Sodium")
vars_MELD3 <- c(vars_MELD, "Albumin", "Gender")
vars_Lille <- c("Age", "Albumin", "Bilirubin.day.7", "Bilirubin.mg.dl", "protime")
vars_CLIF <- c("Bilirubin.mg.dl", "Creatinine.mg.dl", "HE", "INR", "MAP", "Age", "WBC")

stph_short <- stph %>% 
    rename(Age = Age.at.randomisation..calc.) %>%
    select(D90_DTH, all_of(vars_MELD), all_of(vars_MELD3), all_of(vars_Lille), all_of(vars_CLIF))


# inspect the missing data pattern
md.pattern(stph_short, rotate.names = T)

# multiple imputation
imp <- mice(stph_short, m = 20, method = "pmm", seed = 5678)

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

imp$where 

# collection of the m imputed data sets 
imp_data <- complete(imp, action = "long")

imp_data[which(is.na(imp_data)),]

###########################################################
################### Sensitivity analysis ##################
###########################################################
# create  vector that represent the following adjustment values for Bili day 7: 0 for MAR, and -50, -15, and -20 for MNAR.
delta <- c(0, -50, -100, -200)

# perform a dry run (using maxit = 0)
ini <- mice(stph_short, maxit = 0)

imp.all <- vector("list", length(delta))
post <- ini$post
for (i in 1:length(delta)){
    d <- delta[i]
    cmd <- paste("imp[[j]][,i] <- squeeze(imp[[j]][,i] +", d, ",c(0, 1061))")
    post["Bilirubin.day.7"] <- cmd
    imp <- mice(stph_short, post = post, m = 20, maxit = 30, seed = i, print = FALSE)
    imp.all[[i]] <- imp
}

bwplot(imp.all[[1]])
bwplot(imp.all[[4]])

densityplot(imp.all[[1]], lwd = 3)
densityplot(imp.all[[4]], lwd = 3)

summary(with(imp.all[[3]], range(Bilirubin.day.7)))

## post-process
names(imp.all) <- delta # add names to list
complete_list <- lapply(imp.all, complete, action = "long")
imp_sens_df <- map_df(complete_list, ~ as.data.frame(.x), .id = "delta")



