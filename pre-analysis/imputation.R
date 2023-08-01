

library(mice)
library(dplyr)

# data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"
load(paste0(path_data, "full_sample.Rdata"))


vars_MELD <- c("Creatinine.mg.dl", "Bilirubin.mg.dl", "INR", "Sodium")
vars_MELD3 <- c(vars_MELD, "Albumin", "Gender")
vars_Lille <- c("Age", "Albumin", "Bilirubin.day.7", "Bilirubin.mg.dl", "protime")
vars_CLIF <- c("Bilirubin.mg.dl", "Creatinine.mg.dl", "HE", "INR", "MAP", "Age", "WBC")

stph_short <- stph %>% 
    rename(Age = Age.at.randomisation..calc.) %>%
    select(D90_DTH, all_of(vars_MELD), all_of(vars_MELD3), all_of(vars_Lille), all_of(vars_CLIF))

# relationship between Bilirubin value
#plot(stph$Bilirubin.mg.dl*17, stph$Bilirubin.Merged)
#abline(a = 0, b = 1)



dim(stph_short)

summary(stph_short)




md.pattern(stph_short)


imp <- mice(stph_short, m = 20, method = "pmm", seed = 5678)

plot(imp)

imp$pred


imp$method #pmm method


stripplot(imp, Bilirubin.day.7 ~ .imp, pch=20, cex=2)

imp$where 

imp_data <- complete(imp, action = "long")

imp_data[which(is.na(imp_data)),]


imp_data %>% filter(.id == 3)

