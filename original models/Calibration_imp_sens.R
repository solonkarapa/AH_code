
# This script performs all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme

# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))

# imputed data
#path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
#load(paste0(path_data, "original_models.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   
imp_index <- group_split(imp_data3, .imp) 

imp_ind <- max(imp_data3$.imp)

# MELD_1
cal_MELD.surv1 <- tibble()
for(i in 1:imp_ind){
    # MELD 1
    temp <- calibration(imp_index[[i]]$MELD.surv1, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD_1"
    temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
    temp$.imp <- i
    cal_MELD.surv1 <- rbind(cal_MELD.surv1, temp)
}
rm(temp)

# MELD 2
cal_MELD.surv2 <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$MELD.surv2, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD_2"
    temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
    temp$.imp <- i
    cal_MELD.surv2 <- rbind(cal_MELD.surv2, temp)
}

rm(temp)

# MELD VanDerwerken
cal_MELD.VanDerwerken <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$MELD_Van, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD VanDerwerken"
    temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
    temp$.imp <- i
    cal_MELD.VanDerwerken <- rbind(cal_MELD.VanDerwerken, temp)
}

rm(temp)

# Lille
cal_Lille <- tibble()
for(i in 1:imp_ind){
    for(g in 1:length(delta)){
        d <- unique(imp_index[[i]]$delta)
        df <- imp_index[[i]] %>% filter(delta == d[g])
        
        temp <- calibration(df$Lille.surv, y = df$D90_surv)
        temp$Score <- "Lille"
        temp$delta <- unique(df$delta)
        temp$.imp <- i
        cal_Lille <- rbind(cal_Lille, temp)
    }
}

#unique(cal_Lille$delta)
#i <- 1
#imp_index[[i]] %>% group_by(delta) %>% summarise(. ~ calibration, pred = Lille.surv, obs = D90_surv)

rm(temp)

# "CLIF-C ACLF" 
cal_CLIF <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$CLIF.surv, y = imp_index[[i]]$D90_surv)
    temp$Score <- "CLIF-C ACLF"
    temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
    temp$.imp <- i
    cal_CLIF <- rbind(cal_CLIF, temp)
}

# combine dfs
df_cal <- rbind(cal_MELD.surv1, cal_MELD.surv2, cal_MELD.VanDerwerken, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
############################################# 

# plot without ribbon and without MELD 3.0
cal_Lille %>% filter(Score == "Lille") %>%
    ggplot(., aes(x = pred, y = obs, group = delta, col = delta)) +
    geom_line(aes(alpha = 0.5))  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    #facet_grid(. ~ delta) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic() +
    theme(legend.position = "none")

cal_Lille %>% filter(Score == "Lille") %>% 
    mutate(grp = paste0(delta, "_", .imp)) %>%
    ggplot(., aes(x = pred, y = obs, col = grp)) +
    geom_line(aes(alpha = 0.7))  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    #facet_grid(. ~ delta) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic() #+
    #theme(legend.position = "none")
  

cal_Lille %>% filter(delta == "0" & .imp == 1)

cal_Lille %>% ggplot(.) +
    geom_boxplot(aes(x = pred, fill = delta)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") 

imp_data3 %>% select(Lille.surv, delta, .imp) %>%
    group_by(delta) %>%
    ggplot(.) +
    geom_boxplot(aes(Lille.surv, fill = delta)) 


imp_data3 %>% select(Lille.surv, delta, .imp) %>%
    group_by(delta) %>% summarise(mean(Lille.surv))
