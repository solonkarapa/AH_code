
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(forcats)
library(purrr)
library(survminer)
library(psych)

### data 
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_default.Rdata"))

# subset data
data <- test.data %>% 
    select(Subject, meld.surv.updated, lille.surv.updated, clif.surv.updated, D90_surv)

#############################################   
################### ICC  ####################
############################################# 
coords <- c(0.25, 1)

icc_meld_lille <- ICC(data[,c("meld.surv.updated", "lille.surv.updated")])

ICC_meld_lille <- paste0("ICC = ", round(icc_meld_lille$results$ICC[3], 2), 
                         ", p = ", signif(icc_meld_lille$results$p[3], 2))

p1 <- ggscatter(data, x = "meld.surv.updated", y = "lille.surv.updated",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = F, cor.method = "pearson", cor.coef.coord = coords,
          color = "grey", alpha = 0.7,
          add.params = list(color = "purple2")) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(x = "MELD", y = "Lille") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
    annotate("text", x = coords[1], y = coords[2], label = ICC_meld_lille, size = 4, fontface = 2) +
    theme_classic2()

icc_clif_meld <- ICC(data[,c("meld.surv.updated", "clif.surv.updated")])

ICC_CLIF_MELD <- paste0("ICC = ", round(icc_clif_meld$results$ICC[3], 2), ", p = ", signif(icc_clif_meld$results$p[3], 2))

p2 <- ggscatter(data, x = "meld.surv.updated", y = "clif.surv.updated",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = F, cor.method = "pearson", cor.coef.coord = coords,
                color = "grey", alpha = 0.7,
                add.params = list(color = "purple2")) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(x = "MELD", y = "CLIF-C ACLF") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    annotate("text", x = coords[1], y = coords[2], label = ICC_CLIF_MELD, size = 4, fontface = 2) + 
    theme_classic2()

icc_clif_lille <- ICC(data[,c("clif.surv.updated", "lille.surv.updated")])

ICC_CLIF_lille <- paste0("ICC = ", round(icc_clif_lille$results$ICC[3], 2), ", p = ", signif(icc_clif_lille$results$p[3], 2))

p3 <- ggscatter(data, x = "lille.surv.updated", y = "clif.surv.updated",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = F, cor.method = "pearson", cor.coef.coord = coords,
                color = "grey", alpha = 0.7,
                add.params = list(color = "purple2")) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(x = "Lille", y = "CLIF-C ACLF") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    annotate("text", x = coords[1], y = coords[2], label = ICC_CLIF_lille, size = 4, fontface = 2) +  
    theme_classic2()

#############################################   
################ Correlation  ###############
############################################# 
#coords <- c(0.01, 1)
# 
# p1 <- ggscatter(data, x = "meld.surv.updated", y = "lille.surv.updated",
#                 add = "reg.line", conf.int = TRUE, 
#                 cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = coords,
#                 color = "grey", alpha = 0.7,
#                 add.params = list(color = "purple2")) + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     labs(x = "MELD", y = "Lille") +
#     coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
#     theme_classic2()
# 
# p2 <- ggscatter(data, x = "meld.surv.updated", y = "clif.surv.updated",
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = coords,
#           color = "grey", alpha = 0.7,
#           add.params = list(color = "purple2")) + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     labs(x = "MELD", y = "CLIF-C ACLF") +
#     coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
#     theme_classic2()
# 
# p3 <- ggscatter(data, x = "lille.surv.updated", y = "clif.surv.updated",
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = coords,
#           color = "grey", alpha = 0.7,
#           add.params = list(color = "purple2")) + 
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
#     labs(x = "Lille", y = "CLIF-C ACLF") +
#     coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
#     theme_classic2()

#############################################   
################ Correlation  ###############
#############################################
# long format
data_long <- gather(data, model, probs, meld.surv.updated:clif.surv.updated, factor_key = TRUE)

data_long2 <- data_long %>% 
    group_by(Subject) %>% 
    mutate(dev = sd(probs), # sd 
           mad_dev = mad(probs), #median absolute deviation
           max_min_dev = max(probs) - min(probs)) # range 

# plot
p4 <- data_long2 %>% 
    #mutate(Subject = fct_reorder(as.factor(Subject), desc(max_min_dev))) %>%
    ggplot(.) +
    geom_point(aes(x = max_min_dev, y = reorder(Subject, max_min_dev))) +
    labs(x = "Range probabilities ", y = "Subject") +
    theme_classic2() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

p4

# overall survival rate
data_long2 %>% ungroup() %>% summarise(mean(D90_surv))

# fun 
cut_off_surv_rate <- function(data, thres, var_name){
    # calculates mean of `var_name` and sample size for chosen `thres` value
    
    df <- data %>% 
        filter(max_min_dev >= thres) %>% 
        ungroup() %>%
        summarise(sum_var = mean({{ var_name }}, na.rm = T))
    
    df_n <- data %>% 
        filter(max_min_dev >= thres) %>% 
        ungroup() %>%
        summarise(n = length(unique(Subject)))
    
    out <- data.frame(surv_rate = df$sum_var, thres = thres, n = df_n$n)
    
    return(out)
}

data_long23 <- data_long2 %>% distinct(Subject, .keep_all = T)
data_long23%>% filter(Subject == 1005003)


thresholds <- seq(0, 0.5, by = 0.01)
res <- map_df(thresholds, cut_off_surv_rate, data = data_long2, var_name = D90_surv)

# plot
ind <- c(1, 5, 9, 13, 25, 41, 49) # select a subset to visualise
p5 <- ggplot(res, aes(y = surv_rate, x = thres)) +
    geom_point() +
    geom_line() + 
    geom_text(data = res[ind, ], aes(label = n), check_overlap = TRUE, nudge_y = 0.04, nudge_x = 0.015) +
    labs(x = "Range probabilities", y = "Survival Rate") +
    theme_classic2()

p5

# fun 
sum_fun <- function(df1, df2, thres){
    # choses ids based the `thres` value 
    
    df_prelim <- df1 %>% filter(max_min_dev > thres) %>% arrange(Subject)
    
    df_final <- df2 %>% filter(Subject %in% df_prelim$Subject)
    
    df_final$threshold <- thres
    
    return(df_final)
}

res <- map_df(thresholds, sum_fun, df1 = data_long2, df2 = test.data)

vars <- c("Subject", "Bilirubin.mg.dl", "Bilirubin.day.7", "delta.bili", "INR", 
          "Creatinine.mg.dl", "Albumin", "WBC", "INR", "protime", "HE",
          "MAP", "Sodium", "CLIF.OF", 
          "kidney.score", "liver.score", "brain.score", "coag.score", "circ.score",
          "Gender.x", "Age.at.randomisation..calc..x")

sum_df <- res %>% 
    group_by(threshold) %>% 
    select(all_of(vars)) %>% 
    summarise_all(mean) %>%
    gather(., variable, value, Bilirubin.mg.dl:Age.at.randomisation..calc..x, factor_key=TRUE)

vars_to_keep <- c("CLIF.OF")

p6 <- sum_df %>% 
    filter(variable %in% vars_to_keep) %>%
    mutate(variable = ifelse(variable == "CLIF.OF", "CLIF OF", NA)) %>%
    ggplot(., aes(x = threshold, y = value)) +
    geom_point() +
    geom_line() +
    facet_wrap(. ~ variable, scales = "free_y") +
    labs(x = "Range probabilities", y = "") +
    theme_classic2() +
    theme(strip.background = element_blank())

p6

vars_to_remove <- c("Gender.x", "Age.at.randomisation..calc..x", 
                    "liver.score", "kidney.score", "brain.score", "coag.score",
                    "circ.score", 
                    "protime", "delta.bili")

#library(gridExtra)
ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3, labels = "auto", align = "hv")

#################
#################
#################

sum_df %>% filter(!(variable %in% vars_to_remove)) %>%
    filter(!variable %in% "CLIF.OF") %>%
    mutate(variable = case_when(variable == "Bilirubin.mg.dl" ~ "Bilirubin (mg/dL)",
                                variable == "Bilirubin.day.7" ~ "Bilirubin (day 7)",
                                variable == "Creatinine.mg.dl" ~ "Creatinine (mg/dL)", 
                                TRUE ~ variable)) %>%
    ggplot(., aes(x = threshold, y = value)) +
    geom_point() +
    geom_line() +
    facet_wrap(. ~ variable, scales = "free_y") +
    labs(x = "Range probabilities", y = "") +
    theme_bw() +
    theme(strip.background = element_blank())

#############################################   
########### ICC exploration  ################
#############################################
#https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/
# library(psych)
# ICC(data[,c(2:4)])
# 
# ICC(data[,c(2,4)])
# 
#cor.test(data[,c(2)],data[,c(4)], method = c("pearson"))
#ICC(data[,c(2,4)])
# 
#cor.test(data[,c(2)],data[,c(3)], method = c("pearson"))
#ICC(data[,c(2,3)])
# 
#cor.test(data[,c(3)],data[,c(4)], method = c("pearson"))
#ICC(data[,c(3,4)])
# 
# #The intra-class correlation coefficient was computed to assess the agreement between three scores 
# #in predicting 90-day survival. There was a poor agreement between the three scores, 
# # using the two-way mixed-effects model and “single rater” unit, kappa = 0.2, p = 0.056.
# 
# #ICC estimates and their 95% confident intervals were calculated using the R package psych (2.3.6) based on a "single rating" unit, 
# #two-way mixed-effects model. - add https://www.sciencedirect.com/science/article/pii/S1556370716000158
# 
# #library("irr") # an alternative package 
# #icc(data[,c(2:4)], model = "oneway",
# #    type = "consistency", unit = "single")



