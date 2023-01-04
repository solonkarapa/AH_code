
library(dplyr)
library(ggplot2)

path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_default.Rdata"))


colnames(test.data)

data <- test.data %>% select(Subject, meld.surv.updated, lille.surv.updated, clif.surv.updated, D90_surv)

# correlation 
ggplot(data, aes(x = meld.surv.updated, y = lille.surv.updated)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) 

ggscatter(data, x = "meld.surv.updated", y = c("lille.surv.updated"),
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = c(0.10, 0.9)) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_bw()

ggplot(data) +
    geom_point(aes(x = meld.surv.updated, y = clif.surv.updated)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

ggscatter(data, x = "meld.surv.updated", y = c("clif.surv.updated"),
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = c(0.10, 0.9)) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_bw()

ggplot(data) +
    geom_point(aes(x = lille.surv.updated, y = clif.surv.updated)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

ggscatter(data, x = "lille.surv.updated", y = c("clif.surv.updated"),
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", cor.coef.coord = c(0.10, 0.9)) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_bw()


cor(data$meld.surv.updated, data$lille.surv.updated)
cor(data$meld.surv.updated, data$clif.surv.updated)
cor(data$lille.surv.updated, data$clif.surv.updated)


library(tidyr)
data_long <- gather(data, model, probs, meld.surv.updated:clif.surv.updated, factor_key = TRUE)
data_long


ggplot(data_long, aes(x = probs, y = Subject, col = model)) +
    geom_point()
    
data_long2 <- data_long %>% 
    group_by(Subject) %>% 
    mutate(dev = sd(probs),
           mad_dev = mad(probs),
           max_min_dev = max(probs) - min(probs))
    #mutate(Subject = fct_reorder(as.factor(Subject), probs, .fun = 'sd'))

ggplot(data_long2, aes(x = mad_dev, y = Subject, col = model)) +
    geom_point()

ggplot(data_long2, aes(x = max_min_dev, y = Subject, col = model)) +
    geom_point()

ggplot(data_long2, aes(x = mad_dev)) +
    geom_density()

ggplot(data_long2, aes(x = mad_dev, y = dev)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

library(forcats)
data_long2 %>% 
    mutate(Subject = fct_reorder(as.factor(Subject), desc(mad_dev))) %>%
    ggplot(., aes(x = mad_dev, y = reorder(Subject, mad_dev), col = factor(D90_surv))) +
    geom_point(alpha = 0.5) 

data_long2 %>% 
    mutate(Subject = fct_reorder(as.factor(Subject), desc(max_min_dev))) %>%
    ggplot(., aes(x = max_min_dev, y = reorder(Subject, max_min_dev), col = factor(D90_surv))) +
    geom_point(alpha = 0.5) 

range(data_long2$max_min_dev)

data_long3 <- data_long2 %>% filter(max_min_dev > 0.3) #filter(mad_dev > 0.1) 
# subset survival 
data_long2 %>% filter(max_min_dev > 0.2) %>% ungroup() %>% summarise(suriv_rate = mean(D90_surv))
data_long2 %>% filter(max_min_dev > 0.3) %>% ungroup() %>% summarise(mean(D90_surv))
data_long2 %>% filter(max_min_dev > 0.4) %>% ungroup() %>% summarise(mean(D90_surv))
data_long2 %>% filter(max_min_dev > 0.5) %>% ungroup() %>% summarise(mean(D90_surv))

data_long2 %>% filter(max_min_dev > 0.5) %>% ungroup() %>% summarise(length(unique(Subject)))
data_long2 %>% filter(max_min_dev > 0.6) %>% ungroup() %>% summarise(length(unique(Subject)))

# overall surival 
data_long2 %>% ungroup() %>% summarise(mean(D90_surv))

cut_off_surv_rate <- function(data, thres, var_name){
    df <- data %>% 
        filter(max_min_dev > thres) %>% 
        ungroup() %>%
        summarise(sum_var = mean({{ var_name }}, na.rm = T))
    
    df_n <- data %>% 
        filter(max_min_dev > thres) %>% 
        ungroup() %>%
        summarise(n = length(unique(Subject)))
    
    out <- data.frame(surv_rate = df$sum_var, thres = thres, n = df_n$n)
    
    return(out)
}


library(purrr)
thresholds <- seq(0, 0.6, by = 0.01)
res <- map_df(thresholds, cut_off_surv_rate, data = data_long2, var_name = D90_surv)

ggplot(res, aes(y = surv_rate, x = thres, label = n)) +
    geom_point() +
    geom_line() + 
    geom_text(check_overlap = TRUE, nudge_y = 0.05)

extreme_sbj <- data_long2 %>% filter(max_min_dev > 0.55) %>% arrange(Subject)

sum_fun <- function(df1, df2, thres){
    
    df_prelim <- df1 %>% filter(max_min_dev > thres) %>% arrange(Subject)
    
    df_final <- df2 %>% filter(Subject %in% df_prelim$Subject)
    
    df_final$threshold <- thres
    
    return(df_final)
}

res <- map_df(thresholds, sum_fun, df1 = data_long2, df2 = test.data)

sum_df <- res %>% group_by(threshold) %>% select(vars) %>% summarise_all(mean) 

ggplot(sum_df, aes(x = threshold, y = INR)) +
    geom_point() +
    geom_line()

ggplot(data_long3, aes(x = probs, y = reorder(factor(Subject), desc(mad_dev)), col = model)) +
    geom_point() +
    geom_line() +
    coord_cartesian(xlim = c(0,1)) #+
    #coord_flip()

ggplot(data_long3, aes(x = probs, y = reorder(factor(Subject), desc(max_min_dev)), col = model)) +
    geom_point() +
    geom_line() +
    coord_cartesian(xlim = c(0, 1)) 

data_long3 %>% ggplot(.) + geom_density(aes(x = probs, fill = model), alpha = 0.5)


vars <- c("Subject", "Bilirubin.mg.dl", "Bilirubin.day.7", "delta.bili", "INR", 
          "Creatinine.mg.dl", "Albumin", "WBC", "INR", "protime", "HE",
          "MAP", "Sodium", "CLIF.OF", "kidney.score", "liver.score", "brain.score", "coag.score", "circ.score",
          "Gender.x", "Age.at.randomisation..calc..x")

df <- test.data %>% 
    mutate(Group = ifelse(Subject %in% data_long3$Subject, 1, 0)) %>% 
    select(Group, vars) %>%
    group_by(Group) 
 
df %>% ungroup() %>% summarise(mean(Group))

df %>% summarise(across(Bilirubin.mg.dl:Age.at.randomisation..calc..x, ~ mean(.x, na.rm = T)))   

df %>% ggplot(., aes(x = CLIF.OF, group = Group, fill = factor(Group))) +  geom_bar(position = "dodge") #geom_density() #+ geom_histogram()

# chisq.test
#https://data-flair.training/blogs/chi-square-test-in-r/#:~:text=Chi%2DSquare%20test%20in%20R%20is%20a%20statistical%20method%20which,Green%2C%20Yes%2FNo%20etc.
test <- chisq.test(table(df$CLIF.OF, df$Group), simulate.p.value = TRUE)
test

df %>% ggplot(., aes(x = Group, y = Bilirubin.mg.dl, group = Group, fill = factor(Group))) +  geom_violin()
df %>% ggplot(., aes(x = Group, y = Creatinine.mg.dl, group = Group, fill = factor(Group))) +  geom_violin()
df %>% ggplot(., aes(x = HE, group = Group, fill = factor(Group))) +  geom_histogram() # discrete 
df %>% ggplot(., aes(x = INR, group = Group, fill = factor(Group))) +  geom_histogram() 
df %>% ggplot(., aes(x = Group, y = INR, group = Group, fill = factor(Group))) +  geom_violin()
df %>% ggplot(., aes(x = Group, y = MAP, group = Group, fill = factor(Group))) +  geom_violin()

stacked_1 <- df %>% ungroup(.) %>% filter(Group == 1) %>% select(-Group) %>% stack(.) %>% filter(ind != "Group")
stacked_1$Group <- "1"
stacked_2 <- df %>% ungroup(.) %>% filter(Group == 0) %>% select(-Group) %>% stack(.) %>% filter(ind != "Group")
stacked_2$Group <- "0"

stacked <- rbind(stacked_1, stacked_2)

library(ggpubr)
stacked %>% filter(ind != "Subject") %>%  
    ggplot(., aes(x = ind, y = values, fill = factor(Group))) +
    geom_violin() +
    facet_wrap(ind ~ ., scales = "free") +
    stat_compare_means(method = "t.test")

stacked %>% filter(ind != "Subject") %>%  
    ggplot(., aes(x = ind, y = values, fill = factor(Group))) +
    geom_boxplot() +
    facet_wrap(ind ~ ., scales = "free") +
    stat_pwc(method = "t.test", p.adjust.method = "bonferroni")

stacked %>% filter(ind != "Subject") %>%  
    ggplot(., aes(x = ind, y = values, fill = factor(Group))) +
    geom_boxplot() +
    geom_pwc(aes(group = Group), tip.length = 0,
    method = "t_test", p.adjust.method = "none", p.adjust.by = "group",
    hide.ns = TRUE) #+
    #facet_wrap(ind ~ ., scales = "free") 
    
comp_means <- compare_means(c(Bilirubin.mg.dl, Creatinine.mg.dl, HE, INR, MAP, 
                CLIF.OF) ~ Group, data = df, 
                method = "t.test",  p.adjust.method = "holm")


adj_values <- p.adjust(comp_means$p.adj, method = 'holm')
comp_means$adj_values = adj_values
comp_means


# calibration
# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))

#from long to wide 
data_wide <- spread(data_long3, model, probs)
str(data_wide)

# MELD 
cal_MELD.surv <- calibration(data_wide$meld.surv.updated, y = data_wide$D90_surv)
cal_MELD.surv$Score <- "MELD"

# Lille
cal_Lille <- calibration(data_wide$lille.surv.updated, y = data_wide$D90_surv)
cal_Lille$Score <- "Lille"

# CLIF-C ACLF
cal_CLIF <- calibration(data_wide$clif.surv.updated, y = data_wide$D90_surv)
cal_CLIF$Score <- "CLIF-C ACLF"

# combine dfs
df_cal <- rbind(cal_MELD.surv, cal_Lille, cal_CLIF)

# plot with ribbon 
df_cal %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(linewidth = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed survival proportion") + 
    xlab("Predicted survival probability") + 
    theme_classic() 

library(gmish)
ici(data_wide$meld.surv.updated, data_wide$D90_surv)
ici(data_wide$lille.surv.updated, data_wide$D90_surv)
ici(data_wide$clif.surv.updated, data_wide$D90_surv)

mean(abs(cal_MELD.surv$pred - cal_MELD.surv$obs))


mean((data_wide$meld.surv.updated - data_wide$D90_surv)^2)
mean((data_wide$lille.surv.updated - data_wide$D90_surv)^2)
mean((data_wide$clif.surv.updated - data_wide$D90_surv)^2)

#library(DescTools)
#BrierScore(data_wide$D90_surv, data_wide$meld.surv.updated)
#BrierScore(data_wide$D90_surv, data_wide$lille.surv.updated)
#BrierScore(data_wide$D90_surv, data_wide$clif.surv.updated)

library(pROC)
# MELD (survival function 1 and 2 do not matter here)
roc_meld <- roc(data_wide$D90_surv, data_wide$meld.surv.updated)
roc_meld
# Lille
roc_lille <- roc(data_wide$D90_surv, data_wide$lille.surv.updated)
roc_lille
# CLIF-C ACLF
roc_clif <- roc(data_wide$D90_surv, data_wide$clif.surv.updated)
roc_clif


