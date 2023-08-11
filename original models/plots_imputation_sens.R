
library(ggplot2)
library(gridExtra)


path_imp <- "~/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"

# imputed data - sensitivity analysis
load(paste0(path_imp, "imputed_data_sens.Rdata"))


p1 <- densityplot(imp.all[[1]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", 
                  main = expression(paste(delta, " = 0")))
p2 <- densityplot(imp.all[[2]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", 
                  main = expression(paste(delta, " = -50")))
p3 <- densityplot(imp.all[[3]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", 
                  main = expression(paste(delta, " = -100")))
p4 <- densityplot(imp.all[[4]], ~ Bilirubin.day.7, lwd = 3, xlab = "", ylab = "", 
                  main = expression(paste(delta, " = -200")))

library(patchwork)
p_prelim <- grid.arrange(p1, p2, p3, p4, nrow = 1, 
             bottom = textGrob("Bilirubin day 7", gp = gpar(fontsize = 11), vjust = -3),
             left = textGrob("Density", gp = gpar(fontsize = 11), rot = 90, vjust = 1.5, hjust = 0)
)

grid.arrange(p_prelim, p5, p6, nrow = 2, ncol = 3, layout_matrix = rbind(c(1,1), c(2,3)))



top_row <- plot_grid(p1, p2, p3, p4, labels = "a", nrow = 1)
title <- ggdraw() + 
    draw_label("Bilirubin day 7", x = 0.5, y = 0.5, size = 11) 

top_row_with_title <- plot_grid(
    title, top_row,
    ncol = 1
    # rel_heights values control vertical title margins
)

top_row_with_title

bottom_row <- plot_grid(p5, p6, labels = c('b', 'c'), nrow = 1)
plot_grid(top_row_with_title, bottom_row, nrow = 2, rel_heights = c(4, 4))


