#source the other code####
source("code/01-load_packages.R")
source("code/02-load_tidy_data.R")
source("code/03-model.R")

###LRS#####
#emmeans plot

lme4::VarCorr(list_lrs[[9]])
sigma <- sqrt(2.9694e-05^2)

em1a <- emmeans(list_lrs[[9]],
                specs = ~ regime * env ,
                bias.adjust = T,
                sigma = sigma)

  plot_means <- as_tibble(confint(em1a, type = "response", calc = c(n = ~.wgt.),
                        adjust = "mvt"))
  
  lrs <- ggplot(plot_means,
         aes(x = env,
             y = response, 
             colour = regime,
             label = sprintf("%0.2f", round(response, 2)))) +
    geom_point(size = 4,  position = position_dodge(width = 0.25)) + 
    scale_colour_manual(values = c("purple", "orange")) + 
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, 
                  linewidth = 1.2,
                  position = position_dodge(width = 0.25)) + 
    theme_publication() +
    labs(x = "Assay Environment",
         y = "Lifetime Reproductive Success",
         fill = "Thermal Regime",
         colour = "Thermal Regime") +
    geom_label_repel(fontface = "bold",
                     color = "black",
                     max.iter = 3e2,
                     box.padding = 1.8,
                     point.padding = 5.8,
                     segment.color = "grey50",
                     segment.linetype = 6,
                     segment.curvature = -1e-20,
                     force = 1, show.legend = FALSE, direction = "x")
  

ggsave(path = "Figures/",
 filename = "lrs.png",
 width = 150, height = 150,
   units = "mm", dpi = 300)

###Development Time####
#emmeans plot
em1a <- emmeans(m1a,
                specs = ~ regime * env)

plot_means <- as_tibble(confint(em1a, type = "response", calc = c(n = ~.wgt.),
                                adjust = "mvt"))

g1 <- ggplot(plot_means,
             aes(x = env,
                 y = response, 
                 colour = regime,
                 label = sprintf("%0.2f", round(response, 2)))) +
  geom_point(size = 4,  position = position_dodge(width = 0.25),
             show.legend = F) + 
  scale_colour_manual(values = c("purple", "orange")) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, 
                linewidth = 1.2,
                position = position_dodge(width = 0.25)) + 
  theme_publication() +
  labs(x = "Assay Environment",
       y = "Development Time /Days",
       fill = "Thermal Regime",
       colour = "Thermal Regime") +
  geom_label_repel(fontface = "bold",
                   color = "black",
                   max.iter = 3e2,
                   box.padding = 1.8,
                   point.padding = 5.8,
                   segment.color = "grey50",
                   segment.linetype = 6,
                   segment.curvature = -1e-20,
                   force = 1, show.legend = FALSE, direction = "x")



ggsave(path = "Figures/",
 filename = "development_time.png",
 width = 150,
 height = 170,
   units = "mm",
    dpi = 300)
