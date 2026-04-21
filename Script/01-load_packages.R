# Load packages ########
renv::snapshot()
pacman::p_load("tidyverse",
               "hablar",
               "glmmTMB",
               "DHARMa",
               "emmeans", 
               "easystats",
               "MASS",
               "ggeffects", 
               "patchwork",
               "magrittr",
               "MuMIn",
               "broom", 
               "sjPlot",
               "janitor",
               "lme4",
               "lmerTest",
               'robustlmm',
               "grid",
               "ggthemes",
               "PupillometryR",
               "gghalves",
               "ggrepel",
               "rlang",
               "ggdist",
               "palmerpenguins",
               "ggtext",
              "colorspace",
               "ragg")

# Publication theme######
theme_publication <- function(base_size = 15, base_family = "Arial") {
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2), hjust = 0.5
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = element_text(face = "italic"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      legend.key.width = unit(2, "line"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    ))
}

# Modelchecker function######
modelchecker <- function(check, filename) {
  test <- matrix(ncol = 15, nrow = length(check))
  pb <- txtProgressBar(0, length(check), style = 3)

  for (i in seq_along(check)){

    setTxtProgressBar(pb, i)

    s1b <- try(simulateResiduals(check[[i]]), silent = TRUE)
    if ("try-error" %in% class(s1b)) next

    zi <- testZeroInflation(s1b)
    zitest <- zi$statistic
    zip <- zi$p.value

    di <- testDispersion(s1b)
    ditest <- di$statistic
    dip <- di$p.value

    uni <- testUniformity(s1b)
    unitest <- uni$statistic
    unip <- uni$p.value

    com <- AIC(check[[i]])

    we <- model.sel(check[[i]])

    test[i, 1:15] <- c(i,
     paste(check[[i]]$modelInfo$family$family),
     paste(check[[i]]$modelInfo$allForm$formula[3]),
     paste(check[[i]]$modelInfo$allForm$ziformula[2]),
     paste(check[[i]]$modelInfo$allForm$dispformula[2]),
     zitest, zip, ditest, dip, unitest, unip, com,
     paste(check[[i]]$fit$message), paste(summary(check[[i]]$sdr)[1, 2]),
     paste(we$df))

    Sys.sleep(time = 1)
  }

  we <- model.sel(check[[i]])
  test <- as.data.frame(test)
  names(test) <- c("ModelNo",
   "Family",
    "Formula",
     "ziFormula",
     "dispFormula",
      "zi", "zip",
       "di", "dip",
        "uni", "unip",
         "AIC", "Converge?-1",
          "Converge?-2",
           "df")

  test %<>%
    convert(num(zi, zip, di, dip, uni, unip, AIC))

  final <- test %>%
    filter(zi <= 1.10) %>%
    filter(di <= 1.10) %>%
    filter(zi >= 0.80) %>%
    filter(di >= 0.80) %>%
    filter(AIC != "NA") %>%
    filter(`Converge?-1` == "relative convergence (4)") %>%
    filter(`Converge?-2` != "NaN") %>%
    arrange(AIC)

 View(test)
 View(final)
 write.csv(final, paste0(filename, "final.models.csv"), row.names = FALSE)
 write.csv(test, paste0(filename, "all.models.csv"), row.names = FALSE)

}
