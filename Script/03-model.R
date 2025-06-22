# load the source######
source("Script/01-load_packages.R")
source("Script/02-load_tidy_data.R")

# LRS #######
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
#create list for LRS models
list_lrs <- list()

#first model (poisson), detect overdispersion and zi
list_lrs[[1]] <- glmmTMB(lrs ~
                           regime +
                           env +
                           regime:env +
                           (1|group),
                           data = repro_wide,
                           family = "poisson")

#check
summary(list_lrs[[1]])
s1a <- simulateResiduals(list_lrs[[1]], plot = TRUE)
testZeroInflation(s1a)
testDispersion(s1a) 


#second model (poisson + olre), detect overdispersion and zi
list_lrs[[2]]  <- glmmTMB(lrs ~
                            regime +
                            env +
                            regime:env +
                            (1|group) +
                            (1|obs),
                          data = repro_wide,
                          family = "poisson")

#check
summary(list_lrs[[2]])
s1a <- simulateResiduals(list_lrs[[2]], plot = TRUE)
testZeroInflation(s1a)
testDispersion(s1a)

#third model (compois), detect overdispersion and zi
list_lrs[[3]] <- glmmTMB(lrs ~
                           regime +
                           env +
                           regime:env +
                           (1|group),
                           data = repro_wide,
                           family = compois)

#check
summary(list_lrs[[3]])
s1a <- simulateResiduals(list_lrs[[3]], plot = TRUE)
testZeroInflation(s1a)
testDispersion(s1a)

#modelchecker code
modelchecker(list_lrs, filename = "Outputs/prelim_lrs_")

#fit with zero-inflated parameter - zi detected

#compois
list_lrs[[4]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~1, family = compois, data = repro_wide)
list_lrs[[5]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env, family = compois, data = repro_wide)
list_lrs[[6]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~regime, family = compois, data = repro_wide)
list_lrs[[7]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env + regime, family = compois, data = repro_wide)
list_lrs[[8]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env*regime, family = compois, data = repro_wide)
list_lrs[[9]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~group, family = compois, data = repro_wide)

list_lrs[[10]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~1, family = poisson, data = repro_wide)
list_lrs[[11]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env, family = poisson, data = repro_wide)
list_lrs[[12]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~regime, family = poisson, data = repro_wide)
list_lrs[[13]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env + regime, family = poisson, data = repro_wide)
list_lrs[[14]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~env*regime, family = poisson, data = repro_wide)
list_lrs[[15]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group), zi = ~1, disp = ~group, family = poisson, data = repro_wide)

list_lrs[[16]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~1, family = poisson, data = repro_wide)
list_lrs[[17]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~env, family = poisson, data = repro_wide)
list_lrs[[18]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~regime, family = poisson, data = repro_wide)
list_lrs[[19]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~env + regime, family = poisson, data = repro_wide)
list_lrs[[20]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~env*regime, family = poisson, data = repro_wide)
list_lrs[[21]] <- glmmTMB(lrs ~ regime + env + regime:env + (1 | group) + (1|obs), zi = ~1, disp = ~group, family = poisson, data = repro_wide)


#model ranking via aic
model.sel(list_lrs, rank = "AIC")
modelchecker(list_lrs, filename = "outputs/zi_lrs_")

#best model is compois w/ env zi
summary(list_lrs[[9]])
s1a <- simulateResiduals(list_lrs[[9]], plot = T, quantreg = T)
testZeroInflation(s1a)
testDispersion(s1a)
check_predictions(list_lrs[[9]])
testCategorical(s1a,repro_wide$env) 
testCategorical(s1a,repro_wide$regime) 
testCategorical(s1a, repro_wide$group)
hist(resid(list_lrs[[9]]))
check_convergence(list_lrs[[9]])


#summary
tab_model(list_lrs[[9]],
          transform = NULL,
          show.ci = FALSE,
          show.se = TRUE,
          show.r2 = FALSE,
          show.stat = TRUE,
          show.icc = FALSE,
          file = "outputs/two_way_lrs.doc")

#model means + bias adjustment
lme4::VarCorr(list_lrs[[9]])
sigma <- sqrt(2.9694e-05^2)

em1a <- emmeans(list_lrs[[9]],
                specs = ~ regime * env ,
                bias.adjust = T,
                sigma = sigma)

confint(em1a, calc = c(n = ~.wgt.), adjust = "mvt",
        type = "response")

#model contrasts
summary(contrast(em1a, "pairwise",
                 combine = TRUE,
                 simple = list( "regime", "env"),
                 adjust = "mvt"),bias.adjust = T, sigma = sigma,
        type = "response", infer = c(TRUE, TRUE))

#save
summary(contrast(em1a, "pairwise",
                 combine = TRUE,
                 simple = list( "regime", "env"),
                 adjust = "mvt"),bias.adjust = T, sigma = sigma,
        type = "response", infer = c(TRUE, TRUE)) %>%
  print() %>%
  write_csv("outputs/two_way_lrs_emmeans.csv")


# DT######
list_dt <- list()
list_dt[[1]] <- glmmTMB(log(devt_time) ~
                 regime + env +
                 regime:env +
                 (1|group/id),
               REML = TRUE, 
               data = devtime,
               family = gaussian)

summary(list_dt[[1]])
s1a <- simulateResiduals(list_dt[[1]], plot = T, quantreg = T)
testCategorical(s1a, devtime$env) 
testCategorical(s1a, devtime$regime) 
testCategorical(s1a, devtime$group)

#heteroscedatiecty detected run with dispersion

list_dt[[2]] <- glmmTMB(log(devt_time) ~ regime + env + regime:env + (1|group/id),
               REML = TRUE, 
               disp = ~ env,
               data = devtime,
               family = gaussian)

list_dt[[3]] <- glmmTMB(log(devt_time) ~ regime + env + regime:env + (1|group/id),
                        REML = TRUE, 
                        disp = ~ regime,
                        data = devtime,
                        family = gaussian)


list_dt[[4]] <- glmmTMB(log(devt_time) ~ regime + env + regime:env + (1|group/id),
                        REML = TRUE, 
                        disp = ~ env + regime,
                        data = devtime,
                        family = gaussian)

list_dt[[5]] <- glmmTMB(log(devt_time) ~ regime + env + regime:env + (1|group/id),
                        REML = TRUE, 
                        disp = ~ env * regime,
                        data = devtime,
                        family = gaussian)

list_dt[[6]] <- glmmTMB(log(devt_time) ~ regime + env + regime:env + (1|group/id),
                        REML = TRUE, 
                        disp = ~group,
                        data = devtime,
                        family = gaussian)

model.sel(list_dt, rank = "AIC")

#best fitting dispersion model
summary(list_dt[[6]])
s1a <- simulateResiduals(list_dt[[6]], plot = T, quantreg = T)
testDispersion(s1a)
check_predictions(list_dt[[6]])
testCategorical(s1a,devtime$env) 
testCategorical(s1a,devtime$regime) 
testCategorical(s1a, devtime$group)
hist(resid(list_dt[[6]]))
check_convergence(list_dt[[6]])

#still heteroskedastic - robust linear
m1a <- rlmer(log(devt_time) ~
               regime + env +
               regime:env +
               (1|group/id),
             REML = TRUE, 
             data = devtime)

summary(m1a)
hist(resid(m1a))
check_heteroscedasticity(m1a)

tab_model(m1a,
          transform = NULL,
          show.ci = FALSE,
          show.se = TRUE,
          show.r2 = FALSE,
          show.stat = TRUE,
          show.icc = FALSE,
          file = "outputs/robust_dt.doc")

#model means - no bias adjustment needed
em1a <- emmeans(m1a,
                specs = ~ regime * env, type = "response")

confint(em1a, calc = c(n = ~.wgt.), adjust = "mvt",
        type = "response")

#model contrasts
summary(contrast(em1a, "pairwise",
                 combine = TRUE,
                 simple = list("regime", "env"),
                 adjust = "mvt"),
        type = "response", infer = c(TRUE, TRUE)) %>%
  print() %>%
  write_csv("outputs/two_way_dt_emmeans.csv")


