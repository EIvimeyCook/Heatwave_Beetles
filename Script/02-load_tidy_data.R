# General Data#######
development_time <- read_csv("Data/Analysis_dat.csv",
                             col_types = cols(Comments = col_character())) %>%
janitor::clean_names() %>%
  convert(fct(id, treatment, replicate, pair_id, group)) %>%
  mutate(regime = case_when(
    treatment == "F-C" ~ "Fluctuating",
    treatment == "F-F" ~ "Fluctuating",
    treatment == "H-C" ~ "Heatwave",
    treatment == "H-H" ~ "Heatwave")) %>%
  mutate(env = case_when(
    treatment == "F-C" ~ "Constant",
    treatment == "F-F" ~ "Fluctuating",
    treatment == "H-C" ~ "Constant",
    treatment == "H-H" ~ "Fluctuating")) %>%
  convert(fct(regime, env)) 

# LRS#######
#create total sum of data for LRS
repro_wide <- development_time %>%
  group_by(id,group, pair_id,replicate, treatment, regime, env,  day_egg_laid) %>%
  summarise(totrep = sum(total)) %>%
  spread(day_egg_laid, totrep) %>%
  replace(is.na(.), 0) %>%
  rename(day_0 = "0", day_1 = "1", day_2 = "2+") %>%
  mutate(lrs = sum(c_across(day_0:day_2))) %>%
  as.data.frame() %>%
  mutate(obs = seq_len(nrow(.))) %>%
  convert(fct(obs)) 

# Development time#########

#loop over each row to create a development_time for each individual.
dtlist <- list()
for (x in seq_len(nrow(development_time))) {

  store <- development_time[x, ] %>%
    slice(rep(seq_along(n()), each = development_time$total[x]))

  if (development_time[x, ]$males[1] == "0"  &&
     development_time[x, ]$females[1] == "0") {
    sex <- rep("NA", length = development_time[x, ]$total[1])
  } else {
    sex <- c(rep("M", length = development_time[x, ]$males[1]),
             rep("F", length = development_time[x, ]$females[1]))
  }

  dtlist[[x]] <- cbind(store, sex)

}

devtime <- data.table::rbindlist(dtlist)

#remove individuals where sex is not known - none removed.
devtime %<>% convert(fct(sex)) %>%
  filter(sex != "NA")