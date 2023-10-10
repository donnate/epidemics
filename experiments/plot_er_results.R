library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/er_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/er_graph/"
file_list <- list.files(folder_path, pattern = "^9", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
#data <- map_dfr(file_list, read_csv)
data_list <- lapply(file_list, function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  data$filename <- file
  return(data)
})
# Combine all individual data frames into one data frame
data <- do.call(rbind, data_list)

colnames(data)
#### first group by experiment
res  = data %>%
  group_by(filename, lambda, beta_epid, gamma_epid, n, proba_er, 
           steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise_all(mean)

### 
res_all  = res %>%
  ungroup() %>%
  select(-`filename`) %>%
  group_by(lambda, beta_epid, gamma_epid, n, proba_er, 
           steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise_all(mean)

dim(res_all)

unique(res_all$lambda)
unique(res_all$proba_er)
unique(res_all$beta_epid)

ggplot(res_all %>% filter(
                          nb_init == 1),
       aes(x=lambda, l1_error))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  geom_hline(aes(yintercept = oracle )) + 
  facet_wrap(beta_epid/gamma_epid~p_norm)


ggplot(res_all %>% filter(power_pa==1.2,
                          nb_init == 1),
       aes(x=lambda, risk_observed))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  geom_hline(aes(yintercept = oracle )) + 
  facet_wrap(beta_epid/gamma_epid~p_norm)


ggplot(res_all %>% filter(power_pa==1.2,
                          nb_init == 1), aes(x=lambda, l1_error))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(beta_epid/gamma_epid~p_norm)

ggplot(res_all %>% filter(power_pa==1.2,
                          nb_init == 1), aes(x=lambda, l2_error))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(beta_epid/gamma_epid~p_norm)

ggplot(res_all %>% filter(power_pa==1.2,
                          nb_init == 10), aes(x=lambda, l2_error))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  facet_wrap(beta_epid/gamma_epid~p_norm)





