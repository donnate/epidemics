library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/pa_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/pa_graph/"
file_list <- list.files(folder_path, pattern = "^96", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- map_dfr(file_list, read_csv)
data_list <- lapply(file_list, function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  data$filename <- file
  return(data)
})
# Combine all individual data frames into one data frame
data <- do.call(rbind, data_list)

#### first group by experiment
res  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise_all(mean) %>%
  ungroup()

### 
res_all  = res %>%
  ungroup() %>%
  select(-`filename`) %>%
  group_by(lambda, beta_epid, gamma_epid, n, power_pa, steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise_all(mean) %>%
  ungroup()

res_all = res 
dim(res_all)

unique(res_all$lambda)
unique(res_all$power_pa)
unique(res_all$beta_epid)
unique(res_all$gamma_epid)

ggplot(res_all %>% filter(power_pa==1.2,
                          #gamma_epid == 0.1,
                          p_norm == 1,
                          lambda<0.01,
                          nb_init == 1),
       aes(x=lambda, l1_error))+
  geom_line()+
  scale_x_log10()+
  scale_y_log10() +
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       p_norm == 1,
                                       nb_init == 1) %>% summarise(oracle=mean(oracle)),
             aes(yintercept=oracle, colour = "oracle")) + 
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       p_norm == 1,
                                       nb_init == 1) %>% summarise(risk_observed=mean(risk_observed)),
             aes(yintercept=risk_observed, colour = "benchmark")) + 
  facet_wrap(beta_epid/gamma_epid~p_norm)


ggplot(data %>% filter(power_pa==1.2,
                          #gamma_epid == 0.1,
                          lambda<1,
                          nb_init == 1),
       aes(x=lambda, l1_error))+
  geom_smooth()+
  scale_x_log10()+
  scale_y_log10() +
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       nb_init == 1) %>% summarise(oracle=mean(oracle)),
             aes(yintercept=oracle, colour = "oracle")) + 
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       nb_init == 1) %>% summarise(risk_observed=mean(risk_observed)),
             aes(yintercept=risk_observed, colour = "benchmark")) + 
  facet_grid(beta_epid/gamma_epid~p_norm)



ggplot(data %>% filter(power_pa==1.2,
                       #gamma_epid == 0.1,
                       p_norm == "inf",
                       lambda<1,
                       nb_init == 1),
       aes(x=lambda, l1_error))+
  geom_smooth()+
  scale_x_log10()+
  scale_y_log10() +
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       p_norm == "inf",
                                       nb_init == 1) %>% summarise(oracle=mean(oracle)),
             aes(yintercept=oracle, colour = "oracle")) + 
  geom_hline(data = res_all %>% filter(power_pa==1.2,
                                       #gamma_epid == 0.1,
                                       p_norm == "inf",
                                       nb_init == 1) %>% summarise(risk_observed=mean(risk_observed)),
             aes(yintercept=risk_observed, colour = "benchmark")) + 
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





