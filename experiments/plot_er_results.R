library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/er_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/er_graph/"
file_list <- list.files(folder_path, pattern = "^res_984", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- map_dfr(file_list, read_csv)
#data_list <- lapply(file_list, function(file) {
#  data <- read.csv(file, stringsAsFactors = FALSE)
#  data$filename <- file
#  return(data)
#})
# Combine all individual data frames into one data frame
#data <- do.call(rbind, data_list)

colnames(data)
#### first group by experiment
res  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, proba_er, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise_all(median) %>%
  ungroup()

data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, proba_er, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(c=n()) %>%
  ungroup()

res2  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, proba_er, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.25,na.), .names = "q.25_{.col}")) %>%
  ungroup()
res3  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, proba_er, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.75), .names = "q.75_{.col}")) %>%
  ungroup()

res4  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~mean(.x), .names = "mean_{.col}")) %>%
  ungroup()

res = merge(res, res2, by = c("lambda", "beta_epid", "gamma_epid", 
                              "n","power_pa", "steps", "heterogeneity_rates", "nb_init", "p_norm",
                              "diffuse", "mode", "propagation"))
res = merge(res, res3, by = c("lambda", "beta_epid", "gamma_epid", 
                              "n","power_pa", "steps", "heterogeneity_rates",
                              "nb_init", "p_norm", "diffuse", "mode", "propagation"))
res = merge(res, res4, by = c("lambda", "beta_epid", "gamma_epid", 
                              "n","power_pa", "steps", "heterogeneity_rates",
                              "nb_init", "p_norm", "diffuse", "mode", "propagation"))
dim(res)

unique(res$beta_epid)
unique(res$gamma_epid)
unique(res$proba_er)
unique(res$propagation)
unique(res$mode)

ggplot(res %>% filter(#power_pa == 1.2,
  #gamma_epid == 0.1,
  diffuse==1,
  p_norm == 1,
  beta_epid != 0.5,
  nb_init == 1),
  aes(x=lambda, l1_error_1))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_1, ymax=q.75_l1_error_1))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==1,
    propagation == "y",
    beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, proba_er) %>%
      summarise(benchmark_l1_error_1 =median(benchmark_l1_error_1)),
    aes(yintercept=benchmark_l1_error_1, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(beta_epid/gamma_epid~proba_er,
             scales = "free_y", ncol = 3,
             labeller = as_labeller(c(`0.2` = "power_PA = 0.2",
                                      `1.2` = "power_PA = 1.2",
                                      `3` = "power_PA = 3.0",
                                      `50` = "R0 = 50",
                                      `10` = "R0 = 10",
                                      `1` = "R0 = 1",
                                      `80` = "R0 = 80",
                                      `90` = "R0 = 90",
                                      `5`= "R0 = 5")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





unique(res$lambda)
unique(res$power_pa)
unique(res$beta_epid)
unique(res$gamma_epid)
unique(res$nb_init)
unique(res$p_norm)
unique(res$steps)
unique(res$mode)
unique(res$diffuse)
unique(res$propagation)
colnames(res)
colnames(res)
### 


dim(res_all)

unique(res_all$lambda)
unique(res_all$proba_er)
unique(res_all$beta_epid)

ggplot(res %>% filter(
                       p_norm == 1),
       aes(x=lambda, l1_error))+
  geom_line(linewidth=1.2)+
  geom_point(alpha=0.2)+
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(steps~proba_er)

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





