library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/dcsbm_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/dcsbm_graph/"
file_list <- list.files(folder_path, pattern = "^new_res_99", full.names = TRUE)
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
#data <- read_csv("~/Documents/epidemic_modelling/experiments/results/dcsbm_graph/test_local.csv")
#### first group by experiment
unique(data$n)
res  = data %>%
  filter( n > 600) %>%
  group_by(lambda, beta_epid, gamma_epid, alpha_fp,
           dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise_if(is.numeric, median) %>%
  ungroup()

res2  = data %>%
  filter( n > 600) %>%
  group_by(lambda, beta_epid, gamma_epid, alpha_fp,
           dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.25), .names = "q.25_{.col}")) %>%
  ungroup()

res3  = data %>%
  filter( n > 600) %>%
  group_by(lambda, beta_epid, gamma_epid, alpha_fp,
           dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.75), .names = "q.75_{.col}")) %>%
  ungroup()

res4  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~mean(.x), .names = "mean_{.col}")) %>%
  ungroup()

res = merge(res, res2, by = c("lambda", "beta_epid", "gamma_epid", "alpha_fp",
                              "dc_heterogeneity", "steps", "heterogeneity_rates", "nb_init", "p_norm",
                              "diffuse", "mode", "propagation"))
res = merge(res, res3, by = c("lambda", "beta_epid", "gamma_epid", "alpha_fp",
                              "dc_heterogeneity", "steps", "heterogeneity_rates",
                              "nb_init", "p_norm", "diffuse", "mode", "propagation"))
dim(res)

data %>%
  group_by(lambda, beta_epid, gamma_epid,  dc_heterogeneity, steps, 
           heterogeneity_rates, nb_init, p_norm) %>%
  summarise(c = n())
### 

res_all = res 
dim(res_all)

unique(res$lambda)
unique(res$dc_heterogeneity)
unique(res$beta_epid)
unique(res$diffuse)
unique(res$gamma_epid)
unique(res$nb_init)
unique(res$p_norm)
unique(res$diffuse)
unique(res$steps)
unique(res$alpha_fp)
ggplot(res %>% filter(#power_pa == 1.2,
  #gamma_epid == 0.1,
  alpha_fp == 0.001,
  diffuse==10,
  propagation == "true_p",
  p_norm == 1,
  gamma_epid == 0.01,
  beta_epid != 0.5,
  nb_init == 1) %>%
    mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
  aes(x=lambda, l1_error_31))+
  geom_line(linewidth=1.2)+
  geom_point()+
  #geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  #geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==10,
    propagation == "true_p",
    beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, dc_heterogeneity) %>%
      summarise(benchmark_l1_error_31 =median(benchmark_l1_error_31)) %>%
      mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
    aes(yintercept=benchmark_l1_error_31, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(r0~dc_heterogeneity,
             scales = "free_y", ncol = 2)+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(res %>% filter(#power_pa == 1.2,
  #gamma_epid == 0.1,
  diffuse==1,
  propagation == "true_p",
  p_norm == 1,
  beta_epid != 0.5,
  nb_init == 1) %>%
    mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
  aes(x=lambda, l1_error_21))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==1,
    propagation == "true_p",
    beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, dc_heterogeneity) %>%
      summarise(benchmark_l1_error_21 =median(benchmark_l1_error_21)) %>%
      mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
    aes(yintercept=benchmark_l1_error_21, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(r0~dc_heterogeneity,
             scales = "free_y", ncol = 2,
             labeller = as_labeller(c(`0.2` = "power_PA = 0.2",
                                      `1` = "dc_heterogeneity = 1",
                                      `3` = "dc_heterogeneity = 3",
                                      `R0= 50` = "R0 = 50",
                                      `R0 = 10` = "R0 = 10",
                                      `R0 = 1` = "R0 = 1",
                                      `R0 = 80` = "R0 = 80",
                                      `R0 = 90` = "R0 = 90",
                                      `R0 = 5`= "R0 = 5")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data %>% filter(power_pa==1.2,
                          #gamma_epid == 0.1,
                          lambda<1,
                          nb_init == 10),
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





