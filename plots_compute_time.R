library(tidyverse)
setwd("~/Documents/epidemic_modelling/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"
file_list <- list.files(folder_path, pattern = "^results_algo10*", full.names = TRUE)
file_list#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- map_dfr(file_list, read_csv)
#data_list <- lapply(file_list, function(file) {
#  data <- read.csv(file, stringsAsFactors = FALSE)
#  data$filename <- file
#  return(data)
#})
# Combine all individual data frames into one data frame
#data <- do.call(rbind, data_list)

unique(data$n)
colnames(data)
unique(data$lambda)
#### first group by experiment
res  = data %>%
  group_by(Method, Lambda, n, p_er) %>%
  summarise(med_time = median(Time),
            q25_time = quantile(Time,0.25),
            q75_time = quantile(Time,0.75),
            n = n()
            ) %>%
  ungroup()

colnames(res)
ggplot(res, aes(x=Lambda, med_time, colour=Method))+
  geom_line(linewidth=1.)+
  geom_point()+
  geom_errorbar(aes(ymin=q25_time, ymax=q75_time))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(n~p_er,
             scales = "free_y",
             labeller = as_labeller(c(`0.005` = "p_er = 0.005",
                                     `0.02` = "p_er = 0.02",
                                     `200` = "n = 200",
                                     `500` = "n = 500",
                                     `800` = "n = 800",
                                     `1000` = "n = 1000",
                                     `5000` = "n = 5000"
                            ))
             )+
  xlab("lambda (Regularization Strength)") + 
  ylab("Compute time (s)") +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res %>% filter(#power_pa == 1.2,
  gamma_epid == 0.1,
  diffuse==1,
  p_norm == 1,
  propagation == "true_p",
  #beta_epid != 0.5,
  nb_init == 1),
  aes(x=lambda, l1_error_21))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    gamma_epid == 0.1,
    diffuse==1,
    propagation == "true_p",
    #beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, proba_er) %>%
      summarise(benchmark_l1_error_21 =median(benchmark_l1_error_21)),
    aes(yintercept=benchmark_l1_error_21, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(beta_epid/gamma_epid~proba_er,
             scales = "free_y", ncol = 4,
             labeller = as_labeller(c(`0.005` = "proba_er = 0.005",
                                      `0.001` = "proba_er = 0.001",
                                      `0.01` = "proba_er = 0.01",
                                      `0.1` = "proba_er = 0.1",
                                      `50` = "R0 = 50",
                                      `10` = "R0 = 10",
                                      `1` = "R0 = 1",
                                      `80` = "R0 = 80",
                                      `9` = "R0 = 9",
                                      `0.5` = "R0 = 0.5",
                                      `90` = "R0 = 90",
                                      `5`= "R0 = 5")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res %>% filter(#power_pa == 1.2,
  gamma_epid == 0.1,
  diffuse==10,
  p_norm == 1,
  propagation == "y",
  #beta_epid != 0.5,
  nb_init == 1),
  aes(x=lambda, l1_error_21))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    gamma_epid == 0.1,
    diffuse==10,
    propagation == "y",
    #beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, proba_er) %>%
      summarise(benchmark_l1_error_21 =median(benchmark_l1_error_21)),
    aes(yintercept=benchmark_l1_error_21, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(beta_epid/gamma_epid~proba_er,
             scales = "free_y", ncol = 4,
             labeller = as_labeller(c(`0.005` = "proba_er = 0.005",
                                      `0.001` = "proba_er = 0.001",
                                      `0.01` = "proba_er = 0.01",
                                      `0.1` = "proba_er = 0.1",
                                      `50` = "R0 = 50",
                                      `10` = "R0 = 10",
                                      `1` = "R0 = 1",
                                      `80` = "R0 = 80",
                                      `9` = "R0 = 9",
                                      `0.5` = "R0 = 0.5",
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





