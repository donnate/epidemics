library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/pa_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/pa_graph"
file_list <- list.files(folder_path, pattern = "^res_979", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- map_dfr(file_list, read_csv)
#data_list <- lapply(file_list, function(file) {
#  data <- read.csv(file, stringsAsFactors = FALSE)
#  data$filename <- file
#3  return(data)
#})
# Combine all individual data frames into one data frame
#data <- do.call(rbind, data_list)

#### first group by experiment
res  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise_all(median) %>%
  ungroup()
res2  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.25), .names = "q.25_{.col}")) %>%
  ungroup()
res3  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.75), .names = "q.75_{.col}")) %>%
  ungroup()

res = merge(res, res2, by = c("lambda", "beta_epid", "gamma_epid", 
                              "n","power_pa", "steps", "heterogeneity_rates", "nb_init", "p_norm"))
res = merge(res, res3, by = c("lambda", "beta_epid", "gamma_epid", 
                              "n","power_pa", "steps", "heterogeneity_rates", "nb_init", "p_norm"))

res_all = res 
dim(res_all)

unique(res_all$lambda)
unique(res_all$power_pa)
unique(res_all$beta_epid)
unique(res_all$gamma_epid)
unique(res_all$nb_init)
unique(res_all$p_norm)
unique(res_all$steps)
colnames(res_all)

ggplot(res %>% filter(#power_pa == 1.2,
                          #gamma_epid == 0.1,
                          p_norm == 1,
                          lambda < 1e-2,
                          nb_init == 1),
       aes(x=lambda, l1_error))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error, ymax=q.75_l1_error))+
  scale_x_log10()+
  scale_y_log10() +
  #geom_hline(aes(yintercept=oracle, colour = "oracle"),
  #               linewidth=1.) + 
  facet_grid(beta_epid/gamma_epid~power_pa,
             labeller = as_labeller(c(`0.2` = "power_PA = 0.2",
                                      `1.2` = "power_PA = 1.2",
                                      `3` = "power_PA = 3.0",
                                      `0.5` = "R0 = 0.5",
                                      `1` = "R0 = 1",
                                      `8`= "R0 = 8",
                                      `9`= "R0 = 9")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data %>% filter(#gamma_epid == 0.1,
                          p_norm == 1),
       aes(x=lambda, l1_propagated_error_20))+
  geom_smooth(linewidth=1.2)+
  geom_point(alpha=0.2)+
  scale_x_log10()+
  facet_grid(steps~power_pa + nb_init)+
  xlab("lambda (Regularization Strength)") + 
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data %>% filter(nb_init ==1,
                       beta_epid == 0.9,
                       p_norm == 1),
       aes(x=lambda, l1_error))+
  geom_hline(
    aes(yintercept=oracle,  alpha=0.01,
        colour = "oracle"),
    linewidth=1.2) + 
  geom_hline(aes(yintercept=risk_observed, 
                 alpha=0.01, colour = "benchmark"),
             linewidth=1.2) + 
  geom_smooth(linewidth=1.2)+
  geom_point(alpha=0.02)+
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(.~power_pa)+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res_all %>% filter(nb_init ==1,
                       beta_epid == 0.9,
                       gamma_epid == 0.1,
                       lambda<0.01,
                       p_norm == 1),
       aes(x=lambda, l1_error - oracle))+
  #geom_hline(aes(yintercept=risk_observed, 
  #               alpha=0.01, colour = "benchmark"),
  #           linewidth=1.2) + 
  geom_point(data = res_all %>% filter(nb_init ==1,
                                       beta_epid == 0.9,
                                       gamma_epid == 0.1,
                                       lambda<0.01,
                                       p_norm == 1),
             aes(x=lambda, l1_error - oracle),
             alpha=0.02)+
  geom_line(linewidth=1.2)+
  scale_x_log10()+
  #scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~power_pa, labeller = as_labeller(c(`0.1` = "power_PA = 0.1",
                                                  `0.5` = "power_PA = 0.5",
                                                  `0.7` = "power_PA = 0.7",
                                                  `1.2` = "power_PA = 1.2",
                                                  `1.5` = "power_PA = 1.5",
                                                  `3` = "power_PA = 3",
                                                  `9` = "R0=9")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error - Oracle")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res_all %>% filter(nb_init ==1,
                          beta_epid == 0.9,
                          gamma_epid == 0.1,
                          lambda<0.01,
                          p_norm == 1),
       aes(x=lambda, l1_error - risk_observed))+
  #geom_hline(aes(yintercept=risk_observed, 
  #               alpha=0.01, colour = "benchmark"),
  #           linewidth=1.2) + 
  geom_point(data = res_all %>% filter(nb_init ==1,
                                       beta_epid == 0.9,
                                       gamma_epid == 0.1,
                                       lambda<0.01,
                                       p_norm == 1),
             aes(x=lambda, l1_error - risk_observed),
             alpha=0.02)+
  geom_line(linewidth=1.2)+
  scale_x_log10()+
  #scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~power_pa, labeller = as_labeller(c(`0.1` = "power_PA = 0.1",
                                                                     `0.5` = "power_PA = 0.5",
                                                                     `0.7` = "power_PA = 0.7",
                                                                     `1.2` = "power_PA = 1.2",
                                                                     `1.5` = "power_PA = 1.5",
                                                                     `3` = "power_PA = 3",
                                                                     `9` = "R0=9")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error - Benchmark")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data %>% filter(nb_init ==1,
                       beta_epid == 0.9,
                       gamma_epid == 0.1,
                       lambda<0.01,
                       p_norm == 1),
       aes(x=average_degree, l1_error - oracle))+
  #geom_hline(aes(yintercept=risk_observed, 
  #               alpha=0.01, colour = "benchmark"),
  #           linewidth=1.2) + 
  geom_point(alpha=0.02)+
  geom_smooth(linewidth=1.2)+
  scale_x_log10()+
  #scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~power_pa, labeller = as_labeller(c(`0.1` = "power_PA = 0.1",
                                                                     `0.5` = "power_PA = 0.5",
                                                                     `0.7` = "power_PA = 0.7",
                                                                     `1.2` = "power_PA = 1.2",
                                                                     `1.5` = "power_PA = 1.5",
                                                                     `3` = "power_PA = 3",
                                                                     `9` = "R0=9")))+
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error - Benchmark")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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


custom_labeller <- function(variable, value) {
  if (value == 1 || value == "inf"){
    return(paste0("l-", value, " norm"))
  }
  return(paste0("R0=", value))
}
ggplot(data %>% filter(power_pa==1.2,
                       #gamma_epid == 0.1,
                       #p_norm == 1,
                       lambda==0.001,
                       nb_init == 1),
       aes(x=average_degree, l1_error))+
  geom_point()+
  geom_smooth() + 
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~p_norm, labeller = label_context)


ggplot(data %>% filter(power_pa==1.2,
                       #gamma_epid == 0.1,
                       #p_norm == 1,
                       lambda==0.001,
                       nb_init == 1),
       aes(x=average_cls, l1_error))+
  geom_point()+
  geom_smooth() + 
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~p_norm, labeller = label_context)

ggplot(data %>% filter(power_pa==1.2,
                       #gamma_epid == 0.1,
                       #p_norm == 1,
                       lambda==0.001,
                       nb_init == 1),
       aes(x=average_btw, l1_error))+
  geom_point()+
  geom_smooth() + 
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~p_norm, labeller = label_context)


ggplot(data %>% filter(
                       #gamma_epid == 0.1,
                       #p_norm == "inf",
                       lambda==0.001,
                       nb_init == 1),
       aes(x=average_btw, l1_error))+
  geom_point()+
  geom_smooth() + 
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~power_pa)

ggplot(data %>% filter(
  #gamma_epid == 0.1,
  p_norm ==1),
  aes(x=lambda, l1_error))+
  geom_point()+
  geom_smooth() + 
  scale_x_log10()+
  scale_y_log10() +
  facet_grid(beta_epid/gamma_epid~nb_init)


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





