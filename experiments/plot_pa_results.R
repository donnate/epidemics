library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/pa_graph/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/experiments/results/pa_graph"
file_list <- list.files(folder_path, pattern = "^res_98", full.names = TRUE)
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
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise_all(median) %>%
  ungroup()

data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(c=n()) %>%
  ungroup()

res2  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.25), .names = "q.25_{.col}")) %>%
  ungroup()
res3  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~quantile(.x, probs = 0.75), .names = "q.75_{.col}")) %>%
  ungroup()

res4  = data %>%
  group_by(lambda, beta_epid, gamma_epid, 
           n, power_pa, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise(across(everything(), ~mean(.x), .names = "mean{.col}")) %>%
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


ggplot(res %>% filter(#power_pa == 1.2,
  #gamma_epid == 0.1,
  diffuse==1,
  propagation == "y",
  p_norm == 1,
  beta_epid != 0.5,
  nb_init == 1) %>%
    mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
  aes(x=lambda, l1_propagated_error_1))+
  geom_line(linewidth=1.2)+
  geom_point()+
  #geom_errorbar(aes(ymin=q.25_l1_propagated_error_1, ymax=q.75_l1_propagated_error_1))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==1,
    propagation == "y",
    beta_epid != 0.5,
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, p_sw) %>%
      summarise(benchmark_l1_error_1 =median(benchmark_l1_error_1)) %>%
      mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)),
    aes(yintercept=benchmark_l1_error_1, colour = "Benchmark"),
    linewidth=1.) +
  facet_wrap(r0~p_sw,
             scales = "free_y", ncol = 3,
             labeller = as_labeller(c(`0.2` = "power_PA = 0.2",
                                      `1.2` = "power_PA = 1.2",
                                      `3` = "power_PA = 3.0",
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



ggplot(res %>% filter(#power_pa == 1.2,
                          #gamma_epid == 0.1,
                          diffuse==20,
                          propagation == "true_p",
                          p_norm == 1,
                          beta_epid != 0.5,
                          nb_init == 1),
       aes(x=lambda, l1_error_21))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==20,
    propagation == "true_p",
    p_norm == 1,
    beta_epid != 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, power_pa) %>% 
      summarise(benchmark_l1_error_21 =median(benchmark_l1_error_21)),
    aes(yintercept=benchmark_l1_error_21, colour = "Benchmark"),
                 linewidth=1.) + 
  facet_wrap(beta_epid/gamma_epid~power_pa,
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




ggplot(data %>% filter(  diffuse==1,
                         propagation == "y",
                         lambda< 5 * 1e-3,
                         lambda>1e-3,
                         #power_pa==1.2,
                         p_norm == 1),
       aes(x=median_degree, y = benchmark_l1_error_1))+
  geom_point() +
  geom_smooth() + 
  facet_wrap(beta_epid/gamma_epid~power_pa,
             scales = "free", nrow=5)+
  xlab("Degree of Patient 0") + 
  ylab(expression(italic(l[1]) ~ "error for the benchmark")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
ggplot(res %>% filter(#power_pa == 1.2,
  #gamma_epid == 0.1,
  diffuse==20,
  propagation == "y",
  p_norm == 1,
  lambda == 2.212216e-03,
  #beta_epid < 0.5,
  nb_init == 1),
  aes(x=lambda, l1_error_10))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin=q.25_l1_error_10, ymax=q.75_l1_error_10))+
  scale_x_log10()+
  scale_y_log10() +#
  geom_hline(data = res %>% filter(#power_pa == 1.2,
    #gamma_epid == 0.1,
    diffuse==20,
    propagation == "y",
    p_norm == 1,
    #beta_epid < 0.5,
    nb_init == 1) %>% group_by(beta_epid, gamma_epid, power_pa) %>% 
      summarise(benchmark_l1_error_10 =median(benchmark_l1_error_10)),
    aes(yintercept=benchmark_l1_error_10, colour = "Benchmark"),
    linewidth=1.) + 
  facet_wrap(beta_epid/gamma_epid~power_pa,
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




ggplot(res%>%
         filter(diffuse==20,
                p_norm == 1,
                mode == "denoise",
                propagation == "y",
                lambda < 1e-2, 
                lambda > 5 * 1e-4,
                nb_init == 1) %>%
         select(starts_with("l1_error_"), lambda, power_pa, gamma_epid, beta_epid)  %>%
         pivot_longer(cols = -c("lambda", "power_pa", "gamma_epid", "beta_epid"), names_to = "variable", values_to = "value") %>%
         mutate(time = as.integer(gsub("^l1_error_", "", variable)))) +
  geom_line(aes(x=time, y=value, colour = as.factor(lambda)), linewidth=1.2) +theme_bw()+
  geom_line(data = res%>%
              filter(diffuse==20,
                     p_norm == 1,
                     mode == "denoise",
                     propagation == "y",
                     lambda < 1e-2, 
                     lambda > 5 * 1e-4,
                     nb_init == 1) %>%
              select(starts_with("benchmark_l1_error_"), lambda, power_pa, gamma_epid, beta_epid)  %>%
              pivot_longer(cols = -c("lambda", "power_pa", "gamma_epid", "beta_epid"), names_to = "variable", values_to = "value") %>%
              mutate(time = as.integer(gsub("^benchmark_l1_error_", "", variable))),
            aes(x=time, y=value, colour = "Benchmark"), linewidth=1.2) +
  facet_wrap(beta_epid/gamma_epid~power_pa,
             scales = "free_y", ncol = 3,
             labeller = as_labeller(c(`0.2` = "power_PA = 0.2",
                                      `1.2` = "power_PA = 1.2",
                                      `3` = "power_PA = 3.0",
                                      `50` = "R0 = 50",
                                      `1` = "R0 = 1",
                                      `10` = "R0 = 10",
                                      `90` = "R0 = 90",
                                      `5`= "R0 = 5")))+
  labs(color = "Lambda")

ggplot(res%>%
         select(starts_with("l1_error_"), lambda)  %>%
         pivot_longer(cols = -c("lambda"), names_to = "variable", values_to = "value") %>%
         mutate(time = as.integer(gsub("^l1_error_", "", variable))) %>%
         filter(time %in% c(1, 10, 15, 20, 30, 50, 100), lambda<1e-2),
       aes(x=lambda, y=value)) +
  scale_y_log10() +
  scale_x_log10() + 
  geom_point()  + 
  geom_line() +theme_bw() +
  facet_grid(~time)


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





