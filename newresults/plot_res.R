library(tidyverse)
setwd("~/Documents/epidemic_modelling/newresults/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/newresults"
file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- bind_rows(lapply(file_list, read.csv))
#data_list <- lapply(file_list, function(file) {
#  data <- read.csv(file, stringsAsFactors = FALSE)
#  data$filename <- file
#  return(data)
#})
# Combine all individual data frames into one data frame
#data <- do.call(rbind, data_list)
#data <- read_csv("~/Documents/epidemic_modelling/experiments/results/dcsbm_graph/test_local.csv")
#### first group by experiment
unique(data$n_nodes)
#data  = data[which(is.na(data$l1_error_100)==FALSE),]
data  = data %>% mutate(real_beta = ifelse(Method == "SSNAL-opt", gamma, beta),
                        real_gamma = ifelse(Method == "SSNAL-opt", beta, gamma))
res_agg  = data %>%
  group_by(Lambda, graph_type, real_gamma, real_beta,
           Method, alpha_fp, p,m, 
           steps, n_step_predict ) %>%
  summarize(Accuracy_true_p_mean = mean(Accuracy_true_p),
          Accuracy_true_p_q25 = quantile(Accuracy_true_p, 0.25),
          Accuracy_true_p_q50 = quantile(Accuracy_true_p, 0.50),
          Accuracy_true_p_q75 = quantile(Accuracy_true_p, 0.75),
          Accuracy_true_p_pos_mean = mean(Accuracy_true_p_pos),
          Accuracy_true_p_pos_q25 = quantile(Accuracy_true_p_pos, 0.25),
          Accuracy_true_p_pos_q50 = quantile(Accuracy_true_p_pos, 0.50),
          Accuracy_true_p_pos_q75 = quantile(Accuracy_true_p_pos, 0.75),
          Accuracy_true_p_neg_mean = mean(Accuracy_true_p_neg),
          Accuracy_true_p_neg_q25 = quantile(Accuracy_true_p_neg, 0.25),
          Accuracy_true_p_neg_q50 = quantile(Accuracy_true_p_neg, 0.50),
          Accuracy_true_p_neg_q75 = quantile(Accuracy_true_p_neg, 0.75),
          bench_Accuracy_true_p_mean = mean(bench_Accuracy_true_p),
          bench_Accuracy_true_p_q25 = quantile(bench_Accuracy_true_p, 0.25),
          bench_Accuracy_true_p_q50 = quantile(bench_Accuracy_true_p, 0.50),
          bench_Accuracy_true_p_q75 = quantile(bench_Accuracy_true_p, 0.75),
          accuracy_benchmark_prop_10_mean = quantile(accuracy_benchmark_prop_10, 0.5),
          accuracy_prop_10_mean = quantile(accuracy_prop_10, 0.5),
          final_number_infected_mean = mean(final_number_infected),
          final_number_infected_q50 = quantile(final_number_infected, 0.5),
          # bench_Accuracy_true_p_pos_q25 = quantile(bench_Accuracy_true_p_pos, 0.25),
          # bench_Accuracy_true_p_pos_q50 = quantile(bench_Accuracy_true_p, 0.50),
          # bench_Accuracy_true_p_pos_q75 = quantile(bench_Accuracy_true_p, 0.75),
          # bench_Accuracy_true_p_neg_q25 = quantile(bench_Accuracy_true_p, 0.25),
          # bench_Accuracy_true_p_neg_q50 = quantile(bench_Accuracy_true_p, 0.50),
          # bench_Accuracy_true_p_neg_q75 = quantile(bench_Accuracy_true_p, 0.75),
          )




### 

res_all = res_agg 
dim(res_agg)

unique(res_agg$real_beta)
opt = res_agg %>%
  filter(Method == "SSNAL-opt" | (Method == "SSNAL" && Lambda == 1e-4)) %>%
  filter(real_beta>0.1,
         alpha_fp == 0, graph_type == "knn") %>%
  dplyr::select(Lambda, graph_type, real_beta, alpha_fp, p, m, steps, 
                Accuracy_true_p_mean, Accuracy_true_p_pos_mean,
                Accuracy_true_p_q50, bench_Accuracy_true_p_q50,
                bench_Accuracy_true_p_mean,
                accuracy_prop_10_mean,
                accuracy_benchmark_prop_10_mean)

opt = res_agg %>%
  filter(Method == "SSNAL-opt" | (Method == "SSNAL" && Lambda == 1e-4)) %>%
  filter(real_beta>0.1,
         alpha_fp == 0, graph_type == "small-world") %>%
  dplyr::select(Lambda, graph_type, real_beta, alpha_fp, p, m, steps, 
                Accuracy_true_p_mean, Accuracy_true_p_pos_mean,
                Accuracy_true_p_q50, bench_Accuracy_true_p_q50,
                bench_Accuracy_true_p_mean,
                accuracy_prop_10_mean,
                accuracy_benchmark_prop_10_mean)

steps1 = 10
m1 = 5
graph_type1 = "knn"
gamma1 =  0.1
unique(res_agg$gamma)
ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  #m==m1,
  alpha_fp==0,
  graph_type == graph_type1,
  Method == "SSNAL",
   real_gamma == gamma1),
  aes(x=Lambda, Accuracy_true_p_pos_q50, 
      ))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_errorbar(aes(ymin = Accuracy_true_p_pos_q25,
                    ymax = Accuracy_true_p_pos_q75
                   )) + 
  # geom_hline(
  #   data=res_agg %>% filter(
  #     real_beta >0.1,
  #     steps == steps1, m==m1,
  #     graph_type == graph_type1, Method == "SSNAL-opt",
  #     real_gamma == gamma1) %>% group_by(alpha_fp, real_beta) 
  #   %>% summarise_if(is.numeric, mean),
  #   aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
  # )+
  #geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(real_beta ~ m,
           scales = "free_y",
           labeller = as_labeller(c(`0.5` = "beta=0.5",
                                    `0.9` = "beta=0.9",
                                    `pa` = "Preferential\n Attachment Graph\n(m = 5)",
                                    `ER` = "Erdos-Renyi Graph\n(p = 0.005)",
                                    `small-world` = "Small-World Graph \n (m = 5, p = 0.1)",
                                    `1` = "Small-World Graph\n(k=1)",
                                    `5` = "Small-World Graph\n(k=5)",
                                    `10` = "Small-World Graph\n(k=10)",
                                    `2` = "Small-World Graph\n(k=2)"
           ))) + 
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


steps1 = 10
graph_type1 = "small-world"
gamma1 =  0.1
m1 =5
#unique(res_agg$gamma)
ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  m == m1,
  graph_type == graph_type1,
  Method == "SSNAL",
  real_gamma == gamma1),
  aes(x=Lambda, Accuracy_true_p_pos_q50, 
      colour = Method))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_hline(
    data=res_agg %>% filter(
      real_beta >0.1,
      m == m1,
      steps == steps1, 
      graph_type == graph_type1, Method == "SSNAL-opt",
      real_gamma == gamma1) %>% group_by(alpha_fp, real_beta) 
    %>% summarise_if(is.numeric, mean),
    aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
  )+
  #geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(real_beta ~ m,
             scales = "free_y",
             labeller = as_labeller(c(`0.5` = "beta=0.5",
                           `0.9` = "beta=0.9",
                           `pa` = "Preferential\n Attachment Graph\n(m = 5)",
                           `ER` = "Erdos-Renyi Graph\n(p = 0.005)",
                           `small-world` = "Small-World Graph \n (m = 5, p = 0.1)",
                           `1` = "kNN Graph\n(k=1)",
                           `5` = "kNN Graph\n(k=5)",
                           `10` = "kNN Graph\n(k=10)"
             )))+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


steps1 = 10
graph_type1 = "pa"
gamma1 =  0.1
m1 =5
#unique(res_agg$gamma)
ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  m == m1,
  graph_type == graph_type1,
  Method == "SSNAL",
  real_gamma == gamma1),
  aes(x=Lambda, Accuracy_true_p_pos_q50, 
      colour = Method))+
  geom_line(linewidth=1.2)+
  geom_point()+
  geom_hline(
    data=res_agg %>% filter(
      real_beta >0.1,
      m == m1,
      steps == steps1, 
      graph_type == graph_type1, Method == "SSNAL-opt",
      real_gamma == gamma1) %>% group_by(alpha_fp, real_beta) 
    %>% summarise_if(is.numeric, mean),
    aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
  )+
  #geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(real_beta ~ alpha_fp,
             scales = "free_y")+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


steps1 = 10
graph_type1 = "pa"
gamma1 =  0.1
m1 = 5

unique(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  graph_type == "knn",
  alpha_fp == 0,
  Method == "SSNAL",
  real_gamma == gamma1 ) %>% ungroup() %>%dplyr::select(m))

#unique(res_agg$gamma)
ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  m == m1 | graph_type=="ER",
  alpha_fp == 0,
  Method == "SSNAL",
  real_gamma == gamma1 ),
  aes(x=Lambda, Accuracy_true_p_pos_q50))+
  geom_line(linewidth=1.2)+
  geom_point()+
  # geom_hline(
  #   data=res_agg %>% filter(
  #     real_beta >0.1,
  #     m == m1 | graph_type=="ER",
  #     steps == steps1,
  #     alpha_fp == 0,
  #     Method == "SSNAL-opt",
  #     real_gamma == gamma1) %>% group_by(alpha_fp, 
  #                                        graph_type,
  #                                        real_beta)
  #   %>% summarise_if(is.numeric, mean),
  #   aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
  # )+
  geom_errorbar( aes(x=Lambda, ymin=Accuracy_true_p_pos_q25,
                     ymax=Accuracy_true_p_pos_q75))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(graph_type ~ real_beta,
             scales = "free",labeller = as_labeller(c(`0.5` = "beta=0.5",
                                                     `0.9` = "beta=0.9",
                                                     `pa` = "Preferential\n Attachment Graph\n(m = 5)",
                                                     `ER` = "Erdos-Renyi Graph\n(p = 0.005)",
                                                     `small-world` = "Small-World Graph \n (m = 5, p = 0.1)",
                                                     `knn` = "kNN Graph\n(k=5)"
             )))+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



m1=10
ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  m == m1 | graph_type=="ER",
  alpha_fp == 0,
  Method == "SSNAL",
  real_gamma == gamma1 ),
  aes(x=Lambda, Accuracy_true_p_q50))+
  geom_line(linewidth=1.2)+
  geom_point()+
  # geom_hline(
  #   data=res_agg %>% filter(
  #     real_beta >0.1,
  #     m == m1 | graph_type=="ER",
  #     steps == steps1,
  #     alpha_fp == 0,
  #     Method == "SSNAL-opt",
  #     real_gamma == gamma1) %>% group_by(alpha_fp, 
  #                                        graph_type,
  #                                        real_beta)
  #   %>% summarise_if(is.numeric, mean),
#   aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
# )+
geom_errorbar( aes(x=Lambda, ymin=Accuracy_true_p_q25,
                   ymax=Accuracy_true_p_q75))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(graph_type ~ real_beta,
             scales = "free",labeller = as_labeller(c(`0.5` = "beta=0.5",
                                                      `0.9` = "beta=0.9",
                                                      `pa` = "Preferential\n Attachment Graph\n(m = 5)",
                                                      `ER` = "Erdos-Renyi Graph\n(p = 0.005)",
                                                      `small-world` = "Small-World Graph \n (m = 5, p = 0.1)",
                                                      `knn` = "kNN Graph\n(k=5)"
             )))+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res_agg %>% filter(
  real_beta >0.1,
  steps == steps1,
  m == m1 | graph_type=="ER",
  alpha_fp == 0,
  Method == "SSNAL",
  real_gamma == gamma1 ),
  aes(x=Lambda, Accuracy_true_p_neg_q50))+
  geom_line(linewidth=1.2)+
  geom_point()+
  # geom_hline(
  #   data=res_agg %>% filter(
  #     real_beta >0.1,
  #     m == m1 | graph_type=="ER",
  #     steps == steps1,
  #     alpha_fp == 0,
  #     Method == "SSNAL-opt",
  #     real_gamma == gamma1) %>% group_by(alpha_fp, 
  #                                        graph_type,
  #                                        real_beta)
  #   %>% summarise_if(is.numeric, mean),
#   aes(yintercept= Accuracy_true_p_pos_q50, colour="CV" )
# )+
geom_errorbar( aes(x=Lambda, ymin=Accuracy_true_p_neg_q25,
                   ymax=Accuracy_true_p_neg_q75))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(graph_type ~ real_beta,
             scales = "free",labeller = as_labeller(c(`0.5` = "beta=0.5",
                                                      `0.9` = "beta=0.9",
                                                      `pa` = "Preferential\n Attachment Graph\n(m = 5)",
                                                      `ER` = "Erdos-Renyi Graph\n(p = 0.005)",
                                                      `small-world` = "Small-World Graph \n (m = 5, p = 0.1)",
                                                      `knn` = "kNN Graph\n(k=5)"
             )))+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data %>% filter(
  steps == steps1, m==m1,
  graph_type == graph_type1,
  Method == "SSNAL",
  real_gamma == gamma1),
  aes(x=Lambda, Accuracy_true_p_pos, 
      colour = Method))+
  geom_smooth(linewidth=1.2)+
  geom_point()+
  geom_hline(
    data=res_agg %>% filter(
      steps == steps1, m==m1,
      graph_type == graph_type1, Method == "SSNAL-opt",
      real_gamma == gamma1) %>% group_by(alpha_fp, real_beta) %>% summarise_if(is.numeric, mean),
    aes(yintercept= Accuracy_true_p_pos, colour="CV" )
  )+
  #geom_errorbar(aes(ymin=q.25_l1_error_21, ymax=q.75_l1_error_21))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(real_beta ~ alpha_fp,
             scales = "free_y")+
  xlab("Lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### look at the different predictions


res0  = data %>%
  filter( n > 600) %>%
  group_by(lambda, beta_epid, gamma_epid, alpha_fp,
           dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
           diffuse, propagation) %>%
  summarise_if(is.numeric, median) %>%
  ungroup()

res0 = res0 %>% 
  select(-starts_with("l2_error"))%>% 
  select(-starts_with("benchmark_l2_error"))

colnames(res0)
test1 = pivot_longer(res0 %>% 
                       select(-starts_with("benchmark_l1_"))%>%
                       select(lambda, beta_epid, gamma_epid, alpha_fp,
                              dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
                              diffuse, propagation, starts_with("l1_")), 
                     cols =  starts_with("l1_"))
test1["time"] = sapply(test1$name, 
                       function(entry){as.numeric(sub("l1_error_([0-9]+).*", "\\1", 
                                                      entry))})

colnames(test1)[ncol(test1)-1] = "Error"
test_bench = pivot_longer(res0 %>% 
                            select(-starts_with("l1_error"))%>%
                            select(lambda, beta_epid, gamma_epid, alpha_fp,
                                   dc_heterogeneity, steps, heterogeneity_rates, nb_init, p_norm, mode,
                                   diffuse, propagation, starts_with("benchmark_l1_")), 
                          cols =  starts_with("benchmark_l1_"))
test_bench["time"] = sapply(test_bench$name, 
                            function(entry){as.numeric(sub("benchmark_l1_error_([0-9]+).*", "\\1", 
                                                           entry))})


colnames(test_bench)[ncol(test_bench)-1] = "Error_bench"

test_merged = merge(test1 %>% select(-name), test_bench%>% select(-name),
                    by = setdiff(colnames(test1), c("Error", "name")))


alpha_fp =0.1

ggplot(test_merged %>% filter(#power_pa == 1.2,
  gamma_epid == 0.1,
  alpha_fp == alpha_fp,
  #diffuse==10,
  #diffuse == 10,
  propagation == "true_p",
  p_norm == 1,
  nb_init == 1)%>%
    mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)) %>%  
    group_by(time, r0, diffuse, lambda) %>%
    summarise(Error=mean(Error)), 
  aes(x=time, y = Error, color = as.factor(lambda))) + 
  geom_line() + 
  geom_point(data=test_merged %>% filter(#power_pa == 1.2,
    gamma_epid == 0.1,
    alpha_fp == alpha_fp,
    #diffuse==10,
    #diffuse == 10,
    propagation == "true_p",
    p_norm == 1,
    nb_init == 1)%>%
      mutate( r0 = paste0( "R0 = ", beta_epid/gamma_epid)) %>%
      group_by(time, r0, diffuse) %>%
      summarise(Error_bench=mean(Error_bench)),
    aes(x=time, y=Error_bench, color = "benchmark"))+
  facet_wrap(r0~diffuse,
             scales = "free", ncol = 3)+
  xlab("Time") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  labs(colour="Comparison") + 
  #scale_x_log10() + 
  scale_y_log10() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pivot_longer(res0 %>% 
               select(-starts_with("l2_error"))%>% 
               select(-starts_with("benchmark_l2_error")), 
             cols = ends_with("l1_error_1"))


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





