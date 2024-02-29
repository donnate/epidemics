library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"
file_list <- list.files(folder_path, pattern = "^binary_new_CV5_res", full.names = TRUE)
file_list <- list.files(folder_path, pattern = "^binary_new_CV2_res", full.names = TRUE)
theme_set(theme_bw(base_size = 18))
# data_list <- lapply(file_list, function(file) {
#   data <- read_csv(file)
#   data$filename <- file
#   return(data)
# })
# data_list2 <- lapply(file_list2, function(file) {
#   data <- read.csv(file, stringsAsFactors = FALSE)
#   data$filename <- file
#   return(data)
# })
# Combine all individual data frames into one data frame
data <- bind_rows(lapply(file_list, read.csv))

data2 <- bind_rows(lapply(file_list2, read.csv))
#data2 <- do.call(rbind, data_list2)
colnames(data)
#colnames(data2)
#### first group by experiment
# res  = data %>%
#   group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp,
#            gamma, beta, n_infected) %>%
#   summarise_all(mean, na.rm=TRUE)
# res2  = data %>%
#   group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp,
#            gamma, beta) %>%
#   summarise_all(mean, na.rm=TRUE)
res  = data %>%
  filter(Method !="SSNAL") %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise_all(mean, na.rm=TRUE)

res_q = data %>%
  filter(Method !="SSNAL") %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise(Accuracy_true_p_q25 = quantile(Accuracy_true_p, 0.25),
            Accuracy_true_p_q50 = quantile(Accuracy_true_p, 0.5),
            Accuracy_true_p_q75 = quantile(Accuracy_true_p, 0.75),
            Accuracy_true_p_l2_q25 = quantile(Accuracy_true_p_l2, 0.25),
            Accuracy_true_p_l2_q50 = quantile(Accuracy_true_p_l2, 0.5),
            Accuracy_true_p_l2_q75 = quantile(Accuracy_true_p_l2, 0.75),
            BCE_q25 = quantile(BCE, 0.25),
            BCE_q50 = quantile(BCE, 0.5),
            BCE_q75 = quantile(BCE, 0.75),
            bench_Accuracy_true_p_q25 = quantile(bench_Accuracy_true_p, 0.25, na.rm=TRUE),
            bench_Accuracy_true_p_q50 = quantile(bench_Accuracy_true_p, 0.5, na.rm=TRUE),
            bench_Accuracy_true_p_q75 = quantile(bench_Accuracy_true_p, 0.75, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q25 = quantile(bench_Accuracy_true_p_l2, 0.25, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q50 = quantile(bench_Accuracy_true_p_l2, 0.5, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q75 = quantile(bench_Accuracy_true_p_l2, 0.75, na.rm=TRUE),
            n_infected_q25 = quantile(n_infected, 0.25),
            n_infected_q50 = quantile(n_infected, 0.5),
            n_infected_q75 = quantile(n_infected, 0.75),
            accuracy_prop_14_q25 = quantile(accuracy_prop_14, 0.25, na.rm=TRUE),
            accuracy_prop_14_q50 = quantile(accuracy_prop_14, 0.5, na.rm=TRUE),
            accuracy_prop_14_q75 = quantile(accuracy_prop_14, 0.75, na.rm=TRUE),
            accuracy_benchmark_prop_14_q25 = quantile(accuracy_benchmark_prop_14, 0.25, na.rm=TRUE),
            accuracy_benchmark_prop_14_q50 = quantile(accuracy_benchmark_prop_14, 0.5, na.rm=TRUE),
            accuracy_benchmark_prop_14_q75 = quantile(accuracy_benchmark_prop_14, 0.75, na.rm=TRUE),
                        counts = n()
  )


res2_2  = data2 %>%
  filter(Method !="SSNAL") %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise_all(mean, na.rm=TRUE)

res2_q2 =  data2 %>%
  filter(Method !="SSNAL") %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise(Accuracy_true_p_q25 = quantile(Accuracy_true_p, 0.25),
            Accuracy_true_p_q50 = quantile(Accuracy_true_p, 0.5),
            Accuracy_true_p_q75 = quantile(Accuracy_true_p, 0.75),
            Accuracy_true_p_l2_q25 = quantile(Accuracy_true_p_l2, 0.25),
            Accuracy_true_p_l2_q50 = quantile(Accuracy_true_p_l2, 0.5),
            Accuracy_true_p_l2_q75 = quantile(Accuracy_true_p_l2, 0.75),
            BCE_q25 = quantile(BCE, 0.25),
            BCE_q50 = quantile(BCE, 0.5),
            BCE_q75 = quantile(BCE, 0.75),
            bench_Accuracy_true_p_q25 = quantile(bench_Accuracy_true_p, 0.25, na.rm=TRUE),
            bench_Accuracy_true_p_q50 = quantile(bench_Accuracy_true_p, 0.5, na.rm=TRUE),
            bench_Accuracy_true_p_q75 = quantile(bench_Accuracy_true_p, 0.75, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q25 = quantile(bench_Accuracy_true_p_l2, 0.25, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q50 = quantile(bench_Accuracy_true_p_l2, 0.5, na.rm=TRUE),
            bench_Accuracy_true_p_l2_q75 = quantile(bench_Accuracy_true_p_l2, 0.75, na.rm=TRUE),
            n_infected_q25 = quantile(n_infected, 0.25),
            n_infected_q50 = quantile(n_infected, 0.5),
            n_infected_q75 = quantile(n_infected, 0.75),
            accuracy_prop_14_q25 = quantile(accuracy_prop_14, 0.25, na.rm=TRUE),
            accuracy_prop_14_q50 = quantile(accuracy_prop_14, 0.5, na.rm=TRUE),
            accuracy_prop_14_q75 = quantile(accuracy_prop_14, 0.75, na.rm=TRUE),
            accuracy_benchmark_prop_14_q25 = quantile(accuracy_benchmark_prop_14, 0.25, na.rm=TRUE),
            accuracy_benchmark_prop_14_q50 = quantile(accuracy_benchmark_prop_14, 0.5, na.rm=TRUE),
            accuracy_benchmark_prop_14_q75 = quantile(accuracy_benchmark_prop_14, 0.75, na.rm=TRUE),
            counts = n()
  )

res <- res %>% left_join(res_q,
                               by = c("Method",  "steps", "p", "m", 
                                      "graph_type", "alpha_fp",
                                      "gamma", "beta")) 

res2_2 <- res2_2 %>% left_join(res_q2,
                           by = c("Method",  "steps", "p", "m", 
                                  "graph_type", "alpha_fp",
                                  "gamma", "beta")) 

which(is.na(data$steps))
data = data%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))

res_q = res_q%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))
res = res%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))

res2_2 = res2_2%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))
###
ggplot(data %>% filter(alpha_fp == 0, Lambda<1e0,
                      gamma ==0.1,
                      m == 5,
                      Method =="SSNAL-opt",
                      graph_type == "knn"))+
  geom_violin(aes(x = factor(beta), 
                  y = (n_infected),
                  fill = as.factor(beta),
                  ))+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(p ~  steps, scales="free",
             labeller = as_labeller(c(`10` = "Steps = 10",
                                      `20` = "Steps = 20",
                                      `30` = "Steps = 30",
                                      `0.005` = "Erdos-Renyi\n(p=0.005)",
                                      `0.01` = "Erdos-Renyi\n(p=0.01)",
                                      `0.02` = "Erdos-Renyi\n(p=0.02)"
                                      ))) +
  #scale_y_log10() + 
  xlab("Value of Beta") + 
  labs(fill = "Beta")+
  #scale_x_log10() +
  ylab("Number of People Infected")


ggplot(data %>% filter(alpha_fp == 0, Lambda<1e0,
                       gamma ==0.1,
                       Method =="SSNAL-opt",
                       graph_type == ""))+
  geom_violin(aes(x = factor(beta), 
                  y = (n_infected),
                  fill = as.factor(beta),
  ))+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(m ~  steps, scales="free",
             labeller = as_labeller(c(`10` = "Steps = 10",
                                      `20` = "Steps = 20",
                                      `30` = "Steps = 30",
                                      `50` = "Steps = 50",
                                      `2` = "k-NN\n(k=2)",
                                      `5` = "k-NN\n(k=5)",
                                      `7` = "k-NN\n(k=7)"
             ))) +
  scale_y_log10() + 
  xlab("Value of Beta") + 
  labs(fill = "Beta")+
  #scale_x_log10() +
  ylab("Number of People Infected")

unique(res2$p)
df = res %>% filter(alpha_fp == 0,
                    gamma ==0.1,
                    p == 0,
                    steps == 30,
                    #m == 5,
                    graph_type == "knn")
df <- df %>%
  mutate(graph_type_name = paste0( 
    "k-NN graph\nk = ", m),
    steps_name= paste0( 
      "Steps = ", steps),
    beta_name= paste0( 
      "Beta  = ", beta))

df$graph_type_name <- factor(df$graph_type_name , levels = c("k-NN graph\nk = 2",
                                               "k-NN graph\nk = 5",
                                               "k-NN graph\nk = 7",
                                               "k-NN graph\nk = 10"))
ggplot(df %>% dplyr::filter(Method =="SSNAL"))+
  geom_line(aes(x = Lambda, y = (Accuracy_true_p_l2),
                ), alpha=1,
            linewidth = 1.2)+
  geom_hline(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(beta, steps, m, steps_name, name_exp, beta_name) %>%
               summarise_if(is.numeric,mean),
    aes(yintercept = Accuracy_true_p_l2, colour="Denoising Estimator\n with CV"),
            linewidth = 1.2)+
  geom_point(aes(x = Lambda, y = Accuracy_true_p_l2)) +
  geom_errorbar(
  aes(x = Lambda, ymin = Accuracy_true_p_l2_q25,ymax = Accuracy_true_p_l2_q75),
  alpha=0.3) +
  facet_grid(graph_type_name ~  beta_name, scales="free") +
  scale_y_log10() + 
  scale_x_log10() +
  ylab("l2 error")




unique(res$graph_type)
df = res %>% filter(alpha_fp == 0.0,
                     gamma ==0.1,
                     #counts > 100,
                     #p == 0,
                     graph_type == "Berkeley")

df <- df %>%
  mutate(
         steps_name= paste0( 
           "Steps = ", steps))
# 
# df$name_exp <- factor(df$name_exp , levels = c("k-NN graph\nk = 2",
#                                                "k-NN graph\nk = 5",
#                                                "k-NN graph\nk = 7",
#                                                "k-NN graph\nk = 10"))
#df$beta = as.factor(df$beta)
ggplot(data=df %>% filter(Method =="SSNAL-opt"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_q50), colour="Denoised Estimate\n(With CV)"),
            alpha=1, size=2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_q50), colour="Denoised Estimate\n(With CV)"),
            alpha=1,
            linewidth=0.8)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_q25),
                 ymax = (Accuracy_true_p_q75), colour="Denoised Estimate\n(With CV)"), 
              alpha=1, width=0.07)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"), alpha=1,
            linewidth=.8)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_q25),
                 ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
             width=0.07
         )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method") + 
  xlab("Beta") 

ggplot(data=df %>% filter(Method =="SSNAL-opt"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
            alpha=1,
            linewidth=0.8)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_l2_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.07)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
            linewidth=.8)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_l2_q25),
                    ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
                width=0.07
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error") +
  labs(colour = "Method") + 
  xlab("Beta") 


ggplot(data=df %>% filter(Method =="SSNAL-opt"))+
  geom_point(aes(x = beta , y = (n_infected), colour="Denoised Estimate\n(With CV)"),
             alpha=1)+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method") + 
  xlab("Beta") 


ggplot(data=df %>% filter(Method =="SSNAL-opt"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_l2_q50), colour="Denoised Estimate\n(With CV)"),
             alpha=1)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_l2_q50), colour="Denoised Estimate\n(With CV)"),
            alpha=1)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_l2_q25),
                    ymax = (Accuracy_true_p_l2_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.07)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2_q50), colour="Benchmark"), alpha=1)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2_q50), colour="Benchmark"), alpha=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_l2_q25),
                    ymax = (bench_Accuracy_true_p_l2_q75), colour="Benchmark"), alpha=1,
                width=0.07
  )+
  facet_grid(name_exp~steps_name, scales="free_y") +
  scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error") +
  labs(colour = "Method") + 
  xlab("Beta")



unique(res$graph_type)
df = res %>% filter(
                       gamma ==0.1,
                       #p == 0,
                       steps == 30,
                       #counts>100,
                       graph_type == "pa")
df <- df %>%
  mutate(name_exp = paste0(  "Beta = ", beta),
    steps_name= paste0( 
      "Steps = ", steps))

df$name_exp <- factor(df$name_exp , levels = c("Beta = 0.2",
                                               "Beta = 0.5",
                                               "Beta = 0.7",
                                               "Beta = 0.9"))

ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, graph_type_name, steps_name, 
                  alpha_fp, name_exp) %>%
         summarise_if(is.numeric,mean))+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
            alpha=1, linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt") %>%
                  group_by((beta), alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x = alpha_fp , ymin = (Accuracy_true_p_l2_q25),
                    ymax = (Accuracy_true_p_l2_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.005)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, alpha_fp, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
            linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt") %>%
                  group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x =  alpha_fp-  0.005, ymin = (bench_Accuracy_true_p_l2_q25),
                    ymax = (bench_Accuracy_true_p_l2_q75), colour="Benchmark"), alpha=1,
                width=0.005
  )+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(graph_type_name~name_exp, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error") +
  labs(colour = "Method") + 
  xlab("False Positive Rate") 


ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, name_exp, steps_name, 
                  alpha_fp) %>%
         summarise_if(is.numeric,mean))+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(alpha_fp, beta, steps, m, name_exp, steps_name) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (accuracy_prop_0), colour="Denoised Estimate\n(With CV)"),
             alpha=1)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, beta, steps, m, name_exp, steps_name) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (accuracy_prop_0), colour="Denoised Estimate\n(With CV)"),
            alpha=1)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(beta,alpha_fp, steps, m, name_exp, steps_name) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (accuracy_benchmark_prop_0), colour="Benchmark"), alpha=1)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, alpha_fp, steps, m, name_exp, steps_name) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp-  0.005, y = (accuracy_benchmark_prop_0), colour="Benchmark"), alpha=1)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(name_exp~steps_name, scales="free_y") +
  scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error") +
  labs(colour = "Method") + 
  xlab("False") 



unique(res$graph_type)
df = res %>% filter(
  gamma ==0.1,
  #p == 0,
  steps == 30,
  #counts>100,
  graph_type == "pa")
df <- df %>%
  mutate(name_exp = paste0(  "Beta = ", beta),
         steps_name= paste0( 
           "Steps = ", steps))

df$name_exp <- factor(df$name_exp , levels = c("Beta = 0.2",
                                               "Beta = 0.5",
                                               "Beta = 0.7",
                                               "Beta = 0.9"))

ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, graph_type_name, steps_name, 
                  alpha_fp, name_exp) %>%
         summarise_if(is.numeric,mean))+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (Accuracy_true_p_l2), colour="Denoised Estimate\n(With CV)"),
            alpha=1, linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt") %>%
                  group_by((beta), alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x = alpha_fp , ymin = (Accuracy_true_p_l2_q25),
                    ymax = (Accuracy_true_p_l2_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.005)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt") %>%
               group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, alpha_fp, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p_l2), colour="Benchmark"), alpha=1,
            linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt") %>%
                  group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x =  alpha_fp-  0.005, ymin = (bench_Accuracy_true_p_l2_q25),
                    ymax = (accuracy_benchmark_prop_14), colour="Benchmark"), alpha=1,
                width=0.005
  )+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(graph_type_name~name_exp, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error (forecasting)") +
  labs(colour = "Method") + 
  xlab("False Positive Rate") 



df = res2 %>% filter(#alpha_fp == 0.05,
                     gamma ==0.1,
                     #p == 0,
                     m == 10,
                     graph_type == "knn")
ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, alpha_fp) %>%
         summarise_if(is.numeric,mean))+
  geom_line(aes(x = alpha_fp, y = log(Accuracy_true_p), colour="Opt2"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, steps, m, alpha_fp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp, y = log(Accuracy_true_p), colour="Op1"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, steps, m, beta) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp, y = log(bench_Accuracy_true_p), colour="bench"), alpha=1,
            linewidth = 1.2)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_wrap(beta ~  steps, scales="free_y") +
  #scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error")


df = res2_2 %>% filter(#alpha_fp == 0.05,
  gamma ==0.1,
  alpha_fp == 0,
  #p == 0,
  graph_type == "ER")
ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, alpha_fp, p) %>%
         summarise_if(is.numeric,mean))+
  geom_line(aes(x = beta, y = (Accuracy_true_p_l2), colour="Opt2"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, steps, m, alpha_fp, p) %>%
              summarise_if(is.numeric,mean),
            aes(x = beta, y = (Accuracy_true_p_l2), colour="Op1"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, steps, m, beta, p) %>%
              summarise_if(is.numeric,mean),
            aes(x = beta, y = (bench_Accuracy_true_p_l2), colour="bench"), alpha=1,
            linewidth = 1.2)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(p ~  steps, scales="free_y") +
  #scale_y_log10() + 
  scale_x_log10() +
  ylab("l2 error")


ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, alpha_fp) %>%
         summarise_if(is.numeric,mean))+
  geom_line(aes(x = alpha_fp, y = log(Accuracy_true_p), colour="Opt2"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(beta, steps, m, alpha_fp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp, y = log(Accuracy_true_p), colour="Op1"), alpha=1,
            linewidth = 1.2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt") %>%
              group_by(alpha_fp, steps, m, beta) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp, y = log(bench_Accuracy_true_p), colour="bench"), alpha=1,
            linewidth = 1.2)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(beta ~  steps, scales="free_y") +
  #scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error")



ggplot(res %>% filter(alpha_fp == 0, Lambda<1e2, 
                      !((graph_type=="knn") & (m==2)),
                      !((graph_type=="pa") & (m==4)),
                      !((graph_type=="ER") & (p==0.02))
                      ))+
  geom_line(aes(x = Lambda, y = Accuracy_true_p, colour = n_infected), alpha=1,
            linewidth = 1.2)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  geom_errorbar(data = res_q%>% filter(alpha_fp == 0, Lambda<1e2,
                                       !((graph_type=="knn") & (m==2)),
                                       !((graph_type=="pa") & (m==4)),
                                       !((graph_type=="ER") & (p==0.02))
                                       ),
                aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
                    colour = graph_type),
                alpha=0.3) +
  facet_wrap(beta + gamma ~ graph_type_name, scales="free") +
  #scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error") +
  theme(legend.position = "none")

ggplot(data %>% filter(alpha_fp == 0) %>% 
         mutate(exp2 = paste0(filename, "_", Experiment)))+
  geom_line(aes(x = Lambda, y = Accuracy_true_p, colour=exp2), alpha=0.1)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap(. ~ graph_type_name, scales="free") +
  scale_x_log10() +
  theme(legend.position = "none")#+ 
# scale_y_log10()
ggplot(res %>% filter(alpha_fp == 0, Lambda<1e2, !((graph_type=="knn") & (m==2)),
                      !((graph_type=="pa") & (m==4)),
                      !((graph_type=="ER") & (p==0.02))
))+
  geom_line(aes(x = Lambda, y = Accuracy_true_p, colour = graph_type), alpha=1,
            linewidth = 1.2)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  geom_errorbar(data = res_q%>% filter(alpha_fp == 0, Lambda<1e2,
                                       !((graph_type=="knn") & (m==2)),
                                       !((graph_type=="pa") & (m==4)),
                                       !((graph_type=="ER") & (p==0.02))
  ),
  aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
      colour = graph_type),
  alpha=0.3) +
  facet_wrap(. ~ graph_type_name, scales="free") +
  #scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error") +
  theme(legend.position = "none")


ggplot(res_q %>% filter(alpha_fp == 0,))+
  geom_line(aes(x = Lambda, y = Accuracy_true_p_q50), alpha=1)+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap(. ~ graph_type_name, scales="free") +
  scale_x_log10() +
  theme(legend.position = "none")


ggplot(res_q %>% filter(alpha_fp == 0))+
  geom_line(aes(x = Lambda, y = accuracy_prop_14_q50))+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap(. ~ graph_type_name, scales="free") +
  scale_x_log10()  #+ 
# scale_y_log10()


unique(res_q$alpha_fp)
ggplot(data %>% filter(alpha_fp == 0))+
  geom_boxplot(aes(x = graph_type_name, y = n_infected, fill=graph_type))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Graph")+
  ylab("Size of the epidemic\n(# of people infected)") +
  labs(fill = "Graph Type") 
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +


ggplot(res_q %>% filter(!((graph_type == "ER") & p ==0.02) ,
                        !((graph_type == "pa") & m == 2),
                        Lambda> 1e-2,
                        graph_type != "knn"),
       aes(x = Lambda, y = Accuracy_true_p_q50, colour = as.factor(alpha_fp)))+
  geom_point() +
  geom_line() +
  geom_line(aes(x = Lambda, y = Accuracy_true_p_q50, colour = as.factor(alpha_fp))) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap( .~ graph_type_name, scales="free") +
  scale_x_log10()  + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Lambda")+
  ylab("Accuracy") +
  labs(colour = "False Positive\nRate")  





ggplot(res%>% filter(
                       !((graph_type=="knn")),
                       !((graph_type=="small-world") & (m==2) & (p==0.1)),
                       !((graph_type=="small-world") & (m==2) & (p==0.5)),
                       !((graph_type=="pa") & (m==4)),
                       !((graph_type=="pa") & (m==2)),
                       !((graph_type=="ER") & (p==0.02))),
       aes(x = Lambda, y = accuracy_prop_14, 
           colour = as.factor(alpha_fp)))+
  geom_point() +
  geom_line(aes(linetype="Method")) +
  geom_line(aes(y = accuracy_benchmark_prop_14, linetype="Benchmark")) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap( .~ graph_type_name, scales="free") +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_log10()  + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Lambda")+
  ylab("Accuracy After 15 steps") +
  labs(colour = "False Positive\nRate", linetype = "Method") 

ggplot(res%>% filter(
  alpha_fp == 0,
  !((graph_type=="knn")),
  !((graph_type=="small-world") & (m==2) & (p==0.1)),
  !((graph_type=="small-world") & (m==2) & (p==0.5)),
  !((graph_type=="pa") & (m==4)),
  !((graph_type=="pa") & (m==2)),
  !((graph_type=="ER") & (p==0.02))),
  aes(x = Lambda, y = Accuracy_true_p, 
      colour = as.factor(alpha_fp)))+
  geom_point() +
  geom_line(aes(linetype="Method")) +
  geom_line(aes(y = bench_Accuracy_true_p, linetype="Benchmark")) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap( .~ graph_type_name, scales="free") +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_log10()  + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Lambda")+
  ylab("Denoising Accuracy\n (l1 error)") +
  labs(colour = "False Positive\nRate", linetype = "Method")



ggplot(res%>% filter(
  alpha_fp == 0,
  !((graph_type=="knn")),
  !((graph_type=="small-world") & (m==2) & (p==0.1)),
  !((graph_type=="small-world") & (m==2) & (p==0.5)),
  !((graph_type=="pa") & (m==4)),
  !((graph_type=="pa") & (m==2)),
  !((graph_type=="ER") & (p==0.02))),
  aes(x = Lambda, y = Accuracy_true_p))+
  geom_point() +
  geom_line(aes(linetype="Method")) +
  geom_line(aes(y = bench_Accuracy_true_p, linetype="Benchmark")) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap( .~ graph_type_name, scales="free") +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_log10()  + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Lambda")+
  ylab("Denoising Accuracy\n (l1 error)") +
  labs(colour = "False Positive\nRate", linetype = "Method")


ggplot(data %>% filter(
  !((graph_type=="knn") & (m==2)),
  !((graph_type=="pa") & (m==4)),
  !((graph_type=="ER") & (p==0.02))),
  aes(x = Lambda, y = accuracy_prop_14, colour = as.factor(alpha_fp)))+
  geom_point() +
  geom_line(aes(linetype="Method")) +
  geom_line(aes(x = Lambda, y = accuracy_benchmark_prop_14, 
                colour = as.factor(alpha_fp),
                linetype="Benchmark")) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #              aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75 )) +
  facet_wrap( .~ graph_type_name, scales="free") +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_x_log10()  + 
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Lambda")+
  ylab("Accuracy After 15 steps") +
  labs(colour = "False Positive\nRate", linetype = "Method") 

 ### 


ggplot(res %>% filter(alpha_fp == 0))+
  geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  facet_wrap(graph_type ~ m+p, scales="free") +
  scale_x_log10()  + 
  scale_y_log10()

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





