library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"
file_list <- list.files(folder_path, pattern = "^newest_results_algo_semi_synthetic1116", full.names = TRUE)
#file_list2 <- list.files(folder_path, pattern = "^results_algo_gen_graph_109", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
#data <- map_dfr(file_list, read_csv)
file_list2 <- list.files(folder_path, pattern = "^newest_results_algo_semi_synthetic1119", full.names = TRUE)
data_list2 <- lapply(file_list2, function(file) {
  data <- read_csv(file)
  print(file)
  if (nrow(data) == 0) {
    return(NULL)  # Skip this file if it's empty
  }
  data$filename <- file
  ind = which(data$Method=="SSNAL")
  return(data[1:ind[length(ind)],])
})

data_list <- lapply(file_list, function(file) {
  data <- read_csv(file)
  data$filename <- file
  return(data)
})
# data_list2 <- lapply(file_list2, function(file) {
#   data <- read.csv(file, stringsAsFactors = FALSE)
#   data$filename <- file
#   return(data)
# })
# Combine all individual data frames into one data frame
data <- do.call(rbind, data_list)
data2 <- do.call(rbind, data_list2)
colnames(data)
#colnames(data2)
#### first group by experiment
res  = data %>%
  group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp) %>%
  summarise_all(mean, na.rm=TRUE)

res_q = data %>%
  group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp) %>%
  summarise(Accuracy_true_p_q25 = quantile(Accuracy_true_p, 0.25),
            Accuracy_true_p_q50 = quantile(Accuracy_true_p, 0.5),
            Accuracy_true_p_q75 = quantile(Accuracy_true_p, 0.75),
            n_infected_q25 = quantile(n_infected, 0.25),
            n_infected_q50 = quantile(n_infected, 0.5),
            n_infected_q75 = quantile(n_infected, 0.75),
            accuracy_prop_14_q25 = quantile(accuracy_prop_14, 0.25),
            accuracy_prop_14_q50 = quantile(accuracy_prop_14, 0.5),
            accuracy_prop_14_q75 = quantile(accuracy_prop_14, 0.75),
            accuracy_benchmark_prop_14_q25 = quantile(accuracy_benchmark_prop_14, 0.25),
            accuracy_benchmark_prop_14_q50 = quantile(accuracy_benchmark_prop_14, 0.5),
            accuracy_benchmark_prop_14_q75 = quantile(accuracy_benchmark_prop_14, 0.75),
            counts = n()
            )

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
### 
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

ggplot(res %>% filter(alpha_fp == 0, Lambda<1e2, 
                      !((graph_type=="knn") & (m==2)),
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





