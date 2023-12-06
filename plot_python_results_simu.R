library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"
file_list <- list.files(folder_path, pattern = "^new", full.names = TRUE)
#file_list2 <- list.files(folder_path, pattern = "^results_algo_gen_graph_109", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
#data <- map_dfr(file_list, read_csv)
data_list <- lapply(file_list, function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
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
colnames(data2)
#### first group by experiment
res  = data %>%
  group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp) %>%
  summarise_all(mean, na.rm=TRUE)

res_q = data %>%
  group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp) %>%
  summarise(Accuracy_true_p_q25 = quantile(Accuracy_true_p, 0.25),
            Accuracy_true_p_q50 = quantile(Accuracy_true_p, 0.5),
            Accuracy_true_p_q75 = quantile(Accuracy_true_p, 0.75),
            counts = n()
            )
res_q = res_q%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p, ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p, ")"),
                                                      ))))))
### 
ggplot(res %>% filter(alpha_fp == 0) %>%
         mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                        ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                               ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                                      ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p, ")"),
                                                             ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p, ")"),
                                                             )
                                                ))))))+
  geom_point(aes(x = Lambda, y = Accuracy_true_p_q50)) +
  facet_wrap(. ~ graph_type_name, scales="free") +
  scale_x_log10()  + 
  scale_y_log10()

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





