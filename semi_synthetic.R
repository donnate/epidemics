library(tidyverse)
setwd("~/Documents/epidemic_modelling/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"
file_list <- list.files(folder_path, pattern = "results_algo_semi_synthetic*", full.names = TRUE)
file_list2 <- file_list[361:length(file_list)]#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data <- map_dfr(file_list, read_csv)
v#data_list <- lapply(file_list, function(file) {
#  data <- read.csv(file, stringsAsFactors = FALSE)
#  data$filename <- file
#  return(data)
#})
# Combine all individual data frames into one data frame
#data <- do.call(rbind, data_list)
unique(data$graph_type)
unique(data$n_nodes)
colnames(data)
unique(data$lambda)
colnames(data)

ggplot(data, 
       aes(x= Method , final_number_infected))+
  #geom_line(linewidth=1.)+
  geom_boxplot()+
  xlab("lambda (Regularization Strength)") + 
  ylab("Accuracy (s)") +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(res %>% filter(final_number_infected<200), 
       aes(x=Lambda, Accuracy_true_p, colour=final_number_infected))+
  #geom_line(linewidth=1.)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10() +#
  xlab("lambda (Regularization Strength)") + 
  ylab("Accuracy (s)") +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data %>% filter(graph_type =="ER"), 
       aes(x=Lambda, Accuracy_true_p, colour=final_number_infected))+
  #geom_line(linewidth=1.)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10() +#
  facet_wrap(~p,scales = "free_y")+
  xlab("lambda (Regularization Strength)") + 
  ylab("Accuracy (s)") +
  labs(colour="Comparison") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### first group by experiment
res  = data %>%
  group_by(Method, Lambda) %>%
  summarise(mean_acc = median(Accuracy_true_p),
            q25_acc= quantile(Accuracy_true_p, 0.25),
            q75_acc= quantile(Accuracy_true_p, 0.75),
            mean_bench_Accuracy_true_p = median(bench_Accuracy_true_p),
            q25_bench_Accuracy_true_p= quantile(bench_Accuracy_true_p, 0.25),
            q75_bench_Accuracy_true_p= quantile(bench_Accuracy_true_p, 0.75),
            mean_accuracy_prop_14 = median(accuracy_prop_14),
            q25_accuracy_prop_14= quantile(accuracy_prop_14, 0.25),
            q75_accuracy_prop_14= quantile(accuracy_prop_14, 0.75),
            mean_accuracy_benchmark_prop_14 = median(accuracy_benchmark_prop_14),
            q25_accuracy_benchmark_prop_14 =quantile(accuracy_benchmark_prop_14, 0.25),
            q75_accuracy_benchmark_prop_14= quantile(accuracy_benchmark_prop_14, 0.75),
            ) %>%
  ungroup()

ggplot(res, 
       aes(x=Lambda, mean_acc, colour="Denoising"))+
  geom_line(linewidth=1.)+
  geom_point()+
  geom_errorbar(aes(ymin=q25_acc, ymax=q75_acc))+
  #geom_line(aes(x=Lambda, mean_bench_Accuracy_true_p, colour="Benchmark"), linewidth=1.)+
  scale_x_log10()+
  scale_y_log10() +#
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(res, 
       aes(x=Lambda, mean_accuracy_prop_14, colour="Denoising"))+
  geom_line(linewidth=1.)+
  geom_point()+
  geom_errorbar(aes(ymin=q25_accuracy_prop_14, ymax=q75_accuracy_prop_14))+
  #geom_line(aes(x=Lambda, mean_bench_Accuracy_true_p, colour="Benchmark"), linewidth=1.)+
  scale_x_log10()+
  scale_y_log10() +#
  xlab("lambda (Regularization Strength)") + 
  ylab(expression(italic(l[1]) ~ "error")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


res25  = data %>%
  group_by(Method, Lambda, graph_type, n_nodes, p, m, alpha_fp) %>%
  summarise_all(quantile, 25) %>%
  ungroup()


colnames(res)
ggplot(res, aes(x=Lambda, med_time, colour=Method))+
  geom_line(linewidth=1.)+
  geom_point()+
  geom_errorbar(aes(ymin=q25_time, ymax=q75_time))+
  scale_x_log10()+
  scale_y_log10() +#
  facet_grid(alpha_fp~p,
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
