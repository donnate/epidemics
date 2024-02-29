library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Downloads"
file_list <- list.files(folder_path, pattern = "^newest_new_weights_new_proc", full.names = TRUE)
#file_list2 <- list.files(folder_path, pattern = "^binary_new_CV2_res", full.names = TRUE)
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

#data2 <- bind_rows(lapply(file_list2, read.csv))
#data2 <- do.call(rbind, data_list2)
colnames(data)
data$steps <-sapply(1:nrow(data), FUN=function(x){data$steps[ifelse(x%%3 ==1, x, 
                                                                    ifelse(x%%3 == 2, x-1,x-2
                                                                         ))]})
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


q25_rename <- function(x){paste0(x, "_q25")}
q50_rename <- function(x){paste0(x, "_q50")}
q75_rename <- function(x){paste0(x, "_q75")}
res_q25  = data %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise_all(quantile, 0.25, na.rm=TRUE) 
res_q25_groups <- res_q25|>
  ungroup() |>
  select(c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
           "gamma", "beta")) 
res_q25 <- res_q25|>
  ungroup() |>
  select(-c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
           "gamma", "beta"))  |>
  rename_all(
    q25_rename)
res_q25 <- cbind(res_q25_groups,
                 res_q25)

res_q50  = data %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise_all(quantile, 0.5, na.rm=TRUE) 
res_q50_groups <- res_q50|>
  ungroup() |>
  select(c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
           "gamma", "beta")) 
res_q50 <- res_q50|>
  ungroup() |>
  select(-c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
            "gamma", "beta"))  |>
  rename_all(
    q50_rename)
res_q50 <- cbind(res_q50_groups,
                 res_q50)

res_q75  = data %>%
  group_by(Method, steps, p, m, graph_type, alpha_fp,
           gamma, beta) %>%
  summarise_all(quantile, 0.75, na.rm=TRUE) 
res_q75_groups <- res_q75|>
  ungroup() |>
  select(c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
           "gamma", "beta")) 
res_q75 <- res_q75|>
  ungroup() |>
  select(-c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
            "gamma", "beta"))  |>
  rename_all(
    q75_rename)
res_q75 <- cbind(res_q75_groups,
                 res_q75)


res <- res |>
  left_join(res_q25, by = c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
                            "gamma", "beta")) |>
  left_join(res_q50, by = c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
                            "gamma", "beta")) |>
  left_join(res_q75, by = c("Method", "steps", "p", "m", "graph_type", "alpha_fp",
                            "gamma", "beta")) 


res = res%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                       ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                              ifelse(graph_type == "pa", paste0(" Preferential\nAttachment\n( m=", m, ")"),
                                     ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                            ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                   ifelse(graph_type == "SBM", paste0(" SBM \n( m=", m, ", p=", p,   ")"),
                                                          "Berkeley")
                                            ))))))

data = data%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential\nAttachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))
unique(res$steps)
###
ggplot(data %>% filter(alpha_fp == 0,
                      gamma ==0.1,
                      Method =="SSNAL-opt2"))+
  geom_violin(aes(x = factor(beta), 
                  y = (n_infected),
                  fill = as.factor(beta),
                  ))+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(graph_type_name ~  steps, scales="free") +
  #scale_y_log10() + 
  xlab("Value of Beta") + 
  labs(fill = "Beta")+
  #scale_x_log10() +
  ylab("Number of People Infected")



res_l = data %>%
  filter(Method !="SSNAL-opt", Method !="SSNAL-opt2") %>%
  group_by(Method, Lambda, steps, p, m, graph_type, alpha_fp,
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
            accuracy_l2_14_q25 = quantile(Accuracy_true_p_l2_14, 0.25, na.rm=TRUE),
            accuracy_l2_14_q50 = quantile(Accuracy_true_p_l2_14, 0.5, na.rm=TRUE),
            accuracy_l2_14_q75 = quantile(Accuracy_true_p_l2_14, 0.75, na.rm=TRUE),
            accuracy_benchmark_prop_14_q25 = quantile(accuracy_benchmark_prop_14, 0.25, na.rm=TRUE),
            accuracy_benchmark_prop_14_q50 = quantile(accuracy_benchmark_prop_14, 0.5, na.rm=TRUE),
            accuracy_benchmark_prop_14_q75 = quantile(accuracy_benchmark_prop_14, 0.75, na.rm=TRUE),
            accuracy_benchmark_prop_14_l2_q25 = quantile(accuracy_benchmark_prop_l214, 0.25, na.rm=TRUE),
            accuracy_benchmark_prop_14_l2_q50 = quantile(accuracy_benchmark_prop_l214, 0.5, na.rm=TRUE),
            accuracy_benchmark_prop_14_l2_q75 = quantile(accuracy_benchmark_prop_l214, 0.75, na.rm=TRUE),
            counts = n()
  )


unique(res_l$graph_type)
df = res_l %>% filter(alpha_fp == 0,
                    gamma == 0.1,
                   m <2,
                    #p == 0,
                    steps == 20,
                    #m == 5,
                    graph_type %in%c( "power_law",
                                      "pa"))
df2 = res_q %>% filter(alpha_fp == 0,
                            gamma ==0.1,
                            m <2,
                            #p == 0,
                            steps == 20,
                            #m == 5,
                            graph_type %in%c( "power_law",
                                              "pa"))
df <- df %>%
  mutate(steps_name= paste0( 
      "Steps = ", steps),
    beta_name= paste0( 
      "Beta  = ", beta))

df = df%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley"
                                                      ))))))

df2 = df2%>%
  mutate(graph_type_name =ifelse(graph_type == "ER", paste0(" Erdos Renyi\n( p=", p, ")"),
                                 ifelse(graph_type == "knn", paste0(" k-NN graph\n( k=", m, ")"),
                                        ifelse(graph_type == "pa", paste0(" Preferential Attachment\n( m=", m, ")"),
                                               ifelse(graph_type == "power_law", paste0(" Power Law\n( m=", m, ", p=", p,   ")"),
                                                      ifelse(graph_type == "small-world", paste0(" Small World\n( m=", m, ", p=", p,   ")"),
                                                             ifelse(graph_type == "SBM", paste0(" SBM \n( m=", m, ", p=", p,   ")"),
                                                             "Berkeley")
                                                      ))))))
df2 <- df2 %>%
  mutate(steps_name= paste0( 
    "Steps = ", steps),
    beta_name= paste0( 
      "Beta  = ", beta))
legend_order <- c("Graph TV\nEstimator\nwith CV", "Graph TV\nEstimator")
my_colors <- c(  "dodgerblue", 
                 "black"
)

ggplot(df)+
  geom_line(aes(x = Lambda, y = (Accuracy_true_p_q50),
                colour="Graph TV\nEstimator"), alpha=1,
            linewidth = 1.2)+
  geom_point(aes(x = Lambda, y = Accuracy_true_p_q50, colour="Graph TV\nEstimator")) +
  geom_hline(data=df2 %>% filter(Method == "SSNAL-opt2") ,
    aes(yintercept = Accuracy_true_p_q50, colour="Graph TV\nEstimator\nwith CV"),
            linewidth = 1.2)+
  geom_errorbar(
  aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
      colour="Graph TV\nEstimator"),
  alpha=0.3) +
  facet_grid(graph_type_name ~  beta_name, scales="free") +
  scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error")+
  labs(colour = "Method") + 
  scale_color_manual(values = my_colors, breaks = legend_order) 

ggplot(df)+
  geom_line(aes(x = Lambda, y = (Accuracy_true_p_l2_q50),
                colour="Graph TV\nEstimator"), alpha=1,
            linewidth = 1.2)+
  geom_point(aes(x = Lambda, y = Accuracy_true_p_l2_q50, colour="Graph TV\nEstimator")) +
  geom_hline(data=df2 %>% filter(Method == "SSNAL-opt2") ,
             aes(yintercept = Accuracy_true_p_l2_q50, colour="Graph TV\nEstimator\nwith CV"),
             linewidth = 1.2)+
  geom_errorbar(
    aes(x = Lambda, ymin = Accuracy_true_p_l2_q25,ymax = Accuracy_true_p_l2_q75,
        colour="Graph TV\nEstimator"),
    alpha=0.3) +
  facet_grid(graph_type_name ~  beta_name, scales="free") +
  scale_y_log10() + 
  scale_x_log10() +
  ylab("l2 error")+
  labs(colour = "Method") + 
  scale_color_manual(values = my_colors, breaks = legend_order)

ggplot(df)+
  geom_line(aes(x = Lambda, y = (Accuracy_true_p_q50),
  ), alpha=1,
  linewidth = 1.2)+
  geom_point(aes(x = Lambda, y = Accuracy_true_p_q50)) +
  geom_hline(data=df2 %>% filter(Method == "SSNAL-opt2") ,
             aes(yintercept = Accuracy_true_p_q50, colour="Denoising Estimator\n with CV"),
             linewidth = 1.2)+
  geom_errorbar(
    aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75),
    alpha=0.3) +
  facet_grid(graph_type_name ~  beta_name, scales="free") +
  scale_y_log10() + 
  scale_x_log10() +
  ylab("l1 error")


ggplot(df2%>% filter(Method == "SSNAL-opt"))+
  geom_hline(data=df2 %>% filter(Method == "SSNAL-opt") ,
             aes(yintercept = Accuracy_true_p_l2_q50, colour="Denoising Estimator\n with CV"),
             linewidth = 1.2)+
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
unique(res$graph_type)
df = res %>% filter(alpha_fp == 0.0,
                    gamma ==0.1,
                    graph_type == "knn",
                    #counts > 100,
                    #p == 0,
                    #m <2,
                    #graph_type %in% c(#"Berkeley"   
                    #                  "pa", "knn",  "ER" , "power_law",
                    #                  "small-world")
                    )
df <- df %>%
  mutate(
    steps_name= paste0( 
      "Steps = ", steps))
# 
df$graph_type_name <- factor(df$graph_type_name , levels = c(" k-NN graph\n( k=2)",
                                                             " k-NN graph\n( k=5)",
                                                             " k-NN graph\n( k=7)" ,
                                                             " k-NN graph\n( k=10)"))

relevant  = res %>% 
  filter(Method =="SSNAL-opt2", alpha_fp == 0, 
         !graph_type %in% c("ER", "SBM", "knn"),
         steps == 30) %>%
  ungroup() %>%
  dplyr::select(`graph_type_name`,beta,
                n_infected_q50, n_infected_q25, n_infected_q75,
                Accuracy_true_p_q50, Accuracy_true_p_q25, Accuracy_true_p_q75,
                bench_Accuracy_true_p_q50, bench_Accuracy_true_p_q50, bench_Accuracy_true_p_q25, bench_Accuracy_true_p_q75,
                Accuracy_true_p_l2_q50, Accuracy_true_p_l2_q25, Accuracy_true_p_l2_q75,
                accuracy_prop_l27_q50, accuracy_prop_l27_q25, accuracy_prop_l27_q75,
                bench_Accuracy_true_p_l2_q50, bench_Accuracy_true_p_l2_q25, bench_Accuracy_true_p_l2_q75,
                accuracy_prop_l2_bench7_q50, accuracy_prop_l2_bench7_q25,
                accuracy_prop_l2_bench7_q75
                ) %>%
  mutate(n_infected_q50 = paste0(n_infected_q50, " (", n_infected_q25, ",", n_infected_q75, ")"),
         Accuracy_true_p_q50 = paste0(round(Accuracy_true_p_q50 *1000, 2), " (", round(Accuracy_true_p_q25 *1000, 2), ",", round(Accuracy_true_p_q75*1000, 2), ")"),
         bench_Accuracy_true_p_q50 = paste0(round(bench_Accuracy_true_p_q50 *1000, 2), " (", round(bench_Accuracy_true_p_q25 *1000, 2), ",", round(bench_Accuracy_true_p_q75*1000, 2), ")"),
         Accuracy_true_p_l2_q50 = paste0(round(Accuracy_true_p_l2_q50*1000, 2), " (", round(Accuracy_true_p_l2_q25*1000, 2), ",", round(Accuracy_true_p_l2_q25*1000, 2), ")"),
         bench_Accuracy_true_p_l2_q50 = paste0(round(bench_Accuracy_true_p_l2_q50*1000, 2), " (", round(bench_Accuracy_true_p_l2_q25*1000, 2), ",", round(bench_Accuracy_true_p_l2_q75*1000,3) ,")"),
         accuracy_prop_l27_q50 = paste0(round(accuracy_prop_l27_q50, 3), " (", round(accuracy_prop_l27_q25, 4), ",", round(accuracy_prop_l27_q75, 2), ")"),
         accuracy_prop_l2_bench7_q50 = paste0(round(accuracy_prop_l2_bench7_q50, 3), " (", round(accuracy_prop_l2_bench7_q25, 3), ",", round(accuracy_prop_l2_bench7_q75, 3), ")"),
          ) %>%
  dplyr::select(graph_type_name, beta,
                n_infected_q50,
                Accuracy_true_p_q50, 
                bench_Accuracy_true_p_q50,
                Accuracy_true_p_l2_q50, 
                bench_Accuracy_true_p_l2_q50,
                #accuracy_prop_l27_q50,
                #accuracy_prop_l2_bench7_q50
                )%>%
  arrange(graph_type_name)
relevant
library(knitr)
library(kableExtra)

latex_table <- kable(relevant, format = "latex", booktabs = TRUE)
print(latex_table)

theme_set(theme_bw(base_size = 18))

df = res %>% filter(alpha_fp == 0.0,
                    gamma ==0.1,
                    #m>1,
                    graph_type == "knn")

,
                    #counts > 100,
                    #p == 0,
                    #m <2,
                     graph_type %in% c("Berkeley" ,
                                       "SBM",
                                      "pa", "power_law",
                                       "small-world")
)
df <- df %>%
  mutate(
    steps_name= paste0( 
      "Steps = ", steps))
df$graph_type_name <- factor(df$graph_type_name , levels = c(" k-NN graph\n( k=2)",
                                                             " k-NN graph\n( k=5)",
                                                             " k-NN graph\n( k=7)" ,
                                                             " k-NN graph\n( k=10)"))


unique(data$steps)
ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_q25),
                    ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )

ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (n_infected_q50)),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (n_infected_q50)),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (n_infected_q25),
                    ymax = (n_infected_q75)),
                alpha=1, width=0.03, linewidth=1)+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("Nb Infections") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )

ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_l2_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_l2_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_l2_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_l2_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_l2_q25),
                    ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("Square l2 Error") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(data=df %>% filter(Method =="SSNAL-opt2",
                          graph_type== "knn",
                          # graph_type!= "pa"
                          ),
       )+
  geom_point(aes(x = beta , y = (accuracy_prop_l22_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (accuracy_prop_l22_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (accuracy_prop_l22_q25),
                    ymax = (accuracy_prop_l23_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_prop_l2_bench2_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_prop_l2_bench2_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (accuracy_prop_l2_bench2_q25),
                    ymax = (accuracy_prop_l2_bench2_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(steps_name~graph_type_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error (Predictions 2 days ahead)") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(data=df %>% filter(Method =="SSNAL-opt2",
                          graph_type== "knn",
                          # graph_type!= "pa"
),
)+
  geom_point(aes(x = beta , y = (accuracy_prop_7_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (accuracy_prop_7_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (accuracy_prop_7_q25),
                    ymax = (accuracy_prop_7_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (accuracy_benchmark_prop_7_q25),
                    ymax = (accuracy_benchmark_prop_7_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(steps_name~graph_type_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error (Predictions 2 days ahead)") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(data=df %>% filter(Method =="SSNAL-opt2",
                          graph_type== "knn",
                          # graph_type!= "pa"
),
)+
  geom_point(aes(x = beta , y = (accuracy_prop_7), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (accuracy_prop_7), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (accuracy_prop_7_q25),
                    ymax = (accuracy_prop_7_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (accuracy_benchmark_prop_7_q25),
                    ymax = (accuracy_benchmark_prop_7_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(steps_name~graph_type_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error (Predictions 1 week ahead)") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )




ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (Accuracy_true_p_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (bench_Accuracy_true_p_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (bench_Accuracy_true_p_q25),
                    ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )

ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (accuracy_prop_5), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (accuracy_prop_5), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_5), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_5), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )



ggplot(data=df %>% filter(Method =="SSNAL-opt2", 
                          graph_type== "knn",
                          #graph_type!= "pa"
                          ),
)+
  geom_point(aes(x = beta , y = (accuracy_l2_7_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (accuracy_l2_7_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (accuracy_l2_7_q25),
                    ymax = (accuracy_l2_7_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_l2_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_l2_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (accuracy_benchmark_prop_7_l2_q25),
                    ymax = (accuracy_benchmark_prop_7_l2_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error (Predictions 1 week ahead)") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(data=df %>% filter(Method =="SSNAL-opt2", 
                          graph_type== "knn",
                          #graph_type!= "pa"
),
)+
  geom_point(aes(x = beta , y = (Accuracy_true_p_q50), colour="Denoised Estimate (with CV)"),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (Accuracy_true_p_q50), 
                colour="Denoised Estimate (with CV)"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (Accuracy_true_p_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate (with CV)"), 
                alpha=1, width=0.03, linewidth=1)+
  geom_point(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_q50), colour="Benchmark"), alpha=1,
             size=2.2)+
  geom_line(aes(x = beta-  0.03, y = (accuracy_benchmark_prop_7_q50), colour="Benchmark"),
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x =  beta-  0.03, ymin = (accuracy_benchmark_prop_7_q25),
                    ymax = (accuracy_benchmark_prop_7_q75), colour="Benchmark"), alpha=1,
                width=0.03, linewidth=1
  )+
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error (Predictions 1 week ahead)") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


min(df$counts)



ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(aes(x = beta , y = (n_infected_q50)),
             alpha=1, size=2.2)+
  geom_line(aes(x = beta , y = (n_infected_q50)), 
            alpha=1,
            linewidth=1)+
  geom_errorbar(aes(x = beta , ymin = (n_infected_q25),
                                ymax = (n_infected_q75)),
                            alpha=1, width=0.03, linewidth=1)+
  scale_y_log10() + 
  #scale_x_log10() +
  facet_grid(graph_type_name~steps_name, scales="free_y") +
  ylab("Nb of Infections") +
  labs(colour = "Method:") + 
  xlab("Beta") +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


df = res %>% filter(
                    gamma ==0.1,
                    #m>1,
                    steps ==30,
                    graph_type == "knn",
                    #counts > 100,
                    #p == 0,
                    #m <2,
                    # graph_type %in% c("Berkeley" ,
                    #                   #"SBM",
                    #                  "pa", "power_law",
                    #                   "small-world")
)
df <- df %>%
  mutate(
    steps_name= paste0( 
      "Steps = ", steps))
df$graph_type_name <- factor(df$graph_type_name , levels = c(" k-NN graph\n( k=2)",
                                                             " k-NN graph\n( k=5)",
                                                             " k-NN graph\n( k=7)" ,
                                                             " k-NN graph\n( k=10)"))


df <- df %>%
 mutate(name_exp = paste0(  "Beta = ", beta),
   steps_name= paste0(
     "Steps = ", steps))

df$graph_type_name <- factor(df$graph_type_name , levels = c(" k-NN graph\n( k=2)",
                                                             " k-NN graph\n( k=5)",
                                                             " k-NN graph\n( k=7)" ,
                                                             " k-NN graph\n( k=10)"))


df$name_exp <- factor(df$name_exp , levels = c("Beta = 0.2",
                                               "Beta = 0.5",
                                               "Beta = 0.7",
                                               "Beta = 0.9"))

ggplot(data=df %>% filter(Method =="SSNAL-opt2"))+
  geom_point(
             aes(x = alpha_fp , y =1 *  (Accuracy_true_p), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(
            aes(x = alpha_fp , y = 1 * (Accuracy_true_p), colour="Denoised Estimate\n(With CV)"),
            alpha=1, linewidth=0.8)+
  geom_errorbar(
                aes(x = alpha_fp , ymin =1 * (Accuracy_true_p_q25),
                    ymax = 1 * (Accuracy_true_p_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.005)+
  geom_point(
             aes(x = alpha_fp-  0.005, y = 1 * (bench_Accuracy_true_p), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(
            aes(x = alpha_fp-  0.005, y = 1 * (bench_Accuracy_true_p), colour="Benchmark"), alpha=1,
            linewidth=0.8)+
  geom_errorbar(
                aes(x =  alpha_fp-  0.005, ymin = 1 * (bench_Accuracy_true_p_q25),
                    ymax = 1 * (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
                width=0.005
  )+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(name_exp~graph_type_name, scales="free_y") +
  #scale_y_log10() + 
  #scale_x_log10() +
  ylab("l1 error") +
  labs(colour = "Method") + 
  xlab("False Positive Rate")  +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )

ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, graph_type_name, steps_name, 
                  alpha_fp, name_exp) %>%
         summarise_if(is.numeric,mean))+
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (Accuracy_true_p), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
              group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (Accuracy_true_p), colour="Denoised Estimate\n(With CV)"),
            alpha=1, linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt2") %>%
                  group_by((beta), alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x = alpha_fp , ymin = (Accuracy_true_p_q25),
                    ymax = (Accuracy_true_p_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.005)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
              group_by(beta, alpha_fp, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp-  0.005, y = (bench_Accuracy_true_p), colour="Benchmark"), alpha=1,
            linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt2") %>%
                  group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x =  alpha_fp-  0.005, ymin = (bench_Accuracy_true_p_q25),
                    ymax = (bench_Accuracy_true_p_q75), colour="Benchmark"), alpha=1,
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
  ylab("l1 error") +
  labs(colour = "Method") + 
  xlab("False Positive Rate")  +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(data=df %>% filter(Method =="SSNAL-opt2") %>%
         group_by(beta, steps, m, graph_type_name, steps_name, 
                  alpha_fp, name_exp) %>%
         summarise_if(is.numeric,mean))+
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (accuracy_prop_7_q50), colour="Denoised Estimate\n(With CV)"),
             alpha=1, size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
              group_by(alpha_fp, beta, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (accuracy_prop_7_q50), colour="Denoised Estimate\n(With CV)"),
            alpha=1, linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt2") %>%
                  group_by((beta), alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x = alpha_fp , ymin = (accuracy_prop_7_q25),
                    ymax = (accuracy_prop_7_q75), colour="Denoised Estimate\n(With CV)"), 
                alpha=1, width=0.005)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                        name_exp) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (accuracy_benchmark_prop_7_l2_q50), colour="Benchmark"), alpha=1,
             size=2)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
              group_by(beta, alpha_fp, steps, m, graph_type_name, steps_name,
                       name_exp) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp-  0.005, y = (accuracy_benchmark_prop_7_l2_q50), colour="Benchmark"), alpha=1,
            linewidth=0.8)+
  geom_errorbar(data=df %>% filter(Method =="SSNAL-opt2") %>%
                  group_by(beta,alpha_fp, steps, m, graph_type_name, steps_name,
                           name_exp) %>%
                  summarise_if(is.numeric,mean),
                aes(x =  alpha_fp-  0.005, ymin = (accuracy_benchmark_prop_7_l2_q25),
                    ymax = (accuracy_benchmark_prop_7_l2_q75), colour="Benchmark"), alpha=1,
                width=0.005
  )+
  #geom_point(aes(x = Lambda, y = Accuracy_true_p)) +
  #geom_errorbar(data = res_q%>% filter(alpha_fp == 0),
  #aes(x = Lambda, ymin = Accuracy_true_p_q25,ymax = Accuracy_true_p_q75,
  #    colour = graph_type),
  #alpha=0.3) +
  facet_grid(graph_type_name~name_exp, scales="free_y") +
  scale_y_log10() + 
  #scale_x_log10() +
  ylab("l2 error: 1-week prediction") +
  labs(colour = "Method") + 
  xlab("False Positive Rate")  +
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )



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
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(alpha_fp, beta, steps, m, name_exp, steps_name) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp , y = (accuracy_prop_0), colour="Denoised Estimate\n(With CV)"),
             alpha=1)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
              group_by(alpha_fp, beta, steps, m, name_exp, steps_name) %>%
              summarise_if(is.numeric,mean),
            aes(x = alpha_fp , y = (accuracy_prop_0), colour="Denoised Estimate\n(With CV)"),
            alpha=1)+
  geom_point(data=df %>% filter(Method =="SSNAL-opt2") %>%
               group_by(beta,alpha_fp, steps, m, name_exp, steps_name) %>%
               summarise_if(is.numeric,mean),
             aes(x = alpha_fp-  0.005, y = (accuracy_benchmark_prop_0), colour="Benchmark"), alpha=1)+
  geom_line(data=df %>% filter(Method =="SSNAL-opt2") %>%
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





