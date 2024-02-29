library(tidyverse)
setwd("~/Documents/epidemic_modelling/experiments/results/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/python/experiments/results"

file_list <- list.files(folder_path, pattern = "^paramknn", full.names = TRUE)

data_param <- bind_rows(lapply(file_list, read.csv))


#data_param <- read_csv("~/Documents/epidemic_modelling/python/experiments/results/param_binarized_17367310_2.csv")
data_param <- data_param %>% filter(m !=0)
#data_param <- data_param %>% filter(m !=0, alpha_fp %in% c(0, 0.01, 0.05))
data_param$beta.ours[which(data_param$beta.ours <0)] = 0
data_param$beta.ours[which(data_param$beta.ours >1
                           )] = 1
data_param$beta.naive[which(data_param$beta.naive <0)] = 0
data_param$beta.naive[which(data_param$beta.naive >1
)] = 1
data_param$beta.oracle[which(data_param$beta.oracle <0)] = 0
data_param$beta.oracle[which(data_param$beta.oracle >1
)] = 1

data_param$gamma.ours[which(data_param$gamma.ours <0)] = 0
data_param$gamma.ours[which(data_param$gamma.ours >1
)] = 1
data_param$gamma.naive[which(data_param$gamma.naive <0)] = 0
data_param$gamma.naive[which(data_param$gamma.naive >1
)] = 1
data_param$gamma.oracle[which(data_param$gamma.oracle <0)] = 0
data_param$gamma.oracle[which(data_param$gamma.oracle >1
)] = 1


data_param["beta.err"] = abs(data_param$beta.ours - data_param$beta)
data_param["beta.naive.err"] = abs(data_param$beta.naive- data_param$beta)
data_param["beta.oracle.err"] = abs(data_param$beta.oracle - data_param$beta)
data_param["gamma.err"] = abs(data_param$gamma.ours - data_param$gamma)
data_param["gamma.naive.err"] = abs(data_param$gamma.naive- data_param$gamma)
data_param["gamma.oracle.err"] = abs(data_param$gamma.oracle - data_param$gamma)


data_param["R0"] = data_param["beta"]/data_param["gamma"]
data_param["R0.ours"] = data_param["beta.ours"]/data_param["gamma.ours"]
data_param["R0.naive"] = data_param["beta.naive"]/data_param["gamma.naive"]
median_abs <- function(x){mean(abs(x))}


test = data_param %>%
  group_by(steps,  m,
           gamma, beta, K) %>%
  summarise(counts=n())

unique(data$alpha_fp)
median_abs <- function(x){median(abs(x), na.rm=TRUE)} 
q25_abs <- function(x){quantile(abs(x), 0.25, na.rm=TRUE)} 
q75_abs <- function(x){quantile(abs(x), 0.75, na.rm=TRUE)} 
res_param  = data_param %>%
  group_by(steps,  m,K,
           gamma, beta) %>%
  summarise_all(median_abs)

q25_rename <- function(x){paste0(x, "_q25")}
q75_rename <- function(x){paste0(x, "_q75")}
res_q25  = data_param %>%
  group_by(steps,  m,K,
           gamma, beta) %>%
  summarise_all(q25_abs) 
res_q25_groups <- res_q25|>
  ungroup() |>
  select(c("steps",  "m","K",
            "gamma", "beta")) 
res_q25 <- res_q25|>
  ungroup() |>
  select(-c("steps",  "m", "K",
            "gamma", "beta")) |>
  rename_all(
         q25_rename)
res_q25 <- cbind(res_q25_groups,
                 res_q25)

res_q75  = data_param %>%
  group_by(steps,  m,K,
           gamma, beta) %>%
  summarise_all(q75_abs)

res_q75_groups <- res_q75|>
  ungroup() |>
  select(c("steps",  "m","K",
           "gamma", "beta")) 
res_q75 <- res_q75|>
  ungroup() |>
  select(-c("steps",  "m", "K",
            "gamma", "beta")) |>
  rename_all(
    q75_rename)
res_q75 <- cbind(res_q75_groups,
                 res_q75)

res_param <- res_param |>
  left_join(res_q25, by = c("steps",  "m", "K",
                            "gamma", "beta")) |>
  left_join(res_q75, by = c("steps",  "m", "K",
                            "gamma", "beta"))


res_param = res_param%>%
  mutate(graph_type_name = paste0(" k-NN graph\n( k=", m, ")"))


unique(res_param$m)
unique(res_param$alpha_fp)
unique(res_param$steps)



res_param = res_param %>% 
  left_join(test, by = c("steps", "m", "gamma", "beta",
                         "K"))
test %>% filter(counts > 50)


test = df %>% filter(counts > 50)
relevant = res_param %>% filter(
                          steps ==20,
                          K==20)  %>%
  mutate(res_beta = paste0(round(beta.ours,2), " (", round(beta.ours_q25,2), ", ", round(beta.ours_q75,2), ")"),
         res_gamma = paste0(round(gamma.ours,2), " (", round(gamma.ours_q25,2), ", ", round(gamma.ours_q75,2), ")"),
         res_gamma_naive= paste0(round(gamma.naive,2), " (", round(gamma.naive_q25,2), ", ", round(gamma.naive_q75,2), ")"),
         res_beta_naive = paste0(round(beta.naive,2), " (", round(beta.naive_q25,2), ", ", round(beta.naive_q75,2), ")"),
         res_R0_naive = paste0(round(R0.naive,2), " (", round(R0.naive_q25,2), ", ", round(R0.naive_q75,2), ")"),
         res_R0_ours = paste0(round(R0.ours,2), " (", round(R0.ours_q25,2), ", ", round(R0.ours_q75,2), ")")
         ) %>%
  ungroup() %>%
  select(m, beta, gamma, res_beta, res_gamma,
         res_beta_naive,res_gamma_naive, res_R0_ours, res_R0_naive)
                     #beta < 0.9)

relevant
library(knitr)
library(kableExtra)

latex_table <- kable(relevant, format = "latex", booktabs = TRUE)
print(latex_table)


df <- res_param %>%
  mutate(beta_name = paste0(  "Beta = ", beta),
         steps_name= paste0( 
           "Steps = ", steps),
         graph_type_name = paste0(" k-NN graph\n( k=", m, ")"),
         K_name = paste0("K = ", K),
         )
ggplot(df %>% filter(steps==30, K==20), 
       aes(x= beta, y = abs(beta.err),
           colour = "Denoised Estimate\n(With CV)"))+
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=beta.err_q25,
                    ymax=beta.err_q75, colour = "Denoised Estimate\n(With CV)")) + 
  geom_point(aes(x= beta, y = beta.naive.err, colour = "Benchmark")) + 
  geom_line(aes(x= beta, y = beta.naive.err, colour = "Benchmark")) + 
  geom_errorbar(aes(ymin=beta.naive.err_q25,
                    ymax=beta.naive.err_q75, colour = "Benchmark")) +
  # geom_point(aes(x= beta, y = beta.oracle.err, colour = "Oracle")) + 
  # geom_line(aes(x= beta, y = beta.oracle.err, colour = "Oracle")) + 
  # geom_errorbar(aes(ymin=beta.oracle.err_q25,
  #                   ymax=beta.oracle.err_q75, colour = "Oracle")) +
  #geom_point(aes(x= beta, y = err.beta.oracle, colour = "oracle")) + 
  #scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(K_name~graph_type_name, scales =  "free_y" )+
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm") ) # Adjust the spacing as needed


ggplot(df %>% filter(steps==20, K==20), 
       aes(x= beta, y = abs(gamma.err),
           colour = "Denoised Estimate\n(With CV)"))+
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=gamma.err_q25,
                    ymax=gamma.err_q75, colour = "Denoised Estimate\n(With CV)")) + 
  geom_point(aes(x= beta, y = beta.naive.err, colour = "Benchmark")) + 
  geom_line(aes(x= beta, y = beta.naive.err, colour = "Benchmark")) + 
  geom_errorbar(aes(ymin=beta.naive.err_q25,
                    ymax=beta.naive.err_q75, colour = "Benchmark")) +
  # geom_point(aes(x= beta, y = beta.oracle.err, colour = "Oracle")) + 
  # geom_line(aes(x= beta, y = beta.oracle.err, colour = "Oracle")) + 
  # geom_errorbar(aes(ymin=beta.oracle.err_q25,
  #                   ymax=beta.oracle.err_q75, colour = "Oracle")) +
  #geom_point(aes(x= beta, y = err.beta.oracle, colour = "oracle")) + 
  #scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(K_name~graph_type_name, scales =  "free_y" )+
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm") ) # Adjust the spacing as needed



ggplot(df %>% filter(steps==20, K==20), 
       aes(x= beta, y = abs(err.beta.ours),
           colour = "Denoised Estimate\n(With CV)"))+
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=err.beta.ours_q25,
                    ymax=err.beta.ours_q75, colour = "Denoised Estimate\n(With CV)")) + 
  geom_point(aes(x= beta, y = err.beta.naive, colour = "Benchmark")) + 
  geom_line(aes(x= beta, y = err.beta.naive, colour = "Benchmark")) + 
  geom_errorbar(aes(ymin=err.beta.naive_q25,
                    ymax=err.beta.naive_q75, colour = "Benchmark")) +
  geom_point(aes(x= beta, y = err.beta.oracle, colour = "Oracle")) + 
  geom_line(aes(x= beta, y = err.beta.oracle, colour = "Oracle")) + 
  geom_errorbar(aes(ymin=err.beta.oracle_q25,
                    ymax=err.beta.oracle_q75, colour = "Oracle")) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  #scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(K_name~graph_type_name, scales =  "free_y" )+
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )


ggplot(df %>% filter(steps ==30), 
       aes(x= beta, y = abs(err.beta.ours), beta 
           colour = "Denoised Estimate\n(With CV)"))+
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=err.beta.ours_q25,
                    ymax=err.beta.ours_q75, colour = "Denoised Estimate\n(With CV)")) + 
  geom_point(aes(x= beta, y = err.beta.naive, colour = "Benchmark")) + 
  geom_line(aes(x= beta, y = err.beta.naive, colour = "Benchmark")) + 
  geom_errorbar(aes(ymin=err.beta.naive_q25,
                    ymax=err.beta.naive_q75, colour = "Benchmark")) +
  geom_point(aes(x= beta, y = err.beta.oracle, colour = "Oracle")) + 
  geom_line(aes(x= beta, y = err.beta.oracle, colour = "Oracle")) + 
  geom_errorbar(aes(ymin=err.beta.oracle_q25,
                    ymax=err.beta.oracle_q75, colour = "Oracle")) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  #scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(K~m, scales =  "free_y" )+
  theme(legend.position = "top",legend.spacing.x = unit(1, "cm")  # Adjust the spacing as needed
  )

df = res_param %>% filter(alpha_fp == 0.0,
                          beta > 0.2, 
                          #beta < 0.9,
                          m <20,
                          steps == 20)
df$graph_type_name <- factor(df$graph_type_name , levels = unique(df$graph_type_name))

df <- df %>%
  mutate(name_exp = paste0(  "Beta = ", beta),
         steps_name= paste0( 
           "Steps = ", steps))
ggplot(df, 
       aes(x= beta, y = abs(err.beta.ours), colour = "Denoised Estimate\n(With CV)"))+
  geom_line(linewidth=1) + 
  geom_point(size=2.3) + 
  geom_errorbar(aes(ymin=err.beta.ours_q25,
                    ymax=err.beta.ours_q75, colour = "Denoised Estimate\n(With CV)"),
                width=0.02, linewidth=1) + 
  geom_point(aes(x= beta -0.005, y = err.beta.naive, colour = "Benchmark"),size=2.3) + 
  geom_line(aes(x= beta -0.005, y = err.beta.naive, colour = "Benchmark"),
            linewidth=1) + 
  geom_errorbar(aes(x = beta -0.005, ymin=err.beta.naive_q25,
                    ymax=err.beta.naive_q75, colour = "Benchmark"),
                width=0.02, linewidth=1) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(graph_type_name~K, scales =  "free" )



ggplot(df, 
       aes(x= beta, y = abs(err.gamma.ours), colour = "Denoised Estimate\n(With CV)"))+
  geom_line(linewidth=1) + 
  geom_point(size=2.3) + 
  geom_errorbar(aes(ymin=err.gamma.ours_q25,
                    ymax=err.gamma.ours_q75, colour = "Denoised Estimate\n(With CV)"),
                width=0.02, linewidth=1) + 
  geom_point(aes(x= beta -0.005, y = err.gamma.naive, colour = "Benchmark"),size=2.3) + 
  geom_line(aes(x= beta -0.005, y = err.gamma.naive, colour = "Benchmark"),
            linewidth=1) + 
  geom_errorbar(aes(x = beta -0.005, ymin=err.gamma.naive_q25,
                    ymax=err.gamma.naive_q75, colour = "Benchmark"),
                width=0.02, linewidth=1) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(graph_type_name~K, scales =  "free" )
             
             

ggplot(df, 
       aes(x= K, y = abs(err.gamma.ours), colour = "Denoised Estimator"))+
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=err.gamma.ours_q25,
                    ymax=err.gamma.ours_q75, colour = "Denoised Estimator")) + 
  geom_point(aes(x= K, y = err.gamma.naive, colour = "Naive Estimator")) + 
  geom_line(aes(x= K, y = err.gamma.naive, colour = "Naive Estimator")) + 
  geom_errorbar(aes(ymin=err.gamma.naive_q25,
                    ymax=err.gamma.naive_q75, colour = "Naive Estimator")) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  scale_y_log10()+
  ylab("Error in Gamma")+
  labs(colour = "Method") + 
  facet_grid(K~name_exp, scales =  "free" )


df = res_param %>% filter( m<50, m>2,
                           beta > 0.2,
                           #steps == 20,
                          # alpha_fp <0.05,
                          K ==30)

df <- df %>%
  mutate(fpr_exp = paste0(  "False Positive Rate = ", alpha_fp),
         name_exp = paste0(  "k-NN graph\nk = ", m),
           steps_name= paste0( 
             "Steps = ", steps))
df$name_exp <- factor(df$name_exp , levels = c("k-NN graph\nk = 2",
                                                        "k-NN graph\nk = 5",
                                                        "k-NN graph\nk = 7",
                                                        "k-NN graph\nk = 10"))


ggplot(df, 
       aes(x= beta, y = abs(err.gamma.ours), colour = "Denoised Estimator"))+
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=err.gamma.ours_q25,
                    ymax=err.gamma.ours_q75, colour = "Denoised Estimator"),
                width=0.05) + 
  geom_point(aes(x= beta, y = err.gamma.naive, colour = "Naive Estimator")) + 
  geom_line(aes(x= beta, y = err.gamma.naive, colour = "Naive Estimator")) + 
  geom_errorbar(aes(ymin=err.gamma.naive_q25,
                    ymax=err.gamma.naive_q75, colour = "Naive Estimator"),
                width=0.05) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  scale_y_log10()+
  ylab("Error in Gamma")+
  labs(colour = "Method") + 
  facet_grid(name_exp  ~fpr_exp, scales =  "free" )

ggplot(df, 
       aes(x= beta, y = abs(err.beta.ours), colour = "Denoised Estimator"))+
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=err.beta.ours_q25,
                    ymax=err.beta.ours_q75, colour = "Denoised Estimator"),
                width=0.05) + 
  geom_point(aes(x= beta, y = err.beta.naive, colour = "Naive Estimator")) + 
  geom_line(aes(x= beta, y = err.beta.naive, colour = "Naive Estimator")) + 
  geom_errorbar(aes(ymin=err.beta.naive_q25,
                    ymax=err.beta.naive_q75, colour = "Naive Estimator"),
                width=0.05) +
  #geom_point(aes(x= K, y = err.beta.oracle, colour = "oracle")) + 
  scale_y_log10()+
  ylab("Error in Beta")+
  labs(colour = "Method") + 
  facet_grid(name_exp  ~fpr_exp, scales =  "free" )

ggplot(res %>% filter(beta > 0.2, alpha_fp == 0.05), 
       aes(x= K, y = abs(err.gamma.ours), colour = "Estimator"))+
  geom_point() + 
  geom_point(aes(x= K, y = err.gamma.naive, colour = "naive")) + 
  geom_point(aes(x= K, y = err.gamma.oracle, colour = "oracle")) + 
  scale_y_log10()+
  facet_grid(steps~beta, scales =  "free" )
