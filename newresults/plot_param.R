library(tidyverse)
setwd("~/Documents/epidemic_modelling/newresults/")
theme_set(theme_bw(base_size = 14))
folder_path <- "~/Documents/epidemic_modelling/newresults"
file_list <- list.files(folder_path, pattern = "param*", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data_param <- bind_rows(lapply(file_list, read.csv))


data_param %>%
  filter(beta !=0) %>% group_by(beta, gamma, m,  steps)%>%
  summarise(beta.ours = quantile(abs(beta.ours), 0.5),
            gamma.ours = median(abs(gamma.ours)),
            beta.naive = median(abs(beta.naive)),
            gamma.naive = median(abs(gamma.naive)),
            beta.oracle= median(abs(beta.oracle)),
            gamma.oracle = median(abs(gamma.oracle)))
library(knitr)
library(kableExtra)
latex_table <- kable(data_param %>%
                       filter(beta !=0) %>% group_by(beta, gamma, m,  steps)%>%
                       summarise(beta.ours = quantile(abs(beta.ours), 0.5),
                                 gamma.ours = median(abs(gamma.ours)),
                                 beta.naive = median(abs(beta.naive)),
                                 gamma.naive = median(abs(gamma.naive)),
                                 beta.oracle= median(abs(beta.oracle)),
                                 gamma.oracle = median(abs(gamma.oracle))), format = "latex", booktabs = TRUE)
latex_table


file_list <- list.files(folder_path, pattern = "adam*", full.names = TRUE)
#file_list <- c(file_list, list.files(folder_path, pattern = "^759", full.names = TRUE))
# Read all the files into a single data frame using map_dfr.
data_param <- bind_rows(lapply(file_list, read.csv))


data_param %>%
  filter(beta !=0) %>% group_by(beta, gamma, m,  steps)%>%
  summarise_all(mean)
library(knitr)
library(kableExtra)
latex_table <- kable(data_param %>%
                       filter(beta !=0) %>% group_by(beta, gamma, m,  steps)%>%
                       summarise(beta.ours = quantile(abs(beta.ours), 0.5),
                                 gamma.ours = median(abs(gamma.ours)),
                                 beta.naive = median(abs(beta.naive)),
                                 gamma.naive = median(abs(gamma.naive)),
                                 beta.oracle= median(abs(beta.oracle)),
                                 gamma.oracle = median(abs(gamma.oracle))), format = "latex", booktabs = TRUE)
latex_table
