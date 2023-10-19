library(viridis)
library(magick)
library(ggraph)
library(tidyverse)

plot_results_on_graph <- function(g, store_state, variable, variable_name,
                         name_plot) {
  for (t in 1:ncol(store_state)){
    V(g)$color <- sapply(store_state[, t],
                           function(x) {
                             max(log10(x / 1e-5), 0)
                           }) / 5
    plot <- ggraph(g, layout = layout) +
              geom_node_point(aes(colour = color), size=2) + 
              geom_edge_link() +         # Add edges as links
              labs(colour = "Probability") + 
              scale_colour_gradient(low = "navy", high = "red", limits = c(0,1))+
              theme_bw()  +             # Remove axis labels and gridlines
              labs(paste0(variable_name, " = ", variable[t]))   
    title_plot = paste0(name_plot, "-t-",t,  ".png")
    print(plot)
    ggsave(title_plot, plot = plot, width = 9, height = 9)
    # Load an image
    image <- image_read(title_plot)
    # Resize it to even dimensions
    image_resized <- image_resize(image, "2832x2832")
    image_write(image_resized, paste0("resized", title_plot))
    Sys.sleep(1)
  }
}


plot_results <- function(res){
    ggplot(res%>%
             select(starts_with("l1_error_"), lambda)  %>%
             filter(lambda<1e-2) %>%
             pivot_longer(cols = -c("lambda"), names_to = "variable", values_to = "value") %>%
             mutate(number = as.integer(gsub("^l1_error_", "", variable)))) +
      geom_line(aes(x=number, y=value, colour = as.factor(lambda))) +theme_bw()
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
    
    ggplot(res %>% filter(lambda<0.01),
           aes(x=lambda, y=l1_error)) +
      scale_y_log10() +
      scale_x_log10() + 
      geom_line() +theme_bw() +geom_point()
    i.d = which.min(res$l1_error_1)
    ggplot(res[c(1, 2, 4, i.d, 10, 20),] %>%
             rename(l1_error_at_step_0 = l1_error) %>%
             select(starts_with("l1_error_"), lambda)  %>%
             pivot_longer(cols = -c("lambda"), names_to = "variable", values_to = "value") %>%
             mutate(number = as.integer(gsub("^l1_error_", "", variable)))) +
      scale_y_log10() +
      scale_x_log10() + 
      geom_line(aes(x=number, y=value, colour=as.factor(lambda))) +theme_bw()
}