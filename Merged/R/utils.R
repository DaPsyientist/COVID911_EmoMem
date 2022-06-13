# Various utility functions
# Written by Haoxue Fan

# load packages ----------------------------------------------------------------

if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2,  dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR,
       sjPlot, car, multcomp, tidyr, betareg, glmmTMB, foreign,magrittr, bayesplot, emmeans) #Load necessary rPackages
devtools::install_github("traversc/trqwe")
options(mc.cores = parallel::detectCores())
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}
#Function to convert logodds to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


# function ----------------------------------------------------------------

plot_model_IVs <- function(your_model, variable_array, x_range=seq(-2.5, 2.5, by = 0.5)){
  #' plot predicted DV as a function of one (or many) IV(s)
  #' inputs:
  #'         your model: fitted model that sjPlot() accept as an input
  #'         variable_array: a string array for the IV(s)
  #'         x_range: the range and sample frequency of the IV(s). optional.
  #' output:
  #'         plot object (can be modified by ggplot2)
  
  df.combined <- data.frame()
  for (i in c(1:length(variable_array))){
    # calculate prediction
    predict.loop <- your_model %>%
      get_model_data(type = "pred", terms = paste0(variable_array[i], '[', paste0(x_range, collapse = ','), ']', collapse = ''))
    
    df.loop <- predict.loop %>% dplyr::select(x, predicted, std.error) %>%
      mutate(x_name = variable_array[i]) %>%
      mutate(ymin = predicted - std.error,
             ymax = predicted + std.error)
    
    df.combined <- df.combined %>% rbind(df.loop)
  }
  
  p <- df.combined %>% ggplot(aes_string(x = "x", y = "predicted", group = "x_name", color = "x_name")) +
    geom_line() +
    geom_ribbon(aes_string(ymin = "ymin",
                           ymax = "ymax" , fill = "x_name"), alpha = 0.3, color=NA,
                show.legend = FALSE)
  return(p)
}

plot_model_IVs_stan <- function(your_model, variable_array, x_range=seq(-2.5, 2.5, by = 0.5)){
  #' plot predicted DV as a function of one (or many) IV(s)
  #' inputs:
  #'         your model: fitted model that sjPlot() accept as an input
  #'         variable_array: a string array for the IV(s)
  #'         x_range: the range and sample frequency of the IV(s). optional.
  #' output:
  #'         plot object (can be modified by ggplot2)
  
  df.combined <- data.frame()
  for (i in c(1:length(variable_array))){
    # calculate prediction
    predict.loop <- your_model %>%
      get_model_data(type = "pred", terms = paste0(variable_array[i], '[', paste0(x_range, collapse = ','), ']', collapse = ''))
    
    df.loop <- predict.loop %>% dplyr::select(x, predicted, conf.low, conf.high) %>%
      mutate(x_name = variable_array[i]) %>%
      mutate(ymin = conf.low,
             ymax = conf.high)
    
    df.combined <- df.combined %>% rbind(df.loop)
  }
  
  p <- df.combined %>% ggplot(aes_string(x = "x", y = "predicted", group = "x_name", color = "x_name")) +
    geom_line() +
    geom_ribbon(aes_string(ymin = "ymin",
                           ymax = "ymax" , fill = "x_name"), alpha = 0.3, color=NA,
                show.legend = FALSE)
  return(p)
}
# var ---------------------------------------------------------------------
# todo: emomem_theme 
emomem_theme <- theme_pubr(legend = "bottom")+
  theme(text = element_text(size=18),
        axis.text=element_text(size=18),
        legend.text = element_text(size = 15),
        axis.line = element_line(size = 0.7),
        legend.margin = margin(0, -10, 0, 0),
        legend.box.margin=margin(-15,-10,0,-10),
        legend.key.size = unit(2,"line"))
