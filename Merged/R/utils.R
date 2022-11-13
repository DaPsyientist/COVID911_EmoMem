# Various utility functions
# Written by Haoxue Fan

# load packages ----------------------------------------------------------------

if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2,  dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR,
       sjPlot, car, multcomp, tidyr, betareg, glmmTMB, foreign,magrittr, bayesplot, emmeans,
       ggpubr, Hmisc) #Load necessary rPackages
devtools::install_github("traversc/trqwe")
options(mc.cores = parallel::detectCores())
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}
reverse_beta_squeeze <- function(y2, n, likert_min_max=NULL) {
  y <- (n * y2 -  0.5) / (n - 1)
  if (!is.null(likert_min_max)){
    y <- (y * (likert_min_max[2]-likert_min_max[1])) + likert_min_max[1]
  }
  return(y)
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

plot_model_IVs_stan <- function(your_model, variable_array, x_range=seq(-2.5, 2.5, by = 0.5), return_df = FALSE){
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
  if (return_df){return(df.combined)}
  else{return(p)}
}

plot_model_int_catcont <- function(your_model, variable_array, fct_array, x_range=seq(-2.5, 2.5, by = 0.5), fct_range='all',ymin_threshold = NA,
                                   transform_x=TRUE, x_mean_scale_array=c(0,1)){
  
  #' plot predicted DV as a function of the interaction between a continuous IV and a categorical IV (needs to be a factor)
  #' inputs:
  #'         your model: fitted model that sjPlot() accept as an input
  #'         variable_array: a string array for the continuous IV
  #'         fct_array: a string array for the categorical IV
  #'         x_range: the range and sample frequency of the continuous IV. optional.
  #'         fct_range: the range of the categorical IV. optional.
  #'         ymin_threshold: the minimum DV allowed. optional.
  #' output:
  #'         plot object (can be modified by ggplot2)
  
  df.combined <- data.frame()
  variable_array_range <- paste0(variable_array, '[',
                                 paste0(x_range, collapse = ','),
                                 ']', collapse = '')
  predict.loop <- your_model %>%
    get_model_data(type = "pred", terms = c(variable_array_range, fct_array))
  if (transform_x){
    predict.loop$x <- x_mean_scale_array[1] + predict.loop$x * x_mean_scale_array[2]
  }
  df.combined <- predict.loop %>% select(x, predicted, conf.low, conf.high,
                                         group_col) %>%
    mutate(x_name = variable_array) %>%
    mutate(ymin = conf.low,
           ymax = conf.high,
           !!fct_array := group_col)
  
  if (fct_range != 'all'){
    df.combined <- df.combined %>% filter(group_col %in% fct_range)
  }
  
  if (!is.na(ymin_threshold)){
    df.combined$ymin[df.combined$ymin < ymin_threshold] <- ymin_threshold
  }
  
  p <- df.combined %>% ggplot(aes_string(x = "x", y = "predicted", color = fct_array)) +
    geom_ribbon(aes_string(ymin = "ymin",
                           ymax = "ymax" , fill = fct_array), alpha = 0.2, color=NA, show.legend = FALSE) +
    geom_line() +
    xlab(variable_array)
  
  return(p)
}
# var ---------------------------------------------------------------------
emomem_theme <- theme_pubr(legend = "bottom")+
  theme(text = element_text(size=24),
        axis.text=element_text(size=24),
        legend.text = element_text(size = 20),
        axis.line = element_line(size = 0.7),
        legend.margin = margin(0, -10, 0, 0),
        legend.box.margin=margin(-15,-10,0,-10),
        legend.key.size = unit(2,"line"))
palette2var <- c('black','grey')
palette3var <- c('black','grey','darkred')
