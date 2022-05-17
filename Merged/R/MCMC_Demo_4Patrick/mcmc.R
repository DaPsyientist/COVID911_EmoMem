### Questions about MCMC for Patrick
### from Johnny & Haoxue

# general set up ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2,  dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR,
       sjPlot, car, foreign) #Load necessary rPackages
options(mc.cores = parallel::detectCores())
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

# the dataset we used is questionnaire data for 9/11 attack (thats why it is called nine11)
# there was one immediate questionnaire (time1) are three follow up surveys (time2/3/4, 1/5/10 years after). 
# At time1, participants were asked the intensity of their emotion evoked by 9/11, on a scale 1-5 (can rate decimal) for 6 emotions
# At time2/3/4, participants were asked (1) the current intensity of emotions (same scale, same items)
#                                       (2) the remembered intensity of emotion evoked by 9/11 (same scale, same items)
# in nine11_wide, we calculate 
# (1) prev_experience = avg of experienced emotion at t1,
# (2) curr_experience = avg of experienced emotion at t2/3/4,
# (3) stress_memory = avg of remembered emotion at t2/3/4
# dev = avg of (remembered emo at t2/3/4 - experienced emo at t1)
# num_weights: # of emo used to calculate dev (ppl may provide no answer to specific remembered or experienced emo)

nine11_wide_avg <- read.csv('df.emo.wide.avg_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is avg across emotions
nine11_wide <- read.csv('df.emo.wide_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is specific to one emotion
niters <- 1000 # to reduce time cost, set niters for each chain (4 chains in total) to 1000 

# Stat Q 1: how to use weight in stan_glm? ----------------------
# Research q: how does prev_experience and curr_experience influences stress_memory
# because stress_memory is restricted [1,5], we used a beta regression and transformed stress_memory into [0, 1]
nine11_BRem <- nine11_wide_avg %>% mutate(time = relevel(factor(time), ref='2')) %>% 
  group_by(time) %>% mutate(B911_Rem = beta_squeeze((stress_memory-1)/4)) %>% 
  filter(!is.na(B911_Rem))

# We want to give more weight to ppl who have reported more emotions (both at t1 and at t2/3/4 for remembered emo), we used num_weights to weight observations

# prob with Rem_911_MCMC_T2_weighted: effective sample size is small (esp for intercept and prev experience)
Rem_911_MCMC_T2_weighted <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                       family = mgcv::betar, iter= niters, 
                                       weights = num_weights, data = nine11_BRem)
describe_posterior(Rem_911_MCMC_T2_weighted)

# prob with Rem_911_MCMC_T2_unweighted: collinearity? 
Rem_911_MCMC_T2_unweighted <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                         family = mgcv::betar, iter= niters, 
                                         data = nine11_BRem )
describe_posterior(Rem_911_MCMC_T2_unweighted)

Rem_911_MCMC_T2_weighted_1 <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                         family = mgcv::betar, iter= niters,
                                         weights = num_weights_1,
                                         data = nine11_BRem %>% mutate(num_weights_1 = 1))
describe_posterior(Rem_911_MCMC_T2_weighted_1)

# prob with Rem_911_MCMC_T2_weighted_2 & Rem_911_MCMC_T2_weighted_1 & Rem_911_MCMC_T2_weighted: results are different when equal weight is assigned to all observations 
Rem_911_MCMC_T2_weighted_2 <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                         family = mgcv::betar, iter= niters,
                                         weights = num_weights_1,
                                         data = nine11_BRem %>% mutate(num_weights_1 = 2))
describe_posterior(Rem_911_MCMC_T2_weighted_2)




# Stat Q 2: Should we use % or the raw number as DV? ----------------------
# Research q: How does dev changes as a function of time (time 2/3/4)
# The reason we want to use % (i.e., dev / prev_experience) is that we think it could account for the effect of 'baseline'. We then used Gaussian family for the bayesian linear regression.
# Does using % make sense? should we use the raw score instead?

nine11_percent <- nine11_wide_avg %>% mutate(Percent = dev / prev_experience) %>% 
  mutate(time = relevel(factor(time), ref='2')) %>% 
  filter(!is.na(Percent))

nine11_percent %>% group_by(time) %>% 
  summarise(mean=mean(Percent), sd=sd(Percent))

Time_avg_911_MCMC_weighted <- stan_glm(Percent ~ time, 
                                       family = 'gaussian', iter= niters, 
                                       weights = num_weights,
                                       data = nine11_percent)
describe_posterior(Time_avg_911_MCMC_weighted)

# Stat Q 3: How to interpret the interaction term when both var are categorical w/ multiple levels?  ---------------------------
# Research q: how does dev change as a function of emotion category and time?

nine11_wide <- read.csv('../data/df.emo.wide_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is specific to one emotion

# for the demo sake, we used a subset of emotions and time points
nine11_long <- nine11_wide %>% filter(item == 'frustration' | item == 'anger' | item == 'sadness') %>% 
  dplyr::select(Subject, item, memdev2, memdev3) %>% 
  pivot_longer(cols = memdev2:memdev3, names_to = 'MemTime', values_to = 'dev') %>% 
  filter(!is.na(dev)) %>% ungroup() %>% 
  mutate(item = relevel(factor(item), ref='frustration'))
  
StressOTEmo911_MCMC <- stan_glmer(dev ~ MemTime * item + (1|Subject), 
                                  family = "gaussian", iter= niters, data = nine11_long) 
describe_posterior(StressOTEmo911_MCMC)
# ^ the output has five coefficients (excluding intercept): time3, anger, sadness, time3:anger, time3:sadness
# what is the reference (i.e., 'baseline') in this model? a specific q: if we are interested in the change of anger emotion across time, should we look at time3:anger? 







