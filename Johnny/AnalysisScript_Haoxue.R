#### Loading Files ####
##Load all files from RDA
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

load(file = "Covid_StressMemory.rda")


##Individually Load Files
MostDiff_Time <- read.csv("Covid_MostStressPeriod.csv", header = TRUE) #Import data on most difficult time periods
Stress_OthTime <- read.csv("Stress_SelfandOthers.csv", header = TRUE) #Import data on Stress to others over time
Stress_SelfTime <- read.csv("Stress_Self_Time.csv", header = TRUE) #Import data on Stress to Self over time
Threat_SocDist <- read.csv("Threat_SocialDistance.csv", header = TRUE) #Import data on threat to self and others
Stress_Change <- read.csv("StressChanges.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2
Stress_Change_Long <- read.csv("StressChanges_Long.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
Stress_Change_LongBel <- read.csv("StressChanges_LongBel.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
DevxConf_Bel <- read.csv("Deviations_Belief.csv", header = TRUE) #Import data on Deviations of Remembered Belief-related stress
DevxConf_Exp <- read.csv("Deviations_Experience.csv", header = TRUE) #Import data on Deviations of Remembered Experience-related stress
DevxConf_Type <- read.csv("Deviations_BelExp.csv", header = TRUE) #Import data on Deviations of Remembered Belief/Experience-related stress
Rem_Stress <- read.csv("ConstructionOfRemStress.csv", header = TRUE) #Import data on 


##Load Contingencies and Packages
if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2, tidyr, dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR) #Load necessary rPackages
require("devtools")
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#inla.setOption(mkl=TRUE)
if (!require("INLAutils")) {install_github("timcdlucas/INLAutils"); require("INLAutils")}  ## additional plotting utilities 
if (!require("brinla")) {install_github("julianfaraway/brinla"); require("brinla")}   ## additional inla functions
if (!require("coefINLA")) {install_github("hesscl/coefINLA"); require("coefINLA")}    ## nice plots for inla models
#if (!require("rstanarm")) {install.packages("rstanarm"); require("rstanarm")} 
if (!require("INLAOutputs")) {install_github('oswaldosantos/INLAOutputs'); require("INLAOutputs")}
options(mc.cores = parallel::detectCores())
library(INLA)
library(coefINLA)
#Load Beta_Squeeze function:
#Squeezes numbers between 0-1 (~ .0002) in order to keep the values between 0 & 1, and not at 0 or 1 for Beta family GLM Regression.
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}


##
# use the weighted avg for all sub








#### Haoxue: Memory Deviation vs. delta well-being ####
beta_normalize <- function(var, squeeze = TRUE){
  min_var <- min(var, na.rm = TRUE)
  max_var <- max(var, na.rm = TRUE)
  new <- (var - min_var) / (max_var - min_var)
  if (squeeze){
    new <- new %>% beta_squeeze
  }
  return(new)
}

df.all <- read.csv('../df_soc_for_Johnny.csv') %>% rename(Subject = prev_SubjectID_Prolific_MTurk) %>% mutate(Subject = factor(Subject))


df.all <- read.csv('../Haoxue/data/COVIDdata_merged_Haoxue_version.csv') %>%  mutate(Subject = factor(Subject))

df.all

# I only care about the experience part for now
Dev_BelExp_Ovrl.exp <- Dev_BelExp_Ovrl %>% filter(Mem == "Exp")

df.all.Johnny <- Dev_BelExp_Ovrl.exp %>% left_join(df.all, by = ('Subject'='Subject'))
lm(BDev ~ num_weights, df.all.Johnny) %>% summary # may need to worry about this - report more items at time1, experience less stress at time1.
lm(wm_T1_remembered ~ n_T1_remembered, df.pca.weighted.sameItem) %>% summary
lm(wm_T2_experienced ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary
lm(wm_deviation ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary










# remake df.pca.long again
df.all.long <- df.all %>% pivot_longer(cols = NA_prev_COVID_20_stress_lifechanges_1:NA_Q13_5,
                        names_to = 'item', values_to = 'value_item') %>% 
  mutate(time = case_when(str_detect(item, 'prev') ~ 'T1_experienced',
                          str_detect(item, 'Q12') | str_detect(item, 'Q13') ~ 'T2_experienced',
                          str_detect(item, 'Q15') | str_detect(item, 'Q16') ~ 'T1_remembered'),
         category = case_when(str_detect(item, 'lifechanges') | str_detect(item, 'Q12') | str_detect(item, 'Q15') ~ 'experience',
                              TRUE ~ 'belief'),
         item = str_sub(item, start=-2) %>% str_replace('_','') %>% as.double )
df.experience.expend <- df.all.long %>% filter(!is.na(Dat)) %>% filter(category=='experience') %>% 
  pivot_wider(names_from = time, values_from = value_item)

df.pca.weighted <- df.experience.expend %>% group_by(Subject) %>% 
  summarise(wm_T1_experienced = mean(T1_experienced, na.rm = TRUE),
            wm_T1_remembered = mean(T1_remembered, na.rm = TRUE),
            wm_T2_experienced = mean(T2_experienced, na.rm = TRUE),
            n_T1_experienced = sum(!is.na(T1_experienced)),
            n_T1_remembered = sum(!is.na(T1_remembered)),
            n_T2_experienced = sum(!is.na(T2_experienced))) %>% 
  mutate(wm_deviation = wm_T1_remembered - wm_T1_experienced)

df.pca.weighted.sameItem <- df.experience.expend %>% filter(!is.na(T1_experienced) & !is.na(T1_remembered)) %>% 
  group_by(Subject) %>% 
  summarise(wm_T1_experienced = mean(T1_experienced, na.rm = TRUE),
            wm_T1_remembered = mean(T1_remembered, na.rm = TRUE),
            wm_T2_experienced = mean(T2_experienced, na.rm = TRUE),
            n_T1_experienced = sum(!is.na(T1_experienced)),
            n_T1_remembered = sum(!is.na(T1_remembered)),
            n_T2_experienced = sum(!is.na(T2_experienced))) %>% 
  mutate(wm_deviation = wm_T1_remembered - wm_T1_experienced)

df.pca.weighted %>% filter(!df.pca.weighted$Subject %in% df.pca.weighted.sameItem$Subject) # a couple of subjects said that these stressors do not apply to them at all

weighted.merge <- df.pca.weighted %>% merge(df.pca.weighted.sameItem, by = 'Subject') 
weighted.merge %>% select(-Subject, -starts_with('n_')) %>% as.matrix() %>% rcorr # highly correlate with each other

lm(wm_T1_experienced ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary # may need to worry about this - report more items at time1, experience less stress at time1.
lm(wm_T1_remembered ~ n_T1_remembered, df.pca.weighted.sameItem) %>% summary
lm(wm_T2_experienced ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary
lm(wm_deviation ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary

lm(wm_T1_experienced ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary # may need to worry about this - report more items at time1, experience less stress at time1.
lm(wm_T1_remembered ~ n_T1_remembered, df.pca.weighted.sameItem) %>% summary
lm(wm_T2_experienced ~ n_T2_experienced, df.pca.weighted.sameItem) %>% summary
lm(wm_deviation ~ n_T1_experienced, df.pca.weighted.sameItem) %>% summary

# neither of them is perfect.
# anyway lest get to inla.


# for each option there is no difference
t.test(weighted.merge$wm_T1_experienced.x, weighted.merge$wm_T1_experienced.y)
t.test(weighted.merge$wm_T1_remembered.x, weighted.merge$wm_T1_remembered.y)
t.test(weighted.merge$wm_T2_experienced.x, weighted.merge$wm_T2_experienced.y)
t.test(weighted.merge$wm_deviation.x, weighted.merge$wm_deviation.y)

# single observation is definitely better fit as beta. It is not that much clear for wm_deviation. But i guess using beta will not hurt anything?
df.pca.weighted$wm_T1_experienced %>% hist
df.pca.weighted$wm_T1_remembered %>% hist
df.pca.weighted$wm_T2_experienced %>% hist
df.pca.weighted$wm_deviation %>% hist

# beta_normalize
df.all.weighted.sameItem <- df.all %>% merge(df.pca.weighted, by='Subject') %>% filter(!is.na(Dat)) %>% 
  mutate(across(wm_T1_experienced:wm_deviation, beta_normalize, .names='{.col}_squeeze')) %>% filter(!is.na(wm_deviation_squeeze)) %>% 
  mutate(Subject = as.factor(as.character(Subject)))
df.all.weighted.sameItem <- df.all %>% merge(df.pca.weighted.sameItem, by='Subject') %>% filter(!is.na(Dat)) %>% 
  mutate(across(wm_T1_experienced:wm_deviation, beta_normalize, .names='{.col}_squeeze')) %>% filter(!is.na(wm_deviation_squeeze)) %>% 
  mutate(Subject = as.factor(as.character(Subject)))

# FINALLY!!
# tried to include them both, but did not work.
DeltaPCA.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + delta_pca_score + delta_soc,
                 family = "beta", data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced)
summary(DeltaPCA.weighted.sameItem)
coefINLA(DeltaPCA.weighted.sameItem)

DeltaPCA_curr.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T2_experienced + delta_pca_score + delta_soc,
                          family = "beta", data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced)
summary(DeltaPCA_curr.weighted.sameItem)
coefINLA(DeltaPCA_curr.weighted.sameItem)

# replicate the results using sameItem.
DeltaPCA.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + delta_pca_score + delta_soc,
                          family = "beta", data = df.all.weighted.sameItem, verbose = TRUE, weights = n_T1_experienced)
summary(DeltaPCA.weighted.sameItem)
coefINLA(DeltaPCA.weighted.sameItem)

DeltaPCA_curr.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + wm_T2_experienced + delta_pca_score + delta_soc,
                               family = "beta", data = df.all.weighted.sameItem, verbose = TRUE,weights = n_T1_experienced)
summary(DeltaPCA_curr.weighted.sameItem)
coefINLA(DeltaPCA_curr.weighted.sameItem)


#### attention
attention.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + 
                         media_last_month + media_peak_month + 
                         avoid_last_month + avoid_peak_month, 
                       family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(attention.weighted.sameItem) # avoid_peak_month, media_last_month, media_peak_month
coefINLA(attention.weighted.sameItem)

attention.weighted.curr <- inla(wm_deviation_squeeze ~ wm_T2_experienced + wm_T1_experienced +  
                             media_last_month + media_peak_month + 
                             avoid_last_month + avoid_peak_month, 
                           family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(attention.weighted.curr) # avoid_peak_month, media_last_month, media_peak_month
coefINLA(attention.weighted.curr)

# indeed avoid and media negatively correlate with each other, not quite sure how to use them to explain the same direction effect
with(df.all.weighted.sameItem, cor.test(avoid_last_month, media_last_month))
with(df.all.weighted.sameItem, cor.test(avoid_peak_month, media_peak_month))

#### relevance
relevance.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + 
                         add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                         personal_loss + financial_loss_cont + relocate + job_change + 
                         communicate_peak_month + communicate_last_month,
                       family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(relevance.weighted.sameItem)
coefINLA(relevance.weighted.sameItem)

p <- inla(wm_deviation_squeeze ~ wm_T2_experienced + wm_T1_experienced+
                             add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                             personal_loss + financial_loss_cont + relocate + job_change + 
                             communicate_peak_month + communicate_last_month,
                           family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(relevance.curr.weighted.sameItem)
coefINLA(relevance.curr.weighted.sameItem)


df.all.weighted.sameItem <- df.all.weighted %>% mutate(threat_delta_close = Q10_2 - Q10_1, 
                                              threat_delta_us = Q10_3 - Q10_1,
                                              threat_delta_world = Q10_4 - Q10_1,
                                              curr_pca_score_squeeze = curr_pca_score %>% beta_normalize(),
                                              delta_pca_score_squeeze = delta_pca_score %>% beta_normalize(),
                                              delta_soc_squeeze = delta_soc %>% beta_normalize(),
                                              stress_delta_curr = Q6 - Q3,
                                              stress_delta_mostDiff = Q7 - Q4,
                                              stress_delta_future = Q8 - Q5)
#### layperson theory
####### threat - DV: memory deviation
threat_long.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world,
                    family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_long.weighted.sameItem)
coefINLA(threat_long.weighted.sameItem)

threat_long_curr.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T2_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world,
                         family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_long_curr.weighted.sameItem)
coefINLA(threat_long_curr.weighted.sameItem)

stress_long.weighted.sameItem <- inla(wm_deviation_squeeze ~ wm_T1_experienced + Q3 + Q4 + Q6 + Q7,
                                  family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(stress_long.weighted.sameItem)
coefINLA(stress_long.weighted.sameItem)

####### threat - DV: pca
threat_curr_pca.weighted.sameItem <- inla(curr_pca_score_squeeze ~ wm_T2_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world,
                                 family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_curr_pca.weighted.sameItem)
coefINLA(threat_curr_pca.weighted.sameItem)

threat_delta_pca.weighted.sameItem <- inla(delta_pca_score_squeeze ~  prev_pca_score + wm_deviation + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world,
                                  family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_pca.weighted.sameItem)
coefINLA(threat_delta_pca.weighted.sameItem)

threat_delta_pca2.weighted.sameItem <- inla(delta_pca_score_squeeze ~prev_pca_score + wm_deviation + Q10_1 + Q10_2 + Q10_3 + Q10_4,
                                  family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_pca2.weighted.sameItem)
coefINLA(threat_delta_pca2.weighted.sameItem)

# Q10_1: the less you think covid is a threat to you, the better you are doing now in terms of emotion well-being (more improvement)
# Q10_4: the larger the differnece between covid being a threat to you vs. to people across the world, the better you are doing now in terms of emotion well-being (more improvmenet)

####### threat - DV: soc
threat_delta_soc.weighted.sameItem <- inla(delta_soc_squeeze ~  prev_soc_mean + wm_T1_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                           f(Subject, model = "iid") , family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_soc.weighted.sameItem)
coefINLA(threat_delta_soc.weighted.sameItem)

####### temporal stress rating - DV: pca
# for curr_pca, we only look at Q3 and stress_delta_curr
layperson_curr_pca.weighted.sameItem <- inla(curr_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                             Q3 + stress_delta_curr , family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_curr_pca.weighted.sameItem)
coefINLA(layperson_curr_pca.weighted.sameItem)

layperson_delta_pca_delta.weighted.sameItem <- inla(delta_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                                    Q3 + stress_delta_curr + Q4 + stress_delta_mostDiff,
                                  family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_delta.weighted.sameItem)
coefINLA(layperson_delta_pca_delta.weighted.sameItem)

layperson_delta_pca_origin.weighted.sameItem.curr <- inla(curr_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                                     Q3 + Q6 + Q4 + Q7 , family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_origin.weighted.sameItem.curr)
coefINLA(layperson_delta_pca_origin.weighted.sameItem.curr) # suspecting some collinearity is reducing the effect of self questions (see the change of Q4)

layperson_delta_pca_delta.weighted.sameItem.curr <- inla(curr_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                                                      Q3 + stress_delta_curr + Q4 + stress_delta_mostDiff,
                                                    family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_delta.weighted.sameItem.curr)
coefINLA(layperson_delta_pca_delta.weighted.sameItem.curr)

layperson_delta_pca_origin.weighted.sameItem <- inla(delta_pca_score ~ prev_pca_score + wm_deviation + 
                                                       Q3 + Q6 + Q4 + Q7 , family = 'gaussian', data = df.all.weighted.sameItem, verbose = TRUE)
summary(layperson_delta_pca_origin.weighted.sameItem)
coefINLA(layperson_delta_pca_origin.weighted.sameItem) # suspecting some collinearity is reducing the effect of self questions (see the change of Q4)

tr1 <- inla(delta_soc ~  prev_soc_mean + wm_deviation + 
             Q10_1 + Q10_2 + Q10_3 + Q10_4 , family = 'gaussian', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(tr1)
coefINLA(tr1)

tr <- inla(delta_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                                                       Q3 + Q6 + Q4 + Q7 +Q10_1 + Q10_2 + Q10_3 + Q10_4 , family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(tr)
coefINLA(tr) # suspecting some collinearity is reducing the effect of self questions (see the change of Q4)


layperson_delta_pca_delta_future.weighted.sameItem <- inla(delta_pca_score_squeeze ~  prev_pca_score + wm_deviation + 
                                           Q3 + stress_delta_curr + Q4 + stress_delta_mostDiff + Q5 + stress_delta_future,
                                         family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_delta_future.weighted.sameItem)
coefINLA(layperson_delta_pca_delta_future.weighted.sameItem)

layperson_delta_soc.weighted.sameItem <- inla(delta_soc_squeeze ~  prev_soc_mean + wm_deviation + 
                              Q3 + stress_delta_curr + Q4 + stress_delta_mostDiff,
                              family = 'beta', data = df.all.weighted.sameItem, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_soc.weighted.sameItem)
coefINLA(layperson_delta_soc.weighted.sameItem)


#########################################################################################

# no speicifc relationship is find between soc well being and our other measurement here

####### support the avg argument: curr experienced stress reflects both the stress experiencing themselves & other ppl
t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                              Q3 + stress_delta_curr  +  
                              f(int_id, model = "iid2d", n = 2*nid)  + 
                              f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(t2_experienced_long)
coefINLA(t2_experienced_long)

t2_experienced <- inla(NA_mean_curr_experience_squeeze ~  NA_mean_prev_experience + 
                         Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                         f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(t2_experienced)
coefINLA(t2_experienced) # less sensitive than t2_experienced_long (havent tested this for 89)

threa_t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                                    Q10_1 + Q10_2 + Q10_3 + Q10_4+
                                    f(int_id, model = "iid2d", n = 2*nid)  + 
                                    f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(threa_t2_experienced_long)
coefINLA(threa_t2_experienced_long)



###########################################














df.pca <- read.csv('../Haoxue/data/df.pca.csv') %>% mutate(Subject = factor(Subject))
df.pca.long <- read.csv('../Haoxue/data/df.pca.long.csv') %>% mutate(Subject = factor(Subject))
df.pca.long.all <- read.csv('../Haoxue/data/df.pca.long.all.csv') %>% mutate(Subject = factor(Subject))

df.pca.weighted.sameItem <- df.pca.long %>% group_by(Subject) %>% 
  summarise(wm_T1_experienced_squeeze = mean(T1_experienced_squeeze, na.rm = TRUE),
            wm_T1_remembered_squeeze = mean(T1_remembered_squeeze, na.rm = TRUE),
            wm_T2_experienced_squeeze = mean(T2_experienced_squeeze, na.rm = TRUE),
            n_T1_experienced = sum(!is.na(T1_experienced_squeeze)),
            n_T1_remembered = sum(!is.na(T1_remembered_squeeze)),
            n_T2_experienced = sum(!is.na(T2_experienced_squeeze))) %>% 
  mutate(wm_deviation = wm_T1_remembered_squeeze - wm_T1_experienced_squeeze)

# another way is only calculate deviation using the same item in T1_experienced and T1_remembered. will handle T2_experienced later since it is not being taken into the calcualtion yet.

df.pca.weighted.sameItem <- df.pca.long %>% filter(!is.na(T1_experienced_squeeze) & !is.na(T1_remembered_squeeze)) %>% 
  group_by(Subject) %>% 
  summarise(wm_T1_experienced_squeeze = mean(T1_experienced_squeeze, na.rm = TRUE),
            wm_T1_remembered_squeeze = mean(T1_remembered_squeeze, na.rm = TRUE),
            wm_T2_experienced_squeeze = mean(T2_experienced_squeeze, na.rm = TRUE),
            n_T1_experienced = sum(!is.na(T1_experienced_squeeze)),
            n_T1_remembered = sum(!is.na(T1_remembered_squeeze)),
            n_T2_experienced = sum(!is.na(T2_experienced_squeeze))) %>% 
  mutate(wm_deviation = wm_T1_remembered_squeeze - wm_T1_experienced_squeeze)

# compare the correlation between these two ways of doing the measurement
cor.test(df.pca.weighted$wm_T1_experienced, df.pca.weighted.sameItem$wm_T1_experienced)
cor.test(df.pca.weighted$wm_T2_experienced, df.pca.weighted.sameItem$wm_T2_experienced)
cor.test(df.pca.weighted$wm_T1_remembered, df.pca.weighted.sameItem$wm_T1_remembered)
cor.test(df.pca.weighted$wm_deviation, df.pca.weighted.sameItem$wm_deviation)

# no clear relationship 

df.pca.long %>% filter(!is.na(T1_experienced_squeeze) & !is.na(T1_remembered_squeeze)) %>% dim



DeltaPCA <- inla(NA_mean_expectation_deviation_experience_squeeze ~ NA_mean_prev_experience + delta_pca_score + delta_soc + f(Subject, model = "iid"),
                 family = "beta", data = df.pca, verbose = TRUE, weight = n_T1_experienced)
summary(DeltaPCA)
coefINLA(DeltaPCA)

DeltaPCA_curr <- inla(NA_mean_expectation_deviation_experience_squeeze ~ NA_mean_curr_experience + NA_mean_prev_experience + delta_pca_score + delta_soc + f(Subject, model = "iid"),
                 family = "beta", data = df.pca, verbose = TRUE, weight = n_T1_experienced)
summary(DeltaPCA_curr)
coefINLA(DeltaPCA_curr)

DeltaPCA_curr_89 <- inla(NA_mean_expectation_deviation_experience_squeeze ~ NA_mean_curr_experience + delta_pca_score + delta_soc + f(Subject, model = "iid"),
                      family = "beta", data = df.pca, verbose = TRUE, quantiles = c(0.055,0.945))
summary(DeltaPCA_curr_89)
coefINLA(DeltaPCA_curr_89) # the graph will look v weird when you use 89

nid <- length(table(df.pca.long$Subject))       ## number of persons in the sample
nid
int_id <- as.numeric(df.pca.long$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

#Analysis
#### delta pca ~ memory deviation
DeltaPCA_long <- inla(deviation_squeeze ~ T1_experienced + delta_pca_score + delta_soc + 
                        f(int_id, model = "iid2d", n = 2*nid)  + 
                        f(slope_id, item_num, copy = "int_id"), 
                      family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 

summary(DeltaPCA_long) #Summarize results
coefINLA(DeltaPCA_long) #Fixed effects

DeltaPCA_long_curr <- inla(deviation_squeeze ~ T2_experienced + delta_pca_score + delta_soc + 
                             f(int_id, model = "iid2d", n = 2*nid)  + 
                             f(slope_id, item_num, copy = "int_id"), 
                           family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 

summary(DeltaPCA_long_curr) #Summarize results
coefINLA(DeltaPCA_long_curr) #Fixed effects

#### attention
attention_long <- inla(deviation_squeeze ~ T1_experienced + 
                         media_last_month + media_peak_month + 
                         avoid_last_month + avoid_peak_month +
                         f(int_id, model = "iid2d", n = 2*nid)  + 
                         f(slope_id, item_num, copy = "int_id"), 
                       family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(attention_long) # avoid_peak_month, media_last_month, media_peak_month
coefINLA(attention_long)

attention_long_curr <- inla(deviation_squeeze ~ T2_experienced + 
                         media_last_month + media_peak_month + 
                         avoid_last_month + avoid_peak_month +
                           f(int_id, model = "iid2d", n = 2*nid)  + 
                           f(slope_id, item_num, copy = "int_id"), 
                         family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(attention_long_curr)
coefINLA(attention_long_curr) # nothing stands out in this case

# indeed avoid and media negatively correlate with each other
with(df.all, cor.test(avoid_last_month, media_last_month))
with(df.all, cor.test(avoid_peak_month, media_peak_month))

#### relevance
# again, relevance results are different from each other depending on whether we put T1_experience or T2_experience
relevance_long <- inla(deviation_squeeze ~ T1_experienced + 
                         add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                         personal_loss + financial_loss_cont + relocate + job_change + 
                         communicate_peak_month + communicate_last_month +
                         f(int_id, model = "iid2d", n = 2*nid)  + 
                         f(slope_id, item_num, copy = "int_id"), 
                       family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(relevance_long)
coefINLA(relevance_long)

relevance_long_curr <- inla(deviation_squeeze ~ T2_experienced + 
                         add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                         personal_loss + financial_loss_cont + relocate + job_change + 
                         communicate_peak_month + communicate_last_month +
                           f(int_id, model = "iid2d", n = 2*nid)  + 
                           f(slope_id, item_num, copy = "int_id"), 
                         family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(relevance_long_curr)
coefINLA(relevance_long_curr)   

#### layperson theory
####### threat - DV: memory deviation
threat_long <- inla(deviation_squeeze ~ T1_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(int_id, model = "iid2d", n = 2*nid)  + 
                      f(slope_id, item_num, copy = "int_id"), 
                    family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_long)
coefINLA(threat_long)

threat_long_curr <- inla(deviation_squeeze ~ T2_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(int_id, model = "iid2d", n = 2*nid)  + 
                      f(slope_id, item_num, copy = "int_id"), 
                    family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_long_curr)
coefINLA(threat_long_curr)

####### threat - DV: pca
threat_curr_pca <- inla(curr_pca_score_squeeze ~ NA_mean_curr_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid"), family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_curr_pca)
coefINLA(threat_curr_pca)

threat_delta_pca <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_pca)
coefINLA(threat_delta_pca)

threat_delta_pca2 <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                            Q10_1 + Q10_2 + Q10_3 + Q10_4 + 
                           f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_pca2)
coefINLA(threat_delta_pca2)
# Q10_1: the less you think covid is a threat to you, the better you are doing now in terms of emotion well-being (more improvement)
# Q10_4: the larger the differnece between covid being a threat to you vs. to people across the world, the better you are doing now in terms of emotion well-being (more improvmenet)

####### threat - DV: soc
threat_delta_soc <- inla(delta_soc_squeeze ~  prev_soc_mean + NA_mean_expectation_deviation_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(threat_delta_soc)
coefINLA(threat_delta_soc)

####### temporal stress rating - DV: pca
# for curr_pca, we only look at Q3 and stress_delta_curr
layperson_curr_pca <- inla(curr_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                             Q3 + stress_delta_curr+ 
                                             f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_curr_pca)
coefINLA(layperson_curr_pca)

layperson_delta_pca_delta <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                              Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                              f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_delta)
coefINLA(layperson_delta_pca_delta)

layperson_delta_pca_origin <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                              Q3 + Q6 + Q4 + Q7 +  
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca)
coefINLA(layperson_delta_pca) # suspecting some collinearity is reducing the effect of self questions (see the change of Q4)

layperson_delta_pca_delta_future <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                    Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff + Q5 + stress_delta_future +
                                    f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_delta_future)
coefINLA(layperson_delta_pca_delta_future)

layperson_delta_pca_origin_future <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                     Q3 + Q6 + Q4 + Q7 +  Q5 + Q8 +
                                     f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_pca_origin_future)
coefINLA(layperson_delta_pca_origin_future)
# ^ok I think it does not make too much sense to include future then..

layperson_delta_soc <- inla(delta_soc_squeeze ~  prev_soc_mean + NA_mean_expectation_deviation_experience + 
                         Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                         f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(layperson_delta_soc)
coefINLA(layperson_delta_soc)
# no speicifc relationship is find between soc well being and our other measurement here

####### support the avg argument: curr experienced stress reflects both the stress experiencing themselves & other ppl
t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                              Q3 + stress_delta_curr  +  
                              f(int_id, model = "iid2d", n = 2*nid)  + 
                              f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(t2_experienced_long)
coefINLA(t2_experienced_long)

t2_experienced <- inla(NA_mean_curr_experience_squeeze ~  NA_mean_prev_experience + 
                Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE, weight = n_T1_experienced) 
summary(t2_experienced)
coefINLA(t2_experienced) # less sensitive than t2_experienced_long (havent tested this for 89)

threa_t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                              Q10_1 + Q10_2 + Q10_3 + Q10_4+
                              f(int_id, model = "iid2d", n = 2*nid)  + 
                              f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE, weight = n_T1_experienced) 
summary(threa_t2_experienced_long)
coefINLA(threa_t2_experienced_long)

with(df.pca, cor.test(Q10_1, Q))
a <- df.pca %>% select(starts_with('Q10_'), Q3:Q8, starts_with('threat_delta'), starts_with('stress_delta')) %>% as.matrix %>% rcorr()
a$r %>% round(2)
a$P %>% round(2) # Q6-Q8 all correlates most highly with Q10_2 (but how to test this?) caveat being stress_delta_curr correlates highest with threat_delta_us and threat_delta_world, seems to be in contradiction with previous results.
# the underlying q here is: 'typical person' in your community, are you thinking closed others/ppl in us/ppl across the world?
# or maybe these two questions are incomparable, esp we ask for threat perception and stress rating separately.
