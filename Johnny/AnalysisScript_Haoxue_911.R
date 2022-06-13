### AnalysisScript_Haoxue_911.R
#   This file is used to put 9-11 results using the new way to analyze
#   specifically, we would like to do a weighted average of the negative emotion estimate and see whether we still get our previous results

if(!require("pacman")) install.packages("pacman")
p_load(rstudioapi, magrittr, tidyverse, ggplot2, tidyr, dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR, lmerTest, devtools, foreign) #Load necessary 

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

require("devtools")
if (!require("INLA")){install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)}
library(INLA)
inla.setOption(mkl=TRUE)
if (!require("INLAutils")) {install_github("timcdlucas/INLAutils"); require("INLAutils")}  
if (!require("brinla")) {install_github("julianfaraway/brinla"); require("brinla")}   ## additional inla functions
library(brinla)
if (!require("coefINLA")) {install_github("hesscl/coefINLA"); require("coefINLA")}    ## nice plots for inla models
library(coefINLA)
if (!require("rstanarm")) {install.packages("rstanarm"); require("rstanarm")} 
if (!require("INLAOutputs")) {install_github('oswaldosantos/INLAOutputs'); require("INLAOutputs")}
library(INLAOutputs)
options(mc.cores = parallel::detectCores())
#Load Beta_Squeeze function:
#Squeezes numbers between 0-1 (~ .0002) in order to keep the values between 0 & 1, and not at 0 or 1 for Beta family GLM Regression.
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

# Load data ---------------------------------------------------------------
df.911 <- read.spss('../Haoxue/data-raw/911data.sav') %>% as.data.frame()
df.911_test <- haven::read_sav('../Haoxue/data-raw/911data.sav')
# Grab related columns ----------------------------------------------------
df.emo <- df.911 %>% dplyr::select(s64q13f, s65q14f, s66q15f, s67q16f, s68q17f, s69q18f,
                  s235q42s, s236q43s, s237q44s, s238q45s, s239q46s, s240q47s,
                  s237q42t, s238q43t, s239q44t, s240q45t, s241q46t, s242q47t,
                  sa252, sa253, sa254, sa255, sa256, sa257,
                  s72q13s, s73q14s, s74q15s, s75q16s, s76q17s, s77q18s,
                  s74q13t, s75q14t, s76q15t, s77q16t, s78q17t, s79q18t,
                  #s89q19f, s90q19f, s91q19f, s92q19f, s93q19f, s94q19f,
                  sa89, sa90, sa91, sa92, sa93, sa94, sa94,
                  r2mrc6) %>% 
  set_colnames(c('sadness_s1_experienced', 'anger_s1_experienced', 'fear_s1_experienced', 'confusion_s1_experienced', 'frustration_s1_experienced', 'shock_s1_experienced',
                 'sadness_s2_experienced', 'anger_s2_experienced', 'fear_s2_experienced', 'confusion_s2_experienced', 'frustration_s2_experienced', 'shock_s2_experienced',
                 'sadness_s3_experienced', 'anger_s3_experienced', 'fear_s3_experienced', 'confusion_s3_experienced', 'frustration_s3_experienced', 'shock_s3_experienced',
                 'sadness_s4_experienced', 'anger_s4_experienced', 'fear_s4_experienced', 'confusion_s4_experienced', 'frustration_s4_experienced', 'shock_s4_experienced',
                 'sadness_s2_remembered', 'anger_s2_remembered', 'fear_s2_remembered','confusion_s2_remembered','frustration_s2_remembered','shock_s2_remembered',
                 'sadness_s3_remembered', 'anger_s3_remembered', 'fear_s3_remembered','confusion_s3_remembered','frustration_s3_remembered','shock_s3_remembered',
                 'sadness_s4_remembered', 'anger_s4_remembered', 'fear_s4_remembered','confusion_s4_remembered','frustration_s4_remembered','shock_s4_remembered',
                 'Subject'))

emo_miss <- function(x){
  y <- ifelse(x==6|x==0, NA, x)
  return(y)
}
df.emo <- df.emo %>% mutate(across(where(is.double),
                                   .fns = emo_miss))


# wide2long ---------------------------------------------------------------
df.emo.long <- df.emo %>% 
  mutate(Subject = str_trim(Subject)) %>% 
  filter(Subject != "" & Subject != "N/A" & !is.na(Subject)) %>% 
  pivot_longer(cols = -Subject,
                                  names_to = 'type',
                                  values_to = 'rating') %>% 
  mutate(item = sub("\\_s.*", "", type),
         time = str_extract(type, '[0-9]') %>% as.double,
         prompt = sub(".*_s[0-9]_", "", type),
         prompt_time = paste0(prompt, time)) 

df.emo.wide <- df.emo.long %>% select(-prompt) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = 'prompt_time', values_from = 'rating') %>% 
  select(-type, -row, -time) %>% 
  group_by(Subject, item) %>% 
  summarise(experienced1 = mean(experienced1, na.rm = TRUE),
            experienced2 = mean(experienced2, na.rm = TRUE),
            experienced3 = mean(experienced3, na.rm = TRUE),
            experienced4 = mean(experienced4, na.rm = TRUE),
            remembered2 = mean(remembered2, na.rm = TRUE),
            remembered3 = mean(remembered3, na.rm = TRUE),
            remembered4 = mean(remembered4, na.rm = TRUE),
            ) %>% 
  mutate(memdev2 = remembered2 - experienced1,
         memdev3 = remembered3 - experienced1,
         memdev4 = remembered4 - experienced1,
         currdev2 = experienced2 - experienced1,
         currdev3 = experienced3 - experienced1,
         currdev4 = experienced4 - experienced1,
         ) %>% ungroup()

# calculate weighted avg for each time point separately
df.emo.s12.wide.avg <- df.emo.wide %>% 
  filter(!is.na(memdev2)) %>% 
  group_by(Subject) %>% 
  summarise(mean_experienced1 = mean(experienced1, na.rm = TRUE),
            mean_experienced2 = mean(experienced2, na.rm = TRUE),
            mean_experienced3 = mean(experienced3, na.rm = TRUE),
            mean_experienced4 = mean(experienced4, na.rm = TRUE),
            mean_remembered2 = mean(remembered2, na.rm = TRUE),
            mean_remembered3 = mean(remembered3, na.rm = TRUE),
            mean_remembered4 = mean(remembered4, na.rm = TRUE),
            mean_memdev2 = mean(memdev2, na.rm = TRUE),
            mean_memdev3 = mean(memdev3, na.rm = TRUE),
            mean_memdev4 = mean(memdev4, na.rm = TRUE),
            mean_currdev2 = mean(currdev2, na.rm = TRUE),
            mean_currdev3 = mean(currdev3, na.rm = TRUE),
            mean_currdev4 = mean(currdev4, na.rm = TRUE),
            n_experienced1 = sum(!is.na(experienced1)),
            n_experienced2 = sum(!is.na(experienced2)),
            n_experienced3 = sum(!is.na(experienced3)),
            n_experienced4 = sum(!is.na(experienced4)),
            n_remembered2 = sum(!is.na(remembered2)),
            n_remembered3 = sum(!is.na(remembered3)),
            n_remembered4 = sum(!is.na(remembered4))) %>% 
  select(Subject, mean_experienced1, mean_experienced2, mean_remembered2, n_remembered2, mean_memdev2) %>% 
  set_colnames(c('Subject','prev_experience','curr_experience','stress_memory','num_weights','dev')) %>% 
  mutate(time = 2)

df.emo.s13.wide.avg <- df.emo.wide %>% 
  filter(!is.na(memdev3)) %>% 
  group_by(Subject) %>% 
  summarise(mean_experienced1 = mean(experienced1, na.rm = TRUE),
            mean_experienced2 = mean(experienced2, na.rm = TRUE),
            mean_experienced3 = mean(experienced3, na.rm = TRUE),
            mean_experienced4 = mean(experienced4, na.rm = TRUE),
            mean_remembered2 = mean(remembered2, na.rm = TRUE),
            mean_remembered3 = mean(remembered3, na.rm = TRUE),
            mean_remembered4 = mean(remembered4, na.rm = TRUE),
            mean_memdev2 = mean(memdev2, na.rm = TRUE),
            mean_memdev3 = mean(memdev3, na.rm = TRUE),
            mean_memdev4 = mean(memdev4, na.rm = TRUE),
            mean_currdev2 = mean(currdev2, na.rm = TRUE),
            mean_currdev3 = mean(currdev3, na.rm = TRUE),
            mean_currdev4 = mean(currdev4, na.rm = TRUE),
            n_experienced1 = sum(!is.na(experienced1)),
            n_experienced2 = sum(!is.na(experienced2)),
            n_experienced3 = sum(!is.na(experienced3)),
            n_experienced4 = sum(!is.na(experienced4)),
            n_remembered2 = sum(!is.na(remembered2)),
            n_remembered3 = sum(!is.na(remembered3)),
            n_remembered4 = sum(!is.na(remembered4))) %>% 
  select(Subject, mean_experienced1, mean_experienced3, mean_remembered3, n_remembered3) %>% 
  set_colnames(c('Subject','prev_experience','curr_experience','stress_memory','num_weights')) %>% 
  mutate(time = 3)

df.emo.s14.wide.avg <- df.emo.wide %>% 
  filter(!is.na(memdev4)) %>% 
  group_by(Subject) %>% 
  summarise(mean_experienced1 = mean(experienced1, na.rm = TRUE),
            mean_experienced2 = mean(experienced2, na.rm = TRUE),
            mean_experienced3 = mean(experienced3, na.rm = TRUE),
            mean_experienced4 = mean(experienced4, na.rm = TRUE),
            mean_remembered2 = mean(remembered2, na.rm = TRUE),
            mean_remembered3 = mean(remembered3, na.rm = TRUE),
            mean_remembered4 = mean(remembered4, na.rm = TRUE),
            mean_memdev2 = mean(memdev2, na.rm = TRUE),
            mean_memdev3 = mean(memdev3, na.rm = TRUE),
            mean_memdev4 = mean(memdev4, na.rm = TRUE),
            mean_currdev2 = mean(currdev2, na.rm = TRUE),
            mean_currdev3 = mean(currdev3, na.rm = TRUE),
            mean_currdev4 = mean(currdev4, na.rm = TRUE),
            n_experienced1 = sum(!is.na(experienced1)),
            n_experienced2 = sum(!is.na(experienced2)),
            n_experienced3 = sum(!is.na(experienced3)),
            n_experienced4 = sum(!is.na(experienced4)),
            n_remembered2 = sum(!is.na(remembered2)),
            n_remembered3 = sum(!is.na(remembered3)),
            n_remembered4 = sum(!is.na(remembered4))) %>% 
  select(Subject, mean_experienced1, mean_experienced4, mean_remembered4, n_remembered4) %>% 
  set_colnames(c('Subject','prev_experience','curr_experience','stress_memory','num_weights')) %>% 
  mutate(time = 4)

df.emo.wide.avg <- df.emo.s12.wide.avg %>% 
  rbind(df.emo.s13.wide.avg %>% 
          rbind(df.emo.s14.wide.avg)) %>% 
  mutate(beta_stress_memory = ((stress_memory-1)/5) %>% beta_squeeze,
         time=factor(time),
         sum_prev_curr = prev_experience + curr_experience)

        
# quick and dirty -------------------------------------------------


lmer(stress_memory ~ (prev_experience + curr_experience)*time + (1|Subject),
   df.emo.wide.avg) %>% summary

df.emo.wide <- df.emo.wide %>% mutate(item = factor(item) %>% relevel(ref='frustration'))

# interaction with emotion?
mod.memdev2 <- lmer(memdev2 ~ item + (1|Subject), df.emo.wide)
mod.memdev2 %>% plot_model(type='eff')



# INLA  -------------------------------------------------------------------
range(df.emo.wide.avg$stress_memory) # not reaching min/max (-4/4)

inla.constructive.int.time2 <- inla(beta_stress_memory ~ (prev_experience + curr_experience) * time +
                                f(Subject, model = 'iid'),
                                family = 'beta', 
                                data = df.emo.wide.avg, verbose = TRUE) 
inla.constructive.int.time3 <- inla(beta_stress_memory ~ (prev_experience + curr_experience) * time +
                                      f(Subject, model = 'iid'),
                                    family = 'beta', 
                                    data = df.emo.wide.avg %>% 
                                      mutate(time=relevel(time, ref='3')), verbose = TRUE) 
inla.constructive.int.time4 <- inla(beta_stress_memory ~ (prev_experience + curr_experience) * time +
                                      f(Subject, model = 'iid'),
                                    family = 'beta', 
                                    data = df.emo.wide.avg %>% 
                                      mutate(time=relevel(time, ref='4')), verbose = TRUE) 
inla.constructive.int.cont <- inla(beta_stress_memory ~ (prev_experience + curr_experience) * time +
                                      f(Subject, model = 'iid'),
                                    family = 'beta', 
                                    data = df.emo.wide.avg %>% 
                                      mutate(time=as.double(time)), verbose = TRUE) 

summary(inla.constructive.int.time2)
coefINLA(inla.constructive.int.time2)
summary(inla.constructive.int.time3)
coefINLA(inla.constructive.int.time3)
summary(inla.constructive.int.time4)
coefINLA(inla.constructive.int.time4)
summary(inla.constructive.int.cont)
coefINLA(inla.constructive.int.cont)

inla.constructive.int.time2.sum <- inla(beta_stress_memory ~ (sum_prev_curr + curr_experience) * time +
                                      f(Subject, model = 'iid'),
                                    family = 'beta', 
                                    data = df.emo.wide.avg, verbose = TRUE) 
summary(inla.constructive.int.time2.sum)
coefINLA(inla.constructive.int.time2.sum)

inla.constructive.int.time3.sum <- inla(beta_stress_memory ~ (sum_prev_curr + curr_experience) * time +
                                          f(Subject, model = 'iid'),
                                        family = 'beta', 
                                        data = df.emo.wide.avg %>% mutate(time=relevel(time,ref='3')), verbose = TRUE) 
summary(inla.constructive.int.time3.sum)
coefINLA(inla.constructive.int.time3.sum)

inla.constructive.int.time4.sum <- inla(beta_stress_memory ~ (sum_prev_curr + curr_experience) * time +
                                          f(Subject, model = 'iid'),
                                        family = 'beta', 
                                        data = df.emo.wide.avg %>% mutate(time=relevel(time,ref='4')), verbose = TRUE) 
summary(inla.constructive.int.time4.sum)
coefINLA(inla.constructive.int.time4.sum)





inla.constructive.int.time.sum <- inla(beta_stress_memory ~ (sum_prev_curr + curr_experience) * time +
                                          f(Subject, model = 'iid'),
                                        family = 'beta', 
                                        data = df.emo.wide.avg %>% mutate(time=as.double(time)), verbose = TRUE) 
summary(inla.constructive.int.time.sum)
coefINLA(inla.constructive.int.time.sum)


df.emo.wide <- df.emo.wide %>% 
  filter(!is.na(memdev2)) %>% 
  mutate(beta_memdev2 = ((memdev2 + 4)/8) %>% beta_squeeze())

inla.911.emo <- inla(beta_memdev2 ~ item + f(Subject, model = 'iid'),
                     family = 'beta', data = df.emo.wide %>% 
                       mutate(item = factor(item) %>% relevel(ref='sadness')), verbose = TRUE)
summary(inla.911.emo)
coefINLA(inla.911.emo)

df.emo.long %>% filter()


# plot 911 emo data -------------------------------------------------------
df.emo.wide %>% ggplot(aes(x=item, y=memdev2)) +
  stat_summary() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab('')+
  ylab('memory deviation')+
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.text.x = element_text(angle=15),
        axis.title = element_text(size=14))

df.emo.wide.avg %>% pivot_longer(cols=mean_experienced1:mean_experienced2, names_to = 'mean_type', values_to = 'mean_rating') %>% 
  mutate(mean_type = factor(mean_type, levels = c('mean_experienced1', 'mean_experienced2','mean_remembered2'),
                            labels = c('T1 experienced','T2 experienced','T1 memory'))) %>% 
  ggplot(aes(x=mean_type, y=mean_rating)) +
  stat_summary()+
  xlab('')+
  ylab('mean rating')+
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14))

