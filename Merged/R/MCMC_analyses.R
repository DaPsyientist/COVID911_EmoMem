#### MCMC ####
#### written by Johnny and Haoxue
#### conduct bayesian generalized linear regression using (mostly) rstan package

# general setup -----------------------------------------------------------

## set path
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
## compile utils
source('utils.R')
#Load All MCMC objects
load(file = "./911nCovid_StressMemory_new.rda")



## read in data
covid_wide <-  read.csv('../data/Complete_test_Dev_Exp_Only_Data_Manipulation.csv') #each row is one observation, T1 and T2 separated by prefix in the colnmae
nine11_wide_avg <- read.csv('../data/df.emo.wide.avg_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is avg across emotions
nine11_wide <- read.csv('../data/df.emo.wide_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is specific to one emotion
nine11_long_avg <- read.csv('../data/df.emo.long.avg_Only_Data_Manipulation.csv') #each row is one observation at one time point, stress_memory is avg across emotions

#  filter time2 data frome 911 to do shared analysis 
nine11_long_avg_T2 <- nine11_long_avg %>% filter(time==2)
nine11_wide_T2 <- nine11_wide %>% dplyr::select(!ends_with('3') & !ends_with('4'))

## define var related to MCMC
niters <- 20000
seed_no <- 1636

# Shared analysis 1: Stress (NegEmo) ~ type (T1_experienced/T2_remembered/T2_experienced) [beta] ---------------------------------------

## Covid
covid_long_type <- covid_wide %>% mutate(BCovid_Exp1 = beta_squeeze((Covid_Exp1-1)/4),
                                         BCovid_Exp2 = beta_squeeze((Covid_Exp2-1)/4),
                                         BCovid_Rem = beta_squeeze(Covid_Rem-1)/4) %>% 
  pivot_longer(cols = c('BCovid_Exp1','BCovid_Exp2','BCovid_Rem'),
               names_to = 'MemType', values_to = 'BStress') %>% 
  mutate(MemType = relevel(factor(MemType), ref='BCovid_Exp1')) %>% 
  filter(!is.na(BStress))
n_ppt <- length(unique(covid_long_type$Subject)) #Calculate number of participants; 726
covid_long_type %>% group_by(MemType) %>% 
  summarise(mean=mean(BStress, na.rm=T), se=sd(BStress, na.rm=T)/sqrt(n_ppt))

#  sanity check: BStress has been beta_squeezed
covid_long_type %>% ggplot(aes(x=BStress)) + geom_histogram(bins = 20) + facet_wrap(~MemType)
if (!exists("MemType_covid_MCMC")) {
MemType_covid_MCMC <- stan_glmer(BStress ~ MemType + (1|Subject), 
                              family = mgcv::betar, iter= niters, seed=seed_no, data = covid_long_type)
}
#Posterior Predictive Check (Ppc)
y_MemType_covid_MCMC<- covid_long_type$BStress
yrep_y_MemType_covid_MCMC <- posterior_predict(MemType_covid_MCMC, draws = 500)
ppc_dens_overlay(y_MemType_covid_MCMC, yrep_y_MemType_covid_MCMC[1:50, ]) #Looks pretty good!
#Summarize Posterior
plot_model(MemType_covid_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red")
describe_posterior(MemType_covid_MCMC)

## 911
nine11_long_type <- nine11_long_avg_T2 %>% 
  mutate(B911_Exp1 = beta_squeeze((prev_experience-1)/4),
         B911_Exp2 = beta_squeeze((curr_experience-1)/4),
         B911_Rem = beta_squeeze((stress_memory-1)/4)) %>% 
  pivot_longer(cols = c('B911_Exp1', 'B911_Exp2','B911_Rem'),
               names_to = 'MemType', values_to = 'BNegEmo') %>% 
  mutate(MemType = relevel(factor(MemType), ref='B911_Exp1')) %>% 
  filter(!is.na(BNegEmo))
 #Number of 911 participants in case we also need it for SE
n_911_ppt <- length(unique(nine11_long_type$Subject)) #Calculate number of participants; 1430

#   sanity check: BNegEmo has been beta_squeezed
nine11_long_type %>% ggplot(aes(x=BNegEmo)) + geom_histogram(bins = 20) + facet_wrap(~MemType) # may worth noting that sanity check result for covid and 911 looks different
if (!exists("MemType_911_MCMC_T2")) {
MemType_911_MCMC_T2 <- stan_glmer(BNegEmo ~ MemType + (1|Subject), 
                                family = mgcv::betar, iter= niters, seed=seed_no,
                                data = nine11_long_type)
}
#Posterior Predictive Check (Ppc)
y_MemType_911_MCMC_T2<- nine11_long_type$BNegEmo
yrep_MemType_911_MCMC_T2 <- posterior_predict(MemType_911_MCMC_T2, draws = 500)
ppc_dens_overlay(y_MemType_911_MCMC_T2, yrep_MemType_911_MCMC_T2[1:50, ]) #Looks decent, less peaked than data
#Summarize Posterior
plot_model(MemType_911_MCMC_T2, type = "pred", ci.lvl = .95, bpe.color = "red")
describe_posterior(MemType_911_MCMC_T2)


# Shared Analysis 2: Percent ~ Dataset (Covid/911_T2) ---------------------

# calculate percentage change 

covid_percent <- covid_wide %>% mutate(Percent = Covid_Dev / twovar_Covid_Exp1) %>% filter(!is.na(Percent)) 
nine11_percent_T2 <- nine11_long_avg_T2 %>% mutate(Percent = memdev / twovar_prev_experience) %>% filter(!is.na(Percent))
combined_percent <- covid_percent %>% dplyr::select(Subject, Percent, Covid_Dev, twovar_Covid_Exp1, num_weights_2var) %>% 
  rename(dev = Covid_Dev, twovar_prev = twovar_Covid_Exp1, num_weights = num_weights_2var) %>% 
  mutate(Dataset = 'Covid') %>% 
  rbind(
    nine11_percent_T2 %>% dplyr::select(Subject, Percent, memdev, twovar_prev_experience, num_weights_updated) %>% 
      rename(dev = memdev, twovar_prev = twovar_prev_experience, num_weights = num_weights_updated) %>% 
      mutate(Dataset = '911')
  ) %>% 
  mutate(Dataset = relevel(factor(Dataset), ref = 'Covid')) %>% 
  group_by(Dataset) %>% 
  mutate(Bdev = beta_squeeze((dev+4)/8))

# sanity check: Percent looks like Gaussian
combined_percent %>% ggplot(aes(x=Percent)) + geom_histogram(bins = 100) + facet_wrap(~Dataset)
if (!exists("Dataset_Percent_MCMC")) {
Dataset_Percent_MCMC <- stan_glm(Percent ~ Dataset, family = "gaussian", iter= niters, seed=seed_no,data = combined_percent) 
}
#Posterior Predictive Check (Ppc)
y_Dataset_Percent_MCMC<- combined_percent$Percent
yrep_Dataset_Percent_MCMC <- posterior_predict(Dataset_Percent_MCMC, draws = 500)
ppc_dens_overlay(y_Dataset_Percent_MCMC, yrep_Dataset_Percent_MCMC[1:50, ]) #Looks decent, less peaked, more dispersed than data
#Summarize Posterior
plot_model(Dataset_Percent_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red")
describe_posterior(Dataset_Percent_MCMC)

#Dataset_Percent_baseline_MCMC <- stan_glm(Percent ~ Dataset + twovar_prev, family = "gaussian", iter= niters, data = combined_percent) 
if (!exists("Dataset_Delta_baseline_MCMC")) {
Dataset_Delta_baseline_MCMC <- stan_glm(Bdev ~ Dataset + twovar_prev, family = mgcv::betar, iter= niters, seed=seed_no, data = combined_percent) 
}
if (!exists("Dataset_Delta_baseline_MCMC_betareg")) {
  Dataset_Delta_baseline_MCMC_betareg <- stan_betareg(Bdev ~ Dataset + twovar_prev, 
                                                      iter= niters, seed=seed_no, data = combined_percent) 
}
#Posterior Predictive Check (Ppc)
y_Dataset_Delta_baseline_MCMC <- combined_percent$Bdev
#yrep_Dataset_Delta_baseline_MCMC <- rstanarm::posterior_predict(Dataset_Delta_baseline_MCMC, draws = 500) #DOESNT WORK, UNSURE WHY
#ppc_dens_overlay(y_Dataset_Delta_baseline_MCMC, yrep_Dataset_Delta_baseline_MCMC[1:50, ]) 
yrep_Dataset_Delta_baseline_MCMC_betareg <- rstanarm::posterior_predict(Dataset_Delta_baseline_MCMC_betareg, draws = 500) #DOESNT WORK, UNSURE WHY
ppc_dens_overlay(y_Dataset_Delta_baseline_MCMC, yrep_Dataset_Delta_baseline_MCMC_betareg[1:50, ]) 
#Summarize Posterior
plot_model(Dataset_Delta_baseline_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red") 
emmeans(Dataset_Delta_baseline_MCMC, specs = 'Dataset') # Question for Johnny: why here it looks weird?; its averaging across twovar_prev -- thats what emmeans does, marginalizes across all other variables; use my log2probit thing to get probability
plot_model(Dataset_Delta_baseline_MCMC, terms= c('(Intercept)','Dataset911','twovar_prev')) + theme_sjplot()# any way to remove phi?; yes! done
mcmc_intervals(as.array(Dataset_Delta_baseline_MCMC), pars = c('(Intercept)','Dataset911','twovar_prev'))
describe_posterior(Dataset_Delta_baseline_MCMC) # more overestimation for 9-11


# do not think weight here makes sense - given that the q are different for covid and 9-11
#Dataset_Delta_baseline_MCMC_weighted <- stan_glm(Bdev ~ Dataset + twovar_prev, 
#                                                 family = mgcv::betar, iter= niters, data = combined_percent,
#                                                 weights = num_weights) 



# Shared Analysis 3: Rem ~ Exp1 + Exp2 ------------------------------------

## Covid (unweighted and weighted results are qualitatively the same i.e., coef(Exp1) < coef(Exp2))
covid_BRem <- covid_wide %>% mutate(BCovid_Rem = beta_squeeze((Covid_Rem-1)/4)) %>% 
  filter(!is.na(BCovid_Rem)) %>% filter(!is.na(Covid_Exp1)) %>% filter(!is.na(Covid_Exp2))
if (!exists("Rem_covid_MCMC_weighted")) {
Rem_covid_MCMC_weighted <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                  family = mgcv::betar, iter= niters, 
                           weights = num_weights_3var, seed = seed_no, data = covid_BRem)
}
if (!exists("Rem_covid_MCMC_weighted_betareg")) {
  Rem_covid_MCMC_weighted_betareg <- stan_betareg(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                      iter= niters, 
                                      weights = num_weights_3var, seed = seed_no, data = covid_BRem)
}

#Posterior Predictive Check (Ppc)
y_Rem_covid_MCMC_weighted <- covid_BRem$BCovid_Rem
#yrep_Rem_covid_MCMC_weighted <- posterior_predict(Rem_covid_MCMC_weighted, draws = 500) #Also broken, IDK why; guessing multiple variables?
#ppc_dens_overlay(y_Rem_covid_MCMC_weighted, yrep_Rem_covid_MCMC_weighted[1:50, ]) 
yrep_Rem_covid_MCMC_weighted_betareg <- posterior_predict(Rem_covid_MCMC_weighted_betareg, draws = 500) #Also broken, IDK why; guessing multiple variables?
ppc_dens_overlay(y_Rem_covid_MCMC_weighted, yrep_Rem_covid_MCMC_weighted_betareg[1:50, ]) 

#Summarize Posterior
plot_model(Rem_covid_MCMC_weighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_covid_MCMC_weighted)
if (!exists("Rem_covid_MCMC_unweighted")) {
Rem_covid_MCMC_unweighted <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                    family = mgcv::betar, iter= niters, seed = seed_no,
                                    data = covid_BRem)
}
plot_model(Rem_covid_MCMC_unweighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_covid_MCMC_unweighted)

## 911_T2 (unweighted and weighted results are qualitatively the same i.e., coef(Exp1) = coef(Exp2))
nine11_BRem_T2 <- nine11_long_avg_T2 %>% mutate(B911_Rem = beta_squeeze((stress_memory-1)/4)) %>% 
  filter(!is.na(B911_Rem)) %>% filter(!is.na(prev_experience)) %>% filter(!is.na(curr_experience))
if (!exists("Rem_911_MCMC_T2_weighted")) {
Rem_911_MCMC_T2_weighted <- stan_glm(B911_Rem ~ prev_experience + curr_experience,
                                    family = mgcv::betar, iter= niters, seed=seed_no,
                                    weights = num_weights_updated, data = nine11_BRem_T2)
}
if (!exists("Rem_911_MCMC_T2_weighted_betareg")) {
  Rem_911_MCMC_T2_weighted_betareg <- stan_glm(B911_Rem ~ prev_experience + curr_experience,
                                       iter= niters, seed=seed_no,
                                       weights = num_weights_updated, data = nine11_BRem_T2)
}
#Posterior Predictive Check (Ppc)
y_Rem_911_MCMC_T2_weighted <- nine11_BRem_T2$B911_Rem
#yrep_Rem_911_MCMC_T2_weighted <- posterior_predict(Rem_911_MCMC_T2_weighted, draws = 500) #Also broken, IDK why; guessing multiple variables?
#ppc_dens_overlay(y_Rem_911_MCMC_T2_weighted, yrep_Rem_911_MCMC_T2_weighted[1:50, ]) 
yrep_Rem_911_MCMC_T2_weighted_betareg <- posterior_predict(Rem_911_MCMC_T2_weightedbetareg, draws = 500) #Also broken, IDK why; guessing multiple variables?
ppc_dens_overlay(y_Rem_911_MCMC_T2_weighted, yrep_Rem_911_MCMC_T2_weighted_betareg[1:50, ]) 

#Summarize Posterior
plot_model(Rem_911_MCMC_T2_weighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_911_MCMC_T2_weighted)
if (!exists("Rem_911_MCMC_T2_unweighted")) {
Rem_911_MCMC_T2_unweighted <- stan_glm(B911_Rem ~ prev_experience + curr_experience,
                                     family = mgcv::betar, iter= niters, seed=seed_no,
                                     data = nine11_BRem_T2)
}
plot_model(Rem_911_MCMC_T2_unweighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_911_MCMC_T2_unweighted)


# Separate Analysis (9/11): Percent ~ MemTime * Emotion ------------------------------------------------

## Percent (avg over emotions) ~ Time
nine11_percent <- nine11_long_avg %>% mutate(Percent = memdev / twovar_prev_experience) %>% 
  mutate(time = relevel(factor(time), ref='2')) %>% 
  filter(!is.na(Percent))

nine11_percent %>% group_by(time) %>% 
  summarise(mean=mean(Percent), sd=sd(Percent))
if (!exists("Time_avg_911_MCMC_weighted")) {
Time_avg_911_MCMC_weighted <- stan_glm(Percent ~ time, 
                             family = 'gaussian', iter= niters, 
                             weights = num_weights_updated, seed = seed_no,
                             data = nine11_percent)
}
#Posterior Predictive Check (Ppc)
y_Time_avg_911_MCMC_weighted <- nine11_percent$Percent
yrep_Time_avg_911_MCMC_weighted <- posterior_predict(Time_avg_911_MCMC_weighted, draws = 500)
ppc_dens_overlay(y_Time_avg_911_MCMC_weighted, yrep_Time_avg_911_MCMC_weighted[1:50, ]) #Looks fine
#Summarize Posterior
plot_model(Time_avg_911_MCMC_weighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Time_avg_911_MCMC_weighted)
if (!exists("Time_avg_911_MCMC_unweighted")) {
Time_avg_911_MCMC_unweighted <- stan_glm(Percent ~ time, 
                                       family = 'gaussian', iter= niters, seed = seed_no,
                                       data = nine11_percent)
}
plot_model(Time_avg_911_MCMC_unweighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
#Posterior Predictive Check (Ppc)
y_Time_avg_911_MCMC_unweighted <- nine11_percent$Percent
yrep_Time_avg_911_MCMC_unweighted <- posterior_predict(Time_avg_911_MCMC_unweighted, draws = 500) 
ppc_dens_overlay(y_Time_avg_911_MCMC_unweighted, yrep_Time_avg_911_MCMC_unweighted[1:50, ]) #Looks fine; changed very little
#Summarize Posterior
describe_posterior(Time_avg_911_MCMC_unweighted)

## Percent ~ Time * Emotion
nine11_wide_subj <- nine11_wide %>% 
  mutate(   n_experienced1 = sum(!is.na(experienced1)),
            n_experienced2 = sum(!is.na(experienced2)),
            n_experienced3 = sum(!is.na(experienced3)),
            n_experienced4 = sum(!is.na(experienced4)),
            n_remembered2 = sum(!is.na(remembered2)),
            n_remembered3 = sum(!is.na(remembered3)),
            n_remembered4 = sum(!is.na(remembered4)),
            mean_memdev2_perc = memdev2 / experienced2,
            mean_memdev3_perc = memdev3 / experienced3,
            mean_memdev4_perc = memdev4 / experienced4) 
nine11_wide_subj_percent <- nine11_wide_subj %>% dplyr::select(Subject, item, mean_memdev2_perc, mean_memdev3_perc, mean_memdev4_perc) %>% 
  pivot_longer(cols = mean_memdev2_perc:mean_memdev4_perc, names_to = 'MemTime', values_to = 'Percent') %>% 
  filter(!is.na(Percent)) %>% ungroup() %>% 
  mutate(item = relevel(factor(item), ref='frustration'))
if (!exists("StressOTEmo911_MCMC")) {
StressOTEmo911_MCMC <- stan_glmer(Percent ~ MemTime * item + (1|Subject), 
                                  family = "gaussian", iter= niters, seed=seed_no, data = nine11_wide_subj_percent) 
}
#Posterior Predictive Check (Ppc)
y_StressOTEmo911_MCMC <- nine11_wide_subj_percent$Percent
yrep_StressOTEmo911_MCMC <- posterior_predict(StressOTEmo911_MCMC, draws = 500) 
ppc_dens_overlay(y_StressOTEmo911_MCMC, yrep_StressOTEmo911_MCMC[1:50, ]) #This is actually pretty great!
#Summarize Posterior
plot_model(StressOTEmo911_MCMC, type = "int", ci.lvl = .95, bpe.color = "red") + facet_grid("item"~"DevTime")
describe_posterior(StressOTEmo911_MCMC)


# Separate Analysis (9/11): Rem ~ (prev + curr) * time --------------------

nine11_BRem <- nine11_long_avg %>% mutate(time = relevel(factor(time), ref='2')) %>% 
  group_by(time) %>% mutate(B911_Rem = beta_squeeze((stress_memory-1)/4)) %>% 
  filter(!is.na(B911_Rem)) %>% filter(!is.na(prev_experience)) %>% filter(!is.na(curr_experience))
if (!exists("Rem_911_MCMC_weighted")) {
Rem_911_MCMC_weighted <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                     family = mgcv::betar, iter= niters, seed = seed_no,
                                     weights = num_weights_updated, data = nine11_BRem)
}
#Posterior Predictive Check (Ppc)
y_Rem_911_MCMC_weighted <- nine11_BRem$B911_Rem
#yrep_Rem_911_MCMC_weighted <- posterior_predict(Rem_911_MCMC_weighted, draws = 500) #Broken but different error; presumably because multiple variables
#ppc_dens_overlay(y_Rem_911_MCMC_weighted, yrep_Rem_911_MCMC_weighted[1:50, ])
#Summarize Posterior
plot_model(Rem_911_MCMC_weighted, type = "int", ci.lvl = .95, bpe.color = "red") + facet_grid(item~DevTime)
describe_posterior(Rem_911_MCMC_weighted)
if (!exists("Rem_911_MCMC_T2_unweighted")) {
Rem_911_MCMC_T2_unweighted <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                     family = mgcv::betar, seed= seed_no, iter= niters, data = nine11_BRem)
}
#Posterior Predictive Check (Ppc)
y_Rem_911_MCMC_T2_unweighted <- nine11_BRem$B911_Rem
#yrep_Rem_911_MCMC_T2_unweighted <- posterior_predict(Rem_911_MCMC_T2_unweighted, draws = 500) #Broken, same error as previously
#ppc_dens_overlay(y_Rem_911_MCMC_T2_unweighted, yrep_Rem_911_MCMC_T2_unweighted[1:50, ])
#Summarize Posterior
plot_model(Rem_911_MCMC_T2_unweighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_911_MCMC_T2_unweighted)

# Separate Analysis (covid): Overestimation ~ Delta Emotion Well-being --------

## delta emo well-being is larger than 0 (mean_PCA_T1 = 0 so we focus on delta here)
c(covid_wide$PCA_T1 %>% mean(na.rm=T), covid_wide$PCA_T1 %>% sd(na.rm=T))
c(covid_wide$PCA_T2 %>% mean(na.rm=T), covid_wide$PCA_T2 %>% sd(na.rm=T))
if (!exists("covid_PCA_MCMC")) {
covid_PCA_MCMC <- stan_glm(emo_well_being_delta ~ 1,
                           family = "gaussian",
                           iter= niters, seed=seed_no, data = covid_wide)
}
#Posterior Predictive Check (Ppc)
y_covid_PCA_MCMC <- covid_wide %>% filter(emo_well_being_delta != "NA") %>% dplyr::select(emo_well_being_delta)
y_covid_PCA_MCMC <- y_covid_PCA_MCMC$emo_well_being_delta
yrep_covid_PCA_MCMC <- posterior_predict(covid_PCA_MCMC, draws = 500) #Good, but less peaked then the data
ppc_dens_overlay(y_covid_PCA_MCMC, yrep_covid_PCA_MCMC[1:50, ])
#Summarize Posterior
plot_model(covid_PCA_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(covid_PCA_MCMC)

## overestimation ~ delta emo well-being
covid_percent <- covid_wide %>% 
  mutate(BCovid_Dev = beta_squeeze((Covid_Dev+4)/8)) %>% 
  mutate(Percent = Covid_Dev / twovar_Covid_Exp1) %>% filter(!is.na(Percent)) %>% 
  filter(!is.na(BCovid_Dev)) %>% filter(!is.na(emo_well_being_delta))
if (!exists("covid_delta_EmoWell_MCMC")) {
covid_delta_EmoWell_MCMC <- stan_glm(BCovid_Dev ~ emo_well_being_delta + Covid_Exp1,
                                         family = mgcv::betar, seed = seed_no,
                                         iter= niters, weights = num_weights_3var,
                                         covid_percent %>% filter(!is.na(Covid_Exp1)))
}
if (!exists("covid_delta_EmoWell_MCMC_both")) {
covid_delta_EmoWell_MCMC_both <- stan_glm(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                     family = mgcv::betar, seed = seed_no,
                                     iter= niters, weights = num_weights_3var,
                                     covid_percent %>% filter(!is.na(Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}
if (!exists("covid_delta_EmoWell_MCMC_both_2var")) {
covid_delta_EmoWell_MCMC_both_2var <- stan_glm(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                          family = mgcv::betar, seed = seed_no,
                                          iter= niters, weights = num_weights_2var,
                                          covid_percent %>% filter(!is.na(twovar_Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}
if (!exists("covid_delta_EmoWell_MCMC_betareg")) {
  covid_delta_EmoWell_MCMC_betareg <- stan_betareg(BCovid_Dev ~ emo_well_being_delta + Covid_Exp1,
                                       seed = seed_no,
                                       iter= niters, weights = num_weights_3var,
                                       covid_percent %>% filter(!is.na(Covid_Exp1)))
}

if (!exists("covid_delta_EmoWell_MCMC_both_betareg")) {
  covid_delta_EmoWell_MCMC_both_betareg <- stan_betareg(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                                        seed = seed_no,
                                                        iter= niters, weights = num_weights_3var,
                                                        covid_percent %>% filter(!is.na(twovar_Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}
if (!exists("covid_delta_EmoWell_MCMC_both_2var_betareg")) {
  covid_delta_EmoWell_MCMC_both_2var_betareg <- stan_betareg(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                                             seed = seed_no,
                                                             iter= niters, weights = num_weights_2var,
                                                             covid_percent %>% filter(!is.na(twovar_Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}

#Posterior Predictive Check (Ppc)
#y_covid_percent_EmoWell_MCMC <- covid_percent$Percent
#yrep_covid_percent_EmoWell_MCMC <- posterior_predict(covid_percent_EmoWell_MCMC, draws = 500) #Broken
y_covid_delta_EmoWell_MCMC <- (covid_percent %>% filter(!is.na(twovar_Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))$BCovid_Dev
yrep_covid_delta_EmoWell_MCMC_betareg <- posterior_predict(covid_delta_EmoWell_MCMC_both_betareg, draws = 500) #Broken
ppc_dens_overlay(y_covid_delta_EmoWell_MCMC, yrep_covid_delta_EmoWell_MCMC_betareg[1:50, ])
#Summarize Posterior
plot_model(covid_percent_EmoWell_MCMC, type = "int", ci.lvl = .95, bpe.color = "red")
describe_posterior(covid_percent_EmoWell_MCMC)
describe_posterior(covid_percent_EmoWell_MCMC_2)
describe_posterior(covid_delta_EmoWell_MCMC)
describe_posterior(covid_delta_EmoWell_MCMC_both)

# save MCMC objects for plotting ------------------------------------------
save(MemType_covid_MCMC, MemType_911_MCMC_T2, Dataset_Percent_MCMC, Dataset_Delta_baseline_MCMC,
     Dataset_Delta_baseline_MCMC_betareg, 
     Rem_covid_MCMC_weighted, Rem_covid_MCMC_weighted_betareg,
     Rem_911_MCMC_T2_weighted, Rem_911_MCMC_T2_weighted_betareg,
     covid_PCA_MCMC, 
     Rem_covid_MCMC_unweighted,Rem_911_MCMC_unweighted,
     covid_delta_EmoWell_MCMC, covid_delta_EmoWell_MCMC_both, covid_delta_EmoWell_MCMC_both_2var, 
     covid_delta_EmoWell_MCMC_betareg, covid_delta_EmoWell_MCMC_both_betareg, covid_delta_EmoWell_MCMC_both_2var_betareg,
     file = "./911nCovid_StressMemory_simple.rda")

save(Time_avg_911_MCMC_weighted,
     Time_avg_911_MCMC_unweighted, StressOTEmo911_MCMC, 
     Rem_911_MCMC_weighted_ref2,
     Rem_911_MCMC_weighted_ref3, Rem_911_MCMC_weighted_ref4,
     Rem_911_MCMC_T2_unweighted, 
     file = "./911nCovid_StressMemory_mixed.rda")

