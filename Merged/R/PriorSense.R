library(rstanarm, bayestestR)

#Specify Prior Distributions
generic_veryweak <- normal(location = 0, scale = 1, autoscale = FALSE) #Standard stan_glm prior: N(0, 2.5)
generic_weak <- normal(location = 0, scale = .5, autoscale = FALSE) #Generic Weakly Informative Prior: N(0, 1)
InterPrior_weak <- normal(location = 0, scale = .5, autoscale = FALSE)
InterPrior_veryweak <- normal(location = 0, scale = 1, autoscale = FALSE)

#Load all files for prior sensitivity Analysis
load(file = "./911nCovid_Data.rda")
load(file = "./911nCovid_StressMemory_PSBF.rda")
load(file = "./911nCovid_StressMemory_PS.rda")
load(file = "./911nCovid_StressMemory_simple.rda")

#### MCMC Prior Sensitivity ####
# Shared analysis 1: Stress (NegEmo) ~ type (T1_experienced/T2_remembered/T2_experienced) [beta] 
##COVID-19
#Standard priors (0, 2.5)
prior_summary(MemType_covid_MCMC) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(MemType_covid_MCMC, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("MemType_covid_MCMC_VeryWeak")) {
MemType_covid_MCMC_VeryWeak <- stan_glmer(BStress ~ MemType + (1|Subject), 
                                 family = mgcv::betar, iter= niters, seed=seed_no,
                                 prior = generic_veryweak, prior_intercept = InterPrior_veryweak, data = covid_long_type)
}
check_prior(MemType_covid_MCMC_VeryWeak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Prior (0, .5)
if (!exists("MemType_covid_MCMC_Weak")) {
MemType_covid_MCMC_Weak <- stan_glmer(BStress ~ MemType + (1|Subject), 
                                      family = mgcv::betar, iter= niters, seed=seed_no, 
                                      prior = generic_weak, prior_intercept = InterPrior_weak, data = covid_long_type)
}
check_prior(MemType_covid_MCMC_Weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

##911
#Standard priors (0, 2.5)
prior_summary(MemType_911_MCMC_T2) #Normal Prior ~ (0,2.5) & (0, 2.5)
#Very Weakly Informative Prior (0, 1)
if (!exists("MemType_911_MCMC_T2_VeryWeak")) {
MemType_911_MCMC_T2_VeryWeak <- stan_glmer(BNegEmo ~ MemType + (1|Subject), 
                                  family = mgcv::betar, iter= niters, seed=seed_no, 
                                  prior = generic_veryweak, prior_intercept = InterPrior_veryweak, data = nine11_long_type)
}
check_prior(MemType_911_MCMC_T2_VeryWeak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Prior (0, .5)
if (!exists("MemType_911_MCMC_T2_Weak")) {
MemType_911_MCMC_T2_Weak <- stan_glmer(BNegEmo ~ MemType + (1|Subject), 
                                           family = mgcv::betar, iter= niters, seed=seed_no,
                                           prior = generic_weak, prior_intercept = InterPrior_weak, data = nine11_long_type)
}
check_prior(MemType_911_MCMC_T2_Weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

# Shared Analysis 2: Percent ~ Dataset (Covid/911_T2) 
##Percentage
prior_summary(Dataset_Percent_MCMC) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(Dataset_Percent_MCMC, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("Dataset_Percent_MCMC_VeryWeak")) {
Dataset_Percent_MCMC_VeryWeak <- stan_glm(Percent ~ Dataset, family = "gaussian", iter= niters, seed=seed_no, prior = generic_veryweak, prior_intercept = InterPrior_veryweak, data = combined_percent)
}
check_prior(Dataset_Percent_MCMC_VeryWeak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("Dataset_Percent_MCMC_Weak")) {
Dataset_Percent_MCMC_Weak <- stan_glm(Percent ~ Dataset, family = "gaussian", iter= niters, seed=seed_no, prior = generic_weak, prior_intercept = InterPrior_weak, data = combined_percent)
}
check_prior(Dataset_Percent_MCMC_Weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative


# Shared Analysis 3: Rem ~ Exp1 + Exp2 
### COVID-19
## Stan_GLM
prior_summary(Rem_covid_MCMC_weighted) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(Rem_covid_MCMC_weighted, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("Rem_covid_MCMC_weighted_veryweak")) {
Rem_covid_MCMC_weighted_veryweak <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                    family = mgcv::betar, iter= niters, 
                                    weights = num_weights_3var, seed = seed_no, prior = generic_veryweak, prior_intercept = InterPrior_veryweak, data = covid_BRem)
}
check_prior(Rem_covid_MCMC_weighted_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("Rem_covid_MCMC_weighted_weak")) {
Rem_covid_MCMC_weighted_weak <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                             family = mgcv::betar, iter= niters, 
                                             weights = num_weights_3var, seed = seed_no, prior = generic_weak, prior_intercept = InterPrior_weak, data = covid_BRem)
}
check_prior(Rem_covid_MCMC_weighted_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

### 9/11
prior_summary(Rem_911_MCMC_T2_weighted_betareg) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(Rem_911_MCMC_T2_weighted_betareg, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("Rem_911_MCMC_T2_weighted_betareg_veryweak")) {
Rem_911_MCMC_T2_weighted_betareg_veryweak <- stan_glm(B911_Rem ~ prev_experience + curr_experience,
                                             iter= niters, seed=seed_no, prior = generic_veryweak, prior_intercept = InterPrior_veryweak,
                                             weights = num_weights_updated, data = nine11_BRem_T2)
}
check_prior(Rem_911_MCMC_T2_weighted_betareg_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("Rem_911_MCMC_T2_weighted_betareg_weak")) {
Rem_911_MCMC_T2_weighted_betareg_weak <- stan_glm(B911_Rem ~ prev_experience + curr_experience,
                                                      iter= niters, seed=seed_no, prior = generic_weak, prior_intercept = InterPrior_weak,
                                                      weights = num_weights_updated, data = nine11_BRem_T2)
}
check_prior(Rem_911_MCMC_T2_weighted_betareg_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

# Separate Analysis (9/11): Percent ~ MemTime
prior_summary(Time_avg_911_MCMC_weighted) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(Time_avg_911_MCMC_weighted, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("Time_avg_911_MCMC_weighted_veryweak")) {
  Time_avg_911_MCMC_weighted_veryweak <- stan_glm(Percent ~ time, 
                                                  family = 'gaussian', iter= niters, prior = generic_veryweak, prior_intercept = InterPrior_veryweak,
                                                  weights = num_weights_updated, seed = seed_no,
                                                  data = nine11_percent)
}
check_prior(Time_avg_911_MCMC_weighted_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("Time_avg_911_MCMC_weighted_weak")) {
  Time_avg_911_MCMC_weighted_weak <- stan_glm(Percent ~ time, 
                                              family = 'gaussian', iter= niters, prior = generic_weak, prior_intercept = InterPrior_weak,
                                              weights = num_weights_updated, seed = seed_no, 
                                              data = nine11_percent)
}
check_prior(Time_avg_911_MCMC_weighted_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative



## Separate Analysis (9/11): Rem ~ (prev + curr) * time 
prior_summary(Rem_911_MCMC_weighted) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(Rem_911_MCMC_weighted, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("Rem_911_MCMC_weighted_veryweak")) {
Rem_911_MCMC_weighted_veryweak <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                    family = mgcv::betar, iter= niters, seed = seed_no,
                                    prior = generic_veryweak, prior_intercept = InterPrior_veryweak,
                                    weights = num_weights_updated, data = nine11_BRem)
}
check_prior(Rem_911_MCMC_weighted_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("Rem_911_MCMC_weighted_weak")) {
Rem_911_MCMC_weighted_weak <- stan_glmer(B911_Rem ~ (prev_experience + curr_experience) * time + (1|Subject),
                                             family = mgcv::betar, iter= niters, seed = seed_no, 
                                             prior = generic_weak, prior_intercept = InterPrior_weak,
                                             weights = num_weights_updated, data = nine11_BRem)
}
check_prior(Rem_911_MCMC_weighted_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative


# Separate Analysis (covid): Overestimation ~ Delta Emotion Well-being 
prior_summary(covid_PCA_MCMC) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(covid_PCA_MCMC, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("covid_PCA_MCMC_veryweak")) {
covid_PCA_MCMC_veryweak <- stan_glm(emo_well_being_delta ~ 1,
                           family = "gaussian",
                           prior = generic_veryweak, prior_intercept = InterPrior_veryweak,
                           iter= niters, seed=seed_no, data = covid_wide)
}
check_prior(covid_PCA_MCMC_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative

#Weak Informative Prior (0, .5)
if (!exists("covid_PCA_MCMC_weak")) {
covid_PCA_MCMC_weak <- stan_glm(emo_well_being_delta ~ 1,
                                    family = "gaussian",
                                    prior = generic_weak, prior_intercept = InterPrior_weak,
                                    iter= niters, seed=seed_no, data = covid_wide)
}
check_prior(covid_PCA_MCMC_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #Uninformative


## overestimation ~ delta emo well-being
##Stan GLM
prior_summary(covid_delta_EmoWell_MCMC_both) #Normal Prior ~ (0,2.5) & (0, 2.5)
check_prior(covid_delta_EmoWell_MCMC_both, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Very Weakly Informative Prior (0, 1)
if (!exists("covid_delta_EmoWell_MCMC_both_veryweak")) {
covid_delta_EmoWell_MCMC_both_veryweak <- stan_glm(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                          family = mgcv::betar, seed = seed_no,
                                          prior = generic_veryweak, prior_intercept = InterPrior_veryweak,
                                          iter= niters, weights = num_weights_3var,
                                          covid_percent %>% filter(!is.na(Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}
check_prior(covid_delta_EmoWell_MCMC_both_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative

#Weak Informative Prior (0, .5)
if (!exists("covid_delta_EmoWell_MCMC_both_weak")) {
covid_delta_EmoWell_MCMC_both_weak <- stan_glm(BCovid_Dev ~ emo_well_being_delta + twovar_Covid_Exp1 + Covid_Exp2,
                                                   family = mgcv::betar, seed = seed_no, 
                                                   prior = generic_weak, prior_intercept = InterPrior_weak,
                                                   iter= niters, weights = num_weights_3var,
                                                   covid_percent %>% filter(!is.na(Covid_Exp1)) %>% filter(!is.na(Covid_Exp2)))
}
check_prior(covid_delta_EmoWell_MCMC_both_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative


#### Evaluate prior influence ####
# Shared analysis 1: Stress (NegEmo) ~ type (T1_experienced/T2_remembered/T2_experienced) [beta] 
set.seed(14124869)
#Covid
MemType_covid_MCMC_standardBF <- bayesfactor_parameters(MemType_covid_MCMC, null = 0) #Conclusive evidence for both
plot(MemType_covid_MCMC_standardBF)  
MemType_covid_MCMC_VeryWeakBF <- bayesfactor_parameters(MemType_covid_MCMC_VeryWeak, null = 0) #Conclusive Evidence for both
plot(MemType_covid_MCMC_VeryWeakBF)
MemType_covid_MCMC_WeakBF <- bayesfactor_parameters(MemType_covid_MCMC_Weak, null = 0) #Conclusive Evidence for both
plot(MemType_covid_MCMC_WeakBF)
#9/11
MemType_911_MCMC_T2_standardBF <- bayesfactor_parameters(MemType_covid_MCMC, null = 0) #Conclusive evidence for both
plot(MemType_911_MCMC_T2_standardBF)  
MemType_911_MCMC_T2_VeryWeakBF <- bayesfactor_parameters(MemType_911_MCMC_T2_VeryWeak, null = 0) #Conclusive evidence for both
plot(MemType_911_MCMC_T2_VeryWeakBF)
MemType_911_MCMC_T2_WeakBF <- bayesfactor_parameters(MemType_911_MCMC_T2_Weak, null = 0) #Conclusive evidence for both
plot(MemType_911_MCMC_T2_WeakBF)

# Shared Analysis 2: Percent ~ Dataset (Covid/911_T2) 
#Percentage
Dataset_Percent_MCMC_standardBF <- bayesfactor_parameters(Dataset_Percent_MCMC, null = 0) #Conclusive evidence for both
plot(Dataset_Percent_MCMC_standardBF)  
Dataset_Percent_MCMC_VeryWeakBF <- bayesfactor_parameters(Dataset_Percent_MCMC_VeryWeak, null = 0) #Conclusive evidence for both
plot(Dataset_Percent_MCMC_VeryWeakBF)
Dataset_Percent_MCMC_WeakBF <- bayesfactor_parameters(Dataset_Percent_MCMC_Weak, null = 0) #Conclusive evidence for both
plot(Dataset_Percent_MCMC_WeakBF)


# Shared Analysis 3: Rem ~ Exp1 + Exp2 
### COVID-19
Rem_covid_MCMC_weighted_standardBF <- bayesfactor_parameters(Rem_covid_MCMC_weighted, null = 0) #Conclusive evidence for both
plot(Rem_covid_MCMC_weighted_standardBF)  
Rem_covid_MCMC_weighted_veryweakBF <- bayesfactor_parameters(Rem_covid_MCMC_weighted_veryweak, null = 0) #Conclusive evidence for both
plot(Rem_covid_MCMC_weighted_veryweakBF)
Rem_covid_MCMC_weighted_weakBF <- bayesfactor_parameters(Rem_covid_MCMC_weighted_weak, null = 0) #Conclusive evidence for both
plot(Rem_covid_MCMC_weighted_weakBF)

### 9/11
Rem_911_MCMC_T2_weighted_betareg_standardBF <- bayesfactor_parameters(Rem_911_MCMC_T2_weighted_betareg, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_T2_weighted_betareg_standardBF)  
Rem_911_MCMC_T2_weighted_betareg_veryweakBF <- bayesfactor_parameters(Rem_911_MCMC_T2_weighted_betareg_veryweak, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_T2_weighted_betareg_veryweakBF)
Rem_911_MCMC_T2_weighted_betareg_weakBF <- bayesfactor_parameters(Rem_911_MCMC_T2_weighted_betareg_weak, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_T2_weighted_betareg_weakBF)


## Separate Analysis (9/11): Rem ~ (prev + curr) * time 
Rem_911_MCMC_weighted_standardBF <- bayesfactor_parameters(Rem_911_MCMC_weighted, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_weighted_standardBF)  
Rem_911_MCMC_weighted_veryweakBF <- bayesfactor_parameters(Rem_911_MCMC_weighted_veryweak, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_weighted_veryweakBF)
Rem_911_MCMC_weighted_weakBF <- bayesfactor_parameters(Rem_911_MCMC_weighted_weak, null = 0) #Conclusive evidence for both
plot(Rem_911_MCMC_weighted_weakBF)

# Separate Analysis (covid): Overestimation ~ Delta Emotion Well-being**
covid_PCA_MCMC_standardBF <- bayesfactor_parameters(covid_PCA_MCMC, null = 0) #Conclusive evidence for both
plot(covid_PCA_MCMC_standardBF)  
covid_PCA_MCMC_veryweakBF <- bayesfactor_parameters(covid_PCA_MCMC_veryweak, null = 0) #Conclusive evidence for both
plot(covid_PCA_MCMC_veryweakBF)
covid_PCA_MCMC_weakBF <- bayesfactor_parameters(covid_PCA_MCMC_weak, null = 0) #Conclusive evidence for both
plot(covid_PCA_MCMC_weakBF)
#Check prior on both models (informative x 2)
check_prior(covid_PCA_MCMC_veryweak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative
check_prior(covid_PCA_MCMC_weak, method = "gelman") #Gelman Method ~ SD of posterior > .1 SD of prior; #uninformative


## overestimation ~ delta emo well-being
##Stan GLM
covid_delta_EmoWell_MCMC_both_standardBF <- bayesfactor_parameters(covid_delta_EmoWell_MCMC_both, null = 0) #Conclusive evidence for both
plot(covid_delta_EmoWell_MCMC_both_standardBF)  
covid_delta_EmoWell_MCMC_both_veryweakBF <- bayesfactor_parameters(covid_delta_EmoWell_MCMC_both_veryweak, null = 0) #Conclusive evidence for both
plot(covid_delta_EmoWell_MCMC_both_veryweakBF)
covid_delta_EmoWell_MCMC_both_weakBF <- bayesfactor_parameters(covid_delta_EmoWell_MCMC_both_weak, null = 0) #Conclusive evidence for both
plot(covid_delta_EmoWell_MCMC_both_weakBF)

#### Prior Predictive Check ####
### Analysis 1: Stress (NegEmo) ~ type (T1_experienced/T2_remembered/T2_experienced) [beta] 
## COVID-19
covid_long_type$BStress #DV
prior_summary(MemType_covid_MCMC_Weak)
summary(MemType_covid_MCMC_Weak, digits = 4)
## intx: -0.937(0.044)
## MemType: Exp2: -0.283(0.037) // Rem: 0.199(0.036)
nsims <- 100
#Simulate data distribution based on prior
n = nrow(covid_long_type)
covid_long_type$BStress_c<- scale(covid_long_type$BStress, scale = FALSE) # Centering DV
set.seed(123)
sigma <- rexp(nsims, rate = 10.1)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 1
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
Bstress_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) Bstress_rep[i,] <- beta0[i] + beta1[i]*covid_long_type$BStress_c + beta2[i]*covid_long_type$BStress_c + rnorm(n, mean = 0, sd = sigma[i])
str(Bstress_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(covid_long_type$BStress_c[,], Bstress_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5)) 
# Seems okay


## 9/11
nine11_long_type$BNegEmo #DV
prior_summary(MemType_911_MCMC_T2_Weak)
summary(MemType_911_MCMC_T2_Weak, digits = 4)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(nine11_long_type)
nine11_long_type$BNegEmo_c <- scale(nine11_long_type$BNegEmo, scale = FALSE) # Centering DV
set.seed(123)
sigma <- rexp(nsims, rate = 9.1)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 1
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
B911stress_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) B911stress_rep[i,] <- beta0[i] + beta1[i]*nine11_long_type$BNegEmo_c + beta2[i]*nine11_long_type$BNegEmo_c + rnorm(n, mean = 0, sd = sigma[i])
str(B911stress_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(nine11_long_type$BNegEmo_c[,], B911stress_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5)) 
# Seems okay

### Analysis: Percent ~ Dataset (Covid/911_T2) 
combined_percent$Percent
prior_summary(Dataset_Percent_MCMC_Weak)
summary(Dataset_Percent_MCMC_Weak, digits = 4)
## intx: .108(.0172)
## Percent: -0.212(0.024)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(combined_percent)
combined_percent$Percent_c<- scale(combined_percent$Percent, scale = FALSE) # Centering DV
set.seed(123)
sigma <- rexp(nsims, rate = 2.2)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope
hist(beta1)
Percent_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) Percent_rep[i,] <- beta0[i] + beta1[i]*combined_percent$Percent_c + rnorm(n, mean = 0, sd = sigma[i])
str(Percent_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(combined_percent$Percent_c[,], Percent_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5))
# Seems eh

# Shared Analysis 3: Rem ~ Exp1 + Exp2 
### COVID-19
covid_BRem$BCovid_Rem
prior_summary(Rem_covid_MCMC_weighted_weak)
summary(Rem_covid_MCMC_weighted_weak, digits = 4)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(covid_BRem)
covid_BRem$BCovid_Rem_c<- scale(covid_BRem$BCovid_Rem, scale = FALSE) # Centering DV
covid_BRem$Covid_Exp1_c<- scale(covid_BRem$Covid_Exp1, scale = FALSE) # Centering IV1
covid_BRem$Covid_Exp2_c<- scale(covid_BRem$Covid_Exp2, scale = FALSE) # Centering IV2

set.seed(123)
sigma <- rexp(nsims, rate = 8.3275)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
CovidRem_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) CovidRem_rep[i,] <- beta0[i] + beta1[i]*covid_BRem$Covid_Exp1_c + beta2[i]*covid_BRem$Covid_Exp2_c + rnorm(n, mean = 0, sd = sigma[i])
str(CovidRem_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(covid_BRem$BCovid_Rem_c[,], CovidRem_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5))
# Seems okay

### 9/11
nine11_BRem_T2$B911_Rem
prior_summary(Rem_911_MCMC_T2_weighted_betareg_weak)
summary(Rem_911_MCMC_T2_weighted_betareg_weak, digits = 4)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(nine11_BRem_T2)
nine11_BRem_T2$B911_Rem_c <- scale(nine11_BRem_T2$B911_Rem, scale = FALSE) ## centering for better interpretation of intercept (not needed for PrPC, but useful)
nine11_BRem_T2$prev_experience_c <- scale(nine11_BRem_T2$prev_experience, scale = FALSE) # Centering IV1
nine11_BRem_T2$curr_experience_c <- scale(nine11_BRem_T2$curr_experience, scale = FALSE) # Centering IV2

set.seed(123)
sigma <- rexp(nsims, rate = .1274)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
Rem911_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) Rem911_rep[i,] <- beta0[i] + beta1[i]*nine11_BRem_T2$prev_experience_c + beta2[i]*nine11_BRem_T2$curr_experience_c + rnorm(n, mean = 0, sd = sigma[i])
str(Rem911_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(nine11_BRem_T2$B911_Rem_c[,], Rem911_rep, ppc_dens_overlay)+ xlim(c(-5, 5))
# Seems eh; 1 good fit of hundreds


### 9/11 x Time
nine11_percent$Percent
prior_summary(Time_avg_911_MCMC_weighted_weak)
summary(Time_avg_911_MCMC_weighted_weak)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(nine11_percent)
nine11_percent$Percent_c<- scale(nine11_percent$Percent, scale = FALSE) # Centering DV
set.seed(123)
sigma <- rexp(nsims, rate = 3.6)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
Percent911_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) Percent911_rep[i,] <- beta0[i] + beta1[i]*nine11_percent$Percent_c + beta2[i]*nine11_percent$Percent_c + rnorm(n, mean = 0, sd = sigma[i])
str(Percent911_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(nine11_percent$Percent_c[,], Percent911_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5))
# Seems good


### Analysis (911): Rem ~ (prev + curr) * time 
nine11_BRem$B911_Rem
prior_summary(Rem_911_MCMC_weighted_weak)
summary(Rem_911_MCMC_weighted_weak)

nsims <- 100
#Simulate data distribution based on prior
n = nrow(nine11_BRem)
nine11_BRem$B911_Rem_c<- scale(nine11_BRem$B911_Rem, scale = FALSE) # Centering DV
nine11_BRem$prev_experience_c<- scale(nine11_BRem$prev_experience, scale = FALSE) # Centering IV1
nine11_BRem$curr_experience_c<- scale(nine11_BRem$curr_experience, scale = FALSE) # Centering IV2

set.seed(123)
sigma <- rexp(nsims, rate = 40)               ## Estimated phi
hist(sigma)
beta0 <- rnorm(nsims, mean = 0, sd = .5)        ## prior for intercept (gives enough range for heat scale)
hist(beta0)
beta1 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope
hist(beta1)
beta2 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta2)
beta3 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta3)
beta4 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta4)
beta5 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta5)
beta6 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta6)
beta7 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta7)
beta8 <- rnorm(nsims, mean = 0, sd = .5)        ## vague prior for slope 2
hist(beta8)

B911Rem_rep <- matrix(NA, nsims, n)     ## initialize nsims x n matrix
for(i in 1:nsims) B911Rem_rep[i,] <- beta0[i] + beta1[i]*nine11_BRem$prev_experience_c + beta2[i]*nine11_BRem$curr_experience_c + beta3[i]*nine11_BRem$B911_Rem_c + beta4[i]*nine11_BRem$B911_Rem_c + beta5[i]*nine11_BRem$B911_Rem_c + beta6[i]*nine11_BRem$B911_Rem_c + beta7[i]*nine11_BRem$B911_Rem_c + beta8[i]*nine11_BRem$B911_Rem_c + rnorm(n, mean = 0, sd = sigma[i])
str(B911Rem_rep)                        ## 100 response vectors (each of length 85)
#Check fit
bayesplot::pp_check(nine11_BRem$B911_Rem_c[,], B911Rem_rep, ppc_dens_overlay)+ xlim(c(-2.5, 2.5))
# Seems okay
