#### Mediation Analysis ####
## Load Data
Mediation_Data <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/df_soc_for_Johnny.csv", header = TRUE) #Import data for mediation analysis
DevxConf_Bel <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Deviations_Belief.csv", header = TRUE) #Import data on Deviations of Remembered Belief-related stress

## Load Contingencies and Packages
if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2, tidyr, dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR) #Load necessary rPackages
require("devtools")
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
inla.setOption(mkl=TRUE)
if (!require("INLAutils")) {install_github("timcdlucas/INLAutils"); require("INLAutils")}  ## additional plotting utilities 
if (!require("brinla")) {install_github("julianfaraway/brinla"); require("brinla")}   ## additional inla functions
if (!require("coefINLA")) {install_github("hesscl/coefINLA"); require("coefINLA")}    ## nice plots for inla models
#if (!require("rstanarm")) {install.packages("rstanarm"); require("rstanarm")} 
if (!require("INLAOutputs")) {install_github('oswaldosantos/INLAOutputs'); require("INLAOutputs")}
options(mc.cores = parallel::detectCores())

## Evaluate if Self Other predicts differences in wellbeing 
#Prepare PCA Dataset
str(Mediation_Data) #1810 participants
PCA_Both <- Mediation_Data %>% filter(!(is.na(prev_pca_score)) & !(is.na(curr_pca_score))) %>% select(prev_SubjectID_Prolific_MTurk, prev_pca_score, 
                                                                                          curr_pca_score, Q3, Q4, Q6, Q7)
names(PCA_Both)[names(PCA_Both) == "prev_SubjectID_Prolific_MTurk"] <- "Subject" 
names(PCA_Both)[names(PCA_Both) == "Q3"] <- "Current_Self" 
names(PCA_Both)[names(PCA_Both) == "Q4"] <- "MostDiff_Self" 
names(PCA_Both)[names(PCA_Both) == "Q6"] <- "Current_Other" 
names(PCA_Both)[names(PCA_Both) == "Q7"] <- "MostDiff_Other" 

#Prepare Deviations in memory dataset
Deviations_PerQ <- DevxConf_Bel %>% select(Subject, QBelief, Deviation) %>% spread(QBelief, Deviation) #726 Participants total
DevBel_1 <- Deviations_PerQ %>% filter(is.na(expectation_deviation_belief1))  %>% group_by(Subject) %>% summarise(Avg_Dev = mean(expectation_deviation_belief2 + expectation_deviation_belief3 + expectation_deviation_belief4 + expectation_deviation_belief5, na.rm = TRUE)) #21 participants do not have belief # 1
DevBel_2 <- Deviations_PerQ %>% filter(is.na(expectation_deviation_belief2)) %>% group_by(Subject) %>% summarise(Avg_Dev = mean(expectation_deviation_belief1 + expectation_deviation_belief3 + expectation_deviation_belief4 + expectation_deviation_belief5, na.rm = TRUE)) #49 Participants do not have belief # 2
DevBel_3 <- Deviations_PerQ %>% filter(is.na(expectation_deviation_belief3)) %>% group_by(Subject) %>% summarise(Avg_Dev = mean(expectation_deviation_belief1 + expectation_deviation_belief2 + expectation_deviation_belief4 + expectation_deviation_belief5, na.rm = TRUE)) #6 Participants do not have belief # 3
DevBel_4 <- Deviations_PerQ %>% filter(is.na(expectation_deviation_belief4)) %>% group_by(Subject) %>% summarise(Avg_Dev = mean(expectation_deviation_belief1 + expectation_deviation_belief2 + expectation_deviation_belief3 + expectation_deviation_belief5, na.rm = TRUE)) #2 Participants do not have belief # 4
DevBel_5 <- Deviations_PerQ %>% filter(is.na(expectation_deviation_belief5)) %>% group_by(Subject) %>% summarise(Avg_Dev = mean(expectation_deviation_belief1 + expectation_deviation_belief2 + expectation_deviation_belief3 + expectation_deviation_belief4, na.rm = TRUE)) #8 Participants do not have belief # 5

#Get rid of NA
DevBel_NAout <- na.omit(Deviations_PerQ) #657 participants
DevBel_1_NAout <- na.omit(DevBel_1) #10 Participant
DevBel_2_NAout <- na.omit(DevBel_2) #38 Participant
DevBel_3_NAout <- na.omit(DevBel_3) #2 Participant
DevBel_4_NAout <- na.omit(DevBel_4) #1 Participant
DevBel_5_NAout <- na.omit(DevBel_5) #5 Participants
# 56 participants were missing one question

#Calculate Average Belief Stress (At least 4 questions)
DevBel_NAout_Avg <- DevBel_NAout %>% group_by(Subject) %>% mutate(Avg_Dev = (expectation_deviation_belief1 + expectation_deviation_belief2 + expectation_deviation_belief3 + expectation_deviation_belief4 + expectation_deviation_belief5)/5) %>% select(Subject, Avg_Dev) %>%
  full_join(DevBel_1_NAout) %>% full_join(DevBel_2_NAout) %>% full_join(DevBel_3_NAout) %>% full_join(DevBel_4_NAout) %>% full_join(DevBel_5_NAout) #713 

## Join Data
PCADEV_Data <- PCA_Both %>% left_join(DevBel_NAout_Avg) #517 Participants with both PCA data and Deviation data

## Examine if PCA improved (Higher PCA indicated worse wellbeing)
PCADev_Delta <- PCADEV_Data %>% mutate(PCA_Delta = curr_pca_score - prev_pca_score) %>% select(Subject, PCA_Delta)
mean(PCADev_Delta$PCA_Delta)
#Wellbeing improved -.36!

## Examine Current Self-Other Difference
# Difference in current (Self-Other Dist)
Dev_Current <- PCADEV_Data %>% mutate(Current_OS_Diff = Current_Other - Current_Self) %>% select(Subject, Current_OS_Diff)
mean(Dev_Current$Current_OS_Diff)
# Generally people overestimated others current stress compared to their own (+.32)

## Examine MostDiff Self-Other Difference
Dev_MostDiff <- PCADEV_Data %>% mutate(MostDiff_OS_Diff = MostDiff_Other - MostDiff_Self) %>% select(Subject, MostDiff_OS_Diff)
mean(Dev_MostDiff$MostDiff_OS_Diff)
# Generally people overestimated others stress at the most difficult point compared to their own (+.275)

##Combine Dataframes
Cleaned_df <- PCADEV_Data %>% select(Subject, Avg_Dev) %>% full_join(PCADev_Delta) %>% full_join(Dev_Current) %>% full_join(Dev_MostDiff)


## Examine how Deviations in Memory are associated with wellbeing
inla_Dev_Wellbeing <- inla(PCA_Delta ~ Avg_Dev + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE) 
summary(inla_Dev_Wellbeing) #Summarize results
coefINLA(inla_Dev_Wellbeing) #Fixed effects

## Examine how Self/Other differences are associated with wellbeing
inla_OS_Wellbeing <- inla(PCA_Delta ~ Current_OS_Diff + MostDiff_OS_Diff  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE) 
summary(inla_OS_Wellbeing) #Summarize results
coefINLA(inla_OS_Wellbeing) #Fixed effects
#89% CI*
inla_OS_Wellbeing_89 <- inla(PCA_Delta ~ Current_OS_Diff + MostDiff_OS_Diff + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_OS_Wellbeing_89) #Summarize results
coefINLA(inla_OS_Wellbeing_89) # fixed effects

## Examine how Self/Other differences are associated with Deviations in Memory
inla_OS_DevMem <- inla(Avg_Dev ~ Current_OS_Diff + MostDiff_OS_Diff  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE) 
summary(inla_OS_DevMem) #Summarize results
coefINLA(inla_OS_DevMem) #Fixed effects

## Examine Memory Deviations and PCA together
inla_MemDevPCA_Wellbeing <- inla(PCA_Delta ~ Avg_Dev + Current_OS_Diff + MostDiff_OS_Diff  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE) 
summary(inla_MemDevPCA_Wellbeing) #Summarize results
coefINLA(inla_MemDevPCA_Wellbeing) #Fixed effects
#89% CI*
inla_MemDevPCA_Wellbeing_89 <- inla(PCA_Delta ~ Avg_Dev + Current_OS_Diff + MostDiff_OS_Diff  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_MemDevPCA_Wellbeing_89) #Summarize results
coefINLA(inla_MemDevPCA_Wellbeing_89) # fixed effects

## Rerun with average difference
Cleaned_df_avgdiff <- Cleaned_df %>% mutate(OS_Diff_Avg = (Current_OS_Diff + MostDiff_OS_Diff)/2)

## Examine how Self/Other differences are associated with wellbeing
inla_OSavg_Wellbeing <- inla(PCA_Delta ~ OS_Diff_Avg  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df_avgdiff, verbose = TRUE) 
summary(inla_OSavg_Wellbeing) #Summarize results
coefINLA(inla_OSavg_Wellbeing) #Fixed effects
#89% CI*
inla_OSavg_Wellbeing_89 <- inla(PCA_Delta ~ OS_Diff_Avg + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df_avgdiff, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_OSavg_Wellbeing_89) #Summarize results
coefINLA(inla_OSavg_Wellbeing_89) # fixed effects

## Examine how Self/Other differences are associated with Deviations in Memory
inla_OSavg_DevMem <- inla(Avg_Dev ~ OS_Diff_Avg  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df_avgdiff, verbose = TRUE) 
summary(inla_OSavg_DevMem) #Summarize results
coefINLA(inla_OSavg_DevMem) #Fixed effects

## Examine Memory Deviations and PCA together
inla_MemDevPCA_Wellbeing_avg <- inla(PCA_Delta ~ Avg_Dev + OS_Diff_Avg  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df_avgdiff, verbose = TRUE) 
summary(inla_MemDevPCA_Wellbeing_avg) #Summarize results
coefINLA(inla_MemDevPCA_Wellbeing_avg) #Fixed effects
#89% CI*
inla_MemDevPCA_Wellbeing_avg_89 <- inla(PCA_Delta ~ Avg_Dev + OS_Diff_Avg  + f(Subject, model = "iid"), family = "gaussian", data = Cleaned_df_avgdiff, verbose = TRUE, quantiles = c(0.06,0.94)) 
summary(inla_MemDevPCA_Wellbeing_avg_89) #Summarize results
coefINLA(inla_MemDevPCA_Wellbeing_avg_89) # fixed effects

