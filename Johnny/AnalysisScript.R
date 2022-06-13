#### Loading Files ####
##Load all files from RDA
load(file = "/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Covid_StressMemory.rda")


##Individually Load Files
MostDiff_Time <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Covid_MostStressPeriod.csv", header = TRUE) #Import data on most difficult time periods
Stress_OthTime <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Stress_SelfandOthers.csv", header = TRUE) #Import data on Stress to others over time
Stress_SelfTime <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Stress_Self_Time.csv", header = TRUE) #Import data on Stress to Self over time
Threat_SocDist <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Threat_SocialDistance.csv", header = TRUE) #Import data on threat to self and others
Stress_Change <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/StressChanges.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2
Stress_Change_Long <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/StressChanges_Long.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
Stress_Change_LongBel <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/StressChanges_LongBel.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
DevxConf_Bel <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Deviations_Belief.csv", header = TRUE) #Import data on Deviations of Remembered Belief-related stress
DevxConf_Exp <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Deviations_Experience.csv", header = TRUE) #Import data on Deviations of Remembered Experience-related stress
DevxConf_Type <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/Deviations_BelExp.csv", header = TRUE) #Import data on Deviations of Remembered Belief/Experience-related stress
Rem_Stress <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/CovidBox/ConstructionOfRemStress.csv", header = TRUE) #Import data on 


##Load Contingencies and Packages
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

#Load Beta_Squeeze function:
#Squeezes numbers between 0-1 (~ .0002) in order to keep the values between 0 & 1, and not at 0 or 1 for Beta family GLM Regression.
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}



#### Hardest Covid Month (N = 726) ####
##Data Preparation 
MostDiff_2020 <- MostDiff_Time %>% filter(Year == 2020)
MostDiff_2021 <- MostDiff_Time %>% filter(Year == 2021)


##Analysis
#2020
pie(table(MostDiff_2020$Month), lables = paste(round(prop.table(table(MostDiff_2020$Month)))*100)) #Pie Chart of Most Stressful Periods
prop.table(table(as.factor(MostDiff_2020$Month)))*100 #Percentages
# April (28.71%) by far the most difficult month

#2021
pie(table(MostDiff_2021$Month), lables = paste(round(prop.table(table(MostDiff_2021$Month)))*100)) #Pie Chart of Most Stressful Periods
prop.table(table(as.factor(MostDiff_2021$Month)))*100 #Percentages
# January (18.85%) & August (17.2%)  Most difficult months


#### Self/Community Diff Covid Stress over time (N = 730) ####
##Data Preparation 
Stress_Time_SelfOTher <- Stress_OthTime %>% full_join(Stress_SelfTime)
Stress_Time_SelfOTher$Subject <- as.factor(Stress_Time_SelfOTher$Subject)
Stress_Time_SelfOTher$Timepoint <- as.factor(Stress_Time_SelfOTher$Timepoint)


##Visualization
#Type (Self/Other)
Stress_Time_SelfOTher %>% ggplot(aes(x=as.factor(fct_rev(Type)), y=Rating)) + geom_violin() + ggtitle("Stress experienced by self & others") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Type")
Stress_Time_SelfOTher %>% group_by(Type) %>% summarise(avg_stress = mean(Rating)) #Type (Self/Other)

#Timepoint (Current/Future/MostDiff)
Stress_Time_SelfOTher %>% ggplot(aes(x=as.factor(Timepoint), y=Rating)) + geom_violin() + ggtitle("Stress experienced by self over time") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint")
Stress_Time_SelfOTher %>% group_by(Timepoint) %>% summarise(avg_stress = mean(Rating)) #Timepoint (Current/Future/MostDiff)

#Type (Self/Other) x Timepoint (Current/Future/MostDiff)
Stress_Time_SelfOTher %>% ggplot(aes(x=as.factor(fct_rev(Type)), y=Rating)) + geom_violin() + ggtitle("Stress experienced by self & others over time") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(.~Timepoint) + xlab("Type")
Stress_Time_SelfOTher %>% group_by(Type, Timepoint) %>% summarise(avg_stress = mean(Rating)) #Timepoint (Current/Future/MostDiff)


##Analysis
inla_Stress_SelfOther <- inla(BRat ~ Timepoint + Type + f(Subject, model = "iid"), family = "beta", data = Stress_Time_SelfOTher, verbose = TRUE) 
summary(inla_Stress_SelfOther) # Summarise results
coefINLA(inla_Stress_SelfOther) # fixed effects


#### Threat compared to Social Others (N = 730) ####
##Data Preparation 
Threat_SocDist$Scope <- factor(Threat_SocDist$Scope, levels = c("Self", "Close", "USPpl", "World"), ordered = FALSE)
Threat_SocDist$Subject <- as.factor(Threat_SocDist$Subject)


##Visualization
#Social Proximity (Self/Close/USPpl/World)
Threat_SocDist %>% ggplot(aes(x=as.factor(Scope), y=Threat)) + geom_violin() + ggtitle("Perceived Covid Threat") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Scope")
Threat_SocDist %>% group_by(Scope) %>% summarise(avg_threat = mean(Threat)) #Social Proximity (Self/Close/USPpl/World)


##Analysis
inla_Stress_SocialDist <- inla(BThreat ~ Scope + f(Subject, model = "iid"), family = "beta", data = Threat_SocDist, verbose = TRUE) 
summary(inla_Stress_SocialDist) # Summarize results
coefINLA(inla_Stress_SocialDist)      # fixed effects


#### Experience-related stress over time (N = 727) ####
##Q: In Person Visits*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_1)) + geom_violin() + ggtitle("Stress from decreased in-person visits") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_1)))

inla_ExpStress_1 <- inla(DecreasedInPersonVisits ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_1) # Summarize results
coefINLA(inla_ExpStress_1) # fixed effects
#More stressed before due to decreased in-person visits (+.3)


##Q: Decreased Virtual Contact
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_2)) + geom_violin() + ggtitle("Stress from decreased virtual contact") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_2)))

inla_ExpStress_2 <- inla(DcrsdVrtulCntct ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_2) # Summarize results
coefINLA(inla_ExpStress_2) # fixed effects
#Equally stressed due to decreased virtual contact (+.11)


##Q: Increased tension w/ people in household*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_3)) + geom_violin() + ggtitle("Stress from increased tension with people in household") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_3)))

inla_ExpStress_3 <- inla(TnsnPplHshld ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_3) # Summarize results
coefINLA(inla_ExpStress_3) # fixed effects
#More stressed before due to increased tension with people in household (+.19)


##Q: Increased tension w/ others outside of household*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_4)) + geom_violin() + ggtitle("Stress from increased tension with others outside household") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_4)))

inla_ExpStress_4 <- inla(TnsnOtsdHshld ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_4) # Summarize results
coefINLA(inla_ExpStress_4) # fixed effects
#More stressed now due to increased tension with others outside of your household (+.18)


##Q: Loss of Employment*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_5)) + geom_violin() + ggtitle("Stress from loss of employment") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_5)))

inla_ExpStress_5 <- inla(LossofEmploy ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_5) # Summarize results
coefINLA(inla_ExpStress_5) # fixed effects
#More stressed before due to loss of employment(+.39)


## Q: Problems paying for groceries*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_6)) + geom_violin() + ggtitle("Stress from problems paying for groceries") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_6)))

inla_ExpStress_6 <- inla(PrblmPay4Grcrs ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_6) # Summarize results
coefINLA(inla_ExpStress_6) # fixed effects
#More stressed before due to loss of employment(+.39)


## Q: Problems paying for bills*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_7)) + geom_violin() + ggtitle("Stress from problems paying for bills") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_7)))

inla_ExpStress_7 <- inla(Pay4Bills ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_7) # Summarize results
coefINLA(inla_ExpStress_7) # fixed effects
#More stressed before due to problems paying for bills  (+.2)


## Q: Problems accessing healthcare
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_8)) + geom_violin() + ggtitle("Stress from problems accessing healthcare") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_8)))

inla_ExpStress_8 <- inla(PrblmHlthcr ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_8) # Summarize results
coefINLA(inla_ExpStress_8) # fixed effects
#Equally stressed due to problems accessing healthcare (+.13)


## Q: Problems receiving usual paycheck*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_9)) + geom_violin() + ggtitle("Stress from problems receiving usual paycheck") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_9)))

inla_ExpStress_9 <- inla(PrblmPychk ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_9) # Summarize results
coefINLA(inla_ExpStress_9) # fixed effects
#More stressed before due to problems receiving usual paycheck (+.39)


## Q: Difficulties combining childcare and work
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_10)) + geom_violin() + ggtitle("Stress from problems combining childcare and work") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_10)))

inla_ExpStress_10 <- inla(ChldcrWrk ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_10) # Summarize results
coefINLA(inla_ExpStress_10) # fixed effects
#Equally stressed due to difficulties combining childcare and work (+.16)


## Q: Obstacles that make work more difficult
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_11)) + geom_violin() + ggtitle("Stress from obstacles that make work more difficult") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_11)))

inla_ExpStress_11 <- inla(ObstclsWrkDiff ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_11) # Summarize results
coefINLA(inla_ExpStress_11) # fixed effects
#Equally stressed due to obstacles that make work more difficult (+.14)


## Q: Increased work loads
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_12)) + geom_violin() + ggtitle("Stress from increased work loads") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_12)))

inla_ExpStress_12 <- inla(IncrsdWrkLd ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_12) # Summarize results
coefINLA(inla_ExpStress_12) # fixed effects
#Equally stressed due to increased work loads (+.07)


## Q: Working from home
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_13)) + geom_violin() + ggtitle("Stress from working from home") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_13)))

inla_ExpStress_13 <- inla(WrkFrmHm ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_13) # Summarize results
coefINLA(inla_ExpStress_13) # fixed effects
#Equally stressed due to working from home (+.16)


## Q: Decreased physical exercise*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_14)) + geom_violin() + ggtitle("Stress from decreased physical exercise") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_14)))

inla_ExpStress_14 <- inla(PhysclXrcz ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_14) # Summarize results
coefINLA(inla_ExpStress_14) # fixed effects
#More stressed before due to decreased physical exercise (+.12)


## Q: Participation in usual leisure activities*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_15)) + geom_violin() + ggtitle("Stress from decreased participation in usual leisure activities") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_15)))

inla_ExpStress_15 <- inla(DcrsdPtcptnLesre ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_15) # Summarize results
coefINLA(inla_ExpStress_15) # fixed effects
#More stressed before due to decreased participation in usual leisure activities (+.23)


## Q: Decreases in new fun activities*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_16)) + geom_violin() + ggtitle("Stress from decreased participation in new fun activities") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_16)))

inla_ExpStress_16 <- inla(LessNewActvts ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_16) # Summarize results
coefINLA(inla_ExpStress_16) # fixed effects
#More stressed before due to decreases in new fun activities (+18)


## Q: Boredom
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q12_17)) + geom_violin() + ggtitle("Stress from increased boredom") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q12_17)))

inla_ExpStress_17 <- inla(Boredom ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_17) # Summarize results
coefINLA(inla_ExpStress_17) # fixed effects
#Equally stressed due to boredom (+18)


## Overall change in experience-related stress
#Data Preparation
Stress_Change_Long$Subject <- as.factor(Stress_Change_Long$Subject)
Stress_Change_Long$Question <- as.factor(Stress_Change_Long$Question)

#Preparation for INLA Random Slope
nid <- length(Stress_Change_Long$Subject)       ## number of persons in the sample
nid
int_id <- as.numeric(Stress_Change_Long$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

inla_ExpStress_Ovrl <- inla(Rating ~ TP + f(int_id, model = "iid2d", n = 2*nid) + f(slope_id, Question, copy = "int_id"), family = "beta", data = Stress_Change_Long, verbose = TRUE) 
summary(inla_ExpStress_Ovrl) # Summarize results
coefINLA(inla_ExpStress_Ovrl) # fixed effects
#Overall stress was greater in the past

#### Belief-related stress over time (N = 727) ####
## Q: Government not taking appropriate action to combat COVID*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q13_1)) + geom_violin() + ggtitle("Stress from government not taking appropriate action to combat COVID") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q13_1)))

inla_BelStress_1 <- inla(BlvngGvmntNotCmbtCOVID ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_1) # Summarize results
coefINLA(inla_ExpStress_1) # fixed effects
#More stressed before due to decreased in-person visits (+.83)


## Q: Believing criminality rates have increased
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q13_2)) + geom_violin() + ggtitle("Stress from believing criminality rates have increased") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q13_2)))

inla_BelStress_2 <- inla(CrmnltyIncrsd ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_2) # Summarize results
coefINLA(inla_ExpStress_2) # fixed effects
#Equally stressed from believing criminality rates have increased (+.06)


## Q: Believing there is currently an economic crisis*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q13_3)) + geom_violin() + ggtitle("Stress from believing there is currently an economic crisis") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q13_3)))

inla_BelStress_3 <- inla(Blvng_EcnmcCrsis ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_3) # Summarize results
coefINLA(inla_ExpStress_3) # fixed effects
#More stressed before from believing there is currently an economic crisis (+.39)


## Q: Believing other people are not following public health guidelines*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q13_4)) + geom_violin() + ggtitle("Stress from believing other people are not following public health guidelines") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q13_4)))

inla_BelStress_4 <- inla(OthrNotFollowingPblcHlthGuidlns ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_4) # Summarize results
coefINLA(inla_ExpStress_4) # fixed effects
#More stressed before from believing other people are not following public health guidelines (+.19)


## Q: Believing life as we know it is not the same*
Stress_Change %>% ggplot(aes(x=as.factor(fct_rev(TP)), y= Q13_5)) + geom_violin() + ggtitle("Stress from believing life as we know it is not the same") + 
  stat_summary(fun="mean", color="red", shape=15) + theme(plot.title = element_text(hjust = 0.5)) + xlab("Timepoint") + ylab("Rating")
Stress_Change %>% group_by(TP) %>% summarise(avg_stress = mean(na.omit(Q13_5)))

inla_BelStress_5 <- inla(BelLifeIsNotSame ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change, verbose = TRUE) 
summary(inla_ExpStress_5) # Summarize results
coefINLA(inla_ExpStress_5) # fixed effects
#More stressed before  from believing life as we know it is not the same (+.19)

## Overall change in belief-related stress
#Data Preparation
Stress_Change_LongBel$Subject <- as.factor(Stress_Change_LongBel$Subject)
Stress_Change_LongBel$Question <- as.factor(Stress_Change_LongBel$Question)

#Preparation for INLA Random Slope
nid <- length(Stress_Change_LongBel$Subject)       ## number of persons in the sample
nid
int_id <- as.numeric(Stress_Change_LongBel$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

inla_BelStress_Ovrl <- inla(Rating ~ TP + f(int_id, model = "iid2d", n = 2*nid) + f(slope_id, Question, copy = "int_id"), family = "beta", data = Stress_Change_LongBel, verbose = TRUE) 
summary(inla_BelStress_Ovrl) # Summarize results
coefINLA(inla_BelStress_Ovrl) # fixed effects
#Overall Belief-related stress was greater in the past

#### Accuracy of remembered belief-related stress (N = 726) ####

## Q: Government not taking appropriate action to combat COVID* (N = 705)
BelDev_1 <- DevxConf_Bel %>% filter(QBelief == "expectation_deviation_belief1") 
BelDev_1 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(BelDev_1$Deviation)) #BF10 = 1.48e66


## Q: Believing criminality rates have increased* (N = 677)
BelDev_2 <- DevxConf_Bel %>% filter(QBelief == "expectation_deviation_belief2") 
BelDev_2 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(BelDev_2$Deviation)) #BF10 = 1.01e6


## Q: Believing there is currently an economic crisis* (N = 720)
BelDev_3 <- DevxConf_Bel %>% filter(QBelief == "expectation_deviation_belief3") 
BelDev_3 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(BelDev_3$Deviation)) #BF10 = 1.97e30


## Q: Believing other people are not following public health guidelines* (N = 724)
BelDev_4 <- DevxConf_Bel %>% filter(QBelief == "expectation_deviation_belief4") 
BelDev_4 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(BelDev_4$Deviation)) #BF10 = 1.63e25


## Q: Believing life as we know it is not the same* (N = 718)
BelDev_5 <- DevxConf_Bel %>% filter(QBelief == "expectation_deviation_belief5") 
BelDev_5 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(BelDev_5$Deviation)) #BF10 = 5.15e34

#For all questions participants were inaccurate in remembering their belief-related stress at timepoint 1

inla_BelDev_Ovrl <- inla(BDev ~ f(Subject, model = "iid") + f(QBelief, model = "iid"), family = "beta", data = DevxConf_Bel, verbose = TRUE) 
summary(inla_BelDev_Ovrl) # Summarize results
coefINLA(inla_BelDev_Ovrl) # fixed effects
#.514 - .613
#Belief-related stress at timepoint 1 was generally overestimated


###Deviations x Conf
##Data Preparation
DevxConf_Bel$Subject <- as.factor(DevxConf_Bel$Subject)
DevxConf_Bel$QBelief <- as.factor(DevxConf_Bel$QBelief)
DevxConf_Bel$Conf_Rat <- factor(DevxConf_Bel$Conf_Rat, levels = c("Low", "Med", "High"))

#Preparation for INLA Random Slope
nid <- length(table(DevxConf_Bel$Subject))       ## number of persons in the sample
nid
int_id <- as.numeric(DevxConf_Bel$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

##Analysis
inla_Dev_Bel_Conf_int <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid") + f(QBelief, model = "iid"), family = "beta", data = DevxConf_Bel, verbose = TRUE) 
summary(inla_Dev_Bel_Conf_int) #Summarize results
coefINLA(inla_Dev_Bel_Conf_int) # fixed effects

#89% CI*
inla_Dev_Bel_Conf_int_89 <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid") + f(QBelief, model = "iid"), family = "beta", data = DevxConf_Bel, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_Dev_Bel_Conf_int_89) #Summarize results
coefINLA(inla_Dev_Bel_Conf_int_89) # fixed effects



#### Accuracy of remembered experience-related stress (N = 723) ####

## Q: In Person Visits* (N = 690)
ExpDev_1 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience1") 
ExpDev_1 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_1$Deviation)) #BF10 = 9.06e42


## Q: Decreased Virtual Contact* (N = 622)
ExpDev_2 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience2") 
ExpDev_2 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_2$Deviation)) #BF10 = 1.5e23


## Q: Increased tension w/ people in household* (N = 619)
ExpDev_3 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience3") 
ExpDev_3 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_3$Deviation)) #BF10 = 1.04e20


## Q: Increased tension w/ others outside of household* (N = 652)
ExpDev_4 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience4") 
ExpDev_4 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_4$Deviation)) #BF10 = 1.08e7


## Q: Loss of Employment* (N = 421)
ExpDev_5 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience5") 
ExpDev_5 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_5$Deviation)) #BF10 = 2.82e5


## Q: Loss of Employment* (N = 421)
ExpDev_5 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience5") 
ExpDev_5 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_5$Deviation)) #BF10 = 2.82e5


## Q: Problems paying for groceries* (N = 617)
ExpDev_6 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience6") 
ExpDev_6 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_6$Deviation)) #BF10 = 1.51e51


## Q: Problems paying for bills* (N = 617)
ExpDev_7 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience7") 
ExpDev_7 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_7$Deviation)) #BF10 = 3.42e3 


## Q: Problems accessing healthcare* (N = 614)
ExpDev_8 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience8") 
ExpDev_8 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_8$Deviation)) #BF10 = 8.64e5


## Q: Problems receiving usual paycheck* (N = 508)
ExpDev_9 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience9") 
ExpDev_9 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_9$Deviation)) #BF10 = 9.36e3


## Q: Difficulties combining childcare and work* (N = 317)
ExpDev_10 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience10") 
ExpDev_10 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_10$Deviation)) #BF10 = 5.81e4


## Q: Obstacles that make work more difficult* (N = 576)
ExpDev_11 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience11") 
ExpDev_11 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_11$Deviation)) #BF10 = 4.76e12


## Q: Increased work loads* (N = 564)
ExpDev_12 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience12") 
ExpDev_12 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_12$Deviation)) #BF10 = 411


## Q: Working from home (N = 527)
ExpDev_13 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience13") 
ExpDev_13 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_13$Deviation)) #BF10 = 6.35e6


## Q: Decreased physical exercise (N = 655)
ExpDev_14 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience14") 
ExpDev_14 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_14$Deviation)) #BF10 = 8.54


## Q: Participation in usual leisure activities* (N = 685)
ExpDev_15 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience15") 
ExpDev_15 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_15$Deviation)) #BF10 = 6.04e15


## Q: Decreases in new fun activities* (N = 693)
ExpDev_16 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience16") 
ExpDev_16 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_16$Deviation)) #BF10 = 5.70e13


## Q: Boredom* (N = 693)
ExpDev_17 <- DevxConf_Exp %>% filter(QExp == "expectation_deviation_experience17") 
ExpDev_17 %>% summarise(avg_Dev = mean(Deviation))
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(ExpDev_17$Deviation)) #BF10 = 5.41e4


###Deviations x Conf
#Data Preparation
DevxConf_Exp$Subject <- as.factor(DevxConf_Exp$Subject)
DevxConf_Exp$QExp <- as.factor(DevxConf_Exp$QExp)
DevxConf_Exp$Conf_Rat <- factor(DevxConf_Exp$Conf_Rat, levels = c("Low", "Med", "High"))

#Preparation for INLA Random Slope
nid <- length(table(DevxConf_Exp$Subject))       ## number of persons in the sample
nid
int_id <- as.numeric(DevxConf_Exp$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

#Analysis
inla_Dev_Exp_Conf_int <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid") + f(QExp, model = "iid"), family = "beta", data = DevxConf_Exp, verbose = TRUE) 
summary(inla_Dev_Exp_Conf_int) #Summarize results
coefINLA(inla_Dev_Exp_Conf_int) #Fixed effects



#### Differences in Remembered Stress by type:Exp/Bel (N = 728) ####
#Data Preparation
DevxConf_Type$Subject <- as.factor(DevxConf_Type$Subject)
DevxConf_Type$Q <- as.factor(DevxConf_Type$Q)
DevxConf_Type$Mem <- as.factor(DevxConf_Type$Mem)
DevxConf_Type$Conf_Rat <- factor(DevxConf_Type$Conf_Rat, levels = c("Low", "Med", "High"))

#Preparation for INLA Random Slope
nid <- length(table(DevxConf_Type$Subject))       ## number of persons in the sample
nid
int_id <- as.numeric(DevxConf_Type$Subject)        ## numeric representation on the random effect (intercept ID)
int_id
slope_id <- int_id + nid
slope_id

#Analysis
DevxConf_Type %>% group_by(Mem) %>% summarise(avg_Dev = mean(Deviation))

inla_Dev_Type <- inla(BDev ~ Mem + f(int_id, model = "iid2d", n = 2*nid) + f(slope_id, Q, copy = "int_id"), family = "beta", data = DevxConf_Type, verbose = TRUE) 
summary(inla_Dev_Type) #Summarize results
coefINLA(inla_Dev_Type) #Fixed effects

# Participants tended to overestimate belief-related stress more than experience-related stress



#### Construction of Remembered Stress (N = 730) ####
#Data Preparation
Rem_Stress$Subject <- as.factor(Rem_Stress$Subject)

##Analysis
#Experience-Related
RemStress_Exp <- inla(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2 + f(Subject, model = "iid"), family = "beta", data = Rem_Stress, verbose = TRUE) 
summary(RemStress_Exp) #Summarize results
coefINLA(RemStress_Exp) #Fixed effects

#Belief-Related
RemStress_Bel <- inla(BBelief_Rem ~ Belief_Exp1 + Belief_Exp2 + f(Subject, model = "iid"), family = "beta", data = Rem_Stress, verbose = TRUE) 
summary(RemStress_Bel) #Summarize results
coefINLA(RemStress_Bel) #Fixed effects


