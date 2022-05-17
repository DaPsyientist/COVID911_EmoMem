#### Loading Files ####
##Load all files from RDA
load(file = "Johnny/Covid_StressMemory.rda")


##Individually Load Files
MostDiff_Time <- read.csv("Johnny/Covid_MostStressPeriod.csv", header = TRUE) #Import data on most difficult time periods
Stress_OthTime <- read.csv("Johnny/Stress_SelfandOthers.csv", header = TRUE) #Import data on Stress to others over time
Stress_SelfTime <- read.csv("Johnny/Stress_Self_Time.csv", header = TRUE) #Import data on Stress to Self over time
Threat_SocDist <- read.csv("Johnny/Threat_SocialDistance.csv", header = TRUE) #Import data on threat to self and others
Stress_Change <- read.csv("Johnny/StressChanges.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2
Stress_Change_Ovrl <- read.csv("Johnny/Stress_Change_Overall.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
Stress_Change_OvrlBel <- read.csv("Johnny/Stress_Change_Overall_bel.csv", header = TRUE) #Import data on Changes in stress between TP1 and TP2, Long Format
DevxConf_Bel <- read.csv("Johnny/Deviations_Belief.csv", header = TRUE) #Import data on Deviations of Remembered Belief-related stress
DevxConf_Bel_Ovrl <- read.csv("Johnny/Deviations_Belief_Overall.csv", header = TRUE) #Import data on Overall Deviations of Remembered Experience-related stress
DevxConf_Exp <- read.csv("Johnny/Deviations_Experience.csv", header = TRUE) #Import data on Deviations of Remembered Experience-related stress
DevxConf_Exp_Ovrl <- read.csv("Johnny/Deviations_Experience_Overall.csv", header = TRUE) #Import data on Overall Deviations of Remembered Experience-related stress
Dev_BelExp_Ovrl <- read.csv("Johnny/Deviations_BelExp_Ovrl.csv", header = TRUE) #Import data on Deviations of Remembered Belief/Experience-related stress
RemBel_Stress <- read.csv("Johnny/RemBel_Stress.csv", header = TRUE) #Import data on 
RemExp_Stress <- read.csv("Johnny/RemExp_Stress.csv", header = TRUE) #Import data on 

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
#Violin Plots
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
Stress_Time_SelfOTher %>% group_by(Type, Timepoint) %>% summarise(avg_stress = mean(Rating), se = sd(Rating)/sqrt(29)) #Timepoint (Current/Future/MostDiff)

#Barchart
Bar_Stress_Time_SelfOTher<- Stress_Time_SelfOTher %>% group_by(Type, Timepoint) %>% summarise(avg_stress = mean(Rating), se = sd(Rating)/sqrt(730)) #Timepoint (Current/Future/MostDiff)
ggplot(Bar_Stress_Time_SelfOTher, aes(x=Timepoint, y=avg_stress, fill=Type)) + 
  scale_fill_manual(values=c("#CCCCCC","Black")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_stress-se, ymax=avg_stress+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("stress 'experienced' by self and others over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_stress, 2)), position=position_dodge(width=0.8), vjust=3, color = "white")

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
#Barchart
Bar_Threat_SocialProx <- Threat_SocDist %>% group_by(Scope) %>% summarise(avg_threat = mean(Threat), se = sd(Threat)/sqrt(730)) #Timepoint (Current/Future/MostDiff)
Bar_Threat_SocialProx$Scope <- factor(Bar_Threat_SocialProx$Scope, levels = c("Self", "Close", "USPpl", "World"))
ggplot(Bar_Threat_SocialProx, aes(x=Scope, y=avg_threat, fill=Scope)) + 
  scale_fill_manual(values=c("DarkGreen","Gold", "Orange", "Red")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_threat-se, ymax=avg_threat+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("perceived covid threat level by social distance") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_threat, 2)), position=position_dodge(width=0.8), vjust=3)



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
Stress_Change_Ovrl$Subject <- as.factor(Stress_Change_Ovrl$Subject)
Stress_Change_Ovrl$TP <- as.factor(Stress_Change_Ovrl$TP)
Stress_Change_Ovrl <- Stress_Change_Ovrl %>% mutate(Rating = b_Rat )

#Visualization
#Barchart
Stress_Change_Ovrl <- Stress_Change_Ovrl %>% group_by(TP) %>% summarise(avg_rating = mean(Rating), se = sd(Rating)/sqrt(713)) #Timepoint (Current/Future/MostDiff)
Stress_Change_Ovrl$TP <- factor(Stress_Change_Ovrl$TP, levels = c("Prev", "Now"))
ggplot(Stress_Change_Ovrl, aes(x=TP, y=avg_rating, fill = TP)) + 
  scale_fill_manual(values=c("Grey", "Black")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Experience-based stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.8), vjust=3, color = "white")

inla_ExpStress_Ovrl <- inla(b_Rat ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change_Ovrl, verbose = TRUE, weights = num_weights) 
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
Stress_Change_OvrlBel$Subject <- as.factor(Stress_Change_OvrlBel$Subject)
Stress_Change_OvrlBel$TP <- as.factor(Stress_Change_OvrlBel$TP)
Stress_Change_OvrlBel <- Stress_Change_OvrlBel %>% mutate(Rating = b_Rat * 5 )

#Visualization
Bar_Stress_Change_OvrlBel <- Stress_Change_OvrlBel %>% group_by(TP) %>% summarise(avg_rating = mean(Rating), se = sd(Rating)/sqrt(717)) #Timepoint (Current/Future/MostDiff)
Bar_Stress_Change_OvrlBel$TP <- factor(Stress_Change_OvrlBel$TP, levels = c("Prev", "Now"))
ggplot(Bar_Stress_Change_OvrlBel, aes(x=TP, y=avg_rating, fill = TP)) + 
  scale_fill_manual(values=c("Grey", "Black")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Belief-related stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.8), vjust=3, color = "white")



inla_BelStress_Ovrl <- inla(b_Rat ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change_OvrlBel, verbose = TRUE, weights = num_weights) 
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
DevxConf_Bel_Ovrl$Subject <- as.factor(DevxConf_Bel_Ovrl$Subject)
DevxConf_Bel_Ovrl$Conf_Rat <- factor(DevxConf_Bel_Ovrl$Conf_Rat, levels = c("Low", "Med", "High"))

#Visualization
Bar_DevxConf_Bel_Ovrl <- DevxConf_Bel_Ovrl %>% group_by(Conf_Rat) %>% summarise(avg_devi = mean(avg_dev), se = sd(avg_dev, na.rm = TRUE)/sqrt(726)) #Timepoint (Current/Future/MostDiff)
Bar_DevxConf_Bel_Ovrl$Conf_Rat <- factor(Bar_DevxConf_Bel_Ovrl$Conf_Rat, levels = c("Low", "Med", "High"))
ggplot(Bar_DevxConf_Bel_Ovrl, aes(x=Conf_Rat, y=avg_devi, fill = Conf_Rat)) + 
  scale_fill_manual(values=c("Gold", "Orange", "Red")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_devi-se, ymax=avg_devi+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Deviations in remembered belief-related stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_devi, 2)), position=position_dodge(width=0.8), vjust=3, color = "black")


##Analysis
inla_Dev_Bel_Conf_Ovrl <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid"), family = "beta", data = DevxConf_Bel_Ovrl, verbose = TRUE) 
summary(inla_Dev_Bel_Conf_Ovrl) #Summarize results
coefINLA(inla_Dev_Bel_Conf_Ovrl) # fixed effects

#89% CI*
inla_Dev_Bel_Conf_Ovrl_89 <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid"), family = "beta", data = DevxConf_Bel_Ovrl, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_Dev_Bel_Conf_Ovrl_89) #Summarize results
coefINLA(inla_Dev_Bel_Conf_Ovrl_89) # fixed effects


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
DevxConf_Exp_Ovrl$Subject <- as.factor(DevxConf_Exp_Ovrl$Subject)
DevxConf_Exp_Ovrl$Conf_Rat <- as.factor(DevxConf_Exp_Ovrl$Conf_Rat)
DevxConf_Exp_Ovrl$Conf_Rat <- relevel(DevxConf_Exp_Ovrl$Conf_Rat, "Low")

#Visualization
Bar_DevxConf_Exp_Ovrl <- DevxConf_Exp_Ovrl %>% group_by(Conf_Rat) %>% summarise(avg_devi = mean(avg_dev), se = sd(avg_dev, na.rm = TRUE)/sqrt(723)) #Timepoint (Current/Future/MostDiff)
Bar_DevxConf_Exp_Ovrl$Conf_Rat <- factor(Bar_DevxConf_Exp_Ovrl$Conf_Rat, levels = c("Low", "Med", "High"))
ggplot(Bar_DevxConf_Exp_Ovrl, aes(x=Conf_Rat, y=avg_devi, fill = Conf_Rat)) + 
  scale_fill_manual(values=c("Gold", "Orange", "Red")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_devi-se, ymax=avg_devi+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Deviations in remembered experience-based stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_devi, 2)), position=position_dodge(width=0.8), vjust=3, color = "black")


#Analysis
inla_Dev_Exp_Conf_Ovrl <- inla(BDev ~ Conf_Rat + f(Subject, model = "iid"), family = "beta", data = DevxConf_Exp_Ovrl, verbose = TRUE, weights = num_weights) 
summary(inla_Dev_Exp_Conf_Ovrl) #Summarize results
coefINLA(inla_Dev_Exp_Conf_Ovrl) #Fixed effects


#### Differences in Remembered Stress by type:Exp/Bel (N = 728) ####
#Data Preparation
Dev_BelExp_Ovrl$Subject <- as.factor(Dev_BelExp_Ovrl$Subject)
Dev_BelExp_Ovrl$Mem <- as.factor(Dev_BelExp_Ovrl$Mem)
Dev_BelExp_Ovrl$Conf_Rat <- factor(Dev_BelExp_Ovrl$Conf_Rat, levels = c("Low", "Med", "High"))

#Analysis
Dev_BelExp_Ovrl %>% group_by(Mem) %>% summarise(avg_Dev = mean(avg_dev))
Dev_BelExp_Ovrl %>% group_by(Mem, Conf_Rat) %>% summarise(avg_Dev = mean(avg_dev))

#Model Selection

inla_Dev_Type <- inla(BDev ~ Mem  + f(Subject, model = "iid"), family = "beta", data = Dev_BelExp_Ovrl, control.compute = list(waic=TRUE), verbose = TRUE, weights = num_weights) 
inla_Dev_Type_waic <- inla_Dev_Type$waic$waic
inla_Dev_Type_MLK <- as.numeric(inla_Dev_Type$mlik[,1][2])

inla_Dev_TypeConf <- inla(BDev ~ Mem  + Conf_Rat + f(Subject, model = "iid"), family = "beta", data = Dev_BelExp_Ovrl, control.compute = list(waic=TRUE), verbose = TRUE, weights = num_weights) 
inla_Dev_TypeConf_waic <- inla_Dev_TypeConf$waic$waic
inla_Dev_TypeConf_MLK <- as.numeric(inla_Dev_TypeConf$mlik[,1][2])

inla_Dev_TypeXConf <- inla(BDev ~ Mem  * Conf_Rat + f(Subject, model = "iid"), family = "beta", data = Dev_BelExp_Ovrl, control.compute = list(waic=TRUE), verbose = TRUE, weights = num_weights) 
inla_Dev_TypeXConf_waic <- inla_Dev_TypeXConf$waic$waic
inla_Dev_TypeXConf_MLK <- as.numeric(inla_Dev_TypeXConf$mlik[,1][2])

#Model Comparison
inla_Dev_TypeXConf_MLK/inla_Dev_Type_MLK #Unequivical data for either model
inla_Dev_TypeXConf_MLK/inla_Dev_TypeConf_MLK #Unequivical data for either model
inla_Dev_Type_waic - inla_Dev_TypeConf_waic #WAIC supports non-interaction model
inla_Dev_Type_waic - inla_Dev_TypeXConf_waic #WAIC supports interaction model

#Interpretation of best model (int)
summary(inla_Dev_TypeXConf) # Summarise results
coefINLA(inla_Dev_TypeXConf) # Significant interaction
# Participants tended to overestimate belief-related stress more than experience-related stress

#### Construction of Remembered Stress (N = 730) ####
#Data Preparation
RemBel_Stress$Subject <- as.factor(RemBel_Stress$Subject)
RemExp_Stress$Subject <- as.factor(RemExp_Stress$Subject)
RemBel_Stress$num_weights[is.na(RemBel_Stress$num_weights)] <- 0
RemExp_Stress$num_weights[is.na(RemExp_Stress$num_weights)] <- 0

##Analysis
#Experience-Related
RemStress_Exp <- inla(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2, family = "beta", data = RemExp_Stress, verbose = TRUE, weights = num_weights) 
summary(RemStress_Exp) #Summarize results
coefINLA(RemStress_Exp) #Fixed effects

#Belief-Related
RemStress_Bel <- inla(BBelief_Rem ~ Belief_Exp1 + Belief_Exp2 , family = "beta", data = RemBel_Stress, verbose = TRUE, weights = num_weights) 
summary(RemStress_Bel) #Summarize results
coefINLA(RemStress_Bel) #Fixed effects


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

df.all <- read.csv('df_soc_for_Johnny.csv') %>% rename(Subject = prev_SubjectID_Prolific_MTurk) %>% mutate(Subject = factor(Subject))
df.pca <- read.csv('Haoxue/data/df.pca.csv') %>% mutate(Subject = factor(Subject))
df.pca.long <- read.csv('Haoxue/data/df.pca.long.csv') %>% mutate(Subject = factor(Subject))
df.pca.long.all <- read.csv('Haoxue/data/df.pca.long.all.csv') %>% mutate(Subject = factor(Subject))

DeltaPCA <- inla(NA_mean_expectation_deviation_experience_squeeze ~ NA_mean_prev_experience + delta_pca_score + delta_soc + f(Subject, model = "iid"),
                 family = "beta", data = df.pca, verbose = TRUE)
summary(DeltaPCA)
coefINLA(DeltaPCA)

DeltaPCA_curr <- inla(NA_mean_expectation_deviation_experience_squeeze ~ NA_mean_curr_experience + NA_mean_prev_experience + delta_pca_score + delta_soc + f(Subject, model = "iid"),
                 family = "beta", data = df.pca, verbose = TRUE)
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
                      family = 'beta', data = df.pca.long, verbose = TRUE) 

summary(DeltaPCA_long) #Summarize results
coefINLA(DeltaPCA_long) #Fixed effects

DeltaPCA_long_curr <- inla(deviation_squeeze ~ T2_experienced + delta_pca_score + delta_soc + 
                             f(int_id, model = "iid2d", n = 2*nid)  + 
                             f(slope_id, item_num, copy = "int_id"), 
                           family = 'beta', data = df.pca.long, verbose = TRUE) 

summary(DeltaPCA_long_curr) #Summarize results
coefINLA(DeltaPCA_long_curr) #Fixed effects

#### attention
attention_long <- inla(deviation_squeeze ~ T1_experienced + 
                         media_last_month + media_peak_month + 
                         avoid_last_month + avoid_peak_month +
                         f(int_id, model = "iid2d", n = 2*nid)  + 
                         f(slope_id, item_num, copy = "int_id"), 
                       family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(attention_long) # avoid_peak_month, media_last_month, media_peak_month
coefINLA(attention_long)

attention_long_curr <- inla(deviation_squeeze ~ T2_experienced + 
                         media_last_month + media_peak_month + 
                         avoid_last_month + avoid_peak_month +
                           f(int_id, model = "iid2d", n = 2*nid)  + 
                           f(slope_id, item_num, copy = "int_id"), 
                         family = 'beta', data = df.pca.long, verbose = TRUE) 
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
                       family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(relevance_long)
coefINLA(relevance_long)

relevance_long_curr <- inla(deviation_squeeze ~ T2_experienced + 
                         add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                         personal_loss + financial_loss_cont + relocate + job_change + 
                         communicate_peak_month + communicate_last_month +
                           f(int_id, model = "iid2d", n = 2*nid)  + 
                           f(slope_id, item_num, copy = "int_id"), 
                         family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(relevance_long_curr)
coefINLA(relevance_long_curr)   

#### layperson theory
####### threat - DV: memory deviation
threat_long <- inla(deviation_squeeze ~ T1_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(int_id, model = "iid2d", n = 2*nid)  + 
                      f(slope_id, item_num, copy = "int_id"), 
                    family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(threat_long)
coefINLA(threat_long)

threat_long_curr <- inla(deviation_squeeze ~ T2_experienced + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(int_id, model = "iid2d", n = 2*nid)  + 
                      f(slope_id, item_num, copy = "int_id"), 
                    family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(threat_long_curr)
coefINLA(threat_long_curr)

####### threat - DV: pca
threat_curr_pca <- inla(curr_pca_score_squeeze ~ NA_mean_curr_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid"), family = 'beta', data = df.pca, verbose = TRUE) 
summary(threat_curr_pca)
coefINLA(threat_curr_pca)

threat_delta_pca <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(threat_delta_pca)
coefINLA(threat_delta_pca)

threat_delta_pca2 <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                            Q10_1 + Q10_2 + Q10_3 + Q10_4 + 
                           f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(threat_delta_pca2)
coefINLA(threat_delta_pca2)
# Q10_1: the less you think covid is a threat to you, the better you are doing now in terms of emotion well-being (more improvement)
# Q10_4: the larger the differnece between covid being a threat to you vs. to people across the world, the better you are doing now in terms of emotion well-being (more improvmenet)

####### threat - DV: soc
threat_delta_soc <- inla(delta_soc_squeeze ~  prev_soc_mean + NA_mean_expectation_deviation_experience + Q10_1 + threat_delta_close + threat_delta_us + threat_delta_world+
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(threat_delta_soc)
coefINLA(threat_delta_soc)

####### temporal stress rating - DV: pca
# for curr_pca, we only look at Q3 and stress_delta_curr
layperson_curr_pca <- inla(curr_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                             Q3 + stress_delta_curr+ 
                                             f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_curr_pca)
coefINLA(layperson_curr_pca)

layperson_delta_pca_delta <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                              Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                              f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_delta_pca_delta)
coefINLA(layperson_delta_pca_delta)

layperson_delta_pca_origin <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                              Q3 + Q6 + Q4 + Q7 +  
                      f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_delta_pca)
coefINLA(layperson_delta_pca) # suspecting some collinearity is reducing the effect of self questions (see the change of Q4)

layperson_delta_pca_delta_future <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                    Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff + Q5 + stress_delta_future +
                                    f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_delta_pca_delta_future)
coefINLA(layperson_delta_pca_delta_future)

layperson_delta_pca_origin_future <- inla(delta_pca_score_squeeze ~  prev_pca_score + NA_mean_expectation_deviation_experience + 
                                     Q3 + Q6 + Q4 + Q7 +  Q5 + Q8 +
                                     f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_delta_pca_origin_future)
coefINLA(layperson_delta_pca_origin_future)
# ^ok I think it does not make too much sense to include future then..

layperson_delta_soc <- inla(delta_soc_squeeze ~  prev_soc_mean + NA_mean_expectation_deviation_experience + 
                         Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                         f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(layperson_delta_soc)
coefINLA(layperson_delta_soc)
# no speicifc relationship is find between soc well being and our other measurement here

####### support the avg argument: curr experienced stress reflects both the stress experiencing themselves & other ppl
t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                              Q3 + stress_delta_curr  +  
                              f(int_id, model = "iid2d", n = 2*nid)  + 
                              f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(t2_experienced_long)
coefINLA(t2_experienced_long)

t2_experienced <- inla(NA_mean_curr_experience_squeeze ~  NA_mean_prev_experience + 
                Q3 + stress_delta_curr + Q4 + stress_delta_mostdiff +  
                f(Subject, model = "iid") , family = 'beta', data = df.pca, verbose = TRUE) 
summary(t2_experienced)
coefINLA(t2_experienced) # less sensitive than t2_experienced_long (havent tested this for 89)

threa_t2_experienced_long <- inla(T2_experienced_squeeze ~  T1_experienced + 
                              Q10_1 + Q10_2 + Q10_3 + Q10_4+
                              f(int_id, model = "iid2d", n = 2*nid)  + 
                              f(slope_id, item_num, copy = "int_id"),  family = 'beta', data = df.pca.long, verbose = TRUE) 
summary(threa_t2_experienced_long)
coefINLA(threa_t2_experienced_long)

with(df.pca, cor.test(Q10_1, Q))
a <- df.pca %>% select(starts_with('Q10_'), Q3:Q8, starts_with('threat_delta'), starts_with('stress_delta')) %>% as.matrix %>% rcorr()
a$r %>% round(2)
a$P %>% round(2) # Q6-Q8 all correlates most highly with Q10_2 (but how to test this?) caveat being stress_delta_curr correlates highest with threat_delta_us and threat_delta_world, seems to be in contradiction with previous results.
# the underlying q here is: 'typical person' in your community, are you thinking closed others/ppl in us/ppl across the world?
# or maybe these two questions are incomparable, esp we ask for threat perception and stress rating separately.
