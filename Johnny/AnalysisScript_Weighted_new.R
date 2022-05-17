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
## Overall change in experience-related stress
#Data Preparation
Stress_Change_Ovrl$Subject <- as.factor(Stress_Change_Ovrl$Subject)
Stress_Change_Ovrl$TP <- as.factor(Stress_Change_Ovrl$TP)
Stress_Change_Ovrl <- Stress_Change_Ovrl %>% mutate(Rating = b_Rat *5)

#Visualization
#Line chart - Item Level
LineStress_Exp <- Stress_Change %>% dplyr::select(Subject, TP, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, 
                                                                     Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, Q12_14, Q12_15, Q12_16, Q12_17) %>% 
  gather("Q", "Rating", 3:19)
LineStress_Exp$Q <- dplyr::recode(LineStress_Exp$Q, "Q12_1" = 'Decreased_In_Person_Visits', "Q12_2" ="Decreased_Virtual_Contact", 
                                                 "Q12_3" ="Tension_wPpl_inHouse", "Q12_4" ="Tension_wPpl_Outside_House", "Q12_5" ="Loss_of_Employment",
                                                 "Q12_6" ="Problems_Paying_4_Groceries", "Q12_7" ="Problems_Paying_4_Bills", "Q12_8" ="Problem_Accessing_Healthcare",
                                                 "Q12_9" ="Problem_w_Usual_Paycheck", "Q12_10" ="diff_combining_childcare_w_work", "Q12_11" ="obstacles_make_work_more_diff",
                                                 "Q12_12" ="increased_work_load", "Q12_13" ="working_from_home", "Q12_14" ="decreased_physical_exercise" , 
                                                 "Q12_15" ="decreased_partic_leisure_activies", "Q12_16" ="decrease_new_fun_activities", "Q12_17" ="boredom")
LineStress_Exp <- LineStress_Exp %>% group_by(TP, Q) %>% summarize(avg_rating = mean(Rating, na.rm = TRUE), se = sd(Rating, na.rm= TRUE)/sqrt(730))
LineStress_Exp$TP <- factor(LineStress_Exp$TP, levels = c("Prev", "Now"))
ggplot(LineStress_Exp, aes(x=TP, y=avg_rating, group=1, color = TP)) + 
  scale_color_manual(values=c("Black", "blue")) +
  theme_bw() +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Experience-based stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=3, color = "black")+ylim(c(0,3)) + facet_wrap(.~Q, nrow=3)+
  xlab("Timepoint") + ylab("Average Rating")
ggplot(LineStress_Exp, aes(x=fct_rev(Timepoint), y=avg_rating, fill = Timepoint)) + 
  scale_fill_manual(labels = c("T1 Experience", "Now"), values=c("black", "grey")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity")+
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.text.x = element_text(face="bold",size=0),axis.text.y = element_text(face="bold", size=21), legend.text = element_text(size=10), legend.title = element_text(size=15)) + 
  geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=3, color = "white", size = 9) +
  xlab("") + ylab("") +ylim(c(0,2.5))


#Barchart - Overall
Bar_Stress_Change_Ovrl <- Stress_Change_Ovrl %>% group_by(TP) %>% summarise(avg_rating = mean(Rating), se = sd(Rating)/sqrt(713)) #Timepoint (Current/Future/MostDiff)
Bar_Stress_Change_Ovrl$TP <- factor(Bar_Stress_Change_Ovrl$TP, levels = c("Prev", "Now"))
ggplot(Bar_Stress_Change_Ovrl, aes(x=TP, y=avg_rating, fill = TP)) + 
  scale_fill_manual(values=c("Grey", "Black")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Experience-based stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.8), vjust=3, color = "white")+ylim(c(0,3))

inla_ExpStress_Ovrl <- inla(b_Rat ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change_Ovrl, verbose = TRUE, weights = num_weights) 
summary(inla_ExpStress_Ovrl) # Summarize results
coefINLA(inla_ExpStress_Ovrl) # fixed effects
#Overall stress was greater in the past

#### Belief-related stress over time (N = 727) ####
## Overall change in belief-related stress
#Data Preparation
Stress_Change_OvrlBel$Subject <- as.factor(Stress_Change_OvrlBel$Subject)
Stress_Change_OvrlBel$TP <- as.factor(Stress_Change_OvrlBel$TP)
Stress_Change_OvrlBel <- Stress_Change_OvrlBel %>% mutate(Rating = b_Rat * 5 )

#Visualization
#Line plot
#Line chart - Item Level
LineStress_Bel <- Stress_Change %>% dplyr::select(Subject, TP, Q13_1, Q13_2, Q13_3, Q13_4, Q13_5) %>% 
  gather("Q", "Rating", 3:7)
LineStress_Bel$Q <- dplyr::recode(LineStress_Bel$Q, "Q13_1" = 'BlvngGvmntNotCmbtCOVID', "Q13_2" ="CrmnltyIncrsd", 
                                  "Q13_3" ="Blvng_EcnmcCrsis", "Q13_4" ="OthrNotFollowingPblcHlthGuidlns", "Q13_5" ="BelLifeIsNotSame")
LineStress_Bel <- LineStress_Bel %>% group_by(TP, Q) %>% summarize(avg_rating = mean(Rating, na.rm = TRUE), se = sd(Rating, na.rm= TRUE)/sqrt(730))
LineStress_Bel$TP <- factor(LineStress_Bel$TP, levels = c("Prev", "Now"))
ggplot(LineStress_Bel, aes(x=TP, y=avg_rating, group=1, color = TP)) + 
  scale_color_manual(values=c("Black", "blue")) +
  theme_bw() +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Belief-related stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=2.5, color = "black")+ylim(c(2,5)) + facet_wrap(.~Q, nrow=2)+
  xlab("Timepoint") + ylab("Average Rating")


#Bar Chart
Bar_Stress_Change_OvrlBel <- Stress_Change_OvrlBel %>% group_by(TP) %>% summarise(avg_rating = mean(Rating), se = sd(Rating)/sqrt(717)) #Timepoint (Current/Future/MostDiff)
Bar_Stress_Change_OvrlBel$TP <- factor(Stress_Change_OvrlBel$TP, levels = c("Prev", "Now"))
ggplot(Bar_Stress_Change_OvrlBel, aes(x=fct_rev(TP), y=avg_rating, fill = TP)) + 
  scale_fill_manual(values=c("Grey", "Black")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Belief-related stress over time") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.8), vjust=3, color = "white") +ylim(c(0,3))



inla_BelStress_Ovrl <- inla(b_Rat ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change_OvrlBel, verbose = TRUE, weights = num_weights) 
summary(inla_BelStress_Ovrl) # Summarize results
coefINLA(inla_BelStress_Ovrl) # fixed effects
#Overall Belief-related stress was greater in the past

#### Accuracy of remembered belief-related stress (N = 726) ####
inla_BelDev_Ovrl <- inla(BDev ~ f(Subject, model = "iid"), family = "beta", data = DevxConf_Bel_Ovrl, verbose = TRUE, weights = num_weights) 
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
                position=position_dodge(.9)) + ggtitle("Deviations in remembered belief-related stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_devi, 2)), position=position_dodge(width=0.8), vjust=3, color = "black")  + ylim(c(0,.4))


##Analysis
inla_Dev_Bel_Conf_Ovrl <- inla(BDev ~ Conf_Rat, family = "beta", data = DevxConf_Bel_Ovrl, verbose = TRUE) 
summary(inla_Dev_Bel_Conf_Ovrl) #Summarize results
coefINLA(inla_Dev_Bel_Conf_Ovrl) # fixed effects

#89% CI*
inla_Dev_Bel_Conf_Ovrl_89 <- inla(BDev ~ Conf_Rat, family = "beta", data = DevxConf_Bel_Ovrl, verbose = TRUE, quantiles = c(0.055,0.945)) 
summary(inla_Dev_Bel_Conf_Ovrl_89) #Summarize results
coefINLA(inla_Dev_Bel_Conf_Ovrl_89) # fixed effects


#### Accuracy of remembered experience-related stress (N = 723) ####
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
                position=position_dodge(.9)) + ggtitle("Deviations in remembered experience-based stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_devi, 2)), position=position_dodge(width=0.8), vjust=3, color = "black") + ylim(c(0,.4))


 #Analysis
inla_Dev_Exp_Conf_Ovrl <- inla(BDev ~ Conf_Rat, family = "beta", data = DevxConf_Exp_Ovrl, verbose = TRUE, weights = num_weights) 
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

#Model dplyr::selection

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

#Visualization
#Line Plots
Line_RemExp_Stress <- RemExp_Stress %>% dplyr::select(Subject, Covid_Exp1, Covid_Exp2, BCovid_Rem) %>% gather("Experience_Stress", "Rating", 2:4) %>% mutate(Rating = ((Rating*4)+1))
Line_RemBel_Stress <- RemBel_Stress %>% dplyr::select(Subject, Belief_Exp1, Belief_Exp2, BBelief_Rem) %>% gather("Belief_Stress", "Rating", 2:4) %>% mutate(Rating = ((Rating*4)+1))
Line_RemExp_Stress$Experience_Stress <- dplyr::recode(Line_RemExp_Stress$Experience_Stress, "Covid_Exp1" = 'TP1', "Covid_Exp2" ="TP2",  "BCovid_Rem" = "Rem")
Line_RemBel_Stress$Belief_Stress <- dplyr::recode(Line_RemBel_Stress$Belief_Stress, "Belief_Exp1" = 'TP1', "Belief_Exp2" ="TP2",  "BBelief_Rem" = "Rem")

#Experience-based stress
Line_RemExp_Stress <- Line_RemExp_Stress %>% group_by(Experience_Stress) %>% summarize(avg_rating = mean(Rating, na.rm = TRUE), se = sd(Rating, na.rm= TRUE)/sqrt(713))
Line_RemExp_Stress$Experience_Stress <- factor(Line_RemExp_Stress$Experience_Stress, levels = c("TP1", "TP2", "Rem"))
ggplot(Line_RemExp_Stress, aes(x=Experience_Stress, y=avg_rating, group=1, color = Experience_Stress)) + 
  scale_color_manual(values=c("Black", "blue", "red")) +
  theme_bw() +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Average Experience-based stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=3, color = "black")+ylim(c(0,3)) +
  xlab("Timepoint") + ylab("Average Rating")

#Belief-related stress
Line_RemBel_Stress <- Line_RemBel_Stress %>% group_by(Belief_Stress) %>% summarize(avg_rating = mean(Rating, na.rm = TRUE), se = sd(Rating, na.rm= TRUE)/sqrt(730))
Line_RemBel_Stress$Belief_Stress <- factor(Line_RemBel_Stress$Belief_Stress, levels = c("TP1", "TP2", "Rem"))
ggplot(Line_RemBel_Stress, aes(x=Belief_Stress, y=avg_rating, group=1, color = Belief_Stress)) + 
  scale_color_manual(values=c("Black", "blue", "red")) +
  theme_bw() +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Average Belief-related stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=3, color = "black")+ylim(c(0,4)) +
  xlab("Timepoint") + ylab("Average Rating")

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
a <- df.pca %>% dplyr::select(starts_with('Q10_'), Q3:Q8, starts_with('threat_delta'), starts_with('stress_delta')) %>% as.matrix %>% rcorr()
a$r %>% round(2)
a$P %>% round(2) # Q6-Q8 all correlates most highly with Q10_2 (but how to test this?) caveat being stress_delta_curr correlates highest with threat_delta_us and threat_delta_world, seems to be in contradiction with previous results.
# the underlying q here is: 'typical person' in your community, are you thinking closed others/ppl in us/ppl across the world?
# or maybe these two questions are incomparable, esp we ask for threat perception and stress rating separately.

#### Liz Suggestions ####
## Percent Change
#Graph with the percentage of change (n = 717)
Dev_Exp_Perc <- Stress_Out %>% dplyr::select(Subject, Covid_Exp1, Covid_Rem) %>% group_by(Subject) %>% summarize(avg_PercChange = ((Covid_Rem-Covid_Exp1)/Covid_Exp1))  %>% mutate(Type = "Percent") 
Dev_Exp_Perc[Dev_Exp_Perc == "NaN"] <- NA
Dev_Exp_Perc <- na.omit(Dev_Exp_Perc)
Grph_Dev_Exp_Perc <- Dev_Exp_Perc %>% group_by(Type) %>% summarize(avg_PercOver = mean(avg_PercChange)*100, se = (sd(avg_PercChange)/sqrt(717))*100)
ggplot(Grph_Dev_Exp_Perc, aes(x=Type, y=avg_PercOver, fill = Type)) + 
  scale_fill_manual(values=c("grey")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_PercOver-se, ymax=avg_PercOver+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("Overestimation of remembered experience-based stress") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=round(avg_PercOver, 2)), position=position_dodge(width=0.8), vjust=-3, color = "black")  + ylim(c(0,25)) +
  labs(y = "Percent overestimated", x = "")

#### MCMC Versions ####
##Construction of Remembered Stress
#Experience-Related
RemExp_Stress_n713 <- na.omit(RemExp_Stress)
RemStress_Exp_MCMC <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2, family = mgcv::betar, iter= 1000, weights = num_weights, data = RemExp_Stress_n713) 
plot_model(RemStress_Exp_MCMC, type = "pred", ci.lvl = .89, bpe.color = "red") 
describe_posterior(RemStress_Exp_MCMC)

inla_ExpStress_Ovrl <- inla(b_Rat ~ TP + f(Subject, model = "iid"), family = "beta", data = Stress_Change_Ovrl, verbose = TRUE, weights = num_weights) 



#Edits for Degrees of freedom
RemStress_Exp_MCMC.fixed <- RemStress_Exp_MCMC
RemStress_Exp_MCMC.fixed$df.residual <- with(RemStress_Exp_MCMC.fixed, sum(weights)-length(coefficients))
tidyMCMC(RemStress_Exp_MCMC, conf.method = "HPDinterval", conf.int = TRUE, conf.level = .95)
tidyMCMC(RemStress_Exp_MCMC.fixed, conf.method = "HPDinterval", conf.int = TRUE, conf.level = .95)

do.call("rbind", lapply(names(RemStress_Exp_MCMC.fixed), function(n) cbind(model=n), tidy(RemStress_Exp_MCMC.fixed[[n]]) )) 
