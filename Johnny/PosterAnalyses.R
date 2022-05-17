#### Analyses for poster ####
## set path
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
## compile utils
source('utils.R')

## define var related to MCMC
niters <- 20000
seed.no <- 1636


## Change in stress experience over time
Stress_Change <- read.csv('../data/CvdStrss_Wei_Longitudinal.csv') #Load Data
Stress_Change_Ovrl <- read.csv('../data/Stress_Change_Overall.csv') #Load Data
Stress_Change_Ovrl$Subject <- as.factor(Stress_Change_Ovrl$Subject)
Stress_Change_Ovrl$TP <- as.factor(Stress_Change_Ovrl$TP)
Stress_Change_Ovrl$TP <- relevel(Stress_Change_Ovrl$TP, "Prev")
covid_wide <-  read.csv('../data/Complete_test_Dev_Exp_Only_Data_Manipulation.csv') #each row is one observation, T1 and T2 separated by prefix in the colnmae


#Plot Raw
Stress_Change %>% ggplot(aes(x=Rating)) + geom_histogram(bins = 20) + facet_wrap(~fct_rev(Timepoint))
#MCMC Beta[0,1]
Stress_TP_covid_MCMC <- stan_glmer(b_Rat ~ TP + (1|Subject), 
                                 family = mgcv::betar, iter= niters, seed=seed.no, weights=num_weights, data = Stress_Change_Ovrl)
plot_model(Stress_TP_covid_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red")
describe_posterior(Stress_TP_covid_MCMC)


## Differences between subjective recollection and stress experience at TP1
RemExp_Stress <- read.csv("../data/RemExp_Stress.csv", header = TRUE) 
RemExp_Stress$Subject <- as.factor(RemExp_Stress$Subject)
RemExp_Stress$num_weights[is.na(RemExp_Stress$num_weights)] <- 0
RemT1_df <- RemExp_Stress %>% dplyr::select(Subject, num_weights, Covid_Exp1, BCovid_Rem) %>% gather("Memtype", "BScore", 3:4) %>% filter(BScore != "NA") %>%
  filter(num_weights != 0)
RemT1_df$Memtype <- as.factor(RemT1_df$Memtype)
RemT1_df$Memtype <- relevel(RemT1_df$Memtype, "Covid_Exp1")

##Analysis
#MCMC Beta[0,1]
RemStress_Exp_MCMC <- stan_glmer(BScore ~ Memtype + (1|Subject), 
                                   family = mgcv::betar, iter= niters, seed=seed.no, weights=num_weights, data = RemT1_df)
plot_model(RemStress_Exp_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red")
describe_posterior(RemStress_Exp_MCMC)


## Construction of Remembered Stress (Remembered ~ T1 + T2)
## Covid (unweighted and weighted results are qualitatively the same i.e., coef(Exp1) < coef(Exp2))
covid_BRem <- covid_wide %>% mutate(BCovid_Rem = beta_squeeze((Covid_Rem-1)/4)) %>% 
  filter(!is.na(BCovid_Rem))
#MCMC Beta[0,1]
Rem_covid_MCMC_weighted <- stan_glm(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2,
                                    family = mgcv::betar, iter= niters, seed=seed.no,
                                    weights = num_weights_3var, data = covid_BRem)
plot_model(Rem_covid_MCMC_weighted, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(Rem_covid_MCMC_weighted)

## Emotional Well-being improvement
Covid_PCA_Delta <- covid_wide %>% dplyr::select(Subject, PCA_T1, PCA_T2) %>% gather("PCA_TP","PCA_Score", 2:3)
Covid_PCA_Delta$PCA_TP <- as.factor(Covid_PCA_Delta$PCA_TP)
unique(Covid_PCA_Delta$Subject)
Covid_PCA_Delta <- Covid_PCA_Delta %>% mutate(PCA_Score = PCA_Score*-1)
Graph_PCA_MCMC <- Covid_PCA_Delta %>% group_by(PCA_TP) %>% summarize(avg_PCA = mean(PCA_Score, na.rm=T)*-1, se_PCA = sd(PCA_Score, na.rm=T)/sqrt(726))
covid_PCA_MCMC <-  stan_glmer(PCA_Score ~ PCA_TP + (1|Subject), 
                        family = 'gaussian', iter= niters, seed=seed.no,
                        data = Covid_PCA_Delta)
plot_model(covid_PCA_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(covid_PCA_MCMC)

## Emotional Wellbeing and Memory Consistency
PCA_MEM <- covid_wide %>% dplyr::select(Subject, Covid_Dev, PCA_delta) %>% mutate(PCA_delta = PCA_delta*-1)
covid_PCA_MCMC <-  stan_glm(PCA_delta ~ Covid_Dev, 
                              family = 'gaussian', iter= niters, seed=seed.no,
                              data = PCA_MEM)
plot_model(covid_PCA_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red") 
describe_posterior(covid_PCA_MCMC)

plot_model(covid_PCA_MCMC, type = "pred", ci.lvl = .95, bpe.color = "red") +
  theme_bw() +
  geom_vline(xintercept = 0, color = 'blue', linetype = 'dashed')+
  ggtitle('')


p_pca <- mod_pca %>% plot_model(type='pred', terms = 'emo_well_being_delta') +
  xlab('Change in Composite Score of Emotion Well-being') +
  ylab('Overestimation of T1 Experienced Stress (beta squeezed)') +
  theme_bw() +
  geom_vline(xintercept = 0, color = 'blue', linetype = 'dashed')+
  ggtitle('')
describe_posterior(covid_PCA_MCMC)


#### Plots ####
#T1/2
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

#T1/Rem
ggplot(Rem_expT1, aes(x=Timepoint, y=avg_rating, fill = Timepoint)) + 
  scale_fill_manual(labels = c("T1 Experience", "Remembered"), values=c("Grey", "Brown")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_point() +
  geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.text.x = element_text(face="bold",size=0),axis.text.y = element_text(face="bold", size=21), legend.text = element_text(size=10), legend.title = element_text(size=15)) + 
  geom_text(aes(label=round(avg_rating, 2)), position=position_dodge(width=0.6), vjust=2, color = "white", size = 9)+ylim(c(0,2.5)) +
  xlab("") + ylab("")

#Construction of remembered stress
p_curr_STAN <- Rem_covid_MCMC_weighted %>% plot_model_IVs_stan(variable_array = c('Covid_Exp1','Covid_Exp2'),
                                                             x_range = seq(1,5,by=0.2))
p_curr_STAN <- p_curr_STAN + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(face="bold",size=18),axis.text.y = element_text(face="bold", size=18), legend.text = element_text(size=10), legend.title = element_text(size=15)) +
  ylab('') +
  xlab('') +
  scale_color_manual(labels = c('T1','T2'), values = c('dark grey','black'))+
  scale_fill_manual(values = c('grey','black'))+
  labs(colour='') + ylim(0,1) 

#Change in PCA Score
ggplot(Graph_PCA_MCMC, aes(x=PCA_TP, y=avg_PCA, group = 1, color = PCA_TP)) + 
  scale_color_manual(labels = c("T1 PCA_T1", "PCA_T2"), values=c("Grey", "Black")) +
  theme_bw() +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=avg_PCA-se_PCA, ymax=avg_PCA+se_PCA),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.text.x = element_text(face="bold",size=0),axis.text.y = element_text(face="bold", size=21), legend.text = element_text(size=10), legend.title = element_text(size=15)) + 
  geom_text(aes(label=round(avg_PCA, 3)), position=position_dodge(width=0.6), vjust=-3, color = "black", size = 9)+ylim(c(-.4,.4)) +
  xlab("") + ylab("")


