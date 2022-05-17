### PlotScript.R
### Try to put only the plot script here for both COVID and 9/11
## Haoxue: for reproducible sake 

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

source('utils.R')


### read in data
Complete_test_Dev_Exp <-  read.csv('../data/Complete_test_Dev_Exp_Only_Data_Manipulation.csv')
df.emo.wide.avg <- read.csv('../data/df.emo.wide.avg_Only_Data_Manipulation.csv')
df.emo.wide.avg <- read.csv('../data/df.emo.wide.avg_Only_Data_Manipulation.csv')
Stress_Change_Ovrl <- read.csv('../data/Stress_Change_Overall.csv')


# todo: read in MCMCresults

### Fig 1a Percent Change (ONLY COVID)
Stress_Out <- Complete_test_Dev_Exp
Dev_Exp_Perc <- Stress_Out %>% dplyr::select(Subject, Covid_Exp1, Covid_Rem) %>% group_by(Subject) %>% summarize(avg_PercChange = ((Covid_Rem-Covid_Exp1)/Covid_Exp1))  %>% mutate(Type = "Percent") 
Dev_Exp_Perc[Dev_Exp_Perc == "NaN"] <- NA
Dev_Exp_Perc <- na.omit(Dev_Exp_Perc)
Grph_Dev_Exp_Perc_COVID <- Dev_Exp_Perc %>% group_by(Type) %>% 
  summarize(avg_PercOver = mean(avg_PercChange)*100, se = (sd(avg_PercChange)/sqrt(nrow(Stress_Out)))*100) %>% 
  mutate(Dataset = 'COVID', time='2')
ggplot(Grph_Dev_Exp_Perc_COVID, aes(x=Type, y=avg_PercOver, fill = Type)) + 
  scale_fill_manual(values=c("maroon")) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=avg_PercOver-se, ymax=avg_PercOver+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="none", axis.text.x = element_text(face="bold",size=0),axis.text.y = element_text(face="bold", size=21)) + 
  geom_text(aes(label=round(avg_PercOver, 2)), position=position_dodge(width=0.8), vjust=-2, color = "black", size = 9)  + ylim(c(0,17.5)) +
  labs(y = "", x = "")

        
        
new.df <- Dev_Exp_Perc %>% dplyr::select(Subject, avg_PercChange) %>% rename(Percent = avg_PercChange) %>% mutate(Type = 'Covid') %>% 
  rbind(df.emo.wide.avg_Perc %>% filter(time==2) %>% dplyr::select(Subject, PercChange) %>% rename(Percent = PercChange) %>% mutate(Type = '911'))

### Fig 1 Percent Change (COVID + 9/11)
df.emo.wide.avg_Perc <- df.emo.wide.avg %>% mutate(PercChange = dev / prev_experience)
Grph_Dev_Exp_Perc_911 <- df.emo.wide.avg_Perc %>% group_by(time) %>% 
  summarise(avg_PercOver = mean(PercChange*100), se = sd(PercChange)/sqrt(n())*100, n=n()) %>% 
  mutate(Type = 'Percent', Dataset = '911') %>% 
  dplyr::select(Type, avg_PercOver, se, Dataset, time)
Grph_Dev_Exp_Perc <- Grph_Dev_Exp_Perc_COVID %>% rbind(Grph_Dev_Exp_Perc_911)

p_Perc <- Grph_Dev_Exp_Perc %>% filter(time==2) %>% 
  ggplot(aes(y=avg_PercOver, x=Dataset, fill = Dataset)) +
  theme_bw()+
  geom_bar(position = position_dodge(), stat='identity') +
  geom_errorbar(aes(ymin=avg_PercOver-se, ymax = avg_PercOver+se),
                width=.2,
                position=position_dodge(.9))+
  labs(y = "Percent overestimated (%)", x = "")+
  geom_text(aes(label=round(avg_PercOver, 2)), position=position_dodge(width=0.8), vjust=-4, color = "black", size = 7)  + ylim(c(0,16)) 
ggsave('../Output/fig/pdf/p_Perc.pdf', p_Perc, width=5, height=5)
ggsave('../Output/fig/png/p_Perc.png', p_Perc, width=5, height=5)

### Fig 1 Percent Change (9/11 over time)
p_Perc_911_time <- Grph_Dev_Exp_Perc %>% filter(Dataset == '911') %>% 
  ggplot(aes(y=avg_PercOver, x=time, fill = Dataset)) +
  theme_bw()+
  geom_point()+
  geom_line(aes(group=1))+
  #geom_bar(position = position_dodge(), stat='identity') +
  geom_errorbar(aes(ymin=avg_PercOver-se, ymax = avg_PercOver+se),
                width=.2,
                position=position_dodge(.9))+
  labs(y = "Percent overestimated (%)", x = "Survey")+
  geom_text(aes(label=round(avg_PercOver, 2)), position=position_dodge(width=0.8), vjust=-4, color = "black", size = 7)  + 
  ylim(c(0,18)) +
  scale_x_discrete(breaks = c(2:4), labels = c('1 year','5 years','10 years')) +
  theme(legend.position = 'none')
p_Perc_911_time
ggsave('../Output/fig/pdf/p_Perc_911_time.pdf', p_Perc_911_time, width=5, height=5)
ggsave('../Output/fig/png/p_Perc_911_time.png', p_Perc_911_time, width=5, height=5)

#lmer(PercChange ~ time+(1|Subject), df.emo.wide.avg_Perc) %>% Anova

### Fig 1 Percent Change (9/11 t2 diff emotion)

df.emo.wide <-  read.csv('../data/df.emo.wide_Only_Data_Manipulation.csv')
p_Perc_911_time_emo <- df.emo.wide %>% 
  mutate(PercChange2 = memdev2 / experienced2 * 100, 
         PercChange3 = memdev3 / experienced3 * 100, 
         PercChange4 = memdev4 / experienced4 * 100) %>% 
  pivot_longer(cols = PercChange2:PercChange4, names_to = 'time', values_to = 'memdev') %>% 
  mutate(time = str_extract(time,'[0-9]')) %>% 
  group_by(item, time) %>% 
  summarise(mean = mean(memdev, na.rm=T), se = sd(memdev, na.rm=T)/sqrt(n())) %>% 
  ggplot(aes(x=time, y=mean, group=item, color=item)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width=.2)+
  labs(y = "Percent overestimated (%)", x = "Survey")+
  scale_x_discrete(breaks = c(2:4), labels = c('1 year','5 years','10 years')) +
  theme_bw()
ggsave('../Output/fig/pdf/p_Perc_911_time_emo.pdf', p_Perc_911_time_emo, width=5, height=5)
ggsave('../Output/fig/png/p_Perc_911_time_emo.png', p_Perc_911_time_emo, width=5, height=5)



### Fig 1.2 z-score Change Covid_T1 -> Covid_Rem 
#   either this result does not make sense OR suggest that people are in general accurate in remembering stress
Dev_Exp_Perc %>% ggplot(aes(x=avg_PercChange * 100)) +
  geom_histogram(bins = 25, color='black',fill='white') +
  geom_vline(xintercept = 0, color='red', linetype = 'dashed')+
  xlab('Percennt overestimated')

### Fig 2.1 Mem ~ Prev + Curr (Covid)

# beta squeeze Covid_Rem
Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(BetaS_Covid_Rem = beta_squeeze((Covid_Rem-1)/4)) 

mod_curr <- betareg(BetaS_Covid_Rem ~ Covid_Exp1 + Covid_Exp2,
                    Complete_test_Dev_Exp)
p_recent_covid <- mod_curr %>% plot_model_IVs(c('Covid_Exp1','Covid_Exp2'),x_range=seq(1,5,by=0.5)) +
  theme_bw() +
  ylab('Remembered Stress (Squeezed into 0-1)') +
  xlab('Self-reported Stress') +
  scale_color_manual(labels = c('T1 experienced','T2 current'), values = c('purple','green'))+
  scale_fill_manual(values = c('purple','green'))+
  labs(colour='')
ggsave('../Output/fig/pdf/p_recent_covid.pdf', p_recent_covid, width=5, height=5)
ggsave('../Output/fig/png/p_recent_covid.png', p_recent_covid, width=5, height=5)

# contrast analysis shows that the coefficients significantly different from each other
K <- matrix(c(0,1,-1,0),1)
glht_mod_curr <- mod_curr %>% glht(K) 
glht_mod_curr %>% summary

### Fig 2.2 Mem ~ Prev + Curr (9/11)
df.emo.wide.avg <- df.emo.wide.avg %>% 
  group_by(time) %>% 
  mutate(BetaS_stress_memory = beta_squeeze((stress_memory-1)/4))

mod_curr_911 <- glmmTMB(BetaS_stress_memory ~ (prev_experience + curr_experience) * time + (1|Subject),
        df.emo.wide.avg %>% mutate(time=factor(time, levels = c(2,3,4))))
mod_curr_911
K <- matrix(c(0,1,-1,0,0,0,0,0,0,
              0,1,-1,0,0,1,0,-1,0,
              0,1,-1,0,0,0,1,0,-1),3,byrow=TRUE)
glht_mod_curr_911 <- mod_curr_911 %>% glht(K)
glht_mod_curr_911 %>% summary
K <- matrix(c(0,0,0,0,0,1,1,-1,-1),1)
glht_mod_curr_911 <- mod_curr_911 %>% glht(K)
glht_mod_curr_911 %>% summary

mod_curr_911 %>% plot_model(type = 'pred', terms = c('prev_experience','curr_experience'), list=c(time=2))
mod_curr_911 %>% plot_model(type = 'pred', terms = c('curr_experience','time'))

data_int_mod_curr_911_1 <- mod_curr_911 %>% get_model_data(type='pred', terms = c('prev_experience[1,2,3,4,5]','time[2,3,4]'))
data_int_mod_curr_911_2 <- mod_curr_911 %>% get_model_data(type='pred', terms = c('curr_experience[1,2,3,4,5]','time[2,3,4]'))

data_int_mod_curr_911_1 <- data_int_mod_curr_911_1 %>% data.frame %>% dplyr::select(x, predicted, conf.low, conf.high,
                                          group_col) %>% 
  mutate(ymin = conf.low,
         ymax = conf.high,
         time = group_col,
         x_name = 'prev_experience')

data_int_mod_curr_911_2 <- data_int_mod_curr_911_2 %>% data.frame %>% dplyr::select(x, predicted, conf.low, conf.high,
                                                         group_col) %>% 
  mutate(ymin = conf.low,
         ymax = conf.high,
         time = group_col,
         x_name = 'curr_experience')
data_int_mod_curr_911 <- data_int_mod_curr_911_1 %>% rbind(data_int_mod_curr_911_2)
p_recent_911 <- data_int_mod_curr_911 %>% ggplot(aes(x=x,y=predicted))+
  geom_line(aes(group=x_name,color=x_name))+
  geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=x_name),alpha=0.3, color=NA, show.legend=FALSE)+
  facet_wrap(~time, labeller = labeller(time = c('2'='1 year','3'='5 years','4'='10 years'))) +
  theme_bw() +
  ylab('Remembered Stress (Squeezed into 0-1)') +
  xlab('Self-reported Stress') +
  labs(color='') + 
  scale_color_manual(labels = c('T2 current','T1 experienced'), values = c('green','purple'))+
  scale_fill_manual(values = c('green','purple'))
ggsave('../Output/fig/pdf/p_recent_911.pdf', p_recent_911, width=10, height=6)
ggsave('../Output/fig/png/p_recent_911.png', p_recent_911, width=10, height=6)

###Fig 3. Emo well being vs. 

mod_pca <- betareg(BetaS_Covid_Dev_equalN ~ emo_well_being_delta,
                   Complete_test_Dev_Exp_PCA)

p_pca <- mod_pca %>% plot_model(type='pred', terms = 'emo_well_being_delta') +
  xlab('Change in Composite Score of Emotion Well-being') +
  ylab('Overestimation of T1 Experienced Stress (beta squeezed)') +
  theme_bw() +
  geom_vline(xintercept = 0, color = 'blue', linetype = 'dashed')+
  ggtitle('')

ggsave('../Output/fig/pdf/p_pca.pdf', p_pca, width=8, height=6)
ggsave('../Output/fig/png/p_pca.png', p_pca, width=8, height=6)

### New Fig 2 - Use STAN
p_curr_STAN <- RemStress_Exp_MCMC_HF %>% plot_model_IVs_stan(variable_array = c('Covid_Exp1','Covid_Exp2'),
                                              x_range = seq(1,5,by=0.2))
p_curr_STAN <- p_curr_STAN + theme_bw() + 
  ylab('Remembered Stress (Squeezed into 0-1)') +
  xlab('Self-reported Stress') +
  scale_color_manual(labels = c('T1 experienced','T2 current'), values = c('purple','green'))+
  scale_fill_manual(values = c('purple','green'))+
  labs(colour='')
  
ggsave('../Output/fig/pdf/p_curr_STAN.pdf', p_curr_STAN, width=8, height=6)
ggsave('../Output/fig/png/p_curr_STAN.png', p_curr_STAN, width=8, height=6)




