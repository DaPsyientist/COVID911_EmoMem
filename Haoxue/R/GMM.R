############### R code to run GMM 
# written by Haoxue
# this script runs GMM 
# the current setting only uses experience-related question

# some other ideas: GMM part and try to use other metrics?

lm(prev_IUS_total ~ cluster_both_best, Complete_test_wide) %>% Anova()
lm(prev_Demographics_age ~ cluster_both_best, Complete_test_wide) %>% Anova() # not sig
lm(prev_IUS_total ~ cluster_both_best, Complete_test_wide) %>% 
  plot_model(type='pred', terms=c('cluster_both_best'))
lm(prev_IUS_total ~ cluster_both_best, Complete_test_wide) %>% 
  emmeans(specs='cluster_both_best') %>% contrast('pairwise')



source('utils/utils.R')

Complete_test_wide <- read_csv('../data/COVIDdata_merged_Haoxue_version.csv')

output_both_best <- summary(clusfit_both_best, parameters = TRUE)
output_both_best$mean %>% as.data.frame() %>% t() %>% as.data.frame() %>% rownames_to_column('cluster') %>% 
  pivot_longer(cols=NA_expectation_deviation_experience1:NA_expectation_deviation_experience17,names_to='QExp',values_to='deviation') %>% 
  mutate(QExp = factor(QExp, levels = c(levels_experience))) %>% 
  ggplot(aes(x=QExp,y=deviation,color=cluster,fill=cluster))+
  geom_col(width=0.5) +
  coord_flip() + 
  facet_wrap(~cluster) +
  scale_x_discrete(breaks = c(levels_experience),
                   labels=c(labels_experience)) +
  ggtitle("T2_remembered - T1_experienced (2 cluster only using experience)")

### check how the aggregated measurement of stress (T1_experienced, T2_experienced, T1_remembered) differ across groups
lm(NA_mean_prev_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_curr_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_remembered_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')

### a graph showing aggregated measurement of stress ~ type * group
mod_aggreaged <- lmer(stress_aggregated ~ type * cluster_both_best + (1|Subject), 
Complete_test_wide %>% pivot_longer(cols=c(NA_mean_prev_experience, NA_mean_curr_experience, NA_mean_remembered_experience),
                               names_to = 'type', values_to = 'stress_aggregated')) 
mod_aggreaged %>% 
  emmeans(specs = 'cluster_both_best', by='type') %>% contrast('pairwise', adjust = 'bonf')
mod_aggreaged %>% 
  emmeans(specs = 'type', by='cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_aggreaged %>% 
  plot_model(type='eff', terms=c('cluster_both_best','type'))
mod_aggreaged %>% 
  plot_model(type='eff', terms=c('type','cluster_both_best'))

### check how emotion well being differ across group
mod_emowell <- lmer(emowell ~ type * cluster_both_best + (1|Subject), 
                    Complete_test_wide %>% pivot_longer(cols=c(prev_pca_score, curr_pca_score),
                                                   names_to = 'type', values_to = 'emowell')) 
mod_emowell %>% Anova
mod_emowell %>% 
  emmeans(specs = 'cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_emowell %>% 
  emmeans(specs = 'cluster_both_best', by='type') %>% contrast('pairwise', adjust = 'bonf')
mod_emowell %>% 
  emmeans(specs = 'type', by='cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_emowell %>% 
  plot_model(type='eff', terms=c('cluster_both_best','type'))
mod_emowell %>% 
  plot_model(type='eff', terms=c('type','cluster_both_best'))

### check how social well being differ across group
mod_sociowell <- lmer(emowell ~ type * cluster_both_best + (1|Subject), 
                  Complete_test_wide %>% pivot_longer(cols=c(prev_soc_mean, curr_soc_mean),
                                                 names_to = 'type', values_to = 'emowell')) 
mod_sociowell %>% Anova
mod_sociowell %>% 
  emmeans(specs = 'cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_sociowell %>% 
  emmeans(specs = 'cluster_both_best', by='type') %>% contrast('pairwise', adjust = 'bonf')
mod_sociowell %>% 
  emmeans(specs = 'type', by='cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_sociowell %>% 
  plot_model(type='eff', terms=c('cluster_both_best','type'))
mod_sociowell %>% 
  plot_model(type='eff', terms=c('type','cluster_both_best'))

### check how threat perception differs across groups
mod_threat <- lmer(threat ~ type * cluster_both_best + (1|Subject), 
                    Complete_test_wide %>% pivot_longer(cols=Q10_1:Q10_4,
                                                   names_to = 'type', values_to = 'threat')) 
mod_threat %>% Anova
mod_threat %>% 
  emmeans(specs = 'cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')
mod_threat %>% 
  emmeans(specs = 'cluster_both_best', by='type') %>% contrast('pairwise', adjust = 'bonf')
mod_threat %>% 
  emmeans(specs = 'type', by='cluster_both_best') %>% contrast('pairwise', adjust = 'bonf')

mod_threat %>% 
  plot_model(type='eff', terms=c('cluster_both_best'))
mod_threat %>% 
  plot_model(type='eff', terms=c('type','cluster_both_best'))
mod_threat %>% 
  plot_model(type='eff', terms=c('cluster_both_best','type'))

### check how stress perception differs across groups (layperson)
mod_layperson <- lmer(stress ~ agent * time * cluster_both_best + (1|Subject), 
                 Complete_test_wide %>% pivot_longer(cols=Q3:Q8,
                                                names_to = 'type', values_to = 'stress') %>% 
                   mutate(agent = ifelse(type=='Q3'|type=='Q4'|type=='Q5', 'self', 'other'),
                          time = ifelse(type=='Q3'|type=='Q6','curr',
                                        ifelse(type=='Q4'|type=='Q7','mostDiff',
                                               'future')))) 
mod_layperson %>% Anova
mod_layperson %>% 
  emmeans(specs = 'cluster_both_best', by='agent') %>% contrast('pairwise', adjust = 'bonf')
mod_layperson %>% 
  emmeans(by = 'cluster_both_best', specs='agent') %>% contrast('pairwise', adjust = 'bonf')
mod_layperson %>% 
  plot_model(type='eff', terms=c('cluster_both_best', 'agent'))
mod_layperson %>% 
  plot_model(type='eff', terms=c('agent', 'cluster_both_best'))

mod_layperson_ctl <- lmer(stress ~ agent * time * cluster_both_best + NA_mean_curr_experience + NA_mean_prev_experience + (1|Subject), 
                    Complete_test_wide %>% pivot_longer(cols=Q3:Q8,
                                                   names_to = 'type', values_to = 'stress') %>% 
                      mutate(agent = ifelse(type=='Q3'|type=='Q4'|type=='Q5', 'self', 'other'),
                             time = ifelse(type=='Q3'|type=='Q6','curr',
                                           ifelse(type=='Q4'|type=='Q7','mostDiff',
                                                  'future')))) 
mod_layperson_ctl %>% Anova
mod_layperson_ctl %>% 
  emmeans(specs = 'cluster_both_best', by='agent') %>% contrast('pairwise', adjust = 'bonf')
mod_layperson_ctl %>% 
  emmeans(by = 'cluster_both_best', specs='agent') %>% contrast('pairwise', adjust = 'bonf')
mod_layperson_ctl %>% 
  plot_model(type='eff', terms=c('cluster_both_best', 'agent'))
mod_layperson_ctl %>% 
  plot_model(type='eff', terms=c('agent', 'cluster_both_best'))

### ok now lets try to do the overdue analysis: behavior after ~ before behavior + stress
# Q21: days of a week
# Q21_1: At home without socializing with others? (e.g. browsing non news website; reading books; watching Netflix; cooking)
# Q21_2: Outside your home without socializing with others? (e.g. visiting outdoor green spaces alone; visiting tourist attractions alone; dining out alone)
# Q21_3: Socializing with others digitally? (e.g. chatting with others using social media; engaging in online multi-player gaming)
# Q21_4: Socializing with others offline in your home or in other people’s home?
# Q21_5: Socializing with others offline outside your home? (e.g. visiting outdoor green spaces with others; visiting tourist attractions with others; dining out with others)
# Q22: hours of a day
# Q52, Q53



# three thoughts:
# - combine days of the week with hours of the day
# - use only social stress (stress suffer from socializing with people?)
# - 



df.item_dev_prev <- Complete_test_wide %>% pivot_longer(cols=c(NA_expectation_deviation_experience1:NA_expectation_deviation_belief5),
                                    names_to = 'item', values_to = 'deviation') %>% 
  mutate(type = ifelse(str_detect(item, 'experience'), 'experience', 'belief'),
         soc_type = ifelse(item=='NA_expectation_deviation_experience1'|
                             item=='NA_expectation_deviation_experience2'|
                             item=='NA_expectation_deviation_experience3'|
                             item=='NA_expectation_deviation_experience4'|
                             item=='NA_expectation_deviation_belief1'|
                             item=='NA_expectation_deviation_belief2'|
                             item=='NA_expectation_deviation_belief3'|
                             item=='NA_expectation_deviation_belief4', 'soc', 'non-soc'),
         true_prev = case_when(item=='NA_expectation_deviation_experience1' ~ NA_prev_COVID_20_stress_lifechanges_1,
                               item=='NA_expectation_deviation_experience2' ~ NA_prev_COVID_21_stress_lifechanges_2,
                               item=='NA_expectation_deviation_experience3' ~ NA_prev_COVID_22_stress_lifechanges_3,
                               item=='NA_expectation_deviation_experience4' ~ NA_prev_COVID_23_stress_lifechanges_4,
                               item=='NA_expectation_deviation_experience5' ~ NA_prev_COVID_24_stress_lifechanges_5,
                               item=='NA_expectation_deviation_experience6' ~ NA_prev_COVID_25_stress_lifechanges_6,
                               item=='NA_expectation_deviation_experience7' ~ NA_prev_COVID_26_stress_lifechanges_7,
                               item=='NA_expectation_deviation_experience8' ~ NA_prev_COVID_27_stress_lifechanges_8,
                               item=='NA_expectation_deviation_experience9' ~ NA_prev_COVID_28_stress_lifechanges_9,
                               item=='NA_expectation_deviation_experience10' ~ NA_prev_COVID_29_stress_lifechanges_10,
                               item=='NA_expectation_deviation_experience11' ~ NA_prev_COVID_30_stress_lifechanges_11,
                               item=='NA_expectation_deviation_experience12' ~ NA_prev_COVID_31_stress_lifechanges_12,
                               item=='NA_expectation_deviation_experience13' ~ NA_prev_COVID_32_stress_lifechanges_13,
                               item=='NA_expectation_deviation_experience14' ~ NA_prev_COVID_33_stress_lifechanges_14,
                               item=='NA_expectation_deviation_experience15' ~ NA_prev_COVID_34_stress_lifechanges_15,
                               item=='NA_expectation_deviation_experience16' ~ NA_prev_COVID_35_stress_lifechanges_16,
                               item=='NA_expectation_deviation_experience17' ~ NA_prev_COVID_36_stress_lifechanges_17,
                               item=='NA_expectation_deviation_belief1' ~ NA_prev_COVID_37_stress_beliefs_1,
                               item=='NA_expectation_deviation_belief2' ~ NA_prev_COVID_38_stress_beliefs_2,
                               item=='NA_expectation_deviation_belief3' ~ NA_prev_COVID_39_stress_beliefs_3,
                               item=='NA_expectation_deviation_belief4' ~ NA_prev_COVID_40_stress_beliefs_4,
                               item=='NA_expectation_deviation_belief5' ~ NA_prev_COVID_41_stress_beliefs_5
                               ),
         remembered = case_when(item=='NA_expectation_deviation_experience1' ~  NA_Q15_1,
                              item=='NA_expectation_deviation_experience2' ~  NA_Q15_2,
                              item=='NA_expectation_deviation_experience3' ~  NA_Q15_3,
                              item=='NA_expectation_deviation_experience4' ~  NA_Q15_4,
                              item=='NA_expectation_deviation_experience5' ~  NA_Q15_5,
                              item=='NA_expectation_deviation_experience6' ~  NA_Q15_6,
                              item=='NA_expectation_deviation_experience7' ~  NA_Q15_7,
                              item=='NA_expectation_deviation_experience8' ~  NA_Q15_8,
                              item=='NA_expectation_deviation_experience9' ~  NA_Q15_9,
                              item=='NA_expectation_deviation_experience10' ~ NA_Q15_10,
                              item=='NA_expectation_deviation_experience11' ~ NA_Q15_11,
                              item=='NA_expectation_deviation_experience12' ~ NA_Q15_12,
                              item=='NA_expectation_deviation_experience13' ~ NA_Q15_13,
                              item=='NA_expectation_deviation_experience14' ~ NA_Q15_14,
                              item=='NA_expectation_deviation_experience15' ~ NA_Q15_15,
                              item=='NA_expectation_deviation_experience16' ~ NA_Q15_16,
                              item=='NA_expectation_deviation_experience17' ~ NA_Q15_17,
                              item=='NA_expectation_deviation_belief1' ~      NA_Q16_1,
                              item=='NA_expectation_deviation_belief2' ~      NA_Q16_2,
                              item=='NA_expectation_deviation_belief3' ~      NA_Q16_3,
                              item=='NA_expectation_deviation_belief4' ~      NA_Q16_4,
                              item=='NA_expectation_deviation_belief5' ~      NA_Q16_5
         ),
         true_now = case_when(
                              item=='NA_expectation_deviation_experience1' ~  NA_Q12_1,
                              item=='NA_expectation_deviation_experience2' ~  NA_Q12_2,
                              item=='NA_expectation_deviation_experience3' ~  NA_Q12_3,
                              item=='NA_expectation_deviation_experience4' ~  NA_Q12_4,
                              item=='NA_expectation_deviation_experience5' ~  NA_Q12_5,
                              item=='NA_expectation_deviation_experience6' ~  NA_Q12_6,
                              item=='NA_expectation_deviation_experience7' ~  NA_Q12_7,
                              item=='NA_expectation_deviation_experience8' ~  NA_Q12_8,
                              item=='NA_expectation_deviation_experience9' ~  NA_Q12_9,
                              item=='NA_expectation_deviation_experience10' ~ NA_Q12_10,
                              item=='NA_expectation_deviation_experience11' ~ NA_Q12_11,
                              item=='NA_expectation_deviation_experience12' ~ NA_Q12_12,
                              item=='NA_expectation_deviation_experience13' ~ NA_Q12_13,
                              item=='NA_expectation_deviation_experience14' ~ NA_Q12_14,
                              item=='NA_expectation_deviation_experience15' ~ NA_Q12_15,
                              item=='NA_expectation_deviation_experience16' ~ NA_Q12_16,
                              item=='NA_expectation_deviation_experience17' ~ NA_Q12_17,
                              item=='NA_expectation_deviation_belief1' ~      NA_Q13_1,
                              item=='NA_expectation_deviation_belief2' ~      NA_Q13_2,
                              item=='NA_expectation_deviation_belief3' ~      NA_Q13_3,
                              item=='NA_expectation_deviation_belief4' ~      NA_Q13_4,
                              item=='NA_expectation_deviation_belief5' ~      NA_Q13_5
         ),
         Conf = ifelse(item=='NA_expectation_deviation_belief1'|
                         item=='NA_expectation_deviation_belief2'|
                         item=='NA_expectation_deviation_belief3'|
                         item=='NA_expectation_deviation_belief4', Q16_Conf_1, Q15_Conf_1))

mod_attention <- lmer(deviation ~ true_prev + (media_peak_month + media_last_month + avoid_peak_month + avoid_last_month)*soc_type + (1|Subject),
                      df.item_dev_prev %>% filter(type=='experience'))
mod_attention <- lmer(deviation ~ true_prev + (media_peak_month + media_last_month + avoid_peak_month + avoid_last_month)*type + (1|Subject),
                      df.item_dev_prev)
mod_attention %>% Anova
mod_attention %>% plot_model(type='eff', terms='type')

#mod_attention1 <- lm(NA_mean_remembered_experience ~ NA_mean_prev_experience + (media_peak_month + media_last_month + avoid_peak_month + avoid_last_month),
#                      Complete_test_wide)
mod_relevance <- lmer(deviation ~ true_prev + 
                        (add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                           personal_loss + financial_loss_cont + relocate + job_change + 
                           communicate_peak_month + communicate_last_month) * soc_type+ (1|Subject),
   df.item_dev_prev %>% filter(type=='experience')) 
mod_relevance <- lmer(deviation ~ true_prev + 
                        (add_symptom_diagnosis_self + add_symptom_diagnosis_other + 
                           personal_loss + financial_loss_cont + relocate + job_change + 
                           communicate_peak_month + communicate_last_month) * type+ (1|Subject),
                      df.item_dev_prev )
mod_relevance %>% Anova

mod_attention %>% plot_model_IVs(variable_array = c('media_peak_month', 'media_last_month', 'avoid_peak_month', 'avoid_last_month'))
mod_relevance %>% plot_model_IVs(variable_array = c('job_change', 'communicate_peak_month'))


mod_relevance %>% plot_model(type='eff', terms=c('job_change'))
mod_relevance %>% plot_model(type='eff', terms=c('communicate_peak_month'))

mod_relevance %>% plot_model(type='eff', terms=c('communicate_last_month','type'))
mod_relevance %>% plot_model(type='eff', terms=c('add_symptom_diagnosis_self','type'))
mod_relevance %>% emtrends(pairwise~type, var='communicate_last_month')
mod_relevance %>% emtrends(pairwise~type, var='add_symptom_diagnosis_self')
mod_relevance %>% plot_model(type='eff', terms=c('soc_type','type'))

mod_emo_separate <- lmer(deviation ~ true_prev + remembered + (delta_pca_score+delta_soc) * soc_type+ (1|Subject),
                         df.item_dev_prev %>% filter(type=='experience'))
mod_emo_separate <- lmer(deviation ~ true_prev + true_now +(delta_pca_score+delta_soc) * type+ (1|Subject),
                         df.item_dev_prev)
mod_emo_separate %>% Anova














mod_emo_separate <- lmer(deviation ~ true_prev + remembered + (delta_pca_score+delta_soc) * type+ (1|Subject),
                         df.item_dev_prev)

mod_emo_separate_new <- lmer(deviation ~ true_prev + (delta_pca_score+delta_soc)*soc_type + (1|Subject) + (1|item),
                         df.item_dev_prev %>% filter(type=='experience'))
mod_emo_separate_new %>% Anova


# another part of the result we want: treat each item separately and lets do 


mod_soc_day <- lmer(soc_day ~ time * cluster_both_best + (1|Subject) + (1|item),
                    Complete_test_wide %>% pivot_longer(cols=c(Q21_1:Q21_5, Q52_1:Q52_5), names_to='type', values_to='soc_day') %>% 
                      mutate(time=ifelse(str_detect(type,'Q21'),'before','after'),
                             item=str_extract(type,'_[0-9]')))
mod_soc_day %>% Anova

mod_soc_hour <- lmer(soc_day ~ time * cluster_both_best + NA_mean_remembered_experience +
                       NA_mean_prev_experience + (1|Subject) + (1|item),
                     Complete_test_wide %>% pivot_longer(cols=c(Q22_1:Q22_5, Q53_1:Q53_5), names_to='type', values_to='soc_day') %>% 
                       mutate(time=ifelse(str_detect(type,'Q22'),'before','after'),
                              item=str_extract(type,'_[0-9]')))
mod_soc_hour %>% Anova

mod_soc_ba <- lmer(soc_day ~ time * cluster_both_best+ (1|Subject) +(1|item),
Complete_test_wide %>% 
  pivot_longer(cols=c(Tbefore3:Tbefore5, Tafter3:Tafter5), names_to='type', values_to='soc_day') %>% 
  mutate(time=ifelse(str_detect(type,'before'),'before','after'),
         item=str_extract(type,'[0-9]'))) 

mod_soc_ba %>% Anova
mod_soc_ba %>% plot_model(type='eff', terms = 'item')
mod_soc_ba %>% plot_model(type='eff', terms = c('item','cluster_both_best'))
mod_soc_ba %>% plot_model(type='eff', terms = c('cluster_both_best', 'item'))

mod_soc_ba %>% emmeans(specs = 'cluster_both_best', by='item') %>% 
  contrast('pairwise', adjust = 'bonf')
mod_soc_ba %>% emmeans(by = 'cluster_both_best', specs='item') %>% 
  contrast('pairwise', adjust = 'bonf')



lm(Q52_3 ~ Q21_3 + NA_Q15_2 + NA_prev_COVID_21_stress_lifechanges_2, Complete_test_wide) %>% summary
lm(Q53_3 ~ Q22_3 + NA_Q15_2 + NA_prev_COVID_21_stress_lifechanges_2, Complete_test_wide) %>% summary
lm(Tafter3 ~ Tbefore3 + NA_Q15_2 + NA_prev_COVID_21_stress_lifechanges_2, Complete_test_wide) %>% summary
 

Complete_test_wide$NA_prev_COVID_21_stress_lifechanges_2

lm(Tafter1 ~ Tbefore1 + NA_mean_remembered_experience + NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% Anova
lm(Tafter2 ~ Tbefore2 + NA_mean_remembered_experience + NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% Anova
lm(Tafter3 ~ Tbefore3 + NA_mean_remembered_experience + NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% Anova
lm(Tafter4 ~ Tbefore4 + NA_mean_remembered_experience + NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% Anova
lm(Tafter5 ~ Tbefore5 + NA_mean_remembered_experience + NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% Anova

lm(Q52_1 ~ Q21_1 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary

lm(Q52_1 ~ Q21_1 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q52_2 ~ Q21_2 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary

lm(Q52_3 ~ Q21_3 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q52_4 ~ Q21_4 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q52_5 ~ Q21_5 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary

lm(Q52_3 ~ Q21_3 + NA_Q15_2 + NA_prev_COVID_21_stress_lifechanges_2, Complete_test_wide) %>% summary
lm(Q53_3 ~ Q22_3 + NA_Q15_2 + NA_prev_COVID_21_stress_lifechanges_2, Complete_test_wide) %>% summary
lm(Q52_4 ~ Q21_4 + NA_Q15_1 + NA_prev_COVID_20_stress_lifechanges_1, Complete_test_wide) %>% summary
lm(Q53_4 ~ Q22_4 + NA_Q15_1 + NA_prev_COVID_20_stress_lifechanges_1, Complete_test_wide) %>% summary




Complete_test_wide$NA_prev_COVID_21_stress_lifechanges_2
lm(Q53_1 ~ Q22_1 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q53_2 ~ Q22_2 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q53_3 ~ Q22_3 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q53_4 ~ Q22_4 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary
lm(Q53_5 ~ Q22_5 + prev_soc_mean + curr_soc_mean + NA_mean_remembered_experience + NA_mean_prev_experience, Complete_test_wide) %>% summary










# look at our original data
Complete_test_wide %>% pivot_longer(cols=c(NA_expectation_deviation_belief1:NA_expectation_deviation_belief5, NA_expectation_deviation_experience1:NA_expectation_deviation_experience17),
                               names_to='QExp',values_to='deviation') %>% 
  mutate(QExp = factor(QExp, levels = c(levels_belief, levels_experience))) %>% 
  ggplot(aes(x=QExp,y=deviation,color=cluster_both_best,fill=cluster_both_best))+
  stat_summary(geom = 'col', width=0.5)+
  stat_summary(geom = 'errorbar', width=0.5)+
  theme(axis.text.x = element_text(angle=30))+
  coord_flip() + 
  facet_wrap(~cluster_both_best) +
  scale_x_discrete(breaks = c(levels_belief, levels_experience),
                   labels=c(labels_belief, labels_experience)) +
  ggtitle("T2_remembered - T1_experienced (3 cluster using all ratings)") 

lm(Delta_Dat ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(prev_Demographics_age ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(prev_IUS_total ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(CESD_total ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise') # independent measure of curr
lm(shop_after ~ shop_before + cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(work_after ~ work_before + cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')

lm(NA_mean_prev_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_curr_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_prev_belief ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_curr_belief ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_prev_belief ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_curr_belief ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_expectation_deviation_experience ~ cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(NA_mean_expectation_deviation_experience ~ NA_mean_prev_experience + cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')

lm(NA_mean_expectation_deviation_belief ~ NA_mean_prev_belief + cluster_both_best, Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')

lm(curr_soc_mean ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(prev_soc_mean ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(financial_loss_cont ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(personal_loss ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(Q15_Conf_1 ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(Q16_Conf_1 ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(add_symptom_diagnosis_self ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')
lm(add_symptom_diagnosis_other ~ cluster_both_best , Complete_test_wide) %>% emmeans(specs='cluster_both_best') %>% contrast('pairwise')



lm(delta_DASS ~ cluster_kmeans, Complete_test_wide) %>% Anova
lm(delta_DASS ~ cluster_both, Complete_test_wide) %>% plot_model(type='eff')


Complete_test_wide %>% pivot_longer(cols=c('PSS_total','prev_PSS_total','DASS_total', 'prev_DASS_total'),
                               names_to='QExp',values_to='deviation') %>% 
  ggplot(aes(x=QExp,y=deviation,color=cluster_both_best,fill=cluster_both_best))+
  stat_summary(geom = 'col', aes(width=0.3))+
  stat_summary(geom = 'errorbar',aes(width=0.3))+
  coord_flip() + 
  facet_wrap(~cluster_both_best) +
  ggtitle("T2_experienced")

lmer(remembered ~ true_prev + Q3 + Q4 + Q5 + delta_MostDifficult + delta_RightNow + delta_Future + (1|Subject) + (1|item), df.item_dev_prev) %>% summary
lmer(remembered ~ true_prev + avg_self + avg_delta + (1|Subject) + (1|item), df.item_dev_prev %>% mutate(avg_delta = avg_other - avg_self)) %>% summary

lmer(remembered ~ (true_prev + Conf) * type + (1|Subject) + (1|item), 
     df.item_dev_prev  ) %>% Anova # I think Johnny has taken care of it.

lm(deviation ~ delta_MostDifficult + (1|Subject), df.item_dev_prev) %>% summary
Complete_test_wide

Complete_test_wide$avg_delta <- Complete_test_wide$avg_other - Complete_test_wide$avg_self
lm(delta_pca_score ~ NA_mean_expectation_deviation_experience +
     delta_MostDifficult + delta_RightNow + delta_Future + Q3 + Q4 + Q5, Complete_test_wide) %>% summary
lm(delta_pca_score ~ NA_mean_expectation_deviation_experience +
     delta_MostDifficult + delta_RightNow + delta_Future + Q3 + Q4 + Q5, Complete_test_wide) %>% 
  plot_model(type='eff', terms='delta_RightNow')
lm(delta_pca_score ~ NA_mean_expectation_deviation_experience +
     Q3 + Q4 + Q5 + Q6 + Q7 + Q8, Complete_test_wide) %>% 
  plot_model(type='eff', terms='delta_RightNow')
lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q3 + Q4 + Q5 + Q6 + Q7 + Q8, Complete_test_wide) %>% Anova
lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q3 + Q4 + Q5 + delta_MostDifficult + delta_RightNow + delta_Future, Complete_test_wide) %>% Anova

lm(delta_pca_score ~ NA_mean_expectation_deviation_experience + curr_pca_score + 
     Q3 + Q4 + Q5 + delta_MostDifficult + delta_RightNow + delta_Future, Complete_test_wide) %>% summary
lm(delta_pca_score ~ NA_mean_expectation_deviation_experience * (delta_MostDifficult + delta_RightNow + delta_Future), Complete_test_wide) %>% summary

lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q3 + Q4 + Q5 + delta_MostDifficult + delta_RightNow + delta_Future, Complete_test_wide) %>% 
  plot_model_IVs(variable_array=c('delta_Future','delta_RightNow'))
lm(delta_soc ~ NA_mean_expectation_deviation_experience + prev_soc_mean +
     Q3 + Q4 + Q5 + delta_MostDifficult + delta_RightNow + delta_Future, Complete_test_wide) %>% 
  summary

lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q6 + Q7 + Q8 + delta_MostDifficult + delta_RightNow + delta_Future, Complete_test_wide) %>% Anova

lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q3*Q6 + Q4*Q7 + Q5*Q8, Complete_test_wide) %>% Anova
lm(delta_soc ~ NA_mean_expectation_deviation_experience +
     Q3*Q6 + Q4*Q7 + Q5*Q8, Complete_test_wide) %>% 
  plot_model_IVs(variable_array=c('Q6','Q7','Q8'))


lm(delta_pca_score ~ NA_mean_expectation_deviation_experience +
     delta_MostDifficult + delta_RightNow + delta_Future + Q6 + Q7 + Q8, Complete_test_wide) %>% 
  plot_model(type='eff', terms='delta_RightNow')


# ok. if we only focus on experience, we now arbitrarily think that the first four question are social-related 