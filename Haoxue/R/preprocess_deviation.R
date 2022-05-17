############### R code to calculate key var of interests (i.e., deviation)
# written by Haoxue
# this script calculate variables of interest based on merged data (dypte changed) (COVIDdata_merged_dtype_changed.csv)
# signed and unsigned deviation, emotion and social well-being total, and others

source('utils/utils.R')

All_Data  <- read.csv('../data/COVIDdata_merged_dtype_changed.csv')

### signed and unsigned deviance (N/A is treated as 0)
All_Data <- All_Data %>% mutate(expectation_deviation_experience1  = Q15_1 -prev_COVID_20_stress_lifechanges_1 ,
                                expectation_deviation_experience2  = Q15_2 -prev_COVID_21_stress_lifechanges_2 ,
                                expectation_deviation_experience3  = Q15_3 -prev_COVID_22_stress_lifechanges_3 ,
                                expectation_deviation_experience4  = Q15_4 -prev_COVID_23_stress_lifechanges_4 ,
                                expectation_deviation_experience5  = Q15_5 -prev_COVID_24_stress_lifechanges_5 ,
                                expectation_deviation_experience6  = Q15_6 -prev_COVID_25_stress_lifechanges_6 ,
                                expectation_deviation_experience7  = Q15_7 -prev_COVID_26_stress_lifechanges_7 ,
                                expectation_deviation_experience8  = Q15_8 -prev_COVID_27_stress_lifechanges_8 ,
                                expectation_deviation_experience9  = Q15_9 -prev_COVID_28_stress_lifechanges_9 ,
                                expectation_deviation_experience10 = Q15_10-prev_COVID_29_stress_lifechanges_10,
                                expectation_deviation_experience11 = Q15_11-prev_COVID_30_stress_lifechanges_11,
                                expectation_deviation_experience12 = Q15_12-prev_COVID_31_stress_lifechanges_12,
                                expectation_deviation_experience13 = Q15_13-prev_COVID_32_stress_lifechanges_13,
                                expectation_deviation_experience14 = Q15_14-prev_COVID_33_stress_lifechanges_14,
                                expectation_deviation_experience15 = Q15_15-prev_COVID_34_stress_lifechanges_15,
                                expectation_deviation_experience16 = Q15_16-prev_COVID_35_stress_lifechanges_16,
                                expectation_deviation_experience17 = Q15_17-prev_COVID_36_stress_lifechanges_17,
                                expectation_deviation_belief1  = Q16_1 -prev_COVID_37_stress_beliefs_1 ,
                                expectation_deviation_belief2  = Q16_2 -prev_COVID_38_stress_beliefs_2 ,
                                expectation_deviation_belief3  = Q16_3 -prev_COVID_39_stress_beliefs_3 ,
                                expectation_deviation_belief4  = Q16_4 -prev_COVID_40_stress_beliefs_4 ,
                                expectation_deviation_belief5  = Q16_5 -prev_COVID_41_stress_beliefs_5,
) %>% 
  mutate(across(starts_with('expectation_deviation'), function(x){abs(x)}, .names='abs_{.col}'))

### signed and unsigned deviance (N/A is treated as NA)
All_Data <- All_Data %>% mutate(NA_expectation_deviation_experience1  = NA_Q15_1 -NA_prev_COVID_20_stress_lifechanges_1 ,
                                NA_expectation_deviation_experience2  = NA_Q15_2 -NA_prev_COVID_21_stress_lifechanges_2 ,
                                NA_expectation_deviation_experience3  = NA_Q15_3 -NA_prev_COVID_22_stress_lifechanges_3 ,
                                NA_expectation_deviation_experience4  = NA_Q15_4 -NA_prev_COVID_23_stress_lifechanges_4 ,
                                NA_expectation_deviation_experience5  = NA_Q15_5 -NA_prev_COVID_24_stress_lifechanges_5 ,
                                NA_expectation_deviation_experience6  = NA_Q15_6 -NA_prev_COVID_25_stress_lifechanges_6 ,
                                NA_expectation_deviation_experience7  = NA_Q15_7 -NA_prev_COVID_26_stress_lifechanges_7 ,
                                NA_expectation_deviation_experience8  = NA_Q15_8 -NA_prev_COVID_27_stress_lifechanges_8 ,
                                NA_expectation_deviation_experience9  = NA_Q15_9 -NA_prev_COVID_28_stress_lifechanges_9 ,
                                NA_expectation_deviation_experience10 = NA_Q15_10-NA_prev_COVID_29_stress_lifechanges_10,
                                NA_expectation_deviation_experience11 = NA_Q15_11-NA_prev_COVID_30_stress_lifechanges_11,
                                NA_expectation_deviation_experience12 = NA_Q15_12-NA_prev_COVID_31_stress_lifechanges_12,
                                NA_expectation_deviation_experience13 = NA_Q15_13-NA_prev_COVID_32_stress_lifechanges_13,
                                NA_expectation_deviation_experience14 = NA_Q15_14-NA_prev_COVID_33_stress_lifechanges_14,
                                NA_expectation_deviation_experience15 = NA_Q15_15-NA_prev_COVID_34_stress_lifechanges_15,
                                NA_expectation_deviation_experience16 = NA_Q15_16-NA_prev_COVID_35_stress_lifechanges_16,
                                NA_expectation_deviation_experience17 = NA_Q15_17-NA_prev_COVID_36_stress_lifechanges_17,
                                NA_expectation_deviation_belief1  = NA_Q16_1 -NA_prev_COVID_37_stress_beliefs_1 ,
                                NA_expectation_deviation_belief2  = NA_Q16_2 -NA_prev_COVID_38_stress_beliefs_2 ,
                                NA_expectation_deviation_belief3  = NA_Q16_3 -NA_prev_COVID_39_stress_beliefs_3 ,
                                NA_expectation_deviation_belief4  = NA_Q16_4 -NA_prev_COVID_40_stress_beliefs_4 ,
                                NA_expectation_deviation_belief5  = NA_Q16_5 -NA_prev_COVID_41_stress_beliefs_5,
) %>% 
  mutate(across(starts_with('NA_expectation_deviation'), function(x){abs(x)}, .names='abs_{.col}'))

### signed and unsigned aggregated measure (N/A is treated as 0)
All_Data <- All_Data %>% 
  mutate(
    # avg T2_experienced
    mean_curr_experience = 
      All_Data %>% select(starts_with('Q12_') & !contains('Conf')) %>% rowMeans,
    mean_curr_belief = 
      All_Data %>% select(starts_with('Q13_') & !contains('Conf')) %>% rowMeans,
    mean_curr = 
      All_Data %>% select((starts_with('Q12_')|starts_with('Q13_')) & !contains('Conf')) %>% rowMeans,
    # avg T1_experienced
    mean_prev_experience = 
      All_Data %>% select(contains('stress_lifechanges') & !starts_with('NA')) %>% rowMeans,
    mean_prev_belief = 
      All_Data %>% select(contains('stress_beliefs') & !starts_with('NA')) %>% rowMeans,
    mean_prev = 
      All_Data %>% select((contains('stress_lifechanges') | contains('stress_beliefs')) & !starts_with('NA')) %>% rowMeans,
    mean_remembered_experience = 
      All_Data %>% select(starts_with('Q15_') & !contains('Conf')) %>% rowMeans,
    mean_remembered_belief = 
      All_Data %>% select(starts_with('Q16_') & !contains('Conf')) %>% rowMeans,
    mean_remembered = 
      All_Data %>% select((starts_with('Q15_')|starts_with('Q16_')) & !contains('Conf')) %>% rowMeans,
    mean_expectation_deviation_experience =
      All_Data %>% select(starts_with('expectation_deviation_experience')) %>% rowMeans,
    mean_expectation_deviation_belief =
      All_Data %>% select(starts_with('expectation_deviation_belief')) %>% rowMeans,
    mean_expectation_deviation = 
      All_Data %>% select(starts_with('expectation_deviation_experience')|starts_with('expectation_deviation_belief')) %>% rowMeans,
    mean_abs_expectation_deviation_experience =
      All_Data %>% select(starts_with('abs_expectation_deviation_experience')) %>% rowMeans,
    mean_abs_expectation_deviation_belief =
      All_Data %>% select(starts_with('abs_expectation_deviation_belief')) %>% rowMeans,
    mean_abs_expectatin_deviation = 
      All_Data %>% select(starts_with('abs_expectation_deviation_experience')|starts_with('abs_expectation_deviation_belief')) %>% rowMeans,
    delta_experience = mean_curr_experience - mean_prev_experience,
    delta_belief = mean_curr_belief - mean_prev_belief)
    
### signed and unsigned aggregated measure (N/A is treated as NA, exclude those w/ any NA)   
All_Data <- All_Data %>% 
  mutate(
    # avg T2_experienced
    NA_mean_curr_experience = 
      All_Data %>% select(starts_with('NA_Q12_') & !contains('Conf')) %>% rowMeans,
    NA_mean_curr_belief = 
      All_Data %>% select(starts_with('NA_Q13_') & !contains('Conf')) %>% rowMeans,
    NA_mean_curr = 
      All_Data %>% select((starts_with('NA_Q12_')|starts_with('NA_Q13_')) & !contains('Conf')) %>% rowMeans,
    # avg T1_experienced
    NA_mean_prev_experience = 
      All_Data %>% select(contains('stress_lifechanges') & starts_with('NA')) %>% rowMeans,
    NA_mean_prev_belief = 
      All_Data %>% select(contains('stress_beliefs') & starts_with('NA')) %>% rowMeans,
    NA_mean_prev = 
      All_Data %>% select((contains('stress_lifechanges') | contains('stress_beliefs')) & starts_with('NA')) %>% rowMeans,
    NA_mean_remembered_experience = 
      All_Data %>% select(starts_with('NA_Q15_') & !contains('Conf')) %>% rowMeans,
    NA_mean_remembered_belief = 
      All_Data %>% select(starts_with('NA_Q16_') & !contains('Conf')) %>% rowMeans,
    NA_mean_remembered = 
      All_Data %>% select((starts_with('NA_Q15_')|starts_with('NA_Q16_')) & !contains('Conf')) %>% rowMeans,
    NA_mean_expectation_deviation_experience =
      All_Data %>% select(starts_with('NA_expectation_deviation_experience')) %>% rowMeans,
    NA_mean_expectation_deviation_belief =
      All_Data %>% select(starts_with('NA_expectation_deviation_belief')) %>% rowMeans,
    NA_mean_expectation_deviation = 
      All_Data %>% select(starts_with('NA_expectation_deviation_experience')|starts_with('NA_expectation_deviation_belief')) %>% rowMeans,
    NA_mean_abs_expectation_deviation_experience =
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_experience')) %>% rowMeans,
    NA_mean_abs_expectation_deviation_belief =
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_belief')) %>% rowMeans,
    NA_mean_abs_expectatin_deviation = 
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_experience')|starts_with('abs_NA_expectation_deviation_belief')) %>% rowMeans,
    NA_delta_experience = NA_mean_curr_experience - NA_mean_prev_experience,
    NA_delta_belief = NA_mean_curr_belief - NA_mean_prev_belief)

### signed and unsigned aggregated measure (N/A is treated as NA, exclude NA items)  
#   (one caveat here: we did not record the number of options)
All_Data <- All_Data %>% 
  mutate(
    # avg T2_experienced
    item_NA_mean_curr_experience = 
      All_Data %>% select(starts_with('NA_Q12_') & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_curr_belief = 
      All_Data %>% select(starts_with('NA_Q13_') & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_curr = 
      All_Data %>% select((starts_with('NA_Q12_')|starts_with('NA_Q13_')) & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    # avg T1_experienced
    item_NA_mean_prev_experience = 
      All_Data %>% select(contains('stress_lifechanges') & starts_with('NA')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_prev_belief = 
      All_Data %>% select(contains('stress_beliefs') & starts_with('NA')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_prev = 
      All_Data %>% select((contains('stress_lifechanges') | contains('stress_beliefs')) & starts_with('NA')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_remembered_experience = 
      All_Data %>% select(starts_with('NA_Q15_') & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_remembered_belief = 
      All_Data %>% select(starts_with('NA_Q16_') & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_remembered = 
      All_Data %>% select((starts_with('NA_Q15_')|starts_with('NA_Q16_')) & !contains('Conf')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_expectation_deviation_experience =
      All_Data %>% select(starts_with('NA_expectation_deviation_experience')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_expectation_deviation_belief =
      All_Data %>% select(starts_with('NA_expectation_deviation_belief')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_expectation_deviation = 
      All_Data %>% select(starts_with('NA_expectation_deviation_experience')|starts_with('NA_expectation_deviation_belief')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_abs_expectation_deviation_experience =
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_experience')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_abs_expectation_deviation_belief =
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_belief')) %>% rowMeans(na.rm=TRUE),
    item_NA_mean_abs_expectatin_deviation = 
      All_Data %>% select(starts_with('abs_NA_expectation_deviation_experience')|starts_with('abs_NA_expectation_deviation_belief')) %>% rowMeans(na.rm=TRUE),
    item_NA_delta_experience = item_NA_mean_curr_experience - item_NA_mean_prev_experience,
    item_NA_delta_belief = item_NA_mean_curr_belief - item_NA_mean_prev_belief)


### emotion well-being (and other personal traits)
All_Data$CESD_total <- All_Data %>% select(starts_with('CESD_')) %>% rowSums()

All_Data$PSS_total <- # some of PSS items are reverse coded
  All_Data$PSS_1 + All_Data$PSS_2 + All_Data$PSS_3 + 
  (4 -All_Data$PSS_4) + (4-All_Data$PSS_5) + All_Data$PSS_6 + 
  (4-All_Data$PSS_7) + (4-All_Data$PSS_8) + All_Data$PSS_9 + 
  All_Data$PSS_10

All_Data$DASS_total <- All_Data %>% select(starts_with('DASS_')) %>% rowSums()
All_Data$DASS_Stress_total <- All_Data$DASS_1 + All_Data$DASS_6 + All_Data$DASS_8 + 
  All_Data$DASS_11 + All_Data$DASS_12 + All_Data$DASS_14 + All_Data$DASS_18
All_Data$DASS_Depression_total <- All_Data$DASS_3 + All_Data$DASS_5 + All_Data$DASS_10 + 
  All_Data$DASS_13 + All_Data$DASS_16 + All_Data$DASS_17 + All_Data$DASS_21
All_Data$DASS_Anxiety_total <- All_Data$DASS_2 + All_Data$DASS_4 + All_Data$DASS_7 + 
  All_Data$DASS_9 + All_Data$DASS_15 + All_Data$DASS_19 + All_Data$DASS_20

All_Data$prev_PSS_total <- All_Data %>% select(starts_with('prev_PSS_')) %>% rowSums()
All_Data$prev_IUS_total <- All_Data %>% select(starts_with('prev_IUS_')) %>% rowSums()
All_Data$prev_IUS_Factor1_total <- All_Data %>% select(starts_with('prev_IUS_') & ends_with('Factor1')) %>% rowSums()
All_Data$prev_IUS_Factor2_total <- All_Data %>% select(starts_with('prev_IUS_') & ends_with('Factor2')) %>% rowSums()
All_Data$prev_NEO_extraversion_total <- All_Data %>% select(starts_with('prev_NEO_') & contains('extraversion')) %>% rowSums()
All_Data$prev_NEO_conscientiousness_total <- All_Data %>% select(starts_with('prev_NEO_') & contains('conscientiousness')) %>% rowSums()
All_Data$prev_NEO_emotionalStability_total <- All_Data %>% select(starts_with('prev_NEO_') & contains('emotionalStability')) %>% rowSums()
All_Data$prev_NEO_intellectImagination_total <- All_Data %>% select(starts_with('prev_NEO_') & contains('intellectImagination')) %>% rowSums()
All_Data$prev_NEO_agreeableness_total <- All_Data %>% select(starts_with('prev_NEO_') & contains('agreeableness')) %>% rowSums()

All_Data$prev_DASS_total <- All_Data %>% select(starts_with('prev_DASS21_')) %>% rowSums()
All_Data$prev_DASS_Stress_total <- All_Data %>% select(starts_with('prev_DASS21_') & ends_with('Stress')) %>% rowSums()
All_Data$prev_DASS_Depression_total <- All_Data %>% select(starts_with('prev_DASS21_') & ends_with('Depression')) %>% rowSums()
All_Data$prev_DASS_Anxiety_total <- All_Data %>% select(starts_with('prev_DASS21_') & ends_with('Anxiety')) %>% rowSums()

### social well-being (some are reverse coded)
All_Data$curr_soc_mean <- All_Data %>% select((contains('Q23_'))) %>% 
  mutate(Q23_4=6-Q23_4, Q23_6=6-Q23_6, Q23_7=6-Q23_7) %>% 
  rowMeans()

All_Data$prev_soc_mean <- All_Data %>% select((contains('changesCOVID') & contains('prev'))) %>% 
  mutate(prev_COVID_67_changesCOVID_4_r=6-prev_COVID_67_changesCOVID_4_r,
         prev_COVID_69_changesCOVID_6_r=6-prev_COVID_69_changesCOVID_6_r,
         prev_COVID_70_changesCOVID_7_r=6-prev_COVID_70_changesCOVID_7_r) %>% 
  rowMeans()


### delta emotion and social well-being
All_Data <- All_Data %>% 
  mutate(delta_PSS = PSS_total - prev_PSS_total, 
         delta_DASS = DASS_total - prev_DASS_total,
         delta_DASS_Stress = DASS_Stress_total - prev_DASS_Stress_total,
         delta_DASS_Depression = DASS_Depression_total - prev_DASS_Depression_total,
         delta_DASS_Anxiety = DASS_Anxiety_total - prev_DASS_Anxiety_total,
         delta_soc = curr_soc_mean - prev_soc_mean)

### delta date
All_Data$Delta_Dat = as.Date(All_Data$StartDate)-as.Date(All_Data$Dat)

### add PCA (though Deshawn mentioned that it is better to do an EFA?)

df.prev_pca <- All_Data %>% select(prev_SubjectID_Prolific_MTurk, starts_with('prev_PSS') & !contains('total'), starts_with('prev_DASS') & !contains('total')) %>% na.omit 
prev_pca <- df.prev_pca %>% select(-prev_SubjectID_Prolific_MTurk) %>% 
  prcomp(center=TRUE, scale. =TRUE)

df.curr_pca <- All_Data %>% select(prev_SubjectID_Prolific_MTurk, 
                                   starts_with('PSS') & !contains('total') & !contains('prev_'), 
                                   starts_with('DASS') & !contains('total') & !contains('prev')) %>% 
  na.omit %>% 
  mutate(PSS_4=4-PSS_4, PSS_5=4-PSS_5, PSS_7=4-PSS_7, PSS_8=4-PSS_8) %>% 
  set_colnames(colnames(df.prev_pca))

df.prev_pca$prev_pca_score <- (prev_pca$x[,1]) * (-1)
df.curr_pca$curr_pca_score <- ((prev_pca %>% predict(df.curr_pca %>% select(-prev_SubjectID_Prolific_MTurk)))[,1]) * (-1)

All_Data <- All_Data %>% left_join(df.prev_pca %>% select(prev_SubjectID_Prolific_MTurk, prev_pca_score)) %>% 
  left_join(df.curr_pca %>% select(prev_SubjectID_Prolific_MTurk, curr_pca_score)) %>% 
  mutate(delta_pca_score = curr_pca_score - prev_pca_score)

#df.prev_pca_covid <- All_Data %>% select(prev_SubjectID_Prolific_MTurk, prev_COVID_20_stress_lifechanges_1:prev_COVID_41_stress_beliefs_5) %>% na.omit 
#prev_pca_covid <- df.prev_pca_covid %>% select(-prev_SubjectID_Prolific_MTurk) %>% 
#  prcomp(center=TRUE, scale. =TRUE)
#df.prev_pca_covid$prev_pca_score_covid <- (prev_pca_covid$x[,1]) * (-1)
#
#prev_pca_covid$rotation[,1] %>% as.data.frame() %>% 
#  set_colnames('value') %>% rownames_to_column('scale') %>% 
#  ggplot(aes(x=reorder(scale,value),y=-value)) + 
#  geom_col(fill='lightblue') + 
#  ylab('Loading on Dimension 1') + 
#  coord_flip() 
#
#

### Stress Perception most difficult time / now / in the future, delta (self-other)

All_Data <- All_Data %>% 
  mutate(delta_RightNow = Q3-Q6,
         delta_MostDifficult = Q4-Q7,
         delta_Future = Q5-Q8,
         avg_self = (Q3+Q4+Q5)/3,
         avg_other = (Q6+Q7+Q8)/3)

### tidy up social behavior before/after
All_Data <- All_Data %>% mutate(
  across(Q21_1:Q21_5, function(x){case_when(x=='less than 1 day' ~ 0.5,
                                            x=='at least 1 day' ~ 1,
                                            x=='2-3 days' ~ 2.5,
                                            x=='4-5 days' ~ 4.5,
                                            x=='every day or nearly every day' ~ 7)},
         .names = "cont_est_{.col}"),
  across(Q52_1:Q52_5, function(x){case_when(x=='less than 1 day' ~ 0.5,
                                            x=='at least 1 day' ~ 1,
                                            x=='2-3 days' ~ 2.5,
                                            x=='4-5 days' ~ 4.5,
                                            x=='every day or nearly every day' ~ 7)},
         .names = "cont_est_{.col}"),
  across(Q22_1:Q22_5, function(x){case_when(x=='less than 30 minutes' ~ 0.15,
                                            x=='30 minutes - 1 hour' ~ 0.45,
                                            x=='1-2 hours' ~ 1.5,
                                            x=='2-4 hours' ~ 3,
                                            x=='4+ hours' ~ 4)},
         .names = "cont_est_{.col}"),
  across(Q53_1:Q53_5, function(x){case_when(x=='less than 30 minutes' ~ 0.15,
                                            x=='30 minutes - 1 hour' ~ 0.45,
                                            x=='1-2 hours' ~ 1.5,
                                            x=='2-4 hours' ~ 3,
                                            x=='4+ hours' ~ 4)},
         .names = "cont_est_{.col}"),
  across(Q21_1:Q21_5, function(x){case_when(x=='less than 1 day' ~ 1,
                                            x=='at least 1 day' ~ 2,
                                            x=='2-3 days' ~ 3,
                                            x=='4-5 days' ~ 4,
                                            x=='every day or nearly every day' ~ 5)}),
  across(Q52_1:Q52_5, function(x){case_when(x=='less than 1 day' ~ 1,
                                            x=='at least 1 day' ~ 2,
                                            x=='2-3 days' ~ 3,
                                            x=='4-5 days' ~ 4,
                                            x=='every day or nearly every day' ~ 5)}),
  across(Q22_1:Q22_5, function(x){case_when(x=='less than 30 minutes' ~ 1,
                                            x=='30 minutes - 1 hour' ~ 2,
                                            x=='1-2 hours' ~ 3,
                                            x=='2-4 hours' ~ 4,
                                            x=='4+ hours' ~ 5)}),
  across(Q53_1:Q53_5, function(x){case_when(x=='less than 30 minutes' ~ 1,
                                            x=='30 minutes - 1 hour' ~ 2,
                                            x=='1-2 hours' ~ 3,
                                            x=='2-4 hours' ~ 4,
                                            x=='4+ hours' ~ 5)})
)
All_Data <- All_Data %>% mutate(
  Tbefore1 = cont_est_Q21_1 * cont_est_Q22_1,
  Tbefore2 = cont_est_Q21_2 * cont_est_Q22_2,
  Tbefore3 = cont_est_Q21_3 * cont_est_Q22_3,
  Tbefore4 = cont_est_Q21_4 * cont_est_Q22_4,
  Tbefore5 = cont_est_Q21_5 * cont_est_Q22_5,
  Tafter1 = cont_est_Q52_1 * cont_est_Q53_1,
  Tafter2 = cont_est_Q52_2 * cont_est_Q53_2,
  Tafter3 = cont_est_Q52_3 * cont_est_Q53_3,
  Tafter4 = cont_est_Q52_4 * cont_est_Q53_4,
  Tafter5 = cont_est_Q52_5 * cont_est_Q53_5,
)
All_Data %>% write_csv('../data/COVIDdata_merged_Haoxue_version.csv')

### GMM
df.mclust_withSub <- Complete_test_wide %>% select(prev_SubjectID_Prolific_MTurk, starts_with('NA_expectation_deviation_experience')) %>% na.omit() 

df.mclust_exp <- df.mclust_withSub %>% select(-prev_SubjectID_Prolific_MTurk)

set.seed(2021) # has randomness

clustbic_both <- mclustBIC(df.mclust_exp, G = 2:10)
clusfit_both_best <- Mclust(df.mclust_exp, x = clustbic_both)   ## re-fit model with best BIC value
#summary(clusfit_both_best, parameters = TRUE)
#head(round(clusfit_both_best$z, 3))   ## soft cluster memberships
cluster_both_best <- as.factor(clusfit_both_best$classification)  ## hard memberships
df.cluster_both_best_withSub = cbind(prev_SubjectID_Prolific_MTurk=df.mclust_withSub$prev_SubjectID_Prolific_MTurk, cluster_both_best)
All_Data <- All_Data %>% merge(df.cluster_both_best_withSub, all=TRUE)

## PCA on solution for lower-dimensional representation
#clustred_both_best <- MclustDR(clusfit_both_best)
#plot(clustred_both_best, what = "boundaries", ngrid = 200)      ## 2D representation
#plot(clustred_both_best, what = "density", dimens = 1)          ## 1D representation

###
All_Data %>% write_csv('../data/COVIDdata_merged_Haoxue_version_mclust.csv')


