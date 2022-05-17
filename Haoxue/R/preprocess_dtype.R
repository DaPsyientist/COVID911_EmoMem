############### R code to preprocess combined dataset
# written by Haoxue
# this script preprocess merged data (COVIDdata_merged_unpreprocessed.csv)
# (1) convert char to double
# (2) generate columns start with prefix 'NA_' which treat people answering 'N/A' as NA

source('utils/utils.R')

All_Data <- read_csv('../data/COVIDdata_merged_unpreprocessed.csv')

### For T2_experienced, T1_remembered and confidence questions, recode people choosing 'N/A' as 0
### Then convert the data from char to double (so people w/n data is NA, people choosing N/A is 0)
All_Data <- All_Data %>% 
  mutate(across(Q3:Q16_Conf_1, function(x){ifelse(x=="N/A",'0',x)})) %>% 
  mutate(across(Q3:Q16_Conf_1, function(x){str_extract(x,'[0-9]') %>% as.double})) 

### Convert T2 emotion well-being, social well-being and other behavior change data from char to double
All_Data <- All_Data %>%   
  mutate(across(PSS_1:DASS_21, function(x){str_extract(x,'[0-9]') %>% as.double})) %>% 
  mutate(across(Q23_1:Q23_7, function(x){str_extract(x,'[0-9]') %>% as.double})) %>% 
  mutate(across(Q24_1:Q24_9, function(x){str_extract(x,'[0-9]') %>% as.double}))  

### Convert avoidNews from char to double (rename as avoid_last_month and avoid_peak month)
All_Data$avoid_last_month <- All_Data %>% select(Q47) %>% apply(2, function(x){str_extract(x,'[0-9]')}) %>% as.double()
All_Data$avoid_peak_month <- All_Data %>% select(Q41) %>% apply(2, function(x){str_extract(x,'[0-9]')}) %>% as.double()

### Because we do not use people respond 'N/A', we create new columns with prefix 'NA_' in which we treat people answering 'N/A' as NA
All_Data <- All_Data %>%   
  mutate(across(prev_COVID_20_stress_lifechanges_1:prev_COVID_36_stress_lifechanges_17, #T1_experienced experience-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'),
         across(prev_COVID_37_stress_beliefs_1:prev_COVID_41_stress_beliefs_5, #T1_experienced belief-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'),
         across(Q15_1:Q15_17, #T1_remembered experience-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'),
         across(Q16_1:Q16_5, #T1_remembered belief-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'),
         across(Q12_1:Q12_17, #T2_experienced experience-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'),
         across(Q13_1:Q13_5, #T2_experienced belief-related
                function(x){x=ifelse(x==0, NA, x)}, .names='NA_{.col}'))

### recode CESD
All_Data <- All_Data %>% 
  mutate(across(starts_with('CESD'), 
                function(x) {case_when(x =="Rarely or none of the time (less than 1 day )" ~ 0 ,
                                       x == "Some or a little of the time (1-2 days)" ~ 1,
                                       x == "Occasionally or a moderate amount of time (3-4 days)" ~ 2,
                                       x == "Most or all of the time (5-7 days)" ~ 3)}))

### recode personal relevance variables and attention variables and assign new names
All_Data <- All_Data %>% 
  mutate(diagnosis_self = ifelse(Q29=="Yes",1,
                                 ifelse(Q29=="No",0,NA)),
         diagnosis_other = ifelse(Q30=="Yes",1,
                                  ifelse(Q30=="No",0,NA)),
         add_symptom_diagnosis_self = diagnosis_self + prev_COVID_4_self_symptoms,
         add_symptom_diagnosis_other = diagnosis_other + prev_COVID_5_others_symptoms,
         personal_loss = ifelse(Q33=="Yes",1,
                                ifelse(Q33=="No",0,NA)),
         financial_loss_cont = ifelse(Q35=="Minimal (not require any adjustment in lifestyle or future planning and goals)",1,
                                      ifelse(Q35=="Moderate (require major lifestyle changes and some minor adjustments in future planning and goals)",2,
                                             ifelse(Q35=="Severe (require major lifestyle changes, future planning and goals are at risk)",3,
                                                    ifelse(Q35=="Devastating (complete loss of financial assets, require continued or soliciting dependence on others or government)",4,
                                                           0)))),
         relocate = ifelse(Q26=="Yes",1,
                           ifelse(Q26=="No",0,NA)),
         job_change = ifelse(str_detect(Q27, 'I changed my job')|str_detect(Q27, 'I lost my job'), 1, 
                             ifelse(str_detect(Q27, 'Prefer not to answer'), NA, 0)), # data dirty, need further validation
         communicate_last_month = ifelse(Q43 == 'Less than 30 minutes', 1, 
                                         ifelse(Q43 == '30 minutes - 1 hour', 2,
                                                ifelse(Q43 == '1-2 hours', 3,
                                                       ifelse(Q43 == '2-4 hours', 4,
                                                              ifelse(Q43 == '4+ hours', 5, NA))))),
         media_last_month = ifelse(Q44 == 'Less than 30 minutes', 1, 
                                   ifelse(Q44 == '30 minutes - 1 hour', 2,
                                          ifelse(Q44 == '1-2 hours', 3,
                                                 ifelse(Q44 == '2-4 hours', 4,
                                                        ifelse(Q44 == '4+ hours', 5, NA))))),
         communicate_peak_month = ifelse(Q37 == 'Less than 30 minutes', 1, 
                                         ifelse(Q37 == '30 minutes - 1 hour', 2,
                                                ifelse(Q37 == '1-2 hours', 3,
                                                       ifelse(Q37 == '2-4 hours', 4,
                                                              ifelse(Q37 == '4+ hours', 5, NA))))),
         media_peak_month = ifelse(Q38 == 'Less than 30 minutes', 1, 
                                   ifelse(Q38 == '30 minutes - 1 hour', 2,
                                          ifelse(Q38 == '1-2 hours', 3,
                                                 ifelse(Q38 == '2-4 hours', 4,
                                                        ifelse(Q38 == '4+ hours', 5, NA))))))

### recode behavior before/after covid and assign new names
All_Data <- All_Data %>% 
  mutate(work_after = ifelse(Q50 == 'A few times/year', 1,
                             ifelse(Q50 == 'A few times/month', 2,
                                    ifelse(Q50 == 'Once/week', 3, 
                                           ifelse(Q50 == 'A few times/week', 4,
                                                  ifelse(Q50 == 'Every day', 5, 0))))),
         work_before = ifelse(Q19 == 'A few times/year', 1,
                              ifelse(Q19 == 'A few times/month', 2,
                                     ifelse(Q19 == 'Once/week', 3, 
                                            ifelse(Q19 == 'A few times/week', 4,
                                                   ifelse(Q19 == 'Every day', 5, 0))))),
         shop_after = ifelse(Q51 == 'A few times/year', 1,
                             ifelse(Q51 == 'A few times/month', 2,
                                    ifelse(Q51 == 'Once/week', 3, 
                                           ifelse(Q51 == 'A few times/week', 4,
                                                  ifelse(Q51 == 'Every day', 5,
                                                         ifelse(Q51 == 'A few times/day', 6, 0)))))),
         shop_before = ifelse(Q20 == 'A few times/year', 1,
                              ifelse(Q20 == 'A few times/month', 2,
                                     ifelse(Q20 == 'Once/week', 3, 
                                            ifelse(Q20 == 'A few times/week', 4,
                                                   ifelse(Q20 == 'Every day', 5,
                                                          ifelse(Q20 == 'A few times/day', 6, 0))))))
  )

All_Data  %>% write_csv('../data/COVIDdata_merged_dtype_changed.csv')

