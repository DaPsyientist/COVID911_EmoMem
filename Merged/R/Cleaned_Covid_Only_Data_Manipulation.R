
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

source('utils.R')
#### Data Preparation ####
### Timepoint 2 Data ###
#Load Data - Macbook
Prolific_Data_Original <- read.csv("../data-raw/COVID_Prolific.csv", header = TRUE) #Import data; prolific
MTurk_Data_Original <- read.csv("../data-raw/COVID_MTurk_rename.csv", header = TRUE) #Import data; MTurk

#Only keep data that has 100% Complete Rate & Remove potential headers
Prolific_Data <- Prolific_Data_Original %>% filter(Progress == '100')
MTurk_Data <- MTurk_Data_Original %>% filter(Progress == '100')

#Filter Data
Prolific_Filtered <- Prolific_Data %>% dplyr::select(contains(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16")))
MTurk_Filtered <- MTurk_Data %>% dplyr::select(contains(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16")))

Prolific_Filtered <- Prolific_Data %>% 
  mutate(across(Q3:Q8, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q10_1:Q10_4, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q12_1:Q12_17, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q13_1:Q13_5, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q15_1:Q15_17, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q16_1:Q16_5, function(x){as.double(str_extract(x,'[0-9]'))}))

MTurk_Filtered <- MTurk_Data %>% 
  mutate(across(Q3:Q8, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q10_1:Q10_4, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q12_1:Q12_17, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q13_1:Q13_5, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q15_1:Q15_17, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(Q16_1:Q16_5, function(x){as.double(str_extract(x,'[0-9]'))}))

All_Data <- Prolific_Filtered %>% full_join(MTurk_Filtered)
All_Data_Ans <- All_Data %>% rename(Subject = Q2)
All_Data_Ans[All_Data_Ans == ""] <- NA
All_Data_Ans[All_Data_Ans == "N/A"] <- NA
All_Data_Ans$Subject[All_Data_Ans$Subject == ' https://p8pkef0k8l.cognition.run/?external_id=5ebc70e8d7a1780bedd1514e'] <- '5ebc70e8d7a1780bedd1514e'
All_Data_Ans <- All_Data_Ans %>% filter(Subject != 'test') %>% filter(!is.na(Subject)) %>% filter(!duplicated(Subject))

### Timepoint 1 Data ###
#Load Data - IMac
Previous_Data <- read.csv("../data-raw/COVIDdata_combined_final_n1810.csv", header = TRUE) #Import data; prolific

###Count Participants###
Previous_Count <- Previous_Data %>% dplyr::select(SubjectID_Prolific_MTurk) %>% 
  filter(SubjectID_Prolific_MTurk != "") %>% filter(SubjectID_Prolific_MTurk != "test") 
Previous_Count$SubjectID_Prolific_MTurk %>% unique %>% length #Total number of participants: 1810

#Make list of participants with data in both dataframes
Run_1_Ppts <- data.frame(Previous_Count$SubjectID_Prolific_MTurk) %>% rename(Subject = Previous_Count.SubjectID_Prolific_MTurk)
Run_2_Ppts <- data.frame(All_Data_Ans$Subject) %>% rename(Subject = All_Data_Ans.Subject)
Both_Run_Ppts <- Run_1_Ppts %>% right_join(Run_2_Ppts) #730 Participants with Data at both points
Sub_Filt <- as_vector(Both_Run_Ppts)


#Filter Data based on available previous timepoints
Previous_Data <- Previous_Data %>% 
  rename(Subject = SubjectID_Prolific_MTurk,
         Q12_1 = COVID_20_stress_lifechanges_1,
         Q12_2 = COVID_21_stress_lifechanges_2,
         Q12_3 = COVID_22_stress_lifechanges_3,
         Q12_4 = COVID_23_stress_lifechanges_4,
         Q12_5 = COVID_24_stress_lifechanges_5,
         Q12_6 = COVID_25_stress_lifechanges_6,
         Q12_7 = COVID_26_stress_lifechanges_7,
         Q12_8 = COVID_27_stress_lifechanges_8,
         Q12_9 = COVID_28_stress_lifechanges_9,
         Q12_10 = COVID_29_stress_lifechanges_10,
         Q12_11 = COVID_30_stress_lifechanges_11,
         Q12_12 = COVID_31_stress_lifechanges_12,
         Q12_13 = COVID_32_stress_lifechanges_13,
         Q12_14 = COVID_33_stress_lifechanges_14,
         Q12_15 = COVID_34_stress_lifechanges_15,
         Q12_16 = COVID_35_stress_lifechanges_16,
         Q12_17 = COVID_36_stress_lifechanges_17,
         Q13_1 = COVID_37_stress_beliefs_1,
         Q13_2 = COVID_38_stress_beliefs_2,
         Q13_3 = COVID_39_stress_beliefs_3,
         Q13_4 = COVID_40_stress_beliefs_4,
         Q13_5 = COVID_41_stress_beliefs_5)
Previous_Data[,paste0('Q12_',c(1:17))][Previous_Data[,paste0('Q12_',c(1:17))]==0] <- NA
Previous_Data[,paste0('Q13_',c(1:5))][Previous_Data[,paste0('Q13_',c(1:5))]==0] <- NA

#Make data types the same
All_Data_Ans <- All_Data_Ans %>% mutate(
  Subject = as.factor(Subject),
  across(Q12_1:Q12_7,as.integer),
  across(Q13_1:Q13_5,as.integer)
)
Previous_Data <- Previous_Data %>% mutate(
  Subject = as.factor(Subject),
  across(Q12_1:Q12_7,as.integer),
  across(Q13_1:Q13_5,as.integer)
)

#Combine Dataframes
All_Data_Ans <- All_Data_Ans %>% mutate(TP = "Now") 
All_Data_Ans <- All_Data_Ans %>% 
  mutate(across(PSS_1:PSS_10, function(x){as.double(str_extract(x,'[0-9]'))}),
         across(DASS_1:DASS_21, function(x){as.double(str_extract(x,'[0-9]'))}))
Previous_Data <- Previous_Data %>% mutate(TP = "Prev")
Previous_Data <- Previous_Data %>% 
  rename(PSS_4 = PSS_4_r, PSS_5 = PSS_5_r, PSS_7 = PSS_7_r, PSS_8 = PSS_8_r,
         DASS_1 = DASS21_1_Stress, DASS_2 = DASS21_2_Anxiety, DASS_3 = DASS21_13_Depression,
         DASS_4 = DASS21_4_Anxiety, DASS_5 = DASS21_5_Depression, DASS_6 = DASS21_6_Stress,
         DASS_7 = DASS21_7_Anxiety, DASS_8 = DASS21_8_Stress, DASS_9 = DASS21_9_Anxiety,
         DASS_10 = DASS21_10_Depression, DASS_11 = DASS21_11_Stress, DASS_12 = DASS21_12_Stress,
         DASS_13 = DASS21_13_Depression, DASS_14 = DASS21_14_Stress, DASS_15 = DASS21_15_Anxiety,
         DASS_16 = DASS21_16_Depression, DASS_17 = DASS21_17_Depression, DASS_18 = DASS21_18_Stress,
         DASS_19 = DASS21_19_Anxiety, DASS_20 = DASS21_20_Anxiety, DASS_21 = DASS21_21_Depression) 
# Complete_test: Each subject has two rows (Prev & Current), Experienced Stress Q12_1:Q12_17; Remembered Stres Q15_1:Q15_17 
Complete_test <- All_Data_Ans %>% full_join(Previous_Data) %>% filter(Subject %in% Sub_Filt) %>% 
  mutate(TP = factor(TP))
Complete_test <- Complete_test %>% mutate(
  across(Q15_1:Q15_17, as.integer),
  across(Q16_1:Q16_5, as.integer),
)
Complete_test %>% write.csv('../data/Complete_test_Only_Data_Manipulation.csv')

# Complete_test_NewcolName: Each subject has one row, separated by prefix 'T1' and 'T2'
All_Data_Ans_NewcolName <- All_Data_Ans %>% set_colnames(paste0('T2_',colnames(All_Data_Ans))) %>% 
  dplyr::select(-T2_TP) %>% rename(Subject = T2_Subject)
Previous_Data_NewcolName <- Previous_Data %>% set_colnames(paste0('T1_',colnames(Previous_Data))) %>% 
  dplyr::select(-T1_TP) %>% rename(Subject = T1_Subject)
Complete_test_NewcolName <- All_Data_Ans_NewcolName %>% full_join(Previous_Data_NewcolName) %>% filter(Subject %in% Sub_Filt)

Complete_test_NewcolName %>% write.csv('../data/Complet_test_NewcolName_Only_Data_Manipulation.csv')

# Complete_test_Dev_Exp: Each subject has one row, with newly calculated var (dev & pca)
Complete_test_Dev_Exp <- Complete_test_NewcolName %>% mutate(
  expectation_deviation_experience1  = T2_Q15_1  - T1_Q12_1 ,
  expectation_deviation_experience2  = T2_Q15_2  - T1_Q12_2 ,
  expectation_deviation_experience3  = T2_Q15_3  - T1_Q12_3 ,
  expectation_deviation_experience4  = T2_Q15_4  - T1_Q12_4 ,
  expectation_deviation_experience5  = T2_Q15_5  - T1_Q12_5 ,
  expectation_deviation_experience6  = T2_Q15_6  - T1_Q12_6 ,
  expectation_deviation_experience7  = T2_Q15_7  - T1_Q12_7 ,
  expectation_deviation_experience8  = T2_Q15_8  - T1_Q12_8 ,
  expectation_deviation_experience9  = T2_Q15_9  - T1_Q12_9 ,
  expectation_deviation_experience10 = T2_Q15_10 - T1_Q12_10,
  expectation_deviation_experience11 = T2_Q15_11 - T1_Q12_11,
  expectation_deviation_experience12 = T2_Q15_12 - T1_Q12_12,
  expectation_deviation_experience13 = T2_Q15_13 - T1_Q12_13,
  expectation_deviation_experience14 = T2_Q15_14 - T1_Q12_14,
  expectation_deviation_experience15 = T2_Q15_15 - T1_Q12_15,
  expectation_deviation_experience16 = T2_Q15_16 - T1_Q12_16,
  expectation_deviation_experience17 = T2_Q15_17 - T1_Q12_17,
)


Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
 mutate(n_3var_1  = !is.na(T1_Q12_1 ) & !is.na(T2_Q12_1 ) & !is.na(T2_Q15_1 ),
        n_3var_2  = !is.na(T1_Q12_2 ) & !is.na(T2_Q12_2 ) & !is.na(T2_Q15_2 ),
        n_3var_3  = !is.na(T1_Q12_3 ) & !is.na(T2_Q12_3 ) & !is.na(T2_Q15_3 ),
        n_3var_4  = !is.na(T1_Q12_4 ) & !is.na(T2_Q12_4 ) & !is.na(T2_Q15_4 ),
        n_3var_5  = !is.na(T1_Q12_5 ) & !is.na(T2_Q12_5 ) & !is.na(T2_Q15_5 ),
        n_3var_6  = !is.na(T1_Q12_6 ) & !is.na(T2_Q12_6 ) & !is.na(T2_Q15_6 ),
        n_3var_7  = !is.na(T1_Q12_7 ) & !is.na(T2_Q12_7 ) & !is.na(T2_Q15_7 ),
        n_3var_8  = !is.na(T1_Q12_8 ) & !is.na(T2_Q12_8 ) & !is.na(T2_Q15_8 ),
        n_3var_9  = !is.na(T1_Q12_9 ) & !is.na(T2_Q12_9 ) & !is.na(T2_Q15_9 ),
        n_3var_10 = !is.na(T1_Q12_10) & !is.na(T2_Q12_10) & !is.na(T2_Q15_10),
        n_3var_11 = !is.na(T1_Q12_11) & !is.na(T2_Q12_11) & !is.na(T2_Q15_11),
        n_3var_12 = !is.na(T1_Q12_12) & !is.na(T2_Q12_12) & !is.na(T2_Q15_12),
        n_3var_13 = !is.na(T1_Q12_13) & !is.na(T2_Q12_13) & !is.na(T2_Q15_13),
        n_3var_14 = !is.na(T1_Q12_14) & !is.na(T2_Q12_14) & !is.na(T2_Q15_14),
        n_3var_15 = !is.na(T1_Q12_15) & !is.na(T2_Q12_15) & !is.na(T2_Q15_15),
        n_3var_16 = !is.na(T1_Q12_16) & !is.na(T2_Q12_16) & !is.na(T2_Q15_16),
        n_3var_17 = !is.na(T1_Q12_17) & !is.na(T2_Q12_17) & !is.na(T2_Q15_17)) 

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(threevar_T1_Q12_1  = T1_Q12_1  * n_3var_1 , threevar_T2_Q12_1  = T2_Q12_1  * n_3var_1 , threevar_T2_Q15_1  = T2_Q15_1  * n_3var_1 ,
         threevar_T1_Q12_2  = T1_Q12_2  * n_3var_2 , threevar_T2_Q12_2  = T2_Q12_2  * n_3var_2 , threevar_T2_Q15_2  = T2_Q15_2  * n_3var_2 ,
         threevar_T1_Q12_3  = T1_Q12_3  * n_3var_3 , threevar_T2_Q12_3  = T2_Q12_3  * n_3var_3 , threevar_T2_Q15_3  = T2_Q15_3  * n_3var_3 ,
         threevar_T1_Q12_4  = T1_Q12_4  * n_3var_4 , threevar_T2_Q12_4  = T2_Q12_4  * n_3var_4 , threevar_T2_Q15_4  = T2_Q15_4  * n_3var_4 ,
         threevar_T1_Q12_5  = T1_Q12_5  * n_3var_5 , threevar_T2_Q12_5  = T2_Q12_5  * n_3var_5 , threevar_T2_Q15_5  = T2_Q15_5  * n_3var_5 ,
         threevar_T1_Q12_6  = T1_Q12_6  * n_3var_6 , threevar_T2_Q12_6  = T2_Q12_6  * n_3var_6 , threevar_T2_Q15_6  = T2_Q15_6  * n_3var_6 ,
         threevar_T1_Q12_7  = T1_Q12_7  * n_3var_7 , threevar_T2_Q12_7  = T2_Q12_7  * n_3var_7 , threevar_T2_Q15_7  = T2_Q15_7  * n_3var_7 ,
         threevar_T1_Q12_8  = T1_Q12_8  * n_3var_8 , threevar_T2_Q12_8  = T2_Q12_8  * n_3var_8 , threevar_T2_Q15_8  = T2_Q15_8  * n_3var_8 ,
         threevar_T1_Q12_9  = T1_Q12_9  * n_3var_9 , threevar_T2_Q12_9  = T2_Q12_9  * n_3var_9 , threevar_T2_Q15_9  = T2_Q15_9  * n_3var_9 ,
         threevar_T1_Q12_10 = T1_Q12_10 * n_3var_10, threevar_T2_Q12_10 = T2_Q12_10 * n_3var_10, threevar_T2_Q15_10 = T2_Q15_10 * n_3var_10,
         threevar_T1_Q12_11 = T1_Q12_11 * n_3var_11, threevar_T2_Q12_11 = T2_Q12_11 * n_3var_11, threevar_T2_Q15_11 = T2_Q15_11 * n_3var_11,
         threevar_T1_Q12_12 = T1_Q12_12 * n_3var_12, threevar_T2_Q12_12 = T2_Q12_12 * n_3var_12, threevar_T2_Q15_12 = T2_Q15_12 * n_3var_12,
         threevar_T1_Q12_13 = T1_Q12_13 * n_3var_13, threevar_T2_Q12_13 = T2_Q12_13 * n_3var_13, threevar_T2_Q15_13 = T2_Q15_13 * n_3var_13,
         threevar_T1_Q12_14 = T1_Q12_14 * n_3var_14, threevar_T2_Q12_14 = T2_Q12_14 * n_3var_14, threevar_T2_Q15_14 = T2_Q15_14 * n_3var_14,
         threevar_T1_Q12_15 = T1_Q12_15 * n_3var_15, threevar_T2_Q12_15 = T2_Q12_15 * n_3var_15, threevar_T2_Q15_15 = T2_Q15_15 * n_3var_15,
         threevar_T1_Q12_16 = T1_Q12_16 * n_3var_16, threevar_T2_Q12_16 = T2_Q12_16 * n_3var_16, threevar_T2_Q15_16 = T2_Q15_16 * n_3var_16,
         threevar_T1_Q12_17 = T1_Q12_17 * n_3var_17, threevar_T2_Q12_17 = T2_Q12_17 * n_3var_17, threevar_T2_Q15_17 = T2_Q15_17 * n_3var_17)

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(n_2var_1  = !is.na(T1_Q12_1 ) & !is.na(T2_Q15_1 ),
         n_2var_2  = !is.na(T1_Q12_2 ) & !is.na(T2_Q15_2 ),
         n_2var_3  = !is.na(T1_Q12_3 ) & !is.na(T2_Q15_3 ),
         n_2var_4  = !is.na(T1_Q12_4 ) & !is.na(T2_Q15_4 ),
         n_2var_5  = !is.na(T1_Q12_5 ) & !is.na(T2_Q15_5 ),
         n_2var_6  = !is.na(T1_Q12_6 ) & !is.na(T2_Q15_6 ),
         n_2var_7  = !is.na(T1_Q12_7 ) & !is.na(T2_Q15_7 ),
         n_2var_8  = !is.na(T1_Q12_8 ) & !is.na(T2_Q15_8 ),
         n_2var_9  = !is.na(T1_Q12_9 ) & !is.na(T2_Q15_9 ),
         n_2var_10 = !is.na(T1_Q12_10) & !is.na(T2_Q15_10),
         n_2var_11 = !is.na(T1_Q12_11) & !is.na(T2_Q15_11),
         n_2var_12 = !is.na(T1_Q12_12) & !is.na(T2_Q15_12),
         n_2var_13 = !is.na(T1_Q12_13) & !is.na(T2_Q15_13),
         n_2var_14 = !is.na(T1_Q12_14) & !is.na(T2_Q15_14),
         n_2var_15 = !is.na(T1_Q12_15) & !is.na(T2_Q15_15),
         n_2var_16 = !is.na(T1_Q12_16) & !is.na(T2_Q15_16),
         n_2var_17 = !is.na(T1_Q12_17) & !is.na(T2_Q15_17))

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(twovar_T1_Q12_1  = T1_Q12_1  * n_2var_1 , twovar_T2_Q12_1  = T2_Q12_1  * n_2var_1 , twovar_T2_Q15_1  = T2_Q15_1  * n_2var_1 ,
         twovar_T1_Q12_2  = T1_Q12_2  * n_2var_2 , twovar_T2_Q12_2  = T2_Q12_2  * n_2var_2 , twovar_T2_Q15_2  = T2_Q15_2  * n_2var_2 ,
         twovar_T1_Q12_3  = T1_Q12_3  * n_2var_3 , twovar_T2_Q12_3  = T2_Q12_3  * n_2var_3 , twovar_T2_Q15_3  = T2_Q15_3  * n_2var_3 ,
         twovar_T1_Q12_4  = T1_Q12_4  * n_2var_4 , twovar_T2_Q12_4  = T2_Q12_4  * n_2var_4 , twovar_T2_Q15_4  = T2_Q15_4  * n_2var_4 ,
         twovar_T1_Q12_5  = T1_Q12_5  * n_2var_5 , twovar_T2_Q12_5  = T2_Q12_5  * n_2var_5 , twovar_T2_Q15_5  = T2_Q15_5  * n_2var_5 ,
         twovar_T1_Q12_6  = T1_Q12_6  * n_2var_6 , twovar_T2_Q12_6  = T2_Q12_6  * n_2var_6 , twovar_T2_Q15_6  = T2_Q15_6  * n_2var_6 ,
         twovar_T1_Q12_7  = T1_Q12_7  * n_2var_7 , twovar_T2_Q12_7  = T2_Q12_7  * n_2var_7 , twovar_T2_Q15_7  = T2_Q15_7  * n_2var_7 ,
         twovar_T1_Q12_8  = T1_Q12_8  * n_2var_8 , twovar_T2_Q12_8  = T2_Q12_8  * n_2var_8 , twovar_T2_Q15_8  = T2_Q15_8  * n_2var_8 ,
         twovar_T1_Q12_9  = T1_Q12_9  * n_2var_9 , twovar_T2_Q12_9  = T2_Q12_9  * n_2var_9 , twovar_T2_Q15_9  = T2_Q15_9  * n_2var_9 ,
         twovar_T1_Q12_10 = T1_Q12_10 * n_2var_10, twovar_T2_Q12_10 = T2_Q12_10 * n_2var_10, twovar_T2_Q15_10 = T2_Q15_10 * n_2var_10,
         twovar_T1_Q12_11 = T1_Q12_11 * n_2var_11, twovar_T2_Q12_11 = T2_Q12_11 * n_2var_11, twovar_T2_Q15_11 = T2_Q15_11 * n_2var_11,
         twovar_T1_Q12_12 = T1_Q12_12 * n_2var_12, twovar_T2_Q12_12 = T2_Q12_12 * n_2var_12, twovar_T2_Q15_12 = T2_Q15_12 * n_2var_12,
         twovar_T1_Q12_13 = T1_Q12_13 * n_2var_13, twovar_T2_Q12_13 = T2_Q12_13 * n_2var_13, twovar_T2_Q15_13 = T2_Q15_13 * n_2var_13,
         twovar_T1_Q12_14 = T1_Q12_14 * n_2var_14, twovar_T2_Q12_14 = T2_Q12_14 * n_2var_14, twovar_T2_Q15_14 = T2_Q15_14 * n_2var_14,
         twovar_T1_Q12_15 = T1_Q12_15 * n_2var_15, twovar_T2_Q12_15 = T2_Q12_15 * n_2var_15, twovar_T2_Q15_15 = T2_Q15_15 * n_2var_15,
         twovar_T1_Q12_16 = T1_Q12_16 * n_2var_16, twovar_T2_Q12_16 = T2_Q12_16 * n_2var_16, twovar_T2_Q15_16 = T2_Q15_16 * n_2var_16,
         twovar_T1_Q12_17 = T1_Q12_17 * n_2var_17, twovar_T2_Q12_17 = T2_Q12_17 * n_2var_17, twovar_T2_Q15_17 = T2_Q15_17 * n_2var_17)

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(num_weights_3var = rowSums(Complete_test_Dev_Exp %>% dplyr::select(starts_with('n_3var'))),
         num_weights_2var = rowSums(Complete_test_Dev_Exp %>% dplyr::select(starts_with('n_2var'))))

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  rename(Confidence = T2_Q15_Conf_1) %>% 
  mutate(Conf_Rat = case_when(Confidence == 1 ~ "Low",  Confidence == 5  ~ "High", Confidence == 2 ~ "Low", Confidence == 3 ~ "Med", Confidence == 4 ~ "High" ) %>% 
           factor(levels = c('Low','Med','High'), ordered = TRUE))

# note: the following Covid_Exp1 and Covid_Rem are NOT calculated using the same items 
Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>% 
  mutate(Covid_Exp1 = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(T1_Q12_1:T1_Q12_17), na.rm = TRUE),
         Covid_Exp2 = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(T2_Q12_1:T2_Q12_17), na.rm = TRUE),
         Covid_Rem = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(T2_Q15_1:T2_Q15_17), na.rm = TRUE),
         Covid_Dev = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(expectation_deviation_experience1:expectation_deviation_experience17), na.rm = TRUE),
         twovar_Covid_Exp1 = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(twovar_T1_Q12_1:twovar_T1_Q12_17), na.rm = TRUE),
         twovar_Covid_Rem = rowMeans(Complete_test_Dev_Exp %>% dplyr::select(twovar_T2_Q15_1:twovar_T2_Q15_17), na.rm = TRUE))

Complete_test_PCA <- Complete_test_Dev_Exp %>% dplyr::select(contains('PSS'), contains('DASS'), 'Subject') %>%
  mutate(T2_PSS_4 = 4-T2_PSS_4, T2_PSS_5 = 4-T2_PSS_5, T2_PSS_7 = 4-T2_PSS_7, T2_PSS_8 = 4-T2_PSS_8) %>%
  rename(T1_PSS_4 = T1_PSS_4, T1_PSS_5 = T1_PSS_5, T1_PSS_7 = T1_PSS_7, T1_PSS_8 = T1_PSS_8)

Complete_test_PCA_T1 <- Complete_test_PCA %>% dplyr::select(starts_with('T1'), Subject) %>%
  na.omit() 

Complete_test_PCA_T2 <- Complete_test_PCA %>% dplyr::select(starts_with('T2'), Subject) %>% na.omit() %>%
  set_colnames(colnames(Complete_test_PCA_T1))

PCA_T1 <- Complete_test_PCA_T1 %>% dplyr::select(-Subject) %>% prcomp(center=TRUE, scale. = TRUE)

# based on the sign of the correlation between PCA_T1$x and Complete_test_PCA_T1$PSS_total, determine whether we need to multiply -1
Complete_test_PSS_total_T1 <- Complete_test_PCA_T1 %>% dplyr::select(starts_with('T1_PSS')) %>% rowSums()
if (cor.test(Complete_test_PSS_total_T1, PCA_T1$x[,1])$estimate > 0){
  Complete_test_PCA_T1$PCA_T1 <- PCA_T1$x[,1]
  Complete_test_PCA_T2$PCA_T2 <- ((PCA_T1 %>% predict(Complete_test_PCA_T2 %>% dplyr::select(-Subject)))[,1])
}else{
  Complete_test_PCA_T1$PCA_T1 <- PCA_T1$x[,1] * (-1)
  Complete_test_PCA_T2$PCA_T2 <- ((PCA_T1 %>% predict(Complete_test_PCA_T2 %>% dplyr::select(-Subject)))[,1]) * (-1)
}

Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>%
  left_join(Complete_test_PCA_T1 %>% dplyr::select(Subject, PCA_T1)) %>%
  left_join(Complete_test_PCA_T2 %>% dplyr::select(Subject, PCA_T2)) %>%
  mutate(PCA_delta = PCA_T2 - PCA_T1,
         emo_well_being_delta = -PCA_delta)

Complete_test_Dev_Exp %>% write.csv('../data/Complete_test_Dev_Exp_Only_Data_Manipulation.csv')
