############### R code to combine dataset from Prolific and MTurk together
# written by Haoxue, with snippets of code adapted from Johnny
# last updated Jan 24 2022
# this script merge T1 data and T2 data (prolific + mturk) together w/n preprocess on data at all
# the output is saved under the name COVIDdata_merged_unpreprocessed.csv

source('utils/utils.R')

### Load Data
Prolific_Data_original <- read.csv('../data-raw/COVID_Prolific.csv', header = TRUE) #Import data; prolific
MTurk_Data_original <- read.csv('../data-raw/COVID_MTurk.csv', header = TRUE) #Import data; MTurk

### Exclude test, unfinished and duplicated entries
Prolific_Data <- Prolific_Data_original[c(-1,-2),] %>% filter(Progress == "100") %>% 
  select(Q2:DASS_21, prolific_mturk_ID, subjectId, Dat, StartDate) %>% 
  rename(embedId = subjectId) %>% filter(Q2 != "" & Q2 != "test") %>% 
  filter(!duplicated(Q2))
MTurk_Data <- MTurk_Data_original[c(-1,-2),] %>% filter(Progress == "100") %>% 
  select(Q2:Q45_21, prolific_mturk_ID, workerId, Dat, StartDate) %>% 
  set_names(colnames(Prolific_Data)) %>% filter(Q2 != "" & Q2 != "test") %>% 
  filter(!duplicated(Q2)) 

Followup_Data <- rbind(Prolific_Data %>% mutate(Platform = "Prolific"), 
                       MTurk_Data %>% mutate(Platform = "MTurk")) %>% 
  rename(Subject = Q2)

### Load previous data
Previous_Data <- read.csv('../data-raw/COVIDdata_combined_final_n1810.csv', header = TRUE) #Import data; prolific

### Exclude test, unfinished and duplicated entries
### Tidy subNum to prepare for future merge
Previous_Data <- Previous_Data %>% filter(SubjectID_Prolific_MTurk != "" & SubjectID_Prolific_MTurk != "test") %>% 
  set_names(paste0('prev_', colnames(Previous_Data))) %>% 
  mutate(prev_SubjectID_Prolific_MTurk = str_replace_all(prev_SubjectID_Prolific_MTurk, ' ', '')) %>% 
  mutate(prev_SubjectID_Prolific_MTurk = str_replace_all(prev_SubjectID_Prolific_MTurk, '[.]', '')) 

### Merge T1 and T2 data, remove one column of ID
All_Data <- Previous_Data %>% 
  full_join(Followup_Data, by = c('prev_SubjectID_Prolific_MTurk' = 'embedId')) %>% 
  select(-prolific_mturk_ID)

All_Data %>% write_csv('../data/COVIDdata_merged_unpreprocessed.csv')
