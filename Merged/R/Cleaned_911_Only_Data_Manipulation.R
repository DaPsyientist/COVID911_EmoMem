

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

source('utils.R')
#### Data Preparation ####
# Load data ---------------------------------------------------------------
# note to myself: if use as.data.frame on it, no variable labels will be accessible
df.911 <- read.spss('../../Haoxue/data-raw/911data.sav') %>% as.data.frame() 
df.9114label <- read.spss('../../Haoxue/data-raw/911data.sav') 
label.911 <- attr(df.9114label, "variable.labels")
#df.911_test <- haven::read_sav('../../Haoxue/data-raw/911data.sav')
# Grab related columns ----------------------------------------------------
df.emo <- df.911 %>% dplyr::select(s64q13f, s65q14f, s66q15f, s67q16f, s68q17f, s69q18f,
                                   s235q42s, s236q43s, s237q44s, s238q45s, s239q46s, s240q47s,
                                   s237q42t, s238q43t, s239q44t, s240q45t, s241q46t, s242q47t,
                                   sa252, sa253, sa254, sa255, sa256, sa257,
                                   s72q13s, s73q14s, s74q15s, s75q16s, s76q17s, s77q18s,
                                   s74q13t, s75q14t, s76q15t, s77q16t, s78q17t, s79q18t,
                                   #s89q19f, s90q19f, s91q19f, s92q19f, s93q19f, s94q19f,
                                   sa89, sa90, sa91, sa92, sa93, sa94, sa94,
                                   r2mrc6) %>% 
  set_colnames(c('sadness_s1_experienced', 'anger_s1_experienced', 'fear_s1_experienced', 'confusion_s1_experienced', 'frustration_s1_experienced', 'shock_s1_experienced',
                 'sadness_s2_experienced', 'anger_s2_experienced', 'fear_s2_experienced', 'confusion_s2_experienced', 'frustration_s2_experienced', 'shock_s2_experienced',
                 'sadness_s3_experienced', 'anger_s3_experienced', 'fear_s3_experienced', 'confusion_s3_experienced', 'frustration_s3_experienced', 'shock_s3_experienced',
                 'sadness_s4_experienced', 'anger_s4_experienced', 'fear_s4_experienced', 'confusion_s4_experienced', 'frustration_s4_experienced', 'shock_s4_experienced',
                 'sadness_s2_remembered', 'anger_s2_remembered', 'fear_s2_remembered','confusion_s2_remembered','frustration_s2_remembered','shock_s2_remembered',
                 'sadness_s3_remembered', 'anger_s3_remembered', 'fear_s3_remembered','confusion_s3_remembered','frustration_s3_remembered','shock_s3_remembered',
                 'sadness_s4_remembered', 'anger_s4_remembered', 'fear_s4_remembered','confusion_s4_remembered','frustration_s4_remembered','shock_s4_remembered',
                 'Subject')) 
  

emo_miss <- function(x){
  y <- ifelse(x==6|x==0, NA, x)
  return(y)
}
df.emo <- df.emo %>% mutate(across(where(is.double),
                                   .fns = emo_miss))

# separate df for demo (can add more stuff in the future)
df.emo.demo <- df.911 %>% dplyr::select(r2mrc6, s4qdmf, s5qdmf) %>% 
  set_colnames(c('Subject', 'age', 'gender')) %>% 
  mutate(gender = factor(gender, levels = c(0,1,2), labels = c('Not stated', 'Female', 'Male'))) %>% 
  mutate(Subject = str_trim(Subject)) %>% 
  filter(Subject != "" & Subject != "N/A" & !is.na(Subject)) 
df.emo.demo %>% write.csv('../data/df.emo.demographics.csv')
  
# wide2long ---------------------------------------------------------------
df.emo.long <- df.emo %>% 
  mutate(Subject = str_trim(Subject)) %>% 
  filter(Subject != "" & Subject != "N/A" & !is.na(Subject)) %>% 
  pivot_longer(cols = -Subject,
               names_to = 'type',
               values_to = 'rating') %>% 
  mutate(item = sub("\\_s.*", "", type),
         time = str_extract(type, '[0-9]') %>% as.double,
         prompt = sub(".*_s[0-9]_", "", type),
         prompt_time = paste0(prompt, time)) 

df.emo.wide <- df.emo.long %>% dplyr::select(-prompt) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = 'prompt_time', values_from = 'rating') %>% 
  dplyr::select(-type, -row, -time) %>% 
  group_by(Subject, item) %>% 
  summarise(experienced1 = mean(experienced1, na.rm = TRUE),
            experienced2 = mean(experienced2, na.rm = TRUE),
            experienced3 = mean(experienced3, na.rm = TRUE),
            experienced4 = mean(experienced4, na.rm = TRUE),
            remembered2 = mean(remembered2, na.rm = TRUE),
            remembered3 = mean(remembered3, na.rm = TRUE),
            remembered4 = mean(remembered4, na.rm = TRUE),
  ) %>% 
  mutate(memdev2 = remembered2 - experienced1,
         memdev3 = remembered3 - experienced1,
         memdev4 = remembered4 - experienced1,
         currdev2 = experienced2 - experienced1,
         currdev3 = experienced3 - experienced1,
         currdev4 = experienced4 - experienced1,
  ) %>% ungroup()

twovar.df.emo.wide.t2 <- df.emo.wide %>% filter(!is.na(memdev2)) %>% group_by(Subject) %>% 
  summarise(
  twovar_mean_remembered2 = mean(remembered2),
  twovar_mean_experienced1_t2 = mean(experienced1)
)

twovar.df.emo.wide.t3 <- df.emo.wide %>% filter(!is.na(memdev3)) %>% group_by(Subject) %>% 
  summarise(
    twovar_mean_remembered3 = mean(remembered3),
    twovar_mean_experienced1_t3 = mean(experienced1)
  )

twovar.df.emo.wide.t4 <- df.emo.wide %>% filter(!is.na(memdev4)) %>% group_by(Subject) %>% 
  summarise(
    twovar_mean_remembered4 = mean(remembered4),
    twovar_mean_experienced1_t4 = mean(experienced1)
  )


df.emo.wide.avg <- df.emo.wide %>% group_by(Subject, gender) %>% 
  summarise(mean_experienced1 = mean(experienced1, na.rm = TRUE),
         mean_experienced2 = mean(experienced2, na.rm = TRUE),
         mean_experienced3 = mean(experienced3, na.rm = TRUE),
         mean_experienced4 = mean(experienced4, na.rm = TRUE),
         mean_remembered2 = mean(remembered2, na.rm = TRUE),
         mean_remembered3 = mean(remembered3, na.rm = TRUE),
         mean_remembered4 = mean(remembered4, na.rm = TRUE),
         mean_memdev2 = mean(memdev2, na.rm = TRUE),
         mean_memdev3 = mean(memdev3, na.rm = TRUE),
         mean_memdev4 = mean(memdev4, na.rm = TRUE),
         mean_currdev2 = mean(currdev2, na.rm = TRUE),
         mean_currdev3 = mean(currdev3, na.rm = TRUE),
         mean_currdev4 = mean(currdev4, na.rm = TRUE),
         n_3IVs_t2 = sum(!is.na(experienced1 + experienced2 + remembered2)),
         n_3IVs_t3 = sum(!is.na(experienced1 + experienced3 + remembered3)),
         n_3IVs_t4 = sum(!is.na(experienced1 + experienced4 + remembered4)),
         n_2IVs_t2 = sum(!is.na(experienced2 + remembered2)),
         n_2IVs_t3 = sum(!is.na(experienced3 + remembered3)),
         n_2IVs_t4 = sum(!is.na(experienced4 + remembered4))) %>% 
  left_join(twovar.df.emo.wide.t2) %>% 
  left_join(twovar.df.emo.wide.t3) %>% 
  left_join(twovar.df.emo.wide.t4) 

df.emo.wide.avg %>% write.csv('../data/df.emo.wide.avg_Only_Data_Manipulation.csv')

name.emo.long.avg <- c('Subject','prev_experience', 'curr_experience','stress_memory','memdev','curr_dev','num_weights_updated',
                       'twovar_stress_memory', 'twovar_prev_experience')

df.emo.avg.t2 <- df.emo.wide.avg %>% dplyr::select(Subject, mean_experienced1, mean_experienced2, mean_remembered2, mean_memdev2, mean_currdev2, n_3IVs_t2, twovar_mean_remembered2, twovar_mean_experienced1_t2) %>% 
  set_colnames(name.emo.long.avg) %>% mutate(time = 2)
df.emo.avg.t3 <- df.emo.wide.avg %>% dplyr::select(Subject, mean_experienced1, mean_experienced3, mean_remembered3, mean_memdev3, mean_currdev3, n_3IVs_t3, twovar_mean_remembered3, twovar_mean_experienced1_t3) %>% 
  set_colnames(name.emo.long.avg) %>% mutate(time = 3)
df.emo.avg.t4 <- df.emo.wide.avg %>% dplyr::select(Subject, mean_experienced1, mean_experienced4, mean_remembered4, mean_memdev4, mean_currdev4, n_3IVs_t4, twovar_mean_remembered4, twovar_mean_experienced1_t4) %>% 
  set_colnames(name.emo.long.avg) %>% mutate(time = 4)

df.emo.long.avg <- df.emo.avg.t2 %>% 
  rbind(df.emo.avg.t3) %>% 
  rbind(df.emo.avg.t4)

df.emo.long.avg %>% write.csv('../data/df.emo.long.avg_Only_Data_Manipulation.csv')

