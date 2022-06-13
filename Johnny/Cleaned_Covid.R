if(!require("pacman")) install.packages("pacman") #Install pacman to facilitate package installing/loading
p_load(tidyverse, ggplot2, tidyr, dplyr, BayesFactor, brms, lme4, sjPlot, rstanarm, bayestestR) #Load necessary rPackages
options(mc.cores = parallel::detectCores())
beta_squeeze <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

#### Data Preparation ####
### Timepoint 2 Data ###
#Load Data - Macbook
Prolific_Data <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/COVID_Prolific.csv", header = TRUE) #Import data; prolific
MTurk_Data <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/COVID_MTurk.csv", header = TRUE) #Import data; MTurk

#Filter Data
Prolific_Filtered <- Prolific_Data %>% select(contains(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16")))
MTurk_Filtered <- MTurk_Data %>% select(contains(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16")))

#Re-Score Data - Prolific
#Q3
Prolific_Filtered$Q3<-dplyr::recode(Prolific_Filtered$Q3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q4
Prolific_Filtered$Q4<-dplyr::recode(Prolific_Filtered$Q4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q5
Prolific_Filtered$Q5<-dplyr::recode(Prolific_Filtered$Q5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q6
Prolific_Filtered$Q6<-dplyr::recode(Prolific_Filtered$Q6, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q7
Prolific_Filtered$Q7<-dplyr::recode(Prolific_Filtered$Q7, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q8
Prolific_Filtered$Q8<-dplyr::recode(Prolific_Filtered$Q8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q8
Prolific_Filtered$Q8<-dplyr::recode(Prolific_Filtered$Q8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q10
Prolific_Filtered$Q10_1<-dplyr::recode(Prolific_Filtered$Q10_1, "Not a threat at all1" = '1',"A serious threat5"= '5' )
Prolific_Filtered$Q10_2<-dplyr::recode(Prolific_Filtered$Q10_2, "Not a threat at all1" = '1',"A serious threat5"= '5' )
Prolific_Filtered$Q10_3<-dplyr::recode(Prolific_Filtered$Q10_3, "Not a threat at all1" = '1',"A serious threat5"= '5' )
Prolific_Filtered$Q10_4<-dplyr::recode(Prolific_Filtered$Q10_4, "Not a threat at all1" = '1',"A serious threat5"= '5' )
#Q12
Prolific_Filtered$Q12_1<-dplyr::recode(Prolific_Filtered$Q12_1, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_2<-dplyr::recode(Prolific_Filtered$Q12_2, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_3<-dplyr::recode(Prolific_Filtered$Q12_3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_4<-dplyr::recode(Prolific_Filtered$Q12_4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_5<-dplyr::recode(Prolific_Filtered$Q12_5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_6<-dplyr::recode(Prolific_Filtered$Q12_6, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_7<-dplyr::recode(Prolific_Filtered$Q12_7, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_8<-dplyr::recode(Prolific_Filtered$Q12_8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_9<-dplyr::recode(Prolific_Filtered$Q12_9, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_10<-dplyr::recode(Prolific_Filtered$Q12_10, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_11<-dplyr::recode(Prolific_Filtered$Q12_11, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_12<-dplyr::recode(Prolific_Filtered$Q12_12, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_13<-dplyr::recode(Prolific_Filtered$Q12_13, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_14<-dplyr::recode(Prolific_Filtered$Q12_14, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_15<-dplyr::recode(Prolific_Filtered$Q12_15, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q12_16<-dplyr::recode(Prolific_Filtered$Q12_16, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#Q13
Prolific_Filtered$Q13_1<-dplyr::recode(Prolific_Filtered$Q13_1, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q13_2<-dplyr::recode(Prolific_Filtered$Q13_2, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q13_3<-dplyr::recode(Prolific_Filtered$Q13_3, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q13_4<-dplyr::recode(Prolific_Filtered$Q13_4, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q13_5<-dplyr::recode(Prolific_Filtered$Q13_5, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#Q15
Prolific_Filtered$Q15_1<-dplyr::recode(Prolific_Filtered$Q15_1, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_2<-dplyr::recode(Prolific_Filtered$Q15_2, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_3<-dplyr::recode(Prolific_Filtered$Q15_3, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_4<-dplyr::recode(Prolific_Filtered$Q15_4, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_5<-dplyr::recode(Prolific_Filtered$Q15_5, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_6<-dplyr::recode(Prolific_Filtered$Q15_6, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_7<-dplyr::recode(Prolific_Filtered$Q15_7, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_8<-dplyr::recode(Prolific_Filtered$Q15_8, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_9<-dplyr::recode(Prolific_Filtered$Q15_9, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_10<-dplyr::recode(Prolific_Filtered$Q15_10, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_11<-dplyr::recode(Prolific_Filtered$Q15_11, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_12<-dplyr::recode(Prolific_Filtered$Q15_12, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_13<-dplyr::recode(Prolific_Filtered$Q15_13, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_14<-dplyr::recode(Prolific_Filtered$Q15_14, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_15<-dplyr::recode(Prolific_Filtered$Q15_15, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_16<-dplyr::recode(Prolific_Filtered$Q15_16, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q15_17<-dplyr::recode(Prolific_Filtered$Q15_17, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#16
Prolific_Filtered$Q16_1<-dplyr::recode(Prolific_Filtered$Q16_1, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q16_2<-dplyr::recode(Prolific_Filtered$Q16_2, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q16_3<-dplyr::recode(Prolific_Filtered$Q16_3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q16_4<-dplyr::recode(Prolific_Filtered$Q16_4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
Prolific_Filtered$Q16_5<-dplyr::recode(Prolific_Filtered$Q16_5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )


#Re-Score Data - MTurk
#Q3
MTurk_Filtered$Q3<-dplyr::recode(MTurk_Filtered$Q3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q4
MTurk_Filtered$Q4<-dplyr::recode(MTurk_Filtered$Q4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q5
MTurk_Filtered$Q5<-dplyr::recode(MTurk_Filtered$Q5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q6
MTurk_Filtered$Q6<-dplyr::recode(MTurk_Filtered$Q6, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q7
MTurk_Filtered$Q7<-dplyr::recode(MTurk_Filtered$Q7, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q8
MTurk_Filtered$Q8<-dplyr::recode(MTurk_Filtered$Q8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q8
MTurk_Filtered$Q8<-dplyr::recode(MTurk_Filtered$Q8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5"= '5', "Some stress\n\n3" ="3" )
#Q10
MTurk_Filtered$Q10_1<-dplyr::recode(MTurk_Filtered$Q10_1, "Not a threat at all1" = '1',"A serious threat5"= '5' )
MTurk_Filtered$Q10_2<-dplyr::recode(MTurk_Filtered$Q10_2, "Not a threat at all1" = '1',"A serious threat5"= '5' )
MTurk_Filtered$Q10_3<-dplyr::recode(MTurk_Filtered$Q10_3, "Not a threat at all1" = '1',"A serious threat5"= '5' )
MTurk_Filtered$Q10_4<-dplyr::recode(MTurk_Filtered$Q10_4, "Not a threat at all1" = '1',"A serious threat5"= '5' )
#Q12
MTurk_Filtered$Q12_1<-dplyr::recode(MTurk_Filtered$Q12_1, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_2<-dplyr::recode(MTurk_Filtered$Q12_2, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_3<-dplyr::recode(MTurk_Filtered$Q12_3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_4<-dplyr::recode(MTurk_Filtered$Q12_4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_5<-dplyr::recode(MTurk_Filtered$Q12_5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_6<-dplyr::recode(MTurk_Filtered$Q12_6, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_7<-dplyr::recode(MTurk_Filtered$Q12_7, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_8<-dplyr::recode(MTurk_Filtered$Q12_8, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_9<-dplyr::recode(MTurk_Filtered$Q12_9, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_10<-dplyr::recode(MTurk_Filtered$Q12_10, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_11<-dplyr::recode(MTurk_Filtered$Q12_11, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_12<-dplyr::recode(MTurk_Filtered$Q12_12, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_13<-dplyr::recode(MTurk_Filtered$Q12_13, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_14<-dplyr::recode(MTurk_Filtered$Q12_14, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_15<-dplyr::recode(MTurk_Filtered$Q12_15, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_16<-dplyr::recode(MTurk_Filtered$Q12_16, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q12_17<-dplyr::recode(MTurk_Filtered$Q12_17, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#Q13
MTurk_Filtered$Q13_1<-dplyr::recode(MTurk_Filtered$Q13_1, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q13_2<-dplyr::recode(MTurk_Filtered$Q13_2, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q13_3<-dplyr::recode(MTurk_Filtered$Q13_3, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q13_4<-dplyr::recode(MTurk_Filtered$Q13_4, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q13_5<-dplyr::recode(MTurk_Filtered$Q13_5, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#Q15
MTurk_Filtered$Q15_1<-dplyr::recode(MTurk_Filtered$Q15_1, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_2<-dplyr::recode(MTurk_Filtered$Q15_2, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_3<-dplyr::recode(MTurk_Filtered$Q15_3, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_4<-dplyr::recode(MTurk_Filtered$Q15_4, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_5<-dplyr::recode(MTurk_Filtered$Q15_5, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_6<-dplyr::recode(MTurk_Filtered$Q15_6, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_7<-dplyr::recode(MTurk_Filtered$Q15_7, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_8<-dplyr::recode(MTurk_Filtered$Q15_8, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_9<-dplyr::recode(MTurk_Filtered$Q15_9, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_10<-dplyr::recode(MTurk_Filtered$Q15_10, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_11<-dplyr::recode(MTurk_Filtered$Q15_11, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_12<-dplyr::recode(MTurk_Filtered$Q15_12, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_13<-dplyr::recode(MTurk_Filtered$Q15_13, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_14<-dplyr::recode(MTurk_Filtered$Q15_14, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_15<-dplyr::recode(MTurk_Filtered$Q15_15, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_16<-dplyr::recode(MTurk_Filtered$Q15_16, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q15_17<-dplyr::recode(MTurk_Filtered$Q15_17, "No stress at all\n\n1\n" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
#16
MTurk_Filtered$Q16_1<-dplyr::recode(MTurk_Filtered$Q16_1, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q16_2<-dplyr::recode(MTurk_Filtered$Q16_2, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q16_3<-dplyr::recode(MTurk_Filtered$Q16_3, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q16_4<-dplyr::recode(MTurk_Filtered$Q16_4, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )
MTurk_Filtered$Q16_5<-dplyr::recode(MTurk_Filtered$Q16_5, "No stress at all\n\n1" = '1',"A great deal of stress\n\n5\n"= '5', "Some stress\n\n3\n"='3' )

All_Data <- Prolific_Filtered %>% full_join(MTurk_Filtered)
All_Data_Ans <- All_Data
names(All_Data_Ans)[names(All_Data_Ans) == "Q2"] <- "Subject" #Change column title to 'Subject' so merging dataframes together is possible
All_Data_Ans <- All_Data_Ans[3:743,]
All_Data_Ans[All_Data_Ans == ""] <- NA
All_Data_Ans[All_Data_Ans == "N/A"] <- NA
All_Data_Ans <- All_Data_Ans %>% filter(Subject !=" https://p8pkef0k8l.cognition.run/?external_id=5ebc70e8d7a1780bedd1514e") %>% 
  filter(Subject !="test") %>% filter(!is.na(Subject))
All_Data_Ans <- All_Data_Ans[-670,] #Remove Duplicate
All_Data_Ans <- All_Data_Ans[-672,] #Remove Duplicate
All_Data_Ans <- All_Data_Ans[-672,] #Remove Duplicate
All_Data_Ans <- All_Data_Ans[-680,] #Remove Duplicate
All_Data_Ans <- All_Data_Ans[-691,] #Remove Duplicate
All_Data_Ans <- All_Data_Ans[-701,] #Remove Duplicate

#Stress to Self
Self_CovidExp <- All_Data_Ans %>% select(c(Subject, Q3, Q4, Q5)) %>% mutate(Type = "Self")
Self_CovidExp$Subject <- as.factor(Self_CovidExp$Subject)
names(Self_CovidExp)[names(Self_CovidExp) == "Q3"] <- "Current"
names(Self_CovidExp)[names(Self_CovidExp) == "Q4"] <- "MostDiff" #Change column title to 'Subject' so merging dataframes together is possible
names(Self_CovidExp)[names(Self_CovidExp) == "Q5"] <- "Future" #Change column title to 'Subject' so merging dataframes together is possible
Self_CovidExp <- Self_CovidExp %>% gather("Timepoint", "Rating", 2:4) #Long Format
Self_CovidExp <- Self_CovidExp %>% mutate(BRat = ((as.numeric(Rating) -1)/4)) #Transform to Beta Scale
Self_CovidExp %>% group_by()
Self_CovidExp$BRat <- beta_squeeze(Self_CovidExp$BRat)
Self_CovidExp$Rating <- factor(Self_CovidExp$Rating, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Stress to typical community member
ComMem_CovidExp <- All_Data_Ans %>% select(c(Subject, Q6, Q7, Q8)) %>% mutate(Type = "Other")
ComMem_CovidExp$Subject <- as.factor(ComMem_CovidExp$Subject)
names(ComMem_CovidExp)[names(ComMem_CovidExp) == "Q6"] <- "Current"
names(ComMem_CovidExp)[names(ComMem_CovidExp) == "Q7"] <- "MostDiff" #Change column title to 'Subject' so merging dataframes together is possible
names(ComMem_CovidExp)[names(ComMem_CovidExp) == "Q8"] <- "Future" #Change column title to 'Subject' so merging dataframes together is possible
ComMem_CovidExp <- ComMem_CovidExp %>% gather("Timepoint", "Rating", 2:4)
ComMem_CovidExp <- ComMem_CovidExp %>% mutate(BRat = ((as.numeric(Rating) -1)/4))
ComMem_CovidExp$BRat <- beta_squeeze(ComMem_CovidExp$BRat)
#ComMem_CovidExp$Rating <- factor(ComMem_CovidExp$Rating, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Most difficult months in Covid-19 Pandemic
Q10_Both <- data.frame(All_Data_Ans$Q9.1_1,All_Data_Ans$Q9.2_1)
Q10_Both <- na.omit(Q10_Both)
colnames(Q10_Both) <- c("Month","Year")
Q10_2020 <- Q10_Both %>% filter(Year == 2020)
Q10_2021 <- Q10_Both %>% filter(Year == 2021)

#Threat to others x Social distance
Current_Threat <- All_Data_Ans %>% select(c(Subject, Q10_1, Q10_2, Q10_3, Q10_4))
Current_Threat$Subject <- as.factor(Current_Threat$Subject)
names(Current_Threat)[names(Current_Threat) == "Q10_1"] <- "Self"
names(Current_Threat)[names(Current_Threat) == "Q10_2"] <- "Close" 
names(Current_Threat)[names(Current_Threat) == "Q10_3"] <- "USPpl" 
names(Current_Threat)[names(Current_Threat) == "Q10_4"] <- "World" 
Current_Threat <- Current_Threat %>% gather("Scope", "Threat", 2:5)
Current_Threat <- Current_Threat %>% mutate(BThreat = ((as.numeric(Threat) -1)/4))
Current_Threat$BThreat <- beta_squeeze(Current_Threat$BThreat)
#Current_Threat$Threat <- factor(Current_Threat$Threat, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
Current_Threat$Scope <- factor(Current_Threat$Scope, levels = c("Self", "Close", "USPpl", "World"), ordered = FALSE )
levels(Current_Threat$Scope)

### Timepoint 1 Data ###
#Load Data - IMac
Previous_Data <- read.csv("/Users/juc418/Desktop/COVIDdata_combined_final_n1810.csv", header = TRUE) #Import data; prolific
#Load Data - MacBook
Previous_Data <- read.csv("/Users/jcast/Desktop/G2/Covid_Proj/COVIDdata_combined_final_n1810.csv", header = TRUE) #Import data; MTurk

###Count Participants###
Previous_Count <- Previous_Data %>% select(SubjectID_Prolific_MTurk)
Previous_Count <- Previous_Count %>%filter(SubjectID_Prolific_MTurk != "") %>% filter(SubjectID_Prolific_MTurk != "test") 
unique(Previous_Count) #1810 
#Total number of participants: 1810

#Make list of participants with data in both dataframes
Run_1_Ppts <- data.frame(Previous_Count$SubjectID_Prolific_MTurk)
Run_2_Ppts <- data.frame(All_Data_Ans$Subject)
names(Run_1_Ppts)[names(Run_1_Ppts) == "Previous_Count.SubjectID_Prolific_MTurk"] <- "Subject" #Change column title to 'Subject' so merging dataframes together is possible
names(Run_2_Ppts)[names(Run_2_Ppts) == "All_Data_Ans.Subject"] <- "Subject" #Change column title to 'Subject' so merging dataframes together is possible
Both_Run_Ppts <- Run_1_Ppts %>% right_join(Run_2_Ppts) #730 Participants with Data at both points
Sub_Filt <- as_vector(Both_Run_Ppts)

#Filter Data based on available previous timepoints
Previous_Filtered <- Previous_Data %>% select(contains(c("SubjectID_Prolific_MTurk","COVID_20_stress_lifechanges_1", "COVID_21_stress_lifechanges_2", "COVID_22_stress_lifechanges_3","COVID_23_stress_lifechanges_4", "COVID_24_stress_lifechanges_5", "COVID_25_stress_lifechanges_6",
                                                         "COVID_26_stress_lifechanges_7", "COVID_27_stress_lifechanges_8", "COVID_28_stress_lifechanges_9", "COVID_29_stress_lifechanges_10", "COVID_30_stress_lifechanges_11", "COVID_31_stress_lifechanges_12",
                                                         "COVID_32_stress_lifechanges_13", "COVID_33_stress_lifechanges_14", "COVID_34_stress_lifechanges_15", "COVID_35_stress_lifechanges_16", "COVID_36_stress_lifechanges_17", "COVID_37_stress_beliefs_1",
                                                         "COVID_38_stress_beliefs_2", "COVID_39_stress_beliefs_3", "COVID_40_stress_beliefs_4", "COVID_41_stress_beliefs_5")))
names(Previous_Filtered)[names(Previous_Filtered) == "SubjectID_Prolific_MTurk"] <- "Subject" #Change column title to 'Subject' so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_20_stress_lifechanges_1"] <- "Q12_1" #Change column title so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_21_stress_lifechanges_2"] <- "Q12_2" #Change column title so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_22_stress_lifechanges_3"] <- "Q12_3" #Change column title so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_23_stress_lifechanges_4"] <- "Q12_4" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_24_stress_lifechanges_5"] <- "Q12_5" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_25_stress_lifechanges_6"] <- "Q12_6" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_26_stress_lifechanges_7"] <- "Q12_7" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_27_stress_lifechanges_8"] <- "Q12_8" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_28_stress_lifechanges_9"] <- "Q12_9" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_29_stress_lifechanges_10"] <- "Q12_10" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_30_stress_lifechanges_11"] <- "Q12_11" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_31_stress_lifechanges_12"] <- "Q12_12" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_32_stress_lifechanges_13"] <- "Q12_13" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_33_stress_lifechanges_14"] <- "Q12_14" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_34_stress_lifechanges_15"] <- "Q12_15" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_35_stress_lifechanges_16"] <- "Q12_16" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_36_stress_lifechanges_17"] <- "Q12_17" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_37_stress_beliefs_1"] <- "Q13_1" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_38_stress_beliefs_2"] <- "Q13_2" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_39_stress_beliefs_3"] <- "Q13_3" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_40_stress_beliefs_4"] <- "Q13_4" #Change column title to so merging dataframes together is possible
names(Previous_Filtered)[names(Previous_Filtered) == "COVID_41_stress_beliefs_5"] <- "Q13_5" #Change column title to so merging dataframes together is possible

#Re-Score 0 to NA 
Previous_Filtered[Previous_Filtered == 0] <- NA

#Make data types the same
All_Data_Ans$Subject <- as.factor(All_Data_Ans$Subject)
Previous_Filtered$Subject <- as.factor(Previous_Filtered$Subject)
Previous_Filtered$Q12_1 <- as.integer(Previous_Filtered$Q12_1)
All_Data_Ans$Q12_1 <- as.integer(All_Data_Ans$Q12_1)
Previous_Filtered$Q12_2 <- as.integer(Previous_Filtered$Q12_2)
All_Data_Ans$Q12_2 <- as.integer(All_Data_Ans$Q12_2)
Previous_Filtered$Q12_3 <- as.integer(Previous_Filtered$Q12_3)
All_Data_Ans$Q12_3 <- as.integer(All_Data_Ans$Q12_3)
Previous_Filtered$Q12_4 <- as.integer(Previous_Filtered$Q12_4)
All_Data_Ans$Q12_4 <- as.integer(All_Data_Ans$Q12_4)
Previous_Filtered$Q12_5 <- as.integer(Previous_Filtered$Q12_5)
All_Data_Ans$Q12_5 <- as.integer(All_Data_Ans$Q12_5)
Previous_Filtered$Q12_6 <- as.integer(Previous_Filtered$Q12_6)
All_Data_Ans$Q12_6 <- as.integer(All_Data_Ans$Q12_6)
Previous_Filtered$Q12_7 <- as.integer(Previous_Filtered$Q12_7)
All_Data_Ans$Q12_7 <- as.integer(All_Data_Ans$Q12_7)
Previous_Filtered$Q12_8 <- as.integer(Previous_Filtered$Q12_8)
All_Data_Ans$Q12_8 <- as.integer(All_Data_Ans$Q12_8)
Previous_Filtered$Q12_9 <- as.integer(Previous_Filtered$Q12_9)
All_Data_Ans$Q12_9 <- as.integer(All_Data_Ans$Q12_9)
Previous_Filtered$Q12_10 <- as.integer(Previous_Filtered$Q12_10)
All_Data_Ans$Q12_10 <- as.integer(All_Data_Ans$Q12_10)
Previous_Filtered$Q12_11 <- as.integer(Previous_Filtered$Q12_11)
All_Data_Ans$Q12_11 <- as.integer(All_Data_Ans$Q12_11)
Previous_Filtered$Q12_12 <- as.integer(Previous_Filtered$Q12_12)
All_Data_Ans$Q12_12 <- as.integer(All_Data_Ans$Q12_12)
Previous_Filtered$Q12_13 <- as.integer(Previous_Filtered$Q12_13)
All_Data_Ans$Q12_13 <- as.integer(All_Data_Ans$Q12_13)
Previous_Filtered$Q12_14 <- as.integer(Previous_Filtered$Q12_14)
All_Data_Ans$Q12_14 <- as.integer(All_Data_Ans$Q12_14)
Previous_Filtered$Q12_15 <- as.integer(Previous_Filtered$Q12_15)
All_Data_Ans$Q12_15 <- as.integer(All_Data_Ans$Q12_15)
Previous_Filtered$Q12_16 <- as.integer(Previous_Filtered$Q12_16)
All_Data_Ans$Q12_16 <- as.integer(All_Data_Ans$Q12_16)
Previous_Filtered$Q12_17 <- as.integer(Previous_Filtered$Q12_17)
All_Data_Ans$Q12_17 <- as.integer(All_Data_Ans$Q12_17)
Previous_Filtered$Q13_1 <- as.integer(Previous_Filtered$Q13_1)
All_Data_Ans$Q13_1 <- as.integer(All_Data_Ans$Q13_1)
Previous_Filtered$Q13_2 <- as.integer(Previous_Filtered$Q13_2)
All_Data_Ans$Q13_2 <- as.integer(All_Data_Ans$Q13_2)
Previous_Filtered$Q13_3 <- as.integer(Previous_Filtered$Q13_3)
All_Data_Ans$Q13_3 <- as.integer(All_Data_Ans$Q13_3)
Previous_Filtered$Q13_4 <- as.integer(Previous_Filtered$Q13_4)
All_Data_Ans$Q13_4 <- as.integer(All_Data_Ans$Q13_4)
Previous_Filtered$Q13_5 <- as.integer(Previous_Filtered$Q13_5)
All_Data_Ans$Q13_5 <- as.integer(All_Data_Ans$Q13_5)

#Combine Dataframes
All_Data_Ans <- All_Data_Ans %>% mutate(TP = "Now")
Previous_Filtered <- Previous_Filtered %>% mutate(TP = "Prev")
All_Data_Ans$TP <- as.factor(All_Data_Ans$TP)
Previous_Filtered$TP <- as.factor(Previous_Filtered$TP)
Complete_test <- All_Data_Ans %>% full_join(Previous_Filtered) %>% filter(Subject %in% Sub_Filt)
Complete_test$Q15_1 <- as.integer(Complete_test$Q15_1)
Complete_test$Q15_2 <- as.integer(Complete_test$Q15_2)
Complete_test$Q15_3 <- as.integer(Complete_test$Q15_3)
Complete_test$Q15_4 <- as.integer(Complete_test$Q15_4)
Complete_test$Q15_5 <- as.integer(Complete_test$Q15_5)
Complete_test$Q15_6 <- as.integer(Complete_test$Q15_6)
Complete_test$Q15_7 <- as.integer(Complete_test$Q15_7)
Complete_test$Q15_8 <- as.integer(Complete_test$Q15_8)
Complete_test$Q15_9 <- as.integer(Complete_test$Q15_9)
Complete_test$Q15_10 <- as.integer(Complete_test$Q15_10)
Complete_test$Q15_11 <- as.integer(Complete_test$Q15_11)
Complete_test$Q15_12 <- as.integer(Complete_test$Q15_12)
Complete_test$Q15_13 <- as.integer(Complete_test$Q15_13)
Complete_test$Q15_14 <- as.integer(Complete_test$Q15_14)
Complete_test$Q15_15 <- as.integer(Complete_test$Q15_15)
Complete_test$Q15_16 <- as.integer(Complete_test$Q15_16)
Complete_test$Q15_17 <- as.integer(Complete_test$Q15_17)
Complete_test$Q16_1 <- as.integer(Complete_test$Q16_1)
Complete_test$Q16_2 <- as.integer(Complete_test$Q16_2)
Complete_test$Q16_3 <- as.integer(Complete_test$Q16_3)
Complete_test$Q16_4 <- as.integer(Complete_test$Q16_4)
Complete_test$Q16_5 <- as.integer(Complete_test$Q16_5)

### Differences between Timepoints 1 & 2 (Longitudinal Change) ###
#------------How much stress are you experiencing right now as a result of---------#
Complete_test_Anal <-Complete_test
#Recode factors for Bayesian Ordinal Analysis - Exp. Stress
#Complete_test_Anal$Q12_1 <- factor(Complete_test_Anal$Q12_1, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_2 <- factor(Complete_test_Anal$Q12_2, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_3 <- factor(Complete_test_Anal$Q12_3, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_4 <- factor(Complete_test_Anal$Q12_4, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_5 <- factor(Complete_test_Anal$Q12_5, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_6 <- factor(Complete_test_Anal$Q12_6, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_7 <- factor(Complete_test_Anal$Q12_7, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_8 <- factor(Complete_test_Anal$Q12_8, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_9 <- factor(Complete_test_Anal$Q12_9, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_10 <- factor(Complete_test_Anal$Q12_10, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_11 <- factor(Complete_test_Anal$Q12_11, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_12 <- factor(Complete_test_Anal$Q12_12, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_13 <- factor(Complete_test_Anal$Q12_13, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_14 <- factor(Complete_test_Anal$Q12_14, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_15 <- factor(Complete_test_Anal$Q12_15, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_16 <- factor(Complete_test_Anal$Q12_16, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q12_17 <- factor(Complete_test_Anal$Q12_17, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Recode factors for Beta Regression Analysis
Complete_test_Anal <- Complete_test_Anal %>% select(Subject, TP, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, 
                                                    Q12_7, Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, 
                                                    Q12_14, Q12_15, Q12_16, Q12_17, Q13_1, Q13_2, Q13_3, 
                                                    Q13_4, Q13_5) %>% mutate(BRat_1 = ((as.numeric(Q12_1) -1)/4)) %>% 
  mutate(BRat_2 = ((as.numeric(Q12_2) -1)/4)) %>% mutate(BRat_3 = ((as.numeric(Q12_3) -1)/4)) %>% 
  mutate(BRat_4 = ((as.numeric(Q12_4) -1)/4)) %>% mutate(BRat_5 = ((as.numeric(Q12_5) -1)/4)) %>% 
  mutate(BRat_6 = ((as.numeric(Q12_6) -1)/4)) %>% mutate(BRat_7 = ((as.numeric(Q12_7) -1)/4)) %>% 
  mutate(BRat_8 = ((as.numeric(Q12_8) -1)/4)) %>% mutate(BRat_9 = ((as.numeric(Q12_9) -1)/4)) %>% 
  mutate(BRat_10 = ((as.numeric(Q12_10) -1)/4)) %>% mutate(BRat_11 = ((as.numeric(Q12_11) -1)/4)) %>% 
  mutate(BRat_12 = ((as.numeric(Q12_12) -1)/4)) %>% mutate(BRat_13 = ((as.numeric(Q12_13) -1)/4)) %>% 
  mutate(BRat_14 = ((as.numeric(Q12_14) -1)/4)) %>% mutate(BRat_15 = ((as.numeric(Q12_15) -1)/4)) %>% 
  mutate(BRat_16 = ((as.numeric(Q12_16) -1)/4)) %>% mutate(BRat_17 = ((as.numeric(Q12_17) -1)/4)) 
#Transform to Beta Scale between 0 and 1
Complete_test_Anal$BRat_1 <- beta_squeeze(Complete_test_Anal$BRat_1)
Complete_test_Anal$BRat_2 <- beta_squeeze(Complete_test_Anal$BRat_2)
Complete_test_Anal$BRat_3 <- beta_squeeze(Complete_test_Anal$BRat_3)
Complete_test_Anal$BRat_4 <- beta_squeeze(Complete_test_Anal$BRat_4)
Complete_test_Anal$BRat_5 <- beta_squeeze(Complete_test_Anal$BRat_5)
Complete_test_Anal$BRat_6 <- beta_squeeze(Complete_test_Anal$BRat_6)
Complete_test_Anal$BRat_7 <- beta_squeeze(Complete_test_Anal$BRat_7)
Complete_test_Anal$BRat_8 <- beta_squeeze(Complete_test_Anal$BRat_8)
Complete_test_Anal$BRat_9 <- beta_squeeze(Complete_test_Anal$BRat_9)
Complete_test_Anal$BRat_10 <- beta_squeeze(Complete_test_Anal$BRat_10)
Complete_test_Anal$BRat_11 <- beta_squeeze(Complete_test_Anal$BRat_11)
Complete_test_Anal$BRat_12 <- beta_squeeze(Complete_test_Anal$BRat_12)
Complete_test_Anal$BRat_13 <- beta_squeeze(Complete_test_Anal$BRat_13)
Complete_test_Anal$BRat_14 <- beta_squeeze(Complete_test_Anal$BRat_14)
Complete_test_Anal$BRat_15 <- beta_squeeze(Complete_test_Anal$BRat_15)
Complete_test_Anal$BRat_16 <- beta_squeeze(Complete_test_Anal$BRat_16)
Complete_test_Anal$BRat_17 <- beta_squeeze(Complete_test_Anal$BRat_17)


#Change naming Exp. Stress
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_1"] <- "DecreasedInPersonVisits" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_2"] <- "DcrsdVrtulCntct" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_3"] <- "TnsnPplHshld" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_4"] <- "TnsnOtsdHshld" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_5"] <- "LossofEmploy" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_6"] <- "PrblmPay4Grcrs" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_7"] <- "Pay4Bills" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_8"] <- "PrblmHlthcr" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_9"] <- "PrblmPychk" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_10"] <- "ChldcrWrk" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_11"] <- "ObstclsWrkDiff" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_12"] <- "IncrsdWrkLd" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_13"] <- "WrkFrmHm" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_14"] <- "PhysclXrcz" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_15"] <- "DcrsdPtcptnLesre" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_16"] <- "LessNewActvts" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_17"] <- "Boredom" 

#Recode factors for Bayesian Ordinal Analysis - Bel. Stress
#Complete_test_Anal$Q13_1 <- factor(Complete_test_Anal$Q13_1, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q13_2 <- factor(Complete_test_Anal$Q13_2, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q13_3 <- factor(Complete_test_Anal$Q13_3, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q13_4 <- factor(Complete_test_Anal$Q13_4, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )
#Complete_test_Anal$Q13_5 <- factor(Complete_test_Anal$Q13_5, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Change to Beta Regression
Complete_test_Anal <- Complete_test_Anal %>% mutate(BRat_18 = ((as.numeric(Q13_1) -1)/4)) %>% 
  mutate(BRat_19 = ((as.numeric(Q13_2) -1)/4)) %>% mutate(BRat_20 = ((as.numeric(Q13_3) -1)/4)) %>% 
  mutate(BRat_21 = ((as.numeric(Q13_4) -1)/4)) %>% mutate(BRat_22 = ((as.numeric(Q13_5) -1)/4))
#Transform to Beta Scale between 0 and 1
Complete_test_Anal$BRat_18 <- beta_squeeze(Complete_test_Anal$BRat_18)
Complete_test_Anal$BRat_19 <- beta_squeeze(Complete_test_Anal$BRat_19)
Complete_test_Anal$BRat_20 <- beta_squeeze(Complete_test_Anal$BRat_20)
Complete_test_Anal$BRat_21 <- beta_squeeze(Complete_test_Anal$BRat_21)
Complete_test_Anal$BRat_22 <- beta_squeeze(Complete_test_Anal$BRat_22)

#Change naming Bel. Stress
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_18"] <- "BlvngGvmntNotCmbtCOVID" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_19"] <- "CrmnltyIncrsd" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_20"] <- "Blvng_EcnmcCrsis" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_21"] <- "OthrNotFollowingPblcHlthGuidlns" 
names(Complete_test_Anal)[names(Complete_test_Anal) == "BRat_22"] <- "BelLifeIsNotSame" 

#Convert to Long type if needed for overall 
Complete_test_LongAnal <- Complete_test_Anal %>% select(contains(c('Subject', 'TP', 'DecreasedInPersonVisits', 'DcrsdVrtulCntct', 'TnsnPplHshld',
                                                                   'TnsnOtsdHshld', 'LossofEmploy', 'PrblmPay4Grcrs', 'Pay4Bills', 'PrblmHlthcr',
                                                                   'PrblmPychk', 'ChldcrWrk', 'ObstclsWrkDiff', 'IncrsdWrkLd', 'WrkFrmHm', 'PhysclXrcz', 
                                                                   'DcrsdPtcptnLesre', 'LessNewActvts', 'Boredom'))) %>% gather("Question", "Rating", 3:19)



Complete_test_LongAnal_Bel <- Complete_test_Anal %>% select(contains(c('Subject', 'TP', 'BlvngGvmntNotCmbtCOVID', 'CrmnltyIncrsd', 'Blvng_EcnmcCrsis', 'OthrNotFollowingPblcHlthGuidlns', 'BelLifeIsNotSame'))) %>% gather("Question", "Rating", 3:7)

#Experience-Related Stress
#Convert to average Beta with counts
Complete_test_LongAnal_Prev <- Complete_test_LongAnal %>% filter(TP == "Prev") %>% filter(Rating != "NA")
Complete_test_LongAnal_Now <- Complete_test_LongAnal %>% filter(TP == "Now")  %>% filter(Rating != "NA")
count_check_exp <- Complete_test_LongAnal_Prev %>% full_join(Complete_test_LongAnal_Now) %>% group_by(Subject, TP) %>% summarize(Q_count = n()) %>% spread(TP, Q_count) 
count_check_exp$lesser <- ifelse(count_check_exp$Now > count_check_exp$Prev, 'Prev',
                    ifelse(count_check_exp$Now < count_check_exp$Prev, 'Now', 'Neither'))

count_Prev_exp <- count_check_exp %>% filter(lesser == "Prev") #Have to exclude additional questions from now
count_Now_exp <- count_check_exp %>% filter(lesser == "Now") #Have to exclude additional questions from previous
count_na_exp <- count_check_exp %>% filter(is.na(lesser)) #Have to be excluded from analyses
count_neither_exp <- count_check_exp %>% filter(lesser == "Neither") #Can have percentage calculated directly

#count the eligible trials per subject
eligible_count_Prev_exp <- count_Prev_exp[,c(1,3)]
colnames(eligible_count_Prev_exp) <- c("participant","counts")
eligible_count_Now_exp <- count_Now_exp[,c(1,2)]
colnames(eligible_count_Now_exp) <- c("participant","counts")
eligible_count_neither_exp <- count_neither_exp[,c(1,2)]
colnames(eligible_count_neither_exp) <- c("participant","counts")
exp_stress_weights <- rbind(eligible_count_Prev_exp, eligible_count_Now_exp, eligible_count_neither_exp)
colnames(exp_stress_weights) <-c("Subject", "num_weights")
exp_LongAnal_stress_weights <- Complete_test_LongAnal %>% select(Subject, Question, TP, Rating) %>% spread(TP, Rating) %>% filter(! is.na(Now) & ! is.na(Prev) ) %>% group_by(Subject) %>% summarize(Now = mean(Now, na.rm=TRUE), Prev = mean(Prev, na.rm=TRUE)) %>% gather("TP", "b_Rat",2:3) %>% right_join(exp_stress_weights)

#Belief Related Stress
#Convert to average Beta with counts
Complete_test_LongAnal_Bel_Prev <- Complete_test_LongAnal_Bel %>% filter(TP == "Prev") %>% filter(Rating != "NA")
Complete_test_LongAnal_Bel_Now <- Complete_test_LongAnal_Bel %>% filter(TP == "Now")  %>% filter(Rating != "NA")
count_check_bel <- Complete_test_LongAnal_Bel_Prev %>% full_join(Complete_test_LongAnal_Bel_Now) %>% group_by(Subject, TP) %>% summarize(Q_count = n()) %>% spread(TP, Q_count) 
count_check_bel$lesser <- ifelse(count_check_bel$Now > count_check_bel$Prev, 'Prev',
                                 ifelse(count_check_bel$Now < count_check_bel$Prev, 'Now', 'Neither'))

count_Prev_bel <- count_check_bel %>% filter(lesser == "Prev") #Have to exclude additional questions from now
count_Now_bel <- count_check_bel %>% filter(lesser == "Now") #Have to exclude additional questions from previous
count_na_bel <- count_check_bel %>% filter(is.na(lesser)) #Have to be excluded from analyses
count_neither_bel <- count_check_bel %>% filter(lesser == "Neither") #Can have percentage calculated directly

#count the eligible trials per subject
eligible_count_Prev_bel <- count_Prev_bel[,c(1,3)]
colnames(eligible_count_Prev_bel) <- c("participant","counts")
eligible_count_Now_bel <- count_Now_bel[,c(1,2)]
colnames(eligible_count_Now_bel) <- c("participant","counts")
eligible_count_neither_bel <- count_neither_bel[,c(1,2)]
colnames(eligible_count_neither_bel) <- c("participant","counts")
bel_stress_weights <- rbind(eligible_count_Prev_bel, eligible_count_Now_bel, eligible_count_neither_bel)
colnames(bel_stress_weights) <-c("Subject", "num_weights")
bel_Overall_stress_weights <- Complete_test_LongAnal_Bel %>% select(Subject, Question, TP, Rating) %>% spread(TP, Rating) %>% filter(! is.na(Now) & ! is.na(Prev) ) %>% group_by(Subject) %>% summarize(Now = mean(Now, na.rm=TRUE), Prev = mean(Prev, na.rm=TRUE)) %>% gather("TP", "b_Rat",2:3) %>% right_join(bel_stress_weights)


### Longitudinal Deviation ###
#Data Prep: Change data type into integer
#Experience related stress
Complete_test_Dev_Exp <- Complete_test %>% select(Subject, Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_7, Q15_8, Q15_9, Q15_10, Q15_11,
                                              Q15_12, Q15_13, Q15_14, Q15_15, Q15_16, Q15_17, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5,
                                              Q12_6, Q12_7, Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, Q12_14, Q12_15, Q12_16,
                                              Q12_17, Q15_Conf_1) %>%
  mutate(expectation_deviation_experience1 = Q15_1-Q12_1) %>% select(!Q15_1) %>% select(!Q12_1) %>%
  mutate(expectation_deviation_experience2 = Q15_2-Q12_2) %>% select(!Q15_2) %>% select(!Q12_2) %>%
  mutate(expectation_deviation_experience3 = Q15_3-Q12_3) %>% select(!Q15_3) %>% select(!Q12_3) %>%
  mutate(expectation_deviation_experience4 = Q15_4-Q12_4) %>% select(!Q15_4) %>% select(!Q12_4) %>%
  mutate(expectation_deviation_experience5 = Q15_5-Q12_5) %>% select(!Q15_5) %>% select(!Q12_5) %>%
  mutate(expectation_deviation_experience6 = Q15_6-Q12_6) %>% select(!Q15_6) %>% select(!Q12_6) %>%
  mutate(expectation_deviation_experience7 = Q15_7-Q12_7)%>% select(!Q15_7) %>% select(!Q12_7) %>%
  mutate(expectation_deviation_experience8 = Q15_8-Q12_8)%>% select(!Q15_8) %>% select(!Q12_8) %>%
  mutate(expectation_deviation_experience9 = Q15_9-Q12_9)%>% select(!Q15_9) %>% select(!Q12_9) %>%
  mutate(expectation_deviation_experience10 = Q15_10-Q12_10)%>% select(!Q15_10) %>% select(!Q12_10) %>%
  mutate(expectation_deviation_experience11 = Q15_11-Q12_11)%>% select(!Q15_11) %>% select(!Q12_11) %>%
  mutate(expectation_deviation_experience12 = Q15_12-Q12_12)%>% select(!Q15_12) %>% select(!Q12_12) %>%
  mutate(expectation_deviation_experience13 = Q15_13-Q12_13)%>% select(!Q15_13) %>% select(!Q12_13) %>%
  mutate(expectation_deviation_experience14 = Q15_14-Q12_14)%>% select(!Q15_14) %>% select(!Q12_14) %>%
  mutate(expectation_deviation_experience15 = Q15_15-Q12_15)%>% select(!Q15_15) %>% select(!Q12_15) %>%
  mutate(expectation_deviation_experience16 = Q15_16-Q12_16)%>% select(!Q15_16) %>% select(!Q12_16) %>%
  mutate(expectation_deviation_experience17 = Q15_17-Q12_17) %>% select(!Q15_17) %>% select(!Q12_17) %>%
  gather("QExp", "Deviation",starts_with('expectation_deviation_experience')) %>% filter(! is.na(Deviation))
names(Complete_test_Dev_Exp)[names(Complete_test_Dev_Exp) == "Q15_Conf_1"] <- "Confidence" 
Complete_test_Dev_Exp <- Complete_test_Dev_Exp %>%  mutate(Conf_Rat = case_when(Confidence == 1 ~ "Low",  Confidence == 5  ~ "High", Confidence == 2 ~ "Low", Confidence == 3 ~ "Med", Confidence == 4 ~ "High" )) 
Complete_test_Dev_Exp$Conf_Rat <- factor(Complete_test_Dev_Exp$Conf_Rat, levels = c("Low","Med", "High"), ordered = TRUE)

#Convert to average Beta with counts
Complete_test_Dev_Exp_Conf <- Complete_test_Dev_Exp %>% select(Subject, Conf_Rat)
Complete_test_Dev_Exp_Conf <- unique(Complete_test_Dev_Exp_Conf)
Complete_test_Dev_Exp_QDev <- Complete_test_Dev_Exp %>% select(Subject, QExp, Deviation) %>% group_by(Subject) %>% summarize(avg_dev = mean(Deviation)) 
Dev_Exp_Weights <- exp_LongAnal_stress_weights %>% select(Subject,num_weights)
Dev_Exp_Weights <- unique(Dev_Exp_Weights)
Complete_Dev_Exp_Conf_Ovrl <- Complete_test_Dev_Exp_Conf %>% left_join(Complete_test_Dev_Exp_QDev) %>% left_join(Dev_Exp_Weights)
Complete_Dev_Exp_Conf_Ovrl <- Complete_Dev_Exp_Conf_Ovrl %>% mutate(BDev = (avg_dev + 4)/8)
Complete_Dev_Exp_Conf_Ovrl$BDev <- beta_squeeze(Complete_Dev_Exp_Conf_Ovrl$BDev )
Complete_Dev_Exp_Conf_Ovrl$num_weights[is.na(Complete_Dev_Exp_Conf_Ovrl$num_weights)] <- 0

#Belief Related Stress
Complete_test_Dev_Bel <- Complete_test %>% select(Subject, Q16_1, Q16_2, Q16_3, Q16_4, Q16_5, Q13_1, Q13_2, Q13_3, Q13_4, Q13_5, Q16_Conf_1) %>%
  mutate(expectation_deviation_belief1 = Q16_1-Q13_1) %>% select(!Q16_1) %>% select(!Q13_1) %>%
  mutate(expectation_deviation_belief2 = Q16_2-Q13_2)%>%  select(!Q16_2) %>% select(!Q13_2) %>%
  mutate(expectation_deviation_belief3 = Q16_3-Q13_3)%>%  select(!Q16_3) %>% select(!Q13_3) %>%
  mutate(expectation_deviation_belief4 = Q16_4-Q13_4)%>%  select(!Q16_4) %>% select(!Q13_4) %>%
  mutate(expectation_deviation_belief5 = Q16_5-Q13_5) %>% select(!Q16_5) %>% select(!Q13_5) %>% 
  gather("QBelief", "Deviation",starts_with('expectation_deviation_belief'))  %>% filter(! is.na(Deviation))
names(Complete_test_Dev_Bel)[names(Complete_test_Dev_Bel) == "Q16_Conf_1"] <- "Confidence" 
Complete_test_Dev_Bel <- Complete_test_Dev_Bel %>%  mutate(Conf_Rat = case_when(Confidence == 1 ~ "Low", Confidence == 5  ~ "High", Confidence == 2 ~ "Low", Confidence == 3 ~ "Med", Confidence == 4 ~ "High"))
Complete_test_Dev_Bel$Conf_Rat <- factor(Complete_test_Dev_Bel$Conf_Rat, levels = c("Low","Med", "High"), ordered = TRUE)

#Convert to average Beta with counts
Complete_test_Dev_Bel_Conf <- Complete_test_Dev_Bel %>% select(Subject, Conf_Rat)
Complete_test_Dev_Bel_Conf <- unique(Complete_test_Dev_Bel_Conf)
Complete_test_Dev_Bel_QDev <- Complete_test_Dev_Bel %>% select(Subject, QBelief, Deviation) %>% group_by(Subject) %>% summarize(avg_dev = mean(Deviation)) 
Dev_Bel_Weights <- bel_Overall_stress_weights %>% select(Subject,num_weights)
Dev_Bel_Weights <- unique(Dev_Bel_Weights)
Complete_Dev_Bel_Conf_Ovrl <- Complete_test_Dev_Bel_Conf %>% left_join(Complete_test_Dev_Bel_QDev) %>% left_join(Dev_Bel_Weights)
Complete_Dev_Bel_Conf_Ovrl <- Complete_Dev_Bel_Conf_Ovrl %>% mutate(BDev = (avg_dev + 4)/8)
Complete_Dev_Bel_Conf_Ovrl$BDev <- beta_squeeze(Complete_Dev_Bel_Conf_Ovrl$BDev )
Complete_Dev_Bel_Conf_Ovrl$num_weights[is.na(Complete_Dev_Bel_Conf_Ovrl$num_weights)] <- 0

Complete_test_Analy <- Complete_test %>% select(Subject, Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_7, Q15_8, Q15_9, Q15_10, Q15_11,
                                                    Q15_12, Q15_13, Q15_14, Q15_15, Q15_16, Q15_17, Q12_1, Q12_2, Q12_3, Q12_4, Q12_5,
                                                    Q12_6, Q12_7, Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, Q12_14, Q12_15, Q12_16,
                                                    Q12_17, Q16_1, Q16_2, Q16_3, Q16_4, Q16_5, Q13_1, Q13_2, Q13_3, Q13_4, Q13_5) %>%
  mutate(expectation_deviation_experience1 = Q15_1-Q12_1) %>% select(!Q15_1) %>% select(!Q12_1) %>%
  mutate(expectation_deviation_experience2 = Q15_2-Q12_2) %>% select(!Q15_2) %>% select(!Q12_2) %>%
  mutate(expectation_deviation_experience3 = Q15_3-Q12_3) %>% select(!Q15_3) %>% select(!Q12_3) %>%
  mutate(expectation_deviation_experience4 = Q15_4-Q12_4) %>% select(!Q15_4) %>% select(!Q12_4) %>%
  mutate(expectation_deviation_experience5 = Q15_5-Q12_5) %>% select(!Q15_5) %>% select(!Q12_5) %>%
  mutate(expectation_deviation_experience6 = Q15_6-Q12_6) %>% select(!Q15_6) %>% select(!Q12_6) %>%
  mutate(expectation_deviation_experience7 = Q15_7-Q12_7)%>% select(!Q15_7) %>% select(!Q12_7) %>%
  mutate(expectation_deviation_experience8 = Q15_8-Q12_8)%>% select(!Q15_8) %>% select(!Q12_8) %>%
  mutate(expectation_deviation_experience9 = Q15_9-Q12_9)%>% select(!Q15_9) %>% select(!Q12_9) %>%
  mutate(expectation_deviation_experience10 = Q15_10-Q12_10)%>% select(!Q15_10) %>% select(!Q12_10) %>%
  mutate(expectation_deviation_experience11 = Q15_11-Q12_11)%>% select(!Q15_11) %>% select(!Q12_11) %>%
  mutate(expectation_deviation_experience12 = Q15_12-Q12_12)%>% select(!Q15_12) %>% select(!Q12_12) %>%
  mutate(expectation_deviation_experience13 = Q15_13-Q12_13)%>% select(!Q15_13) %>% select(!Q12_13) %>%
  mutate(expectation_deviation_experience14 = Q15_14-Q12_14)%>% select(!Q15_14) %>% select(!Q12_14) %>%
  mutate(expectation_deviation_experience15 = Q15_15-Q12_15)%>% select(!Q15_15) %>% select(!Q12_15) %>%
  mutate(expectation_deviation_experience16 = Q15_16-Q12_16)%>% select(!Q15_16) %>% select(!Q12_16) %>%
  mutate(expectation_deviation_experience17 = Q15_17-Q12_17) %>% select(!Q15_17) %>% select(!Q12_17) %>%
  mutate(expectation_deviation_belief1 = Q16_1-Q13_1) %>% select(!Q16_1) %>% select(!Q13_1) %>%
  mutate(expectation_deviation_belief2 = Q16_2-Q13_2)%>%  select(!Q16_2) %>% select(!Q13_2) %>%
  mutate(expectation_deviation_belief3 = Q16_3-Q13_3)%>%  select(!Q16_3) %>% select(!Q13_3) %>%
  mutate(expectation_deviation_belief4 = Q16_4-Q13_4)%>%  select(!Q16_4) %>% select(!Q13_4) %>%
  mutate(expectation_deviation_belief5 = Q16_5-Q13_5) %>% select(!Q16_5) %>% select(!Q13_5)

#Re-Score 0 to NA#Complete_test_Analy[Complete_test_Analy == 0] <- NA

#Deviation of Expectations: (.129 - .758)
means_Expectation <- Complete_test_Dev_Exp %>% group_by(QExp) %>% summarize(mean = mean(Deviation, na.rm=TRUE))
Complete_test_Dev_Grph_Exp <- Complete_test_Dev_Exp 

Complete_test_Dev_Grph_Bel <- Complete_test_Dev_Bel %>% mutate(BDev = (Deviation + 4)/8) %>% filter(! is.na(QBelief))
Complete_test_Dev_Grph_Exp$QExp <- dplyr::recode(Complete_test_Dev_Grph_Exp$QExp, "expectation_deviation_experience1" = 'Decreased_In_Person_Visits', "expectation_deviation_experience2" ="Decreased_Virtual_Contact", 
                                             "expectation_deviation_experience3" ="Tension_wPpl_inHouse", "expectation_deviation_experience4" ="Tension_wPpl_Outside_House", "expectation_deviation_experience5" ="Loss_of_Employment",
                                             "expectation_deviation_experience6" ="Problems_Paying_4_Groceries", "expectation_deviation_experience7" ="Problems_Paying_4_Bills", "expectation_deviation_experience8" ="Problem_Accessing_Healthcare",
                                             "expectation_deviation_experience9" ="Problem_w_Usual_Paycheck", "expectation_deviation_experience10" ="diff_combining_childcare_w_work", "expectation_deviation_experience11" ="obstacles_make_work_more_diff",
                                             "expectation_deviation_experience12" ="increased_work_load", "expectation_deviation_experience13" ="working_from_home", "expectation_deviation_experience14" ="decreased_physical_exercise" , 
                                             "expectation_deviation_experience15" ="decreased_partic_leisure_activies", "expectation_deviation_experience16" ="decrease_new_fun_activities", "expectation_deviation_experience17" ="boredom")
Complete_test_Dev_Grph_Exp %>% ggplot(aes(x = Deviation, y = QExp, group=QExp)) + geom_violin() +scale_x_continuous(n.breaks = 9)+ ggtitle("Deviation of past stress from remembered covid-related stress") +
  stat_summary(fun.y="mean", color="red", shape=15)
#Deviation of Beliefs (.219 - .987)
Complete_test_Dev_Grph_Bel$QBelief <- dplyr::recode(Complete_test_Dev_Grph_Bel$QBelief, "expectation_deviation_belief1" = 'government_not_appropriate_combat_covid', "expectation_deviation_belief2" ="criminality_rates_have_increased", 
                                                "expectation_deviation_belief3" ="currently_an_economic_crisis", "expectation_deviation_belief4" ="other_not_following_publichealth_guidelines", "expectation_deviation_belief5" ="life_is_not_same")
Complete_test_Dev_Grph_Bel %>% ggplot(aes(x = Deviation, y = QBelief, group=QBelief)) + geom_violin() +scale_x_continuous(n.breaks = 9)+ ggtitle("Deviation of past stress from remembered belief related stress") +
  stat_summary(fun.y="mean", color="red", shape=15)

#Deviations between experience and beliefs
Complete_test_Dev_Bel_1 <- Complete_test_Dev_Bel %>% mutate(Mem = "Bel") 
names(Complete_test_Dev_Bel_1)[names(Complete_test_Dev_Bel_1) == "QBelief"] <- "Q" 
Complete_test_Dev_Exp_2 <- Complete_test_Dev_Exp %>% mutate(Mem = "Exp")
names(Complete_test_Dev_Exp_2)[names(Complete_test_Dev_Exp_2) == "QExp"] <- "Q" 

Dev3 <- Complete_test_Dev_Bel_1 %>% full_join(Complete_test_Dev_Exp_2)
#Dev3$Deviation1 <- factor(Dev3$Deviation1, levels = c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4"), ordered = TRUE )

#Do this for beta with counts
Beta_Complete_test_Dev_Exp_1 <- DevxConf_Exp_Ovrl %>% mutate(Mem = "Exp")
Beta_Complete_test_Dev_Bel_1 <- DevxConf_Bel_Ovrl %>% mutate(Mem = "Bel")
Beta_DevBelExp <- Beta_Complete_test_Dev_Exp_1 %>% full_join(Beta_Complete_test_Dev_Bel_1)


#Deviations of Experience x Confidence
Complete_test_Dev_Exp[Complete_test_Dev_Exp == ""] <- NA
Complete_test_Dev_Exp$Conf_Rat <- na.omit(Complete_test_Dev_Exp$Conf_Rat)
Complete_test_Dev_Exp %>% ggplot(aes(x=as.factor(Conf_Rat), y= BDev, fill=Conf_Rat)) +geom_violin()+theme_bw()+scale_y_continuous(n.breaks = 5)+ ggtitle("Deviations from experienced-related stress x Confidence") +
  stat_summary(fun.y="mean", color="red", shape=15)
#Complete_test_Dev$Q15_Conf_1 <- factor(Complete_test_Dev$Q15_Conf_1, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Deviations of Beliefs x Confidence
Complete_test_Dev_Bel[Complete_test_Dev_Bel == ""] <- NA
Complete_test_Dev_Bel$Conf_Rat <- na.omit(Complete_test_Dev_Bel$Conf_Rat)
Complete_test_Dev_Bel %>% ggplot(aes(x=as.factor(Conf_Rat), y= BDev, fill=Conf_Rat)) +geom_violin()+theme_bw()+scale_y_continuous(n.breaks = 5)+ ggtitle("Deviations from experienced-related stress x Confidence") +
  stat_summary(fun.y="mean", color="red", shape=15)
#Complete_test_Dev$Q15_Conf_1 <- factor(Complete_test_Dev$Q15_Conf_1, levels = c("1", "2", "3", "4", "5"), ordered = TRUE )

#Deviations of Stress x Type (Exp/Bel)
Complete_test_Dev_1 <- Complete_test_Dev_Exp
names(Complete_test_Dev_1)[names(Complete_test_Dev_1) == "QExp"] <- "Q" 
Complete_test_Dev_2 <- Complete_test_Dev_Bel
names(Complete_test_Dev_2)[names(Complete_test_Dev_2) == "QBelief"] <- "Q" 
StressDevxType <- Complete_test_Dev_1 %>% full_join(Complete_test_Dev_2)

##Massage data for regression analyses about what predicts past and future stress/behavior ##
Stress_Experienced_Covid1 <- Previous_Filtered %>% group_by(Subject) %>% summarise(mean=mean(c(Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, Q12_14, Q12_15, Q12_16, Q12_17), na.rm=T))
as.data.frame(Stress_Experienced_Covid1)
names(Stress_Experienced_Covid1)[names(Stress_Experienced_Covid1) == "mean"] <- "Covid_Exp1" 

Stress_Experienced_Covid2 <- All_Data_Ans %>% group_by(Subject) %>% summarise(mean=mean(c(Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q12_8, Q12_9, Q12_10, Q12_11, Q12_12, Q12_13, Q12_14, Q12_15, Q12_16, Q12_17), na.rm=T))
as.data.frame(Stress_Experienced_Covid2)
names(Stress_Experienced_Covid2)[names(Stress_Experienced_Covid2) == "mean"] <- "Covid_Exp2" 

Stress_Experienced_Belief1 <- Previous_Filtered %>% group_by(Subject) %>% summarise(mean=mean(c(Q13_1, Q13_2, Q13_3, Q13_4, Q13_5), na.rm=T))
as.data.frame(Stress_Experienced_Belief1)
names(Stress_Experienced_Belief1)[names(Stress_Experienced_Belief1) == "mean"] <- "Belief_Exp1" 

Stress_Experienced_Belief2 <- All_Data_Ans %>% group_by(Subject) %>% summarise(mean=mean(c(Q13_1, Q13_2, Q13_3, Q13_4, Q13_5), na.rm=T))
as.data.frame(Stress_Experienced_Belief2)
names(Stress_Experienced_Belief2)[names(Stress_Experienced_Belief2) == "mean"] <- "Belief_Exp2" 

Stress_Remembered_Covid <- Complete_test %>% group_by(Subject) %>% summarise(mean=mean(c(Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6, Q15_7, Q15_8, Q15_9, Q15_10, Q15_11, Q15_12, Q15_13, Q15_14, Q15_15, Q15_16, Q15_17), na.rm=T))
as.data.frame(Stress_Remembered_Covid)
names(Stress_Remembered_Covid)[names(Stress_Remembered_Covid) == "mean"] <- "Covid_Rem" 

Stress_Remembered_Belief <- Complete_test %>% group_by(Subject) %>% summarise(mean=mean(c(Q16_1, Q16_2, Q16_3, Q16_4, Q16_5), na.rm=T))
as.data.frame(Stress_Remembered_Belief)
names(Stress_Remembered_Belief)[names(Stress_Remembered_Belief) == "mean"] <- "Belief_Rem" 
#Combine dataframes
Stress_Out <- Stress_Experienced_Covid1 %>% right_join(Stress_Experienced_Covid2) %>% right_join(Stress_Experienced_Belief1)  %>% right_join(Stress_Experienced_Belief2) %>% right_join(Stress_Remembered_Covid) %>% right_join(Stress_Remembered_Belief)

#Does their experience of covid stress at TP 1 or 2 predict their remembered covid stress
Stress_Out_Beta <- Stress_Out %>% mutate(BCovid_Rem=((Covid_Rem-1)/4))
Stress_Out_Beta$BCovid_Rem <- beta_squeeze(Stress_Out_Beta$BCovid_Rem)

#Does their experience of belief stress at TP 1 or 2 predict their remembered belief stress
Stress_Out_Beta <- Stress_Out_Beta %>% mutate(BBelief_Rem=((Belief_Rem-1)/4))
Stress_Out_Beta$BBelief_Rem <- beta_squeeze(Stress_Out_Beta$BBelief_Rem)

#Redo both weighted
Stress_Out_Beta_Rem <- Stress_Out_Beta %>% select(Subject, BCovid_Rem, BBelief_Rem)
Stress_Out_Beta_RemExp <- Stress_Out_Beta_Rem %>% select(Subject, BCovid_Rem)
Stress_Out_Beta_RemBel <- Stress_Out_Beta_Rem %>% select(Subject, BBelief_Rem)
Stress_Out_Beta_Bel <- Stress_Change_OvrlBel %>% select(Subject, num_weights, TP, b_Rat) %>% spread(TP, b_Rat)
colnames(Stress_Out_Beta_Bel) <- c("Subject", "num_weights", "Belief_Exp2", "Belief_Exp1")
Stress_Out_Beta_RemBel<- Stress_Out_Beta_Bel %>% full_join(Stress_Out_Beta_RemBel)
Stress_Out_Beta_Exp <- Stress_Change_Ovrl %>% select(Subject, num_weights, TP, b_Rat) %>% spread(TP, b_Rat)
colnames(Stress_Out_Beta_Exp) <-  c("Subject", "num_weights", "Covid_Exp2", "Covid_Exp1")
Stress_Out_Beta_RemExp<- Stress_Out_Beta_Exp %>% full_join(Stress_Out_Beta_RemExp)
  
## Does either the memory experienced, or current experience predict what will occur in the future. (Q48 - Q53)
Future_Q <- All_Data_Ans %>% select(starts_with(c("Q48", "Q49", "Q50", "Q51", "Q52", "Q53")))
Future_Q <- Future_Q[-1:-2,]
#Future_Q[Future_Q == 0] <- NA
Future_Q[Future_Q == ""] <- NA
Future_Q_res <- Future_Q
#Re-Score Future Data
#Q49
Future_Q_res$Q49<-dplyr::recode(Future_Q_res$Q49, "Yes" = '1', "No" ="0" )
#Q50
Future_Q_res$Q50<-dplyr::recode(Future_Q_res$Q50, "Never" = '0', "A few times/year" ="1", "A few times/month" ="2", "A few times/week" ="3", "Once/week" ="4","Every day" ="5" )
#Q51
Future_Q_res$Q51<-dplyr::recode(Future_Q_res$Q51, "Never" = '0', "A few times/year" ="1", "A few times/month" ="2", "A few times/week" ="3", "Once/week" ="4","Every day" ="5" )
#Q52_1
Future_Q_res$Q52_1<-dplyr::recode(Future_Q_res$Q52_1, "less than 1 day" = '0', "at least 1 day" ="1", "2-3 days" ="2", "4-5 days" ="3", "every day or nearly every day" ="4")
#Q52_2
Future_Q_res$Q52_2<-dplyr::recode(Future_Q_res$Q52_2, "less than 1 day" = '0', "at least 1 day" ="1", "2-3 days" ="2", "4-5 days" ="3", "every day or nearly every day" ="4")
#Q52_3
Future_Q_res$Q52_3<-dplyr::recode(Future_Q_res$Q52_3, "less than 1 day" = '0', "at least 1 day" ="1", "2-3 days" ="2", "4-5 days" ="3", "every day or nearly every day" ="4")
#Q52_4
Future_Q_res$Q52_4<-dplyr::recode(Future_Q_res$Q52_4, "less than 1 day" = '0', "at least 1 day" ="1", "2-3 days" ="2", "4-5 days" ="3", "every day or nearly every day" ="4")
#Q52_5
Future_Q_res$Q52_5<-dplyr::recode(Future_Q_res$Q52_5, "less than 1 day" = '0', "at least 1 day" ="1", "2-3 days" ="2", "4-5 days" ="3", "every day or nearly every day" ="4")
#Q53_1
Future_Q_res$Q53_1<-dplyr::recode(Future_Q_res$Q53_1, "less than 30 minutes" = '0', "30 minutes - 1 hour" ="1", "1-2 hours" ="2", "2-4 hours" ="3", "4+ hours" ="4")
#Q53_2
Future_Q_res$Q53_2<-dplyr::recode(Future_Q_res$Q53_2, "less than 30 minutes" = '0', "30 minutes - 1 hour" ="1", "1-2 hours" ="2", "2-4 hours" ="3", "4+ hours" ="4")
#Q53_3
Future_Q_res$Q53_3<-dplyr::recode(Future_Q_res$Q53_3, "less than 30 minutes" = '0', "30 minutes - 1 hour" ="1", "1-2 hours" ="2", "2-4 hours" ="3", "4+ hours" ="4")
#Q53_4
Future_Q_res$Q53_4<-dplyr::recode(Future_Q_res$Q53_4, "less than 30 minutes" = '0', "30 minutes - 1 hour" ="1", "1-2 hours" ="2", "2-4 hours" ="3", "4+ hours" ="4")
#Q53_5
Future_Q_res$Q53_5<-dplyr::recode(Future_Q_res$Q53_5, "less than 30 minutes" = '0', "30 minutes - 1 hour" ="1", "1-2 hours" ="2", "2-4 hours" ="3", "4+ hours" ="4")

#Past - Work, Shop, Leisure
Mem <- All_Data_Ans %>% select(starts_with(c("Subject", "Q18", "Q19", "Q20", "Q21", "Q22"))) %>% mutate(TP="Past")
Mem <- Mem[-1:-2,]
#Mem[Mem == 0] <- NA
Mem[Mem == ""] <- NA

#Future Work/shope/Leisure (Behavior)
Future <- All_Data_Ans %>% select(starts_with(c("Subject", "Q49", "Q50", "Q51", "Q52", "Q53"))) %>% mutate(TP = "Future")
Future <- Future[-1:-2,]
#Future[Future == 0] <- NA
Future[Future == ""] <- NA
names(Future)[names(Future) == "Q49"] <- "Q18" #ability to work from home 
names(Future)[names(Future) == "Q50"] <- "Q19" #frequency of wfh 
names(Future)[names(Future) == "Q51"] <- "Q20" #grocery frequency
names(Future)[names(Future) == "Q52_1"] <- "Q21_1" 
names(Future)[names(Future) == "Q52_2"] <- "Q21_2" 
names(Future)[names(Future) == "Q52_3"] <- "Q21_3" 
names(Future)[names(Future) == "Q52_4"] <- "Q21_4" 
names(Future)[names(Future) == "Q52_5"] <- "Q21_5" 
names(Future)[names(Future) == "Q53_1"] <- "Q22_1" 
names(Future)[names(Future) == "Q53_2"] <- "Q22_2" 
names(Future)[names(Future) == "Q53_3"] <- "Q22_3" 
names(Future)[names(Future) == "Q53_4"] <- "Q22_4" 
names(Future)[names(Future) == "Q53_5"] <- "Q22_5" 
MemFut <- Mem %>% full_join(Future)
#Change variables into factor
MemFut$Subject <- as.factor(MemFut$Subject)
MemFut$TP <- as.factor(MemFut$TP)

#Recode questions
#Ability to stay home;
MemFut$Q18[is.na(MemFut$Q18)] <- 0
MemFut$Q18<-dplyr::recode(MemFut$Q18, "No" = '0', "Yes" ="1")
MemFut$Q18 <- as.numeric(MemFut$Q18)
#Freq of staying home
MemFut$Q19 <- factor(MemFut$Q19, levels = c("Never", "A few times/year", "A few times/month", "A few times/week",
                                            "Once/week", "Every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q19"] <- "Freq_StayingHome"
#freq of grocery store visits
MemFut$Q20 <- factor(MemFut$Q20, levels = c("Never", "A few times/year", "A few times/month", "A few times/week",
                                            "Once/week", "Every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q20"] <- "Freq_GrocStoreVisits"
#Q21: After COVID-19 is no longer a threat:\n\n\n\nAbout how many days each week do you expect to spend time on leisure ... - 
#_1: At home without socializing with others?
MemFut$Q21_1 <- factor(MemFut$Q21_1, levels = c("less than 1 day", "at least 1 day", "2-3 days", "4-5 days",
                                                "every day or nearly every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q21_1"] <- "CovidNoThreat"
#_2: Outside your home without socializing with others?
MemFut$Q21_2 <- factor(MemFut$Q21_2, levels = c("less than 1 day", "at least 1 day", "2-3 days", "4-5 days",
                                                "every day or nearly every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q21_2"] <- "OutsideHomeSocializingOthers"
#_3: Socializing with others digitally?; LESS in the future
MemFut$Q21_3 <- factor(MemFut$Q21_3, levels = c("less than 1 day", "at least 1 day", "2-3 days", "4-5 days",
                                                "every day or nearly every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q21_3"] <- "SocializingOthersDigitally"
#_4: Socializing with others in person in your home or in other people’s home?; NO DIFFERENCE
MemFut$Q21_4 <- factor(MemFut$Q21_4, levels = c("less than 1 day", "at least 1 day", "2-3 days", "4-5 days",
                                                "every day or nearly every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q21_4"] <- "SocializingOthersInPerson"
#_5: Socializing with others in person outside your home?; MORE in the future
MemFut$Q21_5 <- factor(MemFut$Q21_5, levels = c("less than 1 day", "at least 1 day", "2-3 days", "4-5 days",
                                                "every day or nearly every day"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q21_5"] <- "SocializingOthersOutsideHome"
#Q22: On the days you expect to engage in these activities for leisure, about how much time each day do you expect to spend on them?
#_1: At home without socializing with others; LESS in the future
MemFut$Q22_1 <- factor(MemFut$Q22_1, levels = c("less than 30 minutes", "30 minutes - 1 hour", "1-2 hours", "2-4 hours",
                                                "4+ hours"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q22_1"] <- "AtHomeNotSicializingwOthers"
#_2: Outside your home without socializing with others?; MORE in the future
MemFut$Q22_2 <- factor(MemFut$Q22_2, levels = c("less than 30 minutes", "30 minutes - 1 hour", "1-2 hours", "2-4 hours",
                                                "4+ hours"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q22_2"] <- "OutsideHome_NotSicializingwOthers"
#_3: Socializing with others digitally?;  NO DIFFERENCE
MemFut$Q22_3 <- factor(MemFut$Q22_3, levels = c("less than 30 minutes", "30 minutes - 1 hour", "1-2 hours", "2-4 hours",
                                                "4+ hours"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q22_3"] <- "SocializingwOthers_Digitally"
#_4: Socializing with others in person in your home or in other people’s home?; MORE in the future
MemFut$Q22_4 <- factor(MemFut$Q22_4, levels = c("less than 30 minutes", "30 minutes - 1 hour", "1-2 hours", "2-4 hours",
                                                "4+ hours"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q22_4"] <- "SocializingwOthers_YourorOtherHome"
#_5: Socializing with others in person outside your home?;  MORE in the future
MemFut$Q22_5 <- factor(MemFut$Q22_5, levels = c("less than 30 minutes", "30 minutes - 1 hour", "1-2 hours", "2-4 hours",
                                                "4+ hours"), ordered = TRUE )
names(MemFut)[names(MemFut) == "Q22_5"] <- "SocializingwOthers_InPerson_OutsideHome"

#Q7: Does remembered emotion predict people’s attitude towards future behavior?
#Massage Data - Groceries
MTurk_Groc <- MTurk_Data %>% select(c("Q2", "Q15_6", "Q12_6", "Q51")) #Select all questions
names(MTurk_Groc)[names(MTurk_Groc) == "Q2"] <- "Subject"  
names(MTurk_Groc)[names(MTurk_Groc) == "Q15_6"] <- "Mem_Exp" 
names(MTurk_Groc)[names(MTurk_Groc) == "Q12_6"] <- "Curr_Exp"
names(MTurk_Groc)[names(MTurk_Groc) == "Q51"] <- "Future"
Prolific_Groc <- Prolific_Data %>% select(c("Q2", "Q15_6", "Q12_6", "Q51")) #Select all questions
names(Prolific_Groc)[names(Prolific_Groc) == "Q2"] <- "Subject"  
names(Prolific_Groc)[names(Prolific_Groc) == "Q15_6"] <- "Mem_Exp" 
names(Prolific_Groc)[names(Prolific_Groc) == "Q12_6"] <- "Curr_Exp"
names(Prolific_Groc)[names(Prolific_Groc) == "Q51"] <- "Future"
Previous_Groc <- Previous_Data %>% select(starts_with(c("SubjectID_Prolific_MTurk", "COVID_25_stress_lifechanges_6")))
names(Previous_Groc)[names(Previous_Groc) == "SubjectID_Prolific_MTurk"] <- "Subject" 
names(Previous_Groc)[names(Previous_Groc) == "COVID_25_stress_lifechanges_6"] <- "Past_Exp" 
Q20 <- MemFut %>% filter(TP == "Past") %>% select(Subject, Freq_GrocStoreVisits) %>% filter(! is.na(Freq_GrocStoreVisits) & ! is.na(Subject) )
Q20$Freq_GrocStoreVisits <- as.character(Q20$Freq_GrocStoreVisits)

Full_Groc <- MTurk_Groc %>% full_join(Prolific_Groc) %>% right_join(Previous_Groc) 
Full_Groc$Subject <- as.factor(Full_Groc$Subject)
Full_Groc <- Full_Groc[-1:-2,]
Full_Groc[Full_Groc == ""] <- NA
Full_Groc[Full_Groc == "N/A"] <- NA
#Full_Groc$Past_Exp[Full_Groc$Past_Exp == 0] <- NA
Full_Groc <- Full_Groc[!duplicated(Full_Groc$Subject), ]
#Rescore:
Full_Groc$Mem_Exp<-dplyr::recode(Full_Groc$Mem_Exp, "No stress at all\n\n1\n" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Groc$Mem_Exp <- as.integer(Full_Groc$Mem_Exp)
Full_Groc$Curr_Exp<-dplyr::recode(Full_Groc$Curr_Exp, "No stress at all\n\n1" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Groc$Curr_Exp <- as.integer(Full_Groc$Curr_Exp)
Full_Groc$Future <- factor(Full_Groc$Future, levels = c("Never", "A few times/year", "A few times/month", "A few times/week", "Once/week","Every day", "A few times/day"), ordered = TRUE )
Full_Groc <- Full_Groc %>% mutate(Dev_Mem= Mem_Exp - Past_Exp) %>% filter(! is.na(Subject))


#Massage data - Work
MTurk_Work <- MTurk_Data %>% select(c("Q2", "Q15_10", "Q15_11", "Q15_12", "Q15_13",
                                      "Q12_10", "Q12_11", "Q12_12", "Q12_13",
                                      "Q50")) #Select all questions
names(MTurk_Work)[names(MTurk_Work) == "Q2"] <- "Subject"  
names(MTurk_Work)[names(MTurk_Work) == "Q15_10"] <- "Mem_Exp_1" 
names(MTurk_Work)[names(MTurk_Work) == "Q15_11"] <- "Mem_Exp_2" 
names(MTurk_Work)[names(MTurk_Work) == "Q15_12"] <- "Mem_Exp_3" 
names(MTurk_Work)[names(MTurk_Work) == "Q15_13"] <- "Mem_Exp_4" 
names(MTurk_Work)[names(MTurk_Work) == "Q12_10"] <- "Curr_Exp_1" 
names(MTurk_Work)[names(MTurk_Work) == "Q12_11"] <- "Curr_Exp_2" 
names(MTurk_Work)[names(MTurk_Work) == "Q12_12"] <- "Curr_Exp_3" 
names(MTurk_Work)[names(MTurk_Work) == "Q12_13"] <- "Curr_Exp_4" 
names(MTurk_Work)[names(MTurk_Work) == "Q50"] <- "Future" 
Prolific_Work <- Prolific_Data %>% select(c("Q2", "Q15_10", "Q15_11", "Q15_12", "Q15_13",
                                            "Q12_10", "Q12_11", "Q12_12", "Q12_13",
                                            "Q50")) #Select all questions
names(Prolific_Work)[names(Prolific_Work) == "Q2"] <- "Subject"  
names(Prolific_Work)[names(Prolific_Work) == "Q15_10"] <- "Mem_Exp_1" 
names(Prolific_Work)[names(Prolific_Work) == "Q15_11"] <- "Mem_Exp_2" 
names(Prolific_Work)[names(Prolific_Work) == "Q15_12"] <- "Mem_Exp_3" 
names(Prolific_Work)[names(Prolific_Work) == "Q15_13"] <- "Mem_Exp_4" 
names(Prolific_Work)[names(Prolific_Work) == "Q12_10"] <- "Curr_Exp_1" 
names(Prolific_Work)[names(Prolific_Work) == "Q12_11"] <- "Curr_Exp_2" 
names(Prolific_Work)[names(Prolific_Work) == "Q12_12"] <- "Curr_Exp_3" 
names(Prolific_Work)[names(Prolific_Work) == "Q12_13"] <- "Curr_Exp_4" 
names(Prolific_Work)[names(Prolific_Work) == "Q50"] <- "Future" 
Previous_Work <- Previous_Data %>% select(starts_with(c("SubjectID_Prolific_MTurk", "COVID_29_stress_lifechanges_10", 
                                                        "COVID_30_stress_lifechanges_11", "COVID_31_stress_lifechanges_12",
                                                        "COVID_32_stress_lifechanges_13")))
names(Previous_Work)[names(Previous_Work) == "SubjectID_Prolific_MTurk"] <- "Subject" 
names(Previous_Work)[names(Previous_Work) == "COVID_29_stress_lifechanges_10"] <- "Past_Exp_1" 
names(Previous_Work)[names(Previous_Work) == "COVID_30_stress_lifechanges_11"] <- "Past_Exp_2" 
names(Previous_Work)[names(Previous_Work) == "COVID_31_stress_lifechanges_12"] <- "Past_Exp_3" 
names(Previous_Work)[names(Previous_Work) == "COVID_32_stress_lifechanges_13"] <- "Past_Exp_4" 
Full_Work <- MTurk_Work %>% full_join(Prolific_Work) %>% full_join(Previous_Work) 
Full_Work$Subject <- as.factor(Full_Work$Subject)
Full_Work <- Full_Work[-1:-2,]
Full_Work[Full_Work == ""] <- NA
Full_Work[Full_Work == "N/A"] <- NA
#Rescore:
Full_Work$Mem_Exp_1 <-dplyr::recode(Full_Work$Mem_Exp_1, "No stress at all\n\n1\n" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Mem_Exp_1 <- as.integer(Full_Work$Mem_Exp_1)
Full_Work$Mem_Exp_2 <-dplyr::recode(Full_Work$Mem_Exp_2, "No stress at all\n\n1\n" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Mem_Exp_2 <- as.integer(Full_Work$Mem_Exp_2)
Full_Work$Mem_Exp_3 <-dplyr::recode(Full_Work$Mem_Exp_3, "No stress at all\n\n1\n" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Mem_Exp_3 <- as.integer(Full_Work$Mem_Exp_3)
Full_Work$Mem_Exp_4 <-dplyr::recode(Full_Work$Mem_Exp_4, "No stress at all\n\n1\n" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Mem_Exp_4 <- as.integer(Full_Work$Mem_Exp_4)
Full_Work$Curr_Exp_1 <-dplyr::recode(Full_Work$Curr_Exp_1, "No stress at all\n\n1" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Curr_Exp_1 <- as.integer(Full_Work$Curr_Exp_1)
Full_Work$Curr_Exp_2 <-dplyr::recode(Full_Work$Curr_Exp_2, "No stress at all\n\n1" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Curr_Exp_2 <- as.integer(Full_Work$Curr_Exp_2)
Full_Work$Curr_Exp_3 <-dplyr::recode(Full_Work$Curr_Exp_3, "No stress at all\n\n1" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Curr_Exp_3 <- as.integer(Full_Work$Curr_Exp_3)
Full_Work$Curr_Exp_4 <-dplyr::recode(Full_Work$Curr_Exp_4, "No stress at all\n\n1" = '1', "Some stress\n\n3\n" ='3', "A great deal of stress\n\n5\n" = '5' )
Full_Work$Curr_Exp_4 <- as.integer(Full_Work$Curr_Exp_4)
Full_Work$Future <- factor(Full_Work$Future, levels = c("Never", "A few times/year", "A few times/month", "A few times/week", "Once/week","Every day", "A few times/day"), ordered = TRUE )
Full_Work <- Full_Work %>% mutate(Mem_Exp = (Mem_Exp_1 + Mem_Exp_2 + Mem_Exp_3 + Mem_Exp_4)/4) %>%
  mutate(Curr_Exp = (Curr_Exp_1 + Curr_Exp_2 + Curr_Exp_3 + Curr_Exp_4)/4) %>%
  mutate(Past_Exp = (Past_Exp_1 + Past_Exp_2 + Past_Exp_3 + Past_Exp_4)/4)
Full_Work <- Full_Work %>% mutate(Dev_Mem_PastExp = Mem_Exp - Past_Exp)
Q19<- Prolific_Data %>% select(Q2, Q19)
names(Q19)[names(Q19) == "Q2"] <- "Subject"  
FUll19Work <- Full_Work %>% full_join(Q19)
FUll19Work$Q19 <- dplyr::recode(FUll19Work$Q19, "Never" = '0', "A few times/year" ="1", "A few times/month" ="2", "A few times/week" ="3", "Once/week" ="4","Every day" ="5")
FUll19Work[FUll19Work == ""] <- NA
FUll19Work$Q19  <- as.integer(FUll19Work$Q19)

#### Data Analysis ####
#### Most diff Month ####
### Q9: Which month during the COVID-19 pandemic do you find most difficult? ###
#2020
pie(table(Q10_2020$All_Data_Ans.Q9.1_1), lables = paste(round(prop.table(table(Q10_2020$All_Data_Ans.Q9.1_1)))*100))
prop.table(table(as.factor(Q10_2020$All_Data_Ans.Q9.1_1)))*100
# April (28.71%) by far the most difficult month

#2021
pie(table(Q10_2021$All_Data_Ans.Q9.1_1), lables = paste(round(prop.table(table(Q10_2021$All_Data_Ans.Q9.1_1)))*100))
prop.table(table(as.factor(Q10_2021$All_Data_Ans.Q9.1_1)))*100
# January (18.85%) & August (17.2%)  Most difficult months; interesting :o


### Self/Community Diff Covid Stress over time ###
Full_CovidExp <- ComMem_CovidExp %>% full_join(Self_CovidExp)
brm_Full_Covid <- brm(Rating ~ Timepoint + Type + (1 | Subject), family = cumulative, data = Full_CovidExp) 
summary(brm_Full_Covid)
plot_model(brm_Full_Covid)
marginal_effects(brm_Full_Covid)


#### Social-Proximity threat effect ####
### Q10: Right now, do you think that COVID-19 is a threat...(to others-social proximty effect)  ###
barplot(table(All_Data_Ans$Q10_1))  + title(main = "Threat to Self")
mean(na.omit(as.numeric(All_Data_Ans$Q10_1))) #2.72; to yourself - somewhat of a threat

barplot(table(All_Data_Ans$Q10_2)) + title(main = "Threat to Close Friends/Family")
mean(na.omit(as.numeric(All_Data_Ans$Q10_2))) #3.25; to your close friends or family members - somewhat of a threat

barplot(table(All_Data_Ans$Q10_3)) + title(main = "Threat to People in US")
mean(na.omit(as.numeric(All_Data_Ans$Q10_3))) #3.75; to people in US - a threat

barplot(table(All_Data_Ans$Q10_4)) + title(main = "Threat to People Across the World")
mean(na.omit(as.numeric(All_Data_Ans$Q10_4))) #4.04; to people across the world - somewhat of a threat

brm_threat_covid <- brm(Threat ~ Scope + (1 | Subject), family = cumulative, data = Current_Threat) 
summary(brm_threat_covid)
plot_model(brm_threat_covid)
marginal_effects(brm_threat_covid)

#### Current Exp. stress ####
### "Experience-related" stress ###
#Q12:How much stress are you experiencing right now as a result of... 
barplot(table(All_Data_Ans$Q12_1)) #Q12 - 1 decreased in person visits to close friends or family;
mean(na.omit(as.numeric(All_Data_Ans$Q12_1))) #2.16; little stress 

barplot(table(All_Data_Ans$Q12_2)) #Q12 - 2 decreased virtual contact with close friends or family
mean(na.omit(as.numeric(All_Data_Ans$Q12_2))) #2.16; little to no stress 
#bimodal distribution though; interesting

barplot(table(All_Data_Ans$Q12_3)) #Q12 - 3 increased tension with people in your household
mean(na.omit(as.numeric(All_Data_Ans$Q12_3))) #2.01; little stress 

barplot(table(All_Data_Ans$Q12_4)) #Q12 - 4 increased tension with others outside of your household
mean(na.omit(as.numeric(All_Data_Ans$Q12_4))) #1.98; little stress 

barplot(table(All_Data_Ans$Q12_5)) #Q12 - 5 loss of employment
mean(na.omit(as.numeric(All_Data_Ans$Q12_5))) #1.83; little stress, also N/A

barplot(table(All_Data_Ans$Q12_6)) #Q12 - 6 problems obtaining grocery items
mean(na.omit(as.numeric(All_Data_Ans$Q12_6))) #1.71; little stress, also N/A

barplot(table(All_Data_Ans$Q12_7)) #Q12 - 7 problems paying for bills
mean(na.omit(as.numeric(All_Data_Ans$Q12_7))) #1.96; little stress, also N/A

barplot(table(All_Data_Ans$Q12_8)) #Q12 - 8 problems accessing healthcare
mean(na.omit(as.numeric(All_Data_Ans$Q12_8))) #1.91; little stress, also N/A

barplot(table(All_Data_Ans$Q12_9)) #Q12 - 9 problems receiving your usual payment
mean(na.omit(as.numeric(All_Data_Ans$Q12_9))) #1.58; little to no stress, also N/A

barplot(table(All_Data_Ans$Q12_10)) #Q12 - 10 difficulties combining childcare with work
mean(na.omit(as.numeric(All_Data_Ans$Q12_10))) #1.64; little to no stress, also N/A

barplot(table(All_Data_Ans$Q12_11)) #Q12 - 11 difficulties that make work more difficult
mean(na.omit(as.numeric(All_Data_Ans$Q12_11))) #2.20; little stress

barplot(table(All_Data_Ans$Q12_12)) #Q12 - 12 an increased work load
mean(na.omit(as.numeric(All_Data_Ans$Q12_12))) #2.17; little stress

barplot(table(All_Data_Ans$Q12_13)) #Q12 - 13 working from home
mean(na.omit(as.numeric(All_Data_Ans$Q12_13))) #1.77; little stress, also N/A

barplot(table(All_Data_Ans$Q12_14)) #Q12 - 14 decreased physical exercise
mean(na.omit(as.numeric(All_Data_Ans$Q12_14))) #2.31; little to some stress

barplot(table(All_Data_Ans$Q12_15)) #Q12 - 15 decreased participation in your usual leisure activities
mean(na.omit(as.numeric(All_Data_Ans$Q12_15))) #2.47; little to some stress

barplot(table(All_Data_Ans$Q12_16)) #Q12 - 16 a decrease in new fun activities
mean(na.omit(as.numeric(All_Data_Ans$Q12_16))) #2.49; little to some stress

barplot(table(All_Data_Ans$Q12_17)) #Q12 - 17 boredom
mean(na.omit(as.numeric(All_Data_Ans$Q12_17))) #2.63; little to some stress


#### Current Bel. stress ####
### "Belief-related stress" ###
barplot(table(All_Data_Ans$Q13_1)) #Q13 - 1 the gov is not currently taking appropriate action to combat issues surrounding Covid-19
mean(na.omit(as.numeric(All_Data_Ans$Q12_17))) #2.63; little to some stress

barplot(table(All_Data_Ans$Q13_2)) #Q13 - 2 criminal rates have increased
mean(na.omit(as.numeric(All_Data_Ans$Q13_2))) #2.17; little stress

barplot(table(All_Data_Ans$Q13_3)) #Q13 - 3 there is currently an economic crisis
mean(na.omit(as.numeric(All_Data_Ans$Q13_3))) #2.92; some stress

barplot(table(All_Data_Ans$Q13_4)) #Q13 - 4 other people are not following public health guidelines to prevent the spread of Covid-19
mean(na.omit(as.numeric(All_Data_Ans$Q13_4))) #3.37; some stress to quite stressed

barplot(table(All_Data_Ans$Q13_5)) #Q13 - 5 life as we know it is not the same
mean(na.omit(as.numeric(All_Data_Ans$Q13_5))) #3.01; some stress


#### TP1 Exp. stress ####
barplot(table(All_Data_Ans$Q15_1)) #Q15 - 1 decreased in-person visits to close friends or family
mean(na.omit(as.numeric(All_Data_Ans$Q15_1))) #2.87; some stress

barplot(table(All_Data_Ans$Q15_2)) #Q15 - 2 decreased virtual contact with close friends or family
mean(na.omit(as.numeric(All_Data_Ans$Q15_2))) #2.14; little stress, and NAs

barplot(table(All_Data_Ans$Q15_3)) #Q15 - 3 increased tension with people in your household
mean(na.omit(as.numeric(All_Data_Ans$Q15_3))) #2.44; little to some stress

barplot(table(All_Data_Ans$Q15_4)) #Q15 - 4 increased tension with others outside your household
mean(na.omit(as.numeric(All_Data_Ans$Q15_4))) #2.24; little to some stress

barplot(table(All_Data_Ans$Q15_5)) #Q15 - 5 a loss of employment
mean(na.omit(as.numeric(All_Data_Ans$Q15_5))) #2.14; little to some stress, and NAs

barplot(table(All_Data_Ans$Q15_6)) #Q15 - 6 problems obtaining grocery items
mean(na.omit(as.numeric(All_Data_Ans$Q15_6))) #2.47; little to some stress

barplot(table(All_Data_Ans$Q15_7)) #Q15 - 7 problems paying for bills
mean(na.omit(as.numeric(All_Data_Ans$Q15_7))) #2.16; little stress

barplot(table(All_Data_Ans$Q15_8)) #Q15 - 8 problems accessing healthcare
mean(na.omit(as.numeric(All_Data_Ans$Q15_8))) #2.13; little stress

barplot(table(All_Data_Ans$Q15_9)) #Q15 - 9 problems receiving your usual paycheck
mean(na.omit(as.numeric(All_Data_Ans$Q15_9))) #1.82; little stress, and NAs

barplot(table(All_Data_Ans$Q15_10)) #Q15 - 10 difficulties combining childcare with work
mean(na.omit(as.numeric(All_Data_Ans$Q15_10))) #1.89; little stress, and NAs

barplot(table(All_Data_Ans$Q15_11)) #Q15 - 11 difficulties that make work more difficult
mean(na.omit(as.numeric(All_Data_Ans$Q15_11))) #2.55; little to some stress, and NAs

barplot(table(All_Data_Ans$Q15_12)) #Q15 - 12 an increased work load
mean(na.omit(as.numeric(All_Data_Ans$Q15_12))) #2.36; little to some stress, and NAs

barplot(table(All_Data_Ans$Q15_13)) #Q15 - 13 working from home
mean(na.omit(as.numeric(All_Data_Ans$Q15_13))) #2.06; little stress, and NAs

barplot(table(All_Data_Ans$Q15_14)) #Q15 - 14 decreased physical exercise
mean(na.omit(as.numeric(All_Data_Ans$Q15_14))) #2.44; little to some stress

barplot(table(All_Data_Ans$Q15_15)) #Q15 - 15 decreased participation in your usual leisure activities
mean(na.omit(as.numeric(All_Data_Ans$Q15_15))) #2.85; some stress

barplot(table(All_Data_Ans$Q15_16)) #Q15 - 16 a decrease in new fun activities
mean(na.omit(as.numeric(All_Data_Ans$Q15_16))) #2.83; some stress

barplot(table(All_Data_Ans$Q15_17)) #Q15 - 17 boredom
mean(na.omit(as.numeric(All_Data_Ans$Q15_17))) #2.71; some stress

barplot(table(All_Data_Ans$Q15_Conf)) #Q15 - Confidence in the accuracy of memory for stress about PERSONAL CIRCUMSTANCES
mean(na.omit(as.numeric(All_Data_Ans$Q15_Conf))) #3.11; some stress


#### TP1 Bel. stress ####
barplot(table(All_Data_Ans$Q16_1)) #Q16 - 1 government was not taking appropriate action to combat issues surrounding Covid-19
mean(na.omit(as.numeric(All_Data_Ans$Q16_1))) #3.60; some to quite a bit of stress

barplot(table(All_Data_Ans$Q16_2)) #Q16 - 2 criminality rates were increased
mean(na.omit(as.numeric(All_Data_Ans$Q16_2))) #2.39; little stress

barplot(table(All_Data_Ans$Q16_3)) #Q16 - 3 there was an economic crisis
mean(na.omit(as.numeric(All_Data_Ans$Q16_3))) #3.44; some to quite a bit of stress

barplot(table(All_Data_Ans$Q16_4)) #Q16 - 4 other people were not following public health guidelines to prevent the spread of Covid-19
mean(na.omit(as.numeric(All_Data_Ans$Q16_4))) #3.80; quite a bit of stress

barplot(table(All_Data_Ans$Q16_5)) #Q16 - 5 life as we know it is not the same
mean(na.omit(as.numeric(All_Data_Ans$Q16_5))) #3.55; some to quite a bit of stress

barplot(table(All_Data_Ans$Q16_Conf)) #Q16 - Confidence in the accuracy of memory for stress about SOCIAL CIRCUMSTANCES
mean(na.omit(as.numeric(All_Data_Ans$Q16_Conf))) #3.33 some to quite a bit of stress


#### Delta Exp. stress ####
#Q12_1***
Complete_test %>% ggplot(aes(x=TP, y= Q12_1, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("decreased in-person visits to close friends or family") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_1, na.rm=T))
#More stressed before due to decreased in-person visits (+.31)

Q12_1_OrdBay <- brm(DecreasedInPersonVisits ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_1_OrdBay)
plot_model(Q12_1_OrdBay)
marginal_effects(Q12_1_OrdBay)

#Q12_2***
Complete_test %>% ggplot(aes(x=TP, y= Q12_2, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("decreased virtual contact with close friends or family") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_2, na.rm=T))
#More stressed before due to decreased virtual contact (+.12)

Q12_2_OrdBay <- brm(DcrsdVrtulCntct ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_2_OrdBay)
plot_model(Q12_2_OrdBay)
marginal_effects(Q12_2_OrdBay)

#Q12_3***
Complete_test %>% ggplot(aes(x=TP, y= Q12_3, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("increased tension with people in your household") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_3, na.rm=T))
#More stressed before due to increased tension with people in household (+.20)

Q12_3_OrdBay <- brm(TnsnPplHshld ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_3_OrdBay)
plot_model(Q12_3_OrdBay)
marginal_effects(Q12_3_OrdBay)

#Q12_4***
Complete_test %>% ggplot(aes(x=TP, y= Q12_4, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("increased tension with others outside of your household") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_4, na.rm=T))
#More stressed now due to increased tension with others outside of your household (+.17)

Q12_4_OrdBay <- brm(TnsnOtsdHshld ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_4_OrdBay)
plot_model(Q12_4_OrdBay)
marginal_effects(Q12_4_OrdBay)

#Q12_5***
Complete_test %>% ggplot(aes(x=TP, y= Q12_5, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("a loss of employment") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_5, na.rm=T))
#More stressed before due to loss of employment(+.39)

Q12_5_OrdBay <- brm(LossofEmploy ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_5_OrdBay)
plot_model(Q12_5_OrdBay)
marginal_effects(Q12_5_OrdBay)

#Q12_6***
Complete_test %>% ggplot(aes(x=TP, y= Q12_6, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("problems paying for groceries") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_6, na.rm=T))
#More stressed before due to problems paying for groceries(+.40)

Q12_6_OrdBay <- brm(PrblmPay4Grcrs ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_6_OrdBay)
plot_model(Q12_6_OrdBay)
marginal_effects(Q12_6_OrdBay)

#Q12_7***
Complete_test %>% ggplot(aes(x=TP, y= Q12_7, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("problems paying for bills") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_7, na.rm=T))
#More stressed before due to problems paying for bills (+.2)

Q12_7_OrdBay <- brm(Pay4Bills ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_7_OrdBay)
plot_model(Q12_7_OrdBay)
marginal_effects(Q12_7_OrdBay)

#Q12_8***
Complete_test %>% ggplot(aes(x=TP, y= Q12_8, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("problems accessing healthcare") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_8, na.rm=T))
#More stressed before due to problems accessing healthcare (+.13)

Q12_8_OrdBay <- brm(PrblmHlthcr ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_8_OrdBay)
plot_model(Q12_8_OrdBay)
marginal_effects(Q12_8_OrdBay)

#Q12_9***
Complete_test %>% ggplot(aes(x=TP, y= Q12_9, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("problems receiving your usual paycheck") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_9, na.rm=T))
#More stressed before due to problems receiving usual paycheck (+.39)

Q12_9_OrdBay <- brm(PrblmPychk ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_9_OrdBay)
plot_model(Q12_9_OrdBay)
marginal_effects(Q12_9_OrdBay)

#Q12_10***
Complete_test %>% ggplot(aes(x=TP, y= Q12_10, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("difficulties combining childcare with work") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_10, na.rm=T))
#More stressed before due to difficulties combining childcare and work (+.23)

Q12_10_OrdBay <- brm(ChldcrWrk ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_10_OrdBay)
plot_model(Q12_10_OrdBay)
marginal_effects(Q12_10_OrdBay)

#Q12_11***
Complete_test %>% ggplot(aes(x=TP, y= Q12_11, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("obstacles that make work more difficult") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_11, na.rm=T))
#More stressed before due to obstacles that make work more difficult (+.16)

Q12_11_OrdBay <- brm(ObstclsWrkDiff ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_11_OrdBay)
plot_model(Q12_11_OrdBay)
marginal_effects(Q12_11_OrdBay)

#Q12_12 %XXXX%
Complete_test %>% ggplot(aes(x=TP, y= Q12_12, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("an increased work load") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_12, na.rm=T))
#Subjects were equally stressed due to increased work loads (+.06)

Q12_12_OrdBay <- brm(IncrsdWrkLd ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_12_OrdBay)
plot_model(Q12_12_OrdBay)
marginal_effects(Q12_12_OrdBay)

#Q12_13***
Complete_test %>% ggplot(aes(x=TP, y= Q12_13, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("working from home") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_13, na.rm=T))
#More stressed before due to working from home (+.17)

Q12_13_OrdBay <- brm(WrkFrmHm ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_13_OrdBay)
plot_model(Q12_13_OrdBay)
marginal_effects(Q12_13_OrdBay)

#Q12_14***
Complete_test %>% ggplot(aes(x=TP, y= Q12_14, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("decreased physical exercise") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_14, na.rm=T))
#More stressed before due to decreased physical exercise (+.12)

Q12_14_OrdBay <- brm(PhysclXrcz ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_14_OrdBay)
plot_model(Q12_14_OrdBay)
marginal_effects(Q12_14_OrdBay)

#Q12_15***
Complete_test %>% ggplot(aes(x=TP, y= Q12_15, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("decreased participation in your usual leisure activities") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_15, na.rm=T))
#More stressed before due to decreased participation in usual leisure activities (+.23)

Q12_15_OrdBay <- brm(DcrsdPtcptnLesre ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_15_OrdBay)
plot_model(Q12_15_OrdBay)
marginal_effects(Q12_15_OrdBay)

#Q12_16***
Complete_test %>% ggplot(aes(x=TP, y= Q12_16, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("a decrease in new fun activities") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_16, na.rm=T))
#More stressed before due to decreases in new fun activities

Q12_16_OrdBay <- brm(LessNewActvts ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_16_OrdBay)
plot_model(Q12_16_OrdBay)
marginal_effects(Q12_16_OrdBay)

#Q12_17 %XXXX%
Complete_test %>% ggplot(aes(x=TP, y= Q12_17, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("boredom") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q12_17, na.rm=T))
#More stressed before from boredom (+.12)

Q12_17_OrdBay <- brm(Boredom ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q12_17_OrdBay)
plot_model(Q12_17_OrdBay)
marginal_effects(Q12_17_OrdBay)


#### Delta Bel. stress ####
#Q13_1***
Complete_test %>% ggplot(aes(x=TP, y= Q13_1, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("the government is not currently taking appropriate action to combat issues surrounding COVID-19") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q13_1, na.rm=T))
#More stressed before from believing government not taking appropriate action to combat COVID (+.77)

Q13_1_OrdBay <- brm(BlvngGvmntNotCmbtCOVID ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q13_1_OrdBay)
plot_model(Q13_1_OrdBay)
marginal_effects(Q13_1_OrdBay)

#Q13_2 %XXXX%
Complete_test %>% ggplot(aes(x=TP, y= Q13_2, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("criminality rates have increased") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q13_2, na.rm=T))
#More stressed before from believing criminality rates have increased (+.06)

Q13_2_OrdBay <- brm(CrmnltyIncrsd ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q13_2_OrdBay)
plot_model(Q13_2_OrdBay)
marginal_effects(Q13_2_OrdBay)

#Q13_3***
Complete_test %>% ggplot(aes(x=TP, y= Q13_3, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("there is currently an economic crisis") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q13_3, na.rm=T))
#More stressed before from believing there is currently an economic crisis (+.4)

Q13_3_OrdBay <- brm(Blvng_EcnmcCrsis ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q13_3_OrdBay)
plot_model(Q13_3_OrdBay)
marginal_effects(Q13_3_OrdBay)

#Q13_4***
Complete_test %>% ggplot(aes(x=TP, y= Q13_4, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("other people are not following public health guidelines to prevent the spread of COVID-19") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q13_4, na.rm=T))
#More stressed before from believing other people are not following public health guidelines (+.2)

Q13_4_OrdBay <- brm(OthrNotFollowingPblcHlthGuidlns ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q13_4_OrdBay)
plot_model(Q13_4_OrdBay)
marginal_effects(Q13_4_OrdBay)

#Q13_5***
Complete_test %>% ggplot(aes(x=TP, y= Q13_5, fill=TP))+scale_fill_manual(values=c("#1B7B2A","#FF0000")) +geom_violin()+theme_bw()+geom_jitter(position = position_dodge(w=0.2),aes(group=Subject))+scale_y_continuous(n.breaks = 8)+geom_line(position=position_dodge(w=0.2),aes(group=Subject),alpha=.3)+ ggtitle("life as we know it is not the same") 
Complete_test %>% group_by(TP) %>% summarise(mean=mean(Q13_5, na.rm=T))
#More stressed before  from believing life as we know it is not the same(+.20)

Q13_5_OrdBay <- brm(BelLifeIsNotSame ~ TP + (1 | Subject), family = cumulative, data = Complete_test_Anal) 
summary(Q13_5_OrdBay)
plot_model(Q13_5_OrdBay)
marginal_effects(Q13_5_OrdBay)

#### Deviation Memory Exp. ####
#Individual Questions
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience1)) #BF= 2.29e47

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience2)) #BF= 1.48e26

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience3)) #BF= 2.11e22

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience4)) #BF= 2.58e8

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience5)) #BF= 1.44e6

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience6)) #BF= 3.58e60

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience7)) #BF= 1.92e4

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience8)) #BF= 2.43e6

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience9)) #BF= 2.14e4

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience10)) #BF= 8.87e5

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience11)) #BF= 2.17e14

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience12)) #BF= 4.52e3

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience13)) #BF= 1.24e8

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience14)) #BF= 12.46

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience15)) #BF= 3.93e17

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience16)) #BF= 3.35e14

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_experience17)) #BF= 2.14e5
#In all but one 'strong evidence' case there were significant extreme evidence for those differences

#Overall
Complete_test_Dev_Grph %>% ggplot(aes(x = Deviation1, y = QExp, group=QExp)) + geom_violin() +scale_x_continuous(n.breaks = 9)+ ggtitle("Deviation of past stress from remembered covid-related stress") +
  stat_summary(fun.y="mean", color="red", shape=15)
mean(Complete_test_stuff$Deviation) #.35
ttestBF(x = na.omit(Complete_test_stuff$Deviation)) #BF= 
#Generally, people tended to overestimate their covid-related stress levels during the pandemic (+.35)


#### Deviation Memory Bel. ####
#Individual Questions
#Examine whether differences are large enough to be significant
ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_belief1)) #BF= 6.74e73

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_belief2)) #BF= 2.15e6

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_belief3)) #BF= 3.4e32

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_belief4)) #BF= 5.71e27

ttestBF(x = na.omit(Complete_test_Analy$expectation_deviation_belief5)) #BF= 6.22e38
#In all cases there were extreme evidence for those difference

#Overall 
Complete_test_Dev_Grph %>% ggplot(aes(x = Deviation2, y = QBelief, group=QBelief)) + geom_violin() +scale_x_continuous(n.breaks = 9)+ ggtitle("Deviation of past stress from remembered belief related stress") +
  stat_summary(fun.y="mean", color="red", shape=15)
mean(Complete_test_things$Deviation) #.54
#Generally, people tended to overestimate their belief-related stress levels during the pandemic (+.54)


#### Dev. Mem. ExpBel ####
#People also overestimated their stress from beliefs more than their covid-related stress (+.19)
#Examine whether differences are large enough to be significant
barplot(table(Complete_test_Dev$Deviation1)) + title(main = "Distributions of Deviations")
Dev_Diff <- brm(Deviation1 ~ Type + (1 | Subject), family = cumulative, data = Dev3) 

summary(Dev_Diff)
plot_model(Dev_Diff)
marginal_effects(Dev_Diff)
#There were significant differences between the deviations arising from covid- and belief-related stress with 
# extreme evidence for those differences indicating that while people generally overestimated their covid related stress
# this was greater for their stress induced by beliefs than covid.

### Deviations x Confidence ###
#Deviations from experience stress
Complete_test_Dev %>% group_by(Q15_Conf_1) %>%summarise(mean=mean(na.omit(Deviation1)))
#People significantly less accurate for intermediate confidence values (2,3,4)
Dev_Conf<- brm(Deviation1 ~ Q15_Conf_1 + (1 | Subject), family = cumulative, data = Complete_test_Dev) 
summary(Dev_Conf)
plot_model(Dev_Conf)
marginal_effects(Dev_Conf)

#Deviations from belief stress
Complete_test_Dev2 %>% group_by(Q16_Conf_1) %>%summarise(mean=mean(na.omit(Deviation2))) 
#People less accurate for increasing confidence, general trend, significantly most accurate for lowest confidence (1)
Dev_Conf2<- brm(Deviation2 ~ Q16_Conf_1 + (1 | Subject), family = cumulative, data = Complete_test_Dev2) 
summary(Dev_Conf2)
plot_model(Dev_Conf2)
marginal_effects(Dev_Conf2)

#### Construction of Remembered stress ####
#Experience-related stress
TP2_RemBCovid_Exp12 <- stan_glmer(BCovid_Rem ~ Covid_Exp1 + Covid_Exp2  + (1|Subject), data = Stress_Out_Beta, family = mgcv::betar, iter= 10000)
describe_posterior(TP2_RemBCovid_Exp12)
plot(TP2_RemBCovid_Exp12, plotfun = "mcmc_dens", pars = c("Covid_Exp1", "Covid_Exp2"))  
plot_model(TP2_RemBCovid_Exp12, terms = c("Covid_Exp1", "Covid_Exp2"))
#Both experienced covid stress at TP1 and TP2 predict remembered covid stress

#Belief-related stress
TP2_RemBBelief_Exp12 <- stan_glmer(BBelief_Rem ~ Belief_Exp1 + Belief_Exp2  + (1|Subject), data = Stress_Out_Beta, family = mgcv::betar, iter= 10000)
describe_posterior(TP2_RemBBelief_Exp12)
plot(TP2_RemBBelief_Exp12, plotfun = "mcmc_dens", pars = c("Belief_Exp1", "Belief_Exp2"))  
plot_model(TP2_RemBBelief_Exp12, terms = c("Belief_Exp1", "Belief_Exp2"))
#Both experienced belief stress at TP1 and TP2 predict remembered belief

#### Future Behavior ####
#Visualizing the future
#Q49: Do you expect to be able to work from home, at least some of the time? ; 
barplot(table(na.omit(Future_Q$Q49)))
mean(na.omit(as.numeric(Future_Q_res$Q49))) #60% do

#Q50: How often do you expect to work from home?; Q50
barplot(table(Future_Q$Q50))
mean(na.omit(as.numeric(Future_Q_res$Q50))) #On average people expect to go in once a week but most expect few times/week every day

#Q51: How often do you expect to shop for groceries?
barplot(table(Future_Q$Q51))
mean(na.omit(as.numeric(Future_Q_res$Q51))) #average/most expect few times/week or /month

#Q52: After COVID-19 is no longer a threat:\n\n\n\nAbout how many days each week do you expect to spend time on leisure ... - 
#_1: At home without socializing with others?
barplot(table(Future_Q$Q52_1))
mean(na.omit(as.numeric(Future_Q_res$Q52_1))) #on average expect 2-3 or 4-5, most every day or nearly every day

#_2: Outside your home without socializing with others?
barplot(table(Future_Q$Q52_2))
mean(na.omit(as.numeric(Future_Q_res$Q52_2))) #on average/most expect at least 1 day or 2-3

#_3: Socializing with others digitally?
barplot(table(Future_Q$Q52_3))
mean(na.omit(as.numeric(Future_Q_res$Q52_3))) #on average/most expect 2-3

#_4: Socializing with others in person in your home or in other people’s home?
barplot(table(Future_Q$Q52_4))
mean(na.omit(as.numeric(Future_Q_res$Q52_4))) #on average/most expect at least 1 day to 2-3 days

#_5: Socializing with others in person outside your home?
barplot(table(Future_Q$Q52_5))
mean(na.omit(as.numeric(Future_Q_res$Q52_5))) #on average/most expect at least 1 day to 2-3 days

#Q53: On the days you expect to engage in these activities for leisure, about how much time each day do you expect to spend on them?
#_1: At home without socializing with others
barplot(table(Future_Q$Q53_1))
mean(na.omit(as.numeric(Future_Q_res$Q53_1))) #on average 1-2 or 2-4

#_2: Outside your home without socializing with other
barplot(table(Future_Q$Q53_2))
mean(na.omit(as.numeric(Future_Q_res$Q53_2))) #on average/most expect 1-2 or 2-4,  most .5-1 and 1-2

#_3: Socializing with others digitally
barplot(table(Future_Q$Q53_3))
mean(na.omit(as.numeric(Future_Q_res$Q53_3))) #on average expect 1-2 or 2-4,  most .5-1 and 1-2

#_4: Socializing with others in person in your home or in other people’s home
barplot(table(Future_Q$Q53_4))
mean(na.omit(as.numeric(Future_Q_res$Q53_4))) #on average/most expect 1-2

#_5: Socializing with others in person outside your home
barplot(table(Future_Q$Q53_5))
mean(na.omit(as.numeric(Future_Q_res$Q53_5))) #on average/most expect 1-2


#### Past and Future ####
#By Question
Q18_MemFut <- stan_glmer(Q18 ~ TP + (1|Subject), family = binomial, data = MemFut)
describe_posterior(Q18_MemFut)
plot(Q18_MemFut, plotfun = "mcmc_dens", pars = c("TPPast"))  
plot_model(Q18_MemFut)

Q19_MemFut <- brm(Freq_StayingHome ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q19_MemFut)
plot_model(Q19_MemFut)
marginal_effects(Q19_MemFut)

Q20_MemFut <- brm(Freq_GrocStoreVisits ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q20_MemFut)
plot_model(Q20_MemFut)
marginal_effects(Q20_MemFut)

Q211_MemFut <- brm(CovidNoThreat ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q211_MemFut)
plot_model(Q211_MemFut)
marginal_effects(Q211_MemFut)

Q212_MemFut <- brm(OutsideHomeSocializingOthers ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q212_MemFut)
plot_model(Q212_MemFut)
marginal_effects(Q212_MemFut)

Q213_MemFut <- brm(SocializingOthersDigitally ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q213_MemFut)
plot_model(Q213_MemFut)
marginal_effects(Q213_MemFut)

Q214_MemFut <- brm(SocializingOthersInPerson ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q214_MemFut)
plot_model(Q214_MemFut)
marginal_effects(Q214_MemFut)

Q215_MemFut <- brm(SocializingOthersOutsideHome ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q215_MemFut)
plot_model(Q215_MemFut)
marginal_effects(Q215_MemFut)

Q221_MemFut <- brm(AtHomeNotSicializingwOthers ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q221_MemFut)
plot_model(Q221_MemFut)
marginal_effects(Q221_MemFut)

Q222_MemFut <- brm(OutsideHome_NotSicializingwOthers ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q222_MemFut)
plot_model(Q222_MemFut)
marginal_effects(Q222_MemFut)

Q223_MemFut <- brm(SocializingwOthers_Digitally ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q223_MemFut)
plot_model(Q223_MemFut)
marginal_effects(Q223_MemFut)

Q224_MemFut <- brm(SocializingwOthers_YourorOtherHome ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q224_MemFut)
plot_model(Q224_MemFut)
marginal_effects(Q224_MemFut)

Q225_MemFut <- brm(SocializingwOthers_InPerson_OutsideHome ~ TP + (1|Subject), family = "Cumulative", data = MemFut)
summary(Q225_MemFut) 
plot_model(Q225_MemFut)
marginal_effects(Q225_MemFut)

#### Rem. Stress, Future Beh ####
###Groceries
Groc_ExpRem<- brm(Future ~ Curr_Exp + Mem_Exp + Past_Exp + Q20 + (1|Subject), family = "Cumulative", data = Full_Groc, iter = 6000)
summary(Groc_ExpRem)
plot_model(Groc_ExpRem)
marginal_effects(Groc_ExpRem)
###Work
#EXTRA HAOXUE RECOMMENDED ANALYSIS:Current + Rem + Past
Work_CurMemFut_RemExp <- brm(Future ~ Curr_Exp + Mem_Exp + Past_Exp + Q19 + (1|Subject), family = "Cumulative", data = FUll19Work, iter = 6000)
summary(Work_CurMemFut_RemExp)
plot_model(Work_CurMemFut_RemExp)
aaaa3<-posterior_samples(Work_CurMemFut_RemExp)
welp3<-as.matrix(aaaa3$b_Curr_Exp)
hdi(welp3, ci = .89)
