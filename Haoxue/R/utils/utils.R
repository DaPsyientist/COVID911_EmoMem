
if(!require(pacman)){install.packages('pacman')}

pacman::p_load(ggplot2, ggpubr, docstring,
               here, glmmLasso, lme4, lmerTest,
               caret, MASS, qdap, tidyverse, magrittr,
               broom.mixed,
               foreach, doParallel, imager,
               sjPlot,
               emmeans,
               permute,glmnet,caret,
               BayesFactor, brms, here, mclust, NbClust)

labels_experience <- c('decreased in person visit', 'decreased virtual contact', 'increased tension within household', 'increased tension outside household',
                       'loss of employment', 'problem obtaining grocery items', 'problem paying for bills', 'problem accessing healthcare', 'problem receiving paycheck',
                       'difficulty combining childcare with work', 'work more difficult', 'increase work load', 'work from home',
                       'decreased physical exercise', 'decreased usual leisure activities', 'decreased new fun activities',
                       'boredom')
levels_experience <- paste('NA_expectation_deviation_experience', c(1:17), sep='')
levels_T2_experience <- paste('Q12', c(1:17), sep='_')

labels_belief <- c('goverment not taking appropriate action', 'criminality rate increases', 'current economic crisis', 
                   'people not following public health guideline', 'life as we know it is not the same')
levels_belief <- paste('NA_expectation_deviation_belief', c(1:5), sep='')
