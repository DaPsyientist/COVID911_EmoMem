#### Readme file for the COVID911_EmoMem Github Repository ####

This file is intended to pair with the publication entitled, “Overestimating the intensity of negative emotion in autobiographical memory: evidence from the 9/11 attack and Covid-19 pandemic”

If for any reason there arise issues with downloading the data, or if you have any outstanding questions please feel free to contact the co-lead authors:

Haoxue Fan (haoxue_fan@g.harvard.edu)
Or
Juan Castillo (jcastillo@g.harvard.edu)

Files included in repository:

- Cleaned_911_Only_Data_Manipulation.R (R Script to clean 9/11 related data)

- Cleaned_Covid_Only_Data_Manipulation.R (R Script to clean Covid-19 related data)

- MCMC_analyses.R (Script for analyses included in publication)

- PlotScript.R (Script for all visualizations included in publication)

- PriorSense.R (Script for sensitivity analysis to examine robustness of findings)

- Utils.R (Script for utility functions used in analysis)

- MCMC_analyses_notebook.Rmd / MCMC_analyses_notebook.nb.html / MCMC_analyses_notebook.html (R Markdown of analysis with figures)

Since our analyses are Bayesian and use Markov-Chain Monte-Carlo Sampling in order to estimate the shape of the posterior distribution, many of the regression analyses take time to load. For those who simply want to quickly examine the results we include .Rda files with pre-sampled regressions included. These .Rda files can be located at: https://osf.io/kqe54/?view_only=beb4fcfe04b245969b03b761f721f44f

There are 4 .Rda files in total:
1) 911nCovid_Data.rda (Data used for analyses)
2) 911nCovid_StressMemory_PSBF.rda (Bayes factor simulation data)
3) 911nCovid_StressMemory_simple.rda (MCMC simulation data)
4) Covid_StressMemory.rda (Fit model objects)
