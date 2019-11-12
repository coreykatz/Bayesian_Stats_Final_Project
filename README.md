# Bayesian Stats Final Project
This is the final project for a Bayesian Statistics class at UCI. This repo only includes stan models and R functions that I wrote for the project, not other code written by my partner for the project. 

## Objective:

In this project, we investigated the electricity consumption in Ireland and how different covariates contribute to a household’s energy consumption.

### Part 1:

We look at how 6 covariates, described in the project file, contribute to energy consumption in Ireland for the first day of the study. We investigate different covariates' effects on electricity consumption trough Bayesian Inference. Stan was used to run the MCMC sampling to find the posterior distributions.

### Part 2: 

We investigate the model selection criteria: LPML and Bayes Factors. We also preform sensitivity analysis. 


### Part 3: 

In this part, we look at how energy consumption changes over time. We utilize a hierarchical model to find population and day level parameters to get the best possible estimates.


### Part 4:

In this part we investigate if there is a change in electricity consumption after a change in the tariff structure in 2010. WE now include a before/after level parameter to our hierarchical model.


### Data:

The data includes information from 151 households from Nov 2009 - Mar 2010. Information regarding the covariates can be found in the project information file. 

1. Age of head of household as an ordinal variab
## Note:

The report includes all the code for this project. Any code I wrote is in a the other files. Full samples for the models are not included in this repo. 



