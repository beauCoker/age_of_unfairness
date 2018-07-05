This repository contains all code used in *Age of Unfairness* by Cynthia Rudin, Caroline Wang, and Beau Coker.

## Data



## Files

*	``db2csv.r'' converts each table in compas.db to a csv file. Run this before running Table_construction.rmd.

*	Table_construction.rmd processes the raw data into the features used for our analysis. The output is stored in Table_construction.rdata.

*	predict_scores.rmd and predict_scores_violent.rmd predict general and violent COMPAS raw scores, respectively. Also included are the fitted age polynomial and the logistic regression that includes age, sex, and recidivism as features.  

*	predict_recidivism.rmd and predict_recidivism_violent.rmd predict general and violent two-year recidivism, respectively. 

*	age_only.rmd contains a few plots that investigate the impact of age at COMPAS screening date.

*	functions.r includes a number of custom helper functions.

## Groups

## Data

## Row Filtering