This repository contains all code used in *Age of Unfairness* by Cynthia Rudin, Caroline Wang, and Beau Coker.

## Data

There are two data sources that are used in our analysis but not stored in this repository:
*	The database of [ProPublica data](https://github.com/propublica/compas-analysis), which should be stored at *Raw_data/compas-analysis/compas.db*.
*	The probation data that we purchased. It is available upon request and should be stored at *Raw_data/probation/*.

These data sources are needed to run *Table_construction.rdata*.

## Files

*	*db2csv.r* converts each table in *compas.db* to a csv file. Run this before running Table_construction.rmd.

*	Table_construction.rmd processes the raw data into the features used for our analysis. The output is stored in Table_construction.rdata.

*	predict_scores.rmd and predict_scores_violent.rmd predict general and violent COMPAS raw scores, respectively. Also included are the fitted age polynomial and the logistic regression that includes age, sex, and recidivism as features.  

*	predict_recidivism.rmd and predict_recidivism_violent.rmd predict general and violent two-year recidivism, respectively. 

*	age_only.rmd contains a few plots that investigate the impact of age at COMPAS screening date.

*	functions.r includes a number of custom helper functions.

## Groups

## Data

## Row Filtering