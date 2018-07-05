This repository contains all code used in *Age of Unfairness* by Cynthia Rudin, Caroline Wang, and Beau Coker.

## Data

There are two data sources that are used in our analysis but not stored in this repository:
*	The database of [ProPublica data](https://github.com/propublica/compas-analysis), which should be stored at *Raw_data/compas-analysis/compas.db*.
*	The probation data that we purchased. It is available upon request and should be stored at *Raw_data/probation/*.

These data sources are needed to run *Table_construction.rdata*. Note that the features created by this script are stored in *Table_construction.rdata*.

## Files

*	*db2csv.r* converts each table in *compas.db* to a csv file. Run this before running *Table_construction.rmd*.

*	*Table_construction.rmd* processes the raw data into the features used for our analysis. The output is stored in *Table_construction.rdata*.

*	*predict_scores.rmd* and *predict_scores_violent.rmd* predict general and violent COMPAS raw score remainders, respectively. Also included are the fitted age polynomial and the logistic regression that includes age, sex, and recidivism as features.  

*	*predict_recidivism.rmd* and *predict_recidivism_violent.rmd* predict general and violent two-year recidivism, respectively. 

*	*age_only.rmd* contains a few plots that investigate the impact of age at COMPAS screening date.

*	*functions.r* includes a number of custom helper functions.

## Groups of features

There are a few different groups of features used in predicting the COMPAS raw score remainders and two-year recidivism:
*	**Group 1:** Does not include age at screening date or race.
*	**Group 2:** Includes race but not age at screening date.
*	**Group 3:** Includes age at screening date but not race.
*	**Group 4:** Includes age at screening date and race.
*	**Group 5:** Includes age at screening date and race as well as total number of charges and total number of arrests. Used only for COMPAS raw score remainder prediction.

These groups are referenced in the code.

## Row Filtering

Depending on the analysis, some observations are discarded. For example, some individuals were given a COMPAS score less than two years before the data was pulled, so we cannot asses whether or not they committed a new crime within two years. At most there are 12,381 unique people / COMPAS screening date combinations. The following filters are used at some point in the analysis: 
*	**Filter 1:** Remove observations with -1 decile scores (removes 15 observations).
*	**Filter 2:** Remove all but African-Americans and Caucasians (removes 1929 observations).
*	**Filter 3:** Remove observations with no current offense (removes 3331 observations).
*	**Filter 4:** Remove observations without two years of data past screening date (removes 4474 observations).
*	**Filter 5:** Remove observations with current age <= 18 or > 70 (removes 670 observations).
*	**Filter 6:** Remove all observations except those with lowest COMPAS raw score for each current age.

The following results in the paper use the listed filters:
*	Age histograms, TPR/FPR plot, and mean/median age statistics: Filters 1, 2.

*	Age polynomial fitting: Filters 1, 5, 6

*	COMPAS raw score remainder prediction and priors vs. raw score remainder plots: Filters 1, 3.

*	Recidivism prediction, logistic regression with age, sex, and recidivism, and probability of reoffending plot: Filters 1, 3, 4.
















