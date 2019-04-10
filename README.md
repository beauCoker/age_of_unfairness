This repository contains all code used in *The age of secrecy and unfairness in recidivism prediction* by Cynthia Rudin, Caroline Wang, and Beau Coker.

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

*	*app.r* is a Shiny app (see below).

## Groups of features

There are a few different groups of features used in predicting the COMPAS raw score remainders and two-year recidivism:
*	**Group 1:** Does not include age at screening date or race.
*	**Group 2:** Includes race but not age at screening date.
*	**Group 3:** Includes age at screening date but not race.
*	**Group 4:** Includes age at screening date and race.
*	**Group 5:** Includes age at screening date and race as well as total number of charges and total number of arrests. Used only for COMPAS raw score remainder prediction.

These groups are referenced in the code.

## Row filtering

Depending on the analysis, some observations are discarded. For example, some individuals were given a COMPAS score less than two years before the data was pulled, so we cannot asses whether or not they committed a new crime within two years. At most there are 12,381 unique people / COMPAS screening date combinations. The following filters are used at some point in the analysis, with the number of observations this filter removes from *total* number of observations (i.e., so some filters may overlap) for the general and violent scores given in paranthesis:

*	**Filter 1:** Remove observations with -1 decile scores (removes 15 general, 15 violent).
*	**Filter 2:** Remove all but African-Americans and Caucasians (removes 1929 general, 1929 violent).
*	**Filter 3:** Remove observations with no current offense (removes 3331 general, 3331 violent).
*	**Filter 4:** Remove observations without two years of data past screening date (removes 6615 general, 6615 violent).
*	**Filter 5:** Remove observations with current age <= 18 or >65 (removes 739 general, 739 violent).
*	**Filter 6:** Remove observations with nonzero subscale inputs, for the relavant subscales (removes 8239 general, 5658 violent) 
*	**Filter 7:** Remove all observations 0.05 below the age polynomial (removes 18 general, 11 violent).
*	**Filter 8:** Remove all observations 0.05 below the age spline (removes 22 general, 12 violent).

The following results in the paper use the listed filters:
#### General and Violent Recidivism Results

*	*f(age) and f(viol_age) fitting + plot:*
Filters 1, 5, 6, 7
*	*Logistic regression AND probability of reoffending plot:*
Filters 1, 3, 4
*	*Any COMPAS score prediction (whether or not reversed engineered components subtracted):*
Filters 1, 3
*	*Any recidivism prediction:*
Filters 1, 3, 4


#### General Recidivism Results

*	*raw_score - f(age) vs. number or priors AND vs. criminal involvement plot:*
Filters 1, 3, 8 (but those filtered by 8 added to plot in green)
*	*TPR/FPR plot:*
Filters 1, 2, 3


#### Violent Recidivism Results

*	*raw_score - f(age) vs. history of violence fitting + plot:*
Filters 1, 3, 8 (but those filtered by 8 added to plot in green)
*	*raw_score - f(age) - g(vio_hist) vs. history of noncompliance plot:*
Filters 1, 3, 8 (but those filtered by 8 added to plot in green)


#### Other Results

*	*Age histograms AND mean/median age statistics:*
Filters 1, 2


## Shiny application

We created a Shiny application to display criminal history data, COMPAS scores, and the features we constructed for each individual in the dataset. To choose an individual, type a person identification number and select a COMPAS screening date. 

The Charge, Arrest, Jail, Prison, and COMPAS tabs show data made available by ProPublica. Information is separated by when it occured in relation to the COMPAS screening date (before the date, on the date, and after the date). The Features tab shows our constructed features. There is also some useful information in the Profile tab. 













