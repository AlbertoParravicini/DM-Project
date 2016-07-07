# Data Mining Project: BIP

## Team members
* Alberto Parravicini
* Simone Ripamonti
* Luca Stornaiuolo

## Report
* [LINK](https://albertoparravicini.shinyapps.io/DM-Report/)

## Results
* Results/PREDIZIONE_FINALE_FUTURE.csv
* Results/PREDIZIONE_FINALE_SOFTMAX_FUTURE.csv (Better results than the previous one, with softmax weighting)

## Data preparation flow:
* load xgboost_temp.R, sarima_v2.R, forest.R

* Modify the original dataset and build an appropriate dataset for forecasting.

data preparation.R **->** data preparation pt1.5.R **->** data preparation pt2.R **->** cumulative sum.R 
**->** missing_values_and_outliers_predictions.R **->** adding_gps.R
**->** plotting maps and clustering.R **->** add_holiday.R **->** vendite_giornaliere_step1.R
**->** predizione_aziendale_sarima_step2.R **->** add_overall_prediction_to_dataset.R **->** predset_creation_step3.R 
**->** predset_preparation_step4.R **->** predset_cluster_merge_step5.R

***

## Script summary: 

#### add_holiday.R
* adds column "vacanza" in the dataset, based on a custom Italian holiday list

#### add_overall_prediction_to_dataset.R
* adds column "vendite_giorn_prod" in the dataset, containing the aggregate sales of products by date

#### adding_gps.R
* adds column "longitudine" and "latitudine" in the dataset, by joining gps.csv on "sottoarea"

#### aggregazione anno - key.R
* builds a csv file containing columns "Key", "Anno", "P1", "P2", "Total"
* "P1" is the aggregate sales of product 1 for a given year and a given subarea ("Key")
* same for "P2" and "Total"

#### aggregazione mese - key.R
* builds a csv file containing columns "Key", "Anno", "Mese", "P1", "P2", "Total"
* "P1" is the aggregate sales of product 1 for a given year, month and subarea ("Key")
* same for "P2" and "Total"

#### analysis_of_weird_subareas.R
* Box-Pierce test
* results saved to a csv

#### cumulative sum.R
* cumulative sum of the year for a given product and subarea
* cumulative sum of the month for a given product and subarea
* cumulative sum of the week for a given product and subarea
* values added to the dataset in 3 different columns

#### data preparation.R
* extract zone, area, subarea and product number
* extract month day, week day, year day, month, year from date
* assign weekend to saturdays and sundays
* assign season
* build a key

#### data preparation pt_2.R
* renaming of columns

#### data_exploration.R
* plot sales for each day of the week
* plot sales for each month
* plot sales for each day of the month

#### ensemble.R
* (outdated, use "join_results.R")

#### forest.r
* single random forest model
* multiple random forest model
* error evaluation
* 

#### join_results.R
* join together predictions from random forest, sarima and xgboost
* predictions can come from a test-set for which real data area available, or from real forecasting.
* assign zero prediction to selected subareas+products
* compute errors

#### missing_values_and_outliers_predictions.R
* use random forest to predict values for missing values and outliers

#### plotting maps and clustering.R
* plot all coordinates in a map
* plot all the zones in a map
* hierarchical clustering and knee elbow analysis
* kmeans clustering on k=3, 6, 20

#### predictions.R
* first test on the various models

#### predizione_aziendale_sarima_step2.R
* prediction of "vendite_giorn_prod" for the predset

#### predset_cluster_merge_step5.R
* adding clusters to prediction set

#### predset_creation_step3.R
* creation of prediction set

#### predset_preparation_step4.R
* data preparation on prediction set

#### sarima.R
* (outdated)

#### sarima_v2.R
* prediction using sarima
* compute future cumulative sales
* result evaluation

#### scoring functions.R
* mse
* mean ape
* max ape

#### useful_plots.R
* plot average sales by day and product
* plot average sales by month and product
* plot average sales by season and product
* plot average sales by zone and product

#### vendite_giornaliere_step1.R
* aggregate daily sales

#### xgboost.R
* (outdated)

#### xgboost_temp.R
* single model prediction
* multiple model prediction
* crossvalidation
