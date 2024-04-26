#' Obesity Risk (Training Data)
#'
#' A data set containing information regarding factors of obesity risk and categorization.
#'
#' @format a data frame containing 20758 rows and 18 columns:
#' \itemize{
#'   \item id: Identification number for individual
#'   \item Gender: Gender of the individual
#'   \item Age: Age of the individual (years)
#'   \item Height: Height of the individual (meters)
#'   \item Weight: Weight of the individual (kilograms)
#'   \item family_history_with_overweight: binary yes or no for family history
#'   \item FAVC: binary yes or no for frequent consumption of high caloric food
#'   \item FCVC: 1-3 frequency of consumption of vegetables
#'   \item NCP: 1-4 number of main meals
#'   \item CAEC: Consumption of food between meals (no, sometimes, frequently, always)
#'   \item SMOKE: binary yes or no for smoking
#'   \item CH20: 1-3 Consumption of water per day
#'   \item SCC: binary yes or no for the monitoring of calorie consumption
#'   \item FAF: 0-3 Physical activity frequency
#'   \item TUE: 0-2 Time using technology devices
#'   \item CALC: Consumption of alcohol (no, sometimes, frequently)
#'   \item MTRANS: Mode of transportation (walking, public_transportion, automobile)
#'   \item NObeyesdad: Body composition classification (Insufficient_Weight, Normal_Weight, Overweight_Level_1, Overweight_Level_2, Obesity_Type_1, Obesity_Type_2, Obesity_Type_3)
#' }
#' Note: Please see units/categories within each column description.
#' @source Kaggle: https://www.kaggle.com/competitions/playground-series-s4e2/overview
"OR_Train"

#' Obesity Risk (Testing Data)
#'
#' A data set containing information regarding factors of obesity risk and categorization.
#'
#' @format a data frame containing 13840 rows and 17 columns:
#' \itemize{
#'   \item id: Identification number for individual
#'   \item Gender: Gender of the individual
#'   \item Age: Age of the individual (years)
#'   \item Height: Height of the individual (meters)
#'   \item Weight: Weight of the individual (kilograms)
#'   \item family_history_with_overweight: binary yes or no for family history
#'   \item FAVC: binary yes or no for frequent consumption of high caloric food
#'   \item FCVC: 1-3 frequency of consumption of vegetables
#'   \item NCP: 1-4 number of main meals
#'   \item CAEC: Consumption of food between meals (no, sometimes, frequently, always)
#'   \item SMOKE: binary yes or no for smoking
#'   \item CH20: 1-3 Consumption of water per day
#'   \item SCC: binary yes or no for the monitoring of calorie consumption
#'   \item FAF: 0-3 Physical activity frequency
#'   \item TUE: 0-2 Time using technology devices
#'   \item CALC: Consumption of alcohol (no, sometimes, frequently)
#'   \item MTRANS: Mode of transportation (walking, public_transportion, automobile)
#' }
#' Note: Please see units/categories within each column description.
#' @source Kaggle: https://www.kaggle.com/competitions/playground-series-s4e2/overview
"OR_Test"
