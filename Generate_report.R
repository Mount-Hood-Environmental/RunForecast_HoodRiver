#Generates Hood River run forecast report for a given prediction year
#Author: Mark Roes
#Prepared for: Confederated Tribes of Warm Springs

#To run, simply update the "prediction_year" to the appropriate year, then highlight the lines below and click run.
source("R/generate_forecast_report.r")
generate_forecast_report(prediction_year = 2023)