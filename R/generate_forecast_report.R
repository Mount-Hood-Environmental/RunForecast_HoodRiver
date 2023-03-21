#Function to generate directories, set prediction year, and run the Hood River forecast report

generate_forecast_report = function(prediction_year = year(Sys.Date())){
  
  source('R/package_load.r')
  package_load(package_list = c("rmarkdown"))
  
  save(prediction_year, file = "R/prediction_year.rda")
  
  dir.create(path = paste0("R/model objects/",prediction_year), showWarnings = F)
  dir.create(path = paste0("report output/",prediction_year), showWarnings = F)
  dir.create(path = paste0("report output/",prediction_year,"/figures"), showWarnings = F)
  
  
  rmarkdown::render(input = "Report_markdown.rmd",
                    output_file = paste0("report output/",prediction_year,"/forecast_report_",prediction_year,".docx"))
}