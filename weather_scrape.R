library(jsonlite)
i <- 28
while(TRUE){
  weather <- fromJSON('API URL HERE')
  write.csv(weather, paste0("weather_",i,".csv"))
  i <- i+1
  Sys.sleep(172800)
}


