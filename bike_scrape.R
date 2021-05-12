library(jsonlite)
bikes <- fromJSON('https://bleeperbike.staging.derilinx.com/last_snapshot')
i <- 1075
while(TRUE){
  bikes <- try(fromJSON('https://bleeperbike.staging.derilinx.com/last_snapshot_1'), silent = TRUE)
  if(inherits(bikes, "try-error")){
    bikes <- try(fromJSON('https://bleeperbike.staging.derilinx.com/last_snapshot'), silent = TRUE)
    if(inherits(bikes, "try-error")){
      Sys.sleep(300)
    }else{
      write.csv(bikes, paste0("bikes_",i,".csv"))
      i <- i+1
      Sys.sleep(300)
    }
  }else{
    write.csv(bikes, paste0("bikes_",i,".csv"))
    i <- i+1
    Sys.sleep(300)
  }
}
