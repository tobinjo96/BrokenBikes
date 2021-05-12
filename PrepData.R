library(rdist)
library(FNN)
library(plyr)
library(HDoutliers)

setwd("~/Documents/BIKES")
path <- 'weather_csv/weather_' # use your path
all_files <- paste0(path, 1:28, ".csv")
weather <- lapply(all_files, read.csv)
weather <- do.call(rbind, weather)
weather$validTimeLocal <- as.character(weather$validTimeLocal)
weather$validTimeLocal <- substr(weather$validTimeLocal,1,nchar(weather$validTimeLocal)-5)


weather$validTimeUtc <- as.POSIXct(weather$validTimeUtc, tz = "UTC", origin="1970-01-01") 
weather$validTimeLocal <- as.POSIXct(weather$validTimeLocal, tz = "UTC", format="%Y-%m-%dT%H:%M:%S") 

grids <- read.csv("Grids.csv")
grids$Rent <- gsub( "€", "", as.character(grids$Rent))
grids$Rent <- gsub( ",", "", as.character(grids$Rent))
grids$Rent <- as.numeric(grids$Rent)

for (i in 33:1074){
  bike_i <- read.csv(paste0("Bike_csv/bikes_", as.character(i), ".csv"))
  bike_i$harvest_time <- as.POSIXct(bike_i$harvest_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  idx <- which.min(abs(weather$validTimeLocal - bike_i$harvest_time[1]))
  bike_i <- cbind(bike_i, weather[rep(idx, nrow(bike_i)), ])
  dis <- cdist(grids[, c("Lat.Cen", "Long.Cen")], bike_i[, c("latitude", "longitude")])
  grididx <- apply(dis, 2, FUN = which.min)
  bike_i <- cbind(bike_i, grids[grididx, ])
  bike_i <- bike_i[!duplicated(bike_i[c("lock_id", "frame_id")]),]
  saveRDS(bike_i, paste0("Bike_rds/bikes_", as.character(i), ".RDS"))
}


i <- 33
bike <- readRDS(file= "Bike_rds/bikes_33.RDS")
bike$nn_dis <- get.knn(bike[, c("latitude", "longitude")], k = 1)[[2]]
bike$prev_trip_length = 0
bike$curr_trip_length = 0
bike$time_to_last_trip = 0
bike$weekend = ifelse(bike$dayOfWeek %in% c("Saturday", "Sunday"), 1, 0)
bike$moving = FALSE
saveRDS(bike, file= "Bike_rds/bikes_33.RDS")

for (i in 34:1074){
  bike <- readRDS(file= paste0("Bike_rds/bikes_", as.character(i), ".RDS"))
  bike_prev <- readRDS(file= paste0("Bike_rds/bikes_", as.character(i-1), ".RDS"))
  bike$nn_dis <- get.knn(bike[, c("latitude", "longitude")], k = 1)[[2]]
  mergesub <- c('lock_id', 'frame_id')
  presub <- c('lock_id', 'frame_id', 'prev_trip_length', 'curr_trip_length', 'time_to_last_trip')
  bike <- join(bike, bike_prev[, presub], by = mergesub, type = 'left')
  bike$prev_trip_length[is.na(bike$prev_trip_length)] <- 0
  bike$curr_trip_length[is.na(bike$curr_trip_length)] <- 0
  bike$time_to_last_trip[is.na(bike$time_to_last_trip)] <- 0
  bike$weekend = ifelse(bike$dayOfWeek %in% c("Saturday", "Sunday"), 1, 0)
  
  colsub <- c('lock_id', 'frame_id', 'latitude', 'longitude')
  bike_merge <- join(bike[, colsub], bike_prev[, colsub], by=mergesub, type = "left")
  names(bike_merge) <- c("lock_id", "frame_id", "latitude.x", "longitude.x", "latitude.y", "longitude.y")
  bike_merge$mov_dis <- sqrt((bike_merge$latitude.x - bike_merge$latitude.y)^2 + (bike_merge$longitude.x - bike_merge$longitude.y)^2)
  bike_merge$mov_dis[is.na(bike_merge$mov_dis)] <- 0
  bike$moving = bike_merge$mov_dis != 0
  
  movsub <- c('lock_id', 'frame_id', 'moving')
  bike_mov_merge <- join(bike[, movsub], bike_prev[, movsub], by = mergesub, type = "left")
  names(bike_mov_merge) <- c('lock_id', 'frame_id', 'moving.x', 'moving.y')
  bike_mov_merge$mov_prod <- bike_mov_merge$moving.x & bike_mov_merge$moving.y
  bike_mov_merge$mov_prod[is.na(bike_mov_merge$mov_dis)] <- FALSE
  bike_mov_merge$moving.y[is.na(bike_mov_merge$moving.y)] <- FALSE
  
  bike[bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'curr_trip_length'] <- 
    bike[bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'curr_trip_length'] + bike_merge[bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'mov_dis']
  bike[bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'time_to_last_trip'] <- 0
  
  bike[bike_mov_merge$moving.x & !bike_mov_merge$moving.y, 'curr_trip_length'] <- bike_merge[bike_mov_merge$moving.x & !bike_mov_merge$moving.y, 'mov_dis']
  bike[bike_mov_merge$moving.x & !bike_mov_merge$moving.y, 'time_to_last_trip'] <- 0
  
  bike[!bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'prev_trip_length'] <- bike[!bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'curr_trip_length']
  bike[!bike_mov_merge$moving.x & bike_mov_merge$moving.y, 'time_to_last_trip'] <- bike[1, 'harvest_time'] - bike_prev[1, 'harvest_time']
  
  bike[!bike_mov_merge$moving.x & !bike_mov_merge$moving.y, 'time_to_last_trip'] <- bike[!bike_mov_merge$moving.x & !bike_mov_merge$moving.y, 'time_to_last_trip'] + (bike[1, 'harvest_time'] - bike_prev[1, 'harvest_time'])
  
  saveRDS(bike, file= paste0("Bike_rds/bikes_", as.character(i), ".RDS"))
}





