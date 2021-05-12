library(ggplot2)
library(grid)
library(dplyr)
library(knitr)
library(magick)
library(gifski)
library(gganimate)
setwd("~/Documents/BIKES")
theme_set(theme_bw())

dub_bb <- c(
  left = -6.5,
  bottom = 53.25,
  right = -6.0,
  top = 53.5
)

dub_stamen <- get_stamenmap(
  bbox = dub_bb,
  zoom = 11
)
j <- 33
while(j < 1074){
  i <- j:(j+24)
  filenames <- paste0("Bike_csv/bikes_", i, ".csv")
  all_ts <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files)}))
  all_ts$harvest_time <- as.POSIXct(all_ts$harvest_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  
  sort_ts <- all_ts[order(all_ts$lock_id), ]
  sort_ts$moving <- ifelse(sort_ts$lock_id == sort_ts$lock_id[2:nrow(sort_ts)] & (sort_ts$longitude != sort_ts$longitude[2:nrow(sort_ts)]  | sort_ts$latitude != sort_ts$latitude[2:nrow(sort_ts)] ), 1, 0)
  
  all_ts <- all_ts[order(all_ts$harvest_time, all_ts$lock_id), ]
  sort_ts <-  sort_ts[order(sort_ts$harvest_time, sort_ts$lock_id), ]
  all_ts$moving <- factor(sort_ts$moving)
  
  # p <- ggplot(all_ts, aes(longitude, latitude))+
  #   geom_point(data = all_ts,aes(longitude, latitude, color = moving))+
  #   coord_fixed() + 
  #   transition_time(harvest_time) + 
  #   labs(title = "Time Period: {frame_time}")
  # animate(p, fps=0.5)
  # 
  # p
  # 
  # 
  
  p <- ggmap(dub_stamen) + 
    geom_point(data = all_ts, mapping = aes(longitude, latitude, color = moving))+ 
    scale_color_manual(values=c("#555555", "#FFA500")) + 
    transition_time(harvest_time) + 
    labs(title = "Time Period: {frame_time}")
  
  anim <- animate(p, nframe = length(i), fps=1)
  magick::image_write(anim, path=paste0("Bike_gifs/bikes_", as.character(min(i)), "-",as.character(max(i)), ".gif"))
  j <- j+24
}