library(sp)
library(tidyverse)

#Load data and convert GPS coordinates to a spatial object. Object labeled 'gps' is the positional data of the camera. This can be from a USBL or from the ship itself. CSV should contain
#GPS coordinates for every second of recording
setwd(" ")
gps <- read.csv("CSV containing GPS coordinates")
gps_sp <- SpatialPoints(cbind(gps$lng,gps$lat))

#calculates cumulative distance from the beginning of the transect for each coordinate
distance <- spDistsN1(gps_sp, gps_sp[1,], longlat=TRUE)

# Add output values to dataframe
gps$distance <- distance

# Find rows that pass the 0.01 value threshold. The distance is calculated in km, 0.01km = 10m, so the timestamps being extracted are for every 10m
thresholds <- seq(0.01, max(gps$distance), by = 0.01)

threshold_indices <- as.data.frame(thresholds) %>%
  mutate(index = map(thresholds, ~which(gps$distance >= .x)[1])) %>%
  unnest(cols = c(index))

# Add threshold column
final_gps <- gps %>%
  mutate(row_id = row_number()) %>%
  mutate(passes_threshold = row_id %in% threshold_indices$index) %>%
  select(-row_id)


#calculate time elapsed since start of the video
final_gps$taken_at <- as.POSIXct(final_gps$taken_at, format = "%H:%M:%S")
final_gps$time <- difftime(final_gps$taken_at, final_gps$taken_at[1], units = "secs")
final_gps$time <- as.numeric(final_gps$time)

#The time column is the total number of seconds elapsed since the start of the transect. The code below creates two new columns so that time elapsed can be read in minutes and seconds.
final_gps$mins <- floor(final_gps$time / 60)
final_gps$sec <- final_gps$time %% 60

#write over the csv that was read in at the beginning so that it now also contains the time stamps for the video.
write.csv(final_gps, "CSV containing GPS coordinates", row.names=FALSE)

#filter to show only rows where 10m distance threshold is passed
filtered <- final_gps %>% 
  filter(passes_threshold == TRUE)

#add a still number column so that you can check the metadata in your CSV for each still you extract. 
#Make sure VLC has "sequential numbering" ticked off in the video snapshot preferences so that rows match stills
filtered <- mutate(filtered, still_number = row_number()) %>%
  select(still_number, everything())
#add an empty column for making notes on stills if necessary
filtered$notes = " "

#save filtered list to a CSV 
write.csv(filtered, "filtered.csv", row.names=FALSE)

