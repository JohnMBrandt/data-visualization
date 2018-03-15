require(lubridate)
require(urltools)
library(dplyr)
require(jsonlite)

pages <- list()
results <- list()
baseurl <- "https://api.data.gov.sg/v1/environment/pm25?date="
PSI_url <- "https://api.data.gov.sg/v1/environment/psi?date="
air_url <- "https://api.data.gov.sg/v1/environment/air-temperature?date_time="
park_url <- "https://api.data.gov.sg/v1/transport/carpark-availability?date_time="

dates <- seq(ymd('2017-06-01'), ymd('2018-02-01'), by="1 week")


#dates <- seq(ymd_hms('2018-01-01 00:10:00'), ymd_hms('2018-01-03 23:10:00'), by="24 hours")

dates <- as.character(dates)
for (i in seq_along(dates)) {
  dates[i] <- gsub(" ", "T", dates[i])
}
dates_enc <- toupper(url_encode(dates))

for(i in c(1:length(dates_enc))){
  try({
    mydata <- fromJSON(paste0(PSI_url, dates_enc[i]))
    print(paste0(PSI_url, dates_enc[i]))
    pages[[i]] <- mydata
  })
}

results <- rbind(pages)


#PM25.all <- rbind_pages(pages[sapply(pages, length)>0])
#PSI.all <- rbind_pages(pages[sapply(pages, length)>0])
#PSI.all <- as.data.frame(PSI.all)

weather.all <- unlist(rbind(results[sapply(results, length)>0]), recursive=FALSE)

weather.times <- weather.all[seq_along(weather.all) %% 2 > 0]
weather.times <- unlist(weather.times)

weather.readings <- do.call("rbind",  weather.all[seq_along(weather.all) %% 2 == 0])
final_readings <- weather.all[length(weather.all)]$carpark_data
final_readings2 <- rbind(weather.all$carpark_data)

final_readings <- as.data.frame(final_readings)


final_split <- strsplit(as.character(unnested$carpark_info), '"')

total_lots = list()
lots_type = list()
lots_available = list()

data <- list()

results <- results[sapply(results, length)>0]

for (i in c(1:length(results))) {
  data[[i]] <- unnest(results[[i]])
}

unnested <- do.call("rbind", data)

for (i in c(1:nrow(unnested))) {
  total_lots[i] <- final_split[[i]][2]
  lots_type[i] <- final_split[[i]][4]
  lots_available[i] <- final_split[[i]][6]
}

total_lots <- unlist(total_lots)
lots_type <- unlist(lots_type)
lots_available <- unlist(lots_available)
total_lots <- as.numeric(total_lots)
lots_available <- as.numeric(lots_available)

unnested$total <- total_lots
unnested$lots_type <- lots_type
unnested$lots_available <- lots_available
unnested <- unnested[,-c(2,4)]


for (i in c(1:length(final_readings))) {
  final_readings[[i]][3] <- i
}

weather.readings <- do.call("rbind", final_readings)
weather.readings$station_id <- as.factor(weather.readings$station_id)

weather.readings <- spread(weather.readings, station_id, value)

#weather.readings <- t(weather.readings)
#colnames(weather.readings) <- weather.readings[1,]
#weather.readings <- weather.readings[-1,]
#weather.readings <- as.data.frame(weather.readings)
#weather.readings <- weather.readings[c(1:length(weather.times)),]
weather.readings$date <- weather.times



#PM25.all$south <- PM25.all$readings$pm25_one_hourly$south
#PM25.all$north <- PM25.all$readings$pm25_one_hourly$north
#PM25.all$east <- PM25.all$readings$pm25_one_hourly$east
#PM25.all$central <- PM25.all$readings$pm25_one_hourly$central
#PM25.all$west <- PM25.all$readings$pm25_one_hourly$west

variable <- list()
location <- list()
for (i in c(1:length(PSI_split))) {
  variable[i] <- PSI_split[[i]][2]
  location[i] <- PSI_split[[i]][3]
}

variable <- unlist(variable)
location <- unlist(location)

