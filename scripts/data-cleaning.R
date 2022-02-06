#### Preamble ####
# Purpose: Download and clean data from https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/
# Author: Hudson Yuen
# Data: 13 January 2021
# Contact: hudson.yuen@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(tidyr)
library(dplyr)


#### Data download ####

# data for each month needs to be downloaded separately
months <- sprintf("%02d", 1:12)
df_list <- list()

for(i in 1:length(months)) {
  dataname = sprintf("Bike share ridership 2021-%s", months[i])
  
  bikes_df <- 
    # https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/
    list_package_resources("7e876c24-177c-4605-9cef-e50dd74c617f") %>%
    filter(name == dataname) %>% 
    get_resource()
  
  df_list[[i]] <- bikes_df
  
  filename = sprintf("toronto-bikes-%s.csv", months[i])
  write_csv(
    x = bikes_df, 
    file = filename
  )
  
}

# joining june-december; missing data from jan-may
june_dec <- df_list[6:12]

df_june_dec <- bind_rows(june_dec)

# saving to csv
write_csv(
  x = df_june_dec, 
  file = "../inputs/toronto-bikes-june-dec-2021.csv"
)

# df_june_dec <- read_csv("toronto-bikes-june-dec-2021.csv")

# Data Cleaning
df_clean <- 
  clean_names(df_june_dec) %>% 
  mutate(start_time = mdy_hm(start_time,tz=Sys.timezone())) %>%
  mutate(end_time = mdy_hm(end_time,tz=Sys.timezone())) %>%
  select(-c(x_u_feff_trip_id))

df_clean <- data.table(df_clean)
head(df_clean)

df_clean <- df_clean[trip_duration >= 2, ] # around 4000 sub-2 minute trips removed

quantile(df_clean[, trip_duration], prob=c(.25, .5, .75, .9, .98)) # IQR = 718, 1.5x = 1077
df_clean <- df_clean[trip_duration < 2241] # remove Q3 + IQR outliers


# Replacing null-name station IDs with matched IDs from the rest of the dataset
start_station_pairs <- unique(df_clean[,c('start_station_id','start_station_name')])
start_station_pairs <- setNames(start_station_pairs, c('id', 'name'))

end_station_pairs <- unique(df_clean[,c('end_station_id','end_station_name')])
end_station_pairs <- setNames(end_station_pairs, c('id', 'name'))
end_station_pairs$id <- as.integer(end_station_pairs$id)

station_pairs <- bind_rows(start_station_pairs, end_station_pairs)
station_pairs <- unique(station_pairs[, c('id', 'name')])

start_null_ids <- start_station_pairs[name == 'NULL', id]
start_null_ids # originally 7682 7683 7684 7685 7686 7687 7688
end_null_ids <- end_station_pairs[name == 'NULL', id]
end_null_ids # originally 7675 7676 7677 7679 7678 7680 7681 7682 7683 7684 7686 7685 7687 7688

null_names <- station_pairs[id %in% null_ids & name != 'NULL', ]

for (id_iter in null_names[, id]) {
  df_clean <- df_clean[start_station_id == id_iter & start_station_name == 'NULL', start_station_name := null_names[id == id_iter, name]]
}

for (id_iter in null_names[, id]) {
  df_clean <- df_clean[end_station_id == id_iter & end_station_name == 'NULL', end_station_name := null_names[id == id_iter, name]]
}

df_clean <- df_clean[start_station_name != 'NULL' | end_station_name != 'NULL'] # removing 75 remaining unmatched null station rows
# df_clean <- df_clean[, !"name_type"] 
head(df_clean)
# saving cleaned df to csv, in total removed ~9.4k rows from ~2.6m total rows
write_csv(
  x = df_clean, 
  file = "../inputs/toronto-bikes-clean.csv"
)
