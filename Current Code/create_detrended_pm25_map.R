##### Purpose: Functions to create leaflet maps:

###  1. detrended PM2.5 levels across the city  (create_pm25_map(df_cleaned))
###  2. density of sampling across city  (create_measurement_density_map(df_cleaned))

##### Author: Sanelma Heinonen (sanelma.heinonen@gmail.com)
##### Date: May 2023


library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

# read in cleaned data, as outputted from data_preprocess.R

setwd("~/Documents/ETH Sem 2/StatsLab/Air-Quality-Gaussian-Process/Data")  # set working directory as required
df_cleaned <- fread("data_cleaned_final.csv")

#######################################################################################

###### (1) Function to create map to show median difference from baseline
###########  Inputs: df_cleaned, as outputted from data_preprocess.R

#######################################################################################


create_pm25_map = function(df_cleaned) {
  
  # process cleaned data by aggregating by 250m squares
  df_processed <- df_cleaned %>% 
    dplyr::group_by(tile_id) %>% 
    dplyr::summarise(max_lon = max(lon),    # tile boundaries for plotting
                     max_lat = max(lat), 
                     min_lon = min(lon), 
                     min_lat = min(lat), 
                     n_measurements = n(),     # number of total measurements from square
                     med_pm25 = median(pm25_detrended_comb)   # median detrended PM2.5 level across square
    ) %>%
    mutate(quantile = ntile(med_pm25, 10)) %>%  # calculate 10th quantiles for better visualization
    group_by(quantile) %>%
    mutate(lim = round(max(med_pm25))) %>%
    filter(!is.na(med_pm25))

  # setting color palette
  pal <- colorFactor("RdYlGn", domain = df_processed$lim, reverse = TRUE)
  
  # create map
  leaflet(df_processed) %>%

    addMapPane("rail", zIndex=570)%>%
    addMapPane("streets", zIndex=560) %>%
    addMapPane("Grenze", zIndex=580) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addRectangles(data = df_processed, 
                  lng1 = df_processed$min_lon, lng2 = df_processed$max_lon, 
                  lat1 = df_processed$min_lat, lat2 = df_processed$max_lat,
                  opacity = 0,
                  fillOpacity = 1,
                  fillColor = ~pal(df_processed$lim))  %>%
    addLegend(pal = pal, values = ~df_processed$lim, opacity = 1, 
              title = "PM2.5: Median diff from baseline")
}



#######################################################################################

###### (2) Function to create map that shows the number of measurements available from each square (sampling distribution across city)
###########  Inputs: df_cleaned, as outputted from data_preprocess.R

#######################################################################################


create_measurement_density_map <- function(df){

  #get counts per tile_id and divide into quantiles
  test <- df %>% 
    group_by(factor(tile_id)) %>% 
    dplyr::summarise(max_lon = max(lon), 
                     max_lat = max(lat), 
                     min_lon = min(lon), 
                     min_lat = min(lat), 
                     n_measure = n()) %>%
    mutate(quintile = ntile(n_measure, 10)) %>%
    dplyr::group_by(quintile) %>%
    dplyr::mutate(limit = max(n_measure))
  
  #color palette for map
  pal <- colorFactor("RdYlBu",  test$limit, reverse = TRUE)
  
  
  #leaflet map to show frequency of measurements within rectangles
  
  leaflet(df) %>%
    addMapPane("rail", zIndex=570)%>%
    addMapPane("streets", zIndex=560) %>%
    addMapPane("Grenze", zIndex=580) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addRectangles(data = test, 
                  lng1 = test$min_lon, lng2 = test$max_lon, 
                  lat1 = test$min_lat, lat2 = test$max_lat,
                  opacity = 0,
                  fillOpacity = 1,
                  fillColor = ~pal(test$limit)) %>%
    addLegend(pal = pal, values = ~test$limit, opacity = 1, 
              title = "n measurements per square") %>%
  #Adding caption at bottom
  addControl("<p> Number of measurements per square are shown in 10-quantiles. Legend gives upper limit of quantile.", position = "bottomleft") 
    
}



# plot maps
create_pm25_map(df_cleaned)
create_measurement_density_map(df_cleaned)



