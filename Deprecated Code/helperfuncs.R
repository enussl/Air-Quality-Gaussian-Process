# (I) Helper-functions to be called. Suggest we input all custom functions into here
# for easy readability of the scripts themselves.

################################################################################

# (II) ENVIRONMENT AND PACKAGES

# Empty environment
rm(list = ls())

# Load used packages
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(BayesGPfit)
library(plotly)
library(ggplot2)
library(ggmap)
library(pacman)
library(slider)
library(sf)
library(osmdata)
library(geosphere)
library(crs)
library(rlist)

################################################################################

data_cleanse <- function(data_alldata){

  #Description:
  #Emanuel's data cleansing process as a function
  #Transferring it here so we can import it & all use the same steps in data cleansing

  #Inputs: 
  #data_alldata: a dataframe compiled from reading in Peter's raw .csv files 
  #Plus a 'device' column identifying the driver

  #Outputs: 
  #data_alldata: filtered dataframe with important columns selected, anomalies removed, etc

  data_alldata = fread("./Data/Rawdata/alldata_new.csv")
  
  data_alldata = data_alldata %>%
    dplyr::select(Latitude, Longitude, PM2.5, Date, Time, Millis, device, Speed) %>% # interesting variables
    distinct() %>% # remove duplicates
    rename(lat = Latitude, lon = Longitude, pm25 = PM2.5) %>%
    filter(Date != 0 & Time != 0) %>% # faulty measurements
    filter(lat != 0 & lon != 0) %>% # faulty measurements
    mutate(lat = lat*(-1)) %>% # correct latitude
    mutate(Date_ft = as.Date(as.character(Date), format = "%Y%m%d")) %>% # date stuff
    mutate(Time_ft = format(strptime(sprintf('%06d', Time), format='%H%M%S'), '%H:%M:%S')) %>% # time stuff
    filter(Date_ft >= "2021-09-01" & Date_ft <= "2021-09-20") %>% # september 21 is our sample
    mutate(datetime = paste(Date_ft, Time_ft),
           datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>% # make combined date to compute difference between obs. later
    group_by(device, Date, Time) %>% # pick first observation per driver, Date and Time (which corresponds to second); we dont need intra-second measurements
    slice(1) %>% # could also do summarize and compute the mean but let`s just take the first one; its within one second anyway
    ungroup() %>%
    dplyr::select(-Millis) %>% # do not need milliseconds therefore
    group_by(Date, Time) %>% # compute active drivers
    mutate(count = n()) %>% # compute active drivers
    ungroup() %>%
    group_by(device) %>% # compute trajectories (drives) by looking at the differences between observations per driver, which sure might be handy later
    arrange(datetime) %>%
    mutate(time_diff = difftime(datetime, lag(datetime), units = "secs")) %>%
    mutate(trajectory_id = cumsum(time_diff > 30 | is.na(time_diff))) %>% # 30 seconds arbitrary as in in there is no measurement per driver for longer than 30 sec, this is a new traj.
    ungroup() %>%
    filter(Speed != 0) %>% 
    filter(pm25 <= 500) %>%
    filter(-2.16 <= lat & lat <= -1.85) %>%
    filter(29.9 <= lon & lon <= 30.25) %>%
    rolling_detrend() %>%
    aggregate_squares() %>%
    get_location_characteristics() 
  
  return(data_alldata)
}

# construct detrending function that uses rolling median or average over previous 15 min and 1 day, and then computes detrended value by subtracting the average of the two
rolling_detrend <- function(df, func = median){
  
  # Inputs
  # df: data set as pre-processed
  # func: summary function for baseline. Default median
  
  # Output: data set with 15 min baseline, 1 hr baseline, and de-trended pm 2.5 (by 15 min, 1 day, and average of the two)

  #creating moving baseline
  df = df %>% arrange(datetime)
  
  df = df %>% mutate(
    baseline_15 = slide_index_dbl(
      pm25,                       # calculate baseline based on pm25 column
      .i = datetime,       # indexed with datetime 
      .f = ~func(.x, na.rm = TRUE),     # summarise by function 
      .before = minutes(15)),               # window is 15 prior minutes
    baseline_day = slide_index_dbl(
      pm25,                      
      .i = datetime,      
      .f = ~func(.x, na.rm = TRUE),     
      .before = days(1)),  
    pm25_detrended_15 = pm25 - baseline_15, 
    pm25_detrended_day = pm25 - baseline_day,
    pm25_detrended_comb = pm25 - 1/2*(baseline_15+baseline_day)
    )
  
}

#function to assign a tile_id to each observation, in case we want to use data aggregated geographically. Tiles are squares of size that can be inputted
aggregate_squares <- function(df, len = 200){

  # data set as pre-processed
  # len: side length of square for aggregation, in meters. Defaults to 200 
  
  # Output: data set with additional variables of tile_id, which geogtraphic tile the observation lies in
  
  #convert to geometry
  df_geo = df
  df_geo = df_geo %>% st_as_sf(coords = c("lon", "lat"))

  #set coordinate reference system
  st_crs(df_geo$geometry) <- 4326

  #transform coordinate reference system to be able to measure in meters
  df_geo <- st_transform(df_geo, crs=st_crs(7801))

  #create grid
  my_grid <- sf::st_make_grid(x = df_geo, cellsize = c(len, len))

  #  st_area(my_grid[1])
  
  #transform back to original coordinate system
  my_grid <- st_transform(my_grid, crs = 4326)
  df_geo <- st_transform(df_geo, crs=st_crs(4326))

  #get intersection of points and grid
  intersects <- st_intersects(df_geo$geometry, my_grid)
  
  df$tile_id <- as.integer(intersects)
  
  return(df)
  
}

# function that adds industrial area and road type classifications
get_location_characteristics <- function(df){
  # input: data set as pre-processed
  
  # Output: data set with additional binary variables of industrial, major.road, and res.road, 
  # which indicate whether measurement is from an industrial area, from a major road, or from a residential area
  # landuse and road characteristics as defined from open street map data
  # major roads are all of the following osm labels: primary, primary_link, trunk, trunk_link, secondary, secondary_link, tertiary, tertiary_link, unclassified
  
  
  osm.kigali<- getbb("Kigali", format_out = "data.frame")
  
  #query industrial landuse data
  kigali.industrial <- osm.kigali %>%
    opq(timeout = 25*100) %>%
    add_osm_feature(key = "landuse", value = "industrial") %>%
    osmdata_sf()
  
  #query road data
  highway_tags <- c(available_tags("highway"))
  kigali.roads <- osm.kigali %>%
    opq(timeout = 25*100) %>%
    add_osm_feature(key = "highway", value = highway_tags) %>%
    osmdata_sf()
  
  roads <- kigali.roads$osm_lines  %>% 
    mutate(residential = ifelse(highway %in% c("residential", "living_street"), 1, 0), 
           through.road = ifelse(highway %in% c("primary", "primary_link", "trunk", 
                                                "trunk_link", "secondary", "secondary_link", 
                                                "tertiary", "tertiary_link", "unclassified"), 1, 0))
  

  
  ## adding variable for whether in an industrial zone or not
  industrial <- kigali.industrial$osm_polygons
  df_geo = df
  df_geo = df_geo %>% st_as_sf(coords = c("lon", "lat"))
  st_crs(df_geo$geometry) = st_crs(industrial$geometry)
  df$industrial <- as.numeric(lengths(st_intersects(df_geo$geometry, industrial$geometry)))
  
  
  ## adding variable for whether on a major road or not
  major.roads <- roads %>% filter(through.road == 1)
  
  #need to add a buffer to the major roads as otherwise the intersection is empty
  major.roads$geometry_trans <- st_transform(major.roads$geometry, crs = 7801)
  major.roads$buffer_trans <- st_buffer(major.roads$geometry_trans, dist = 5)  #5 meter buffer
  major.roads$buffer<- st_transform(major.roads$buffer_trans, crs = 4326)
  
  #finding intersection
  df$major.road <- as.numeric(lengths(st_intersects(df_geo$geometry, major.roads$buffer)))
  df <- df %>% mutate(major.road = ifelse(major.road > 0 , 1, 0))
  
  ## adding variable for residential roads
  res.roads <- roads %>% filter(residential == 1)
  
  #need to add a buffer to the res roads as otherwise the intersection is empty
  res.roads$geometry_trans <- st_transform(res.roads$geometry, crs = 7801)
  res.roads$buffer_trans <- st_buffer(res.roads$geometry_trans, dist = 3) #3 meter buffer
  res.roads$buffer<- st_transform(res.roads$buffer_trans, crs = 4326)
  
  #finding intersection
  df$res.road <- as.numeric(lengths(st_intersects(df_geo$geometry, res.roads$buffer)))
  df <- df %>% mutate(res.road = ifelse(res.road > 0 , 1, 0))
  
  #return df
  return(df)
}


#function that creates leaflet map showing density of measurements per tile, given subset of cleaned dataframe
create_measurement_density_map_cont <- function(df){
  minLng <- 29.9
  minLat <- -2.08 
  maxLng <- 30.2
  maxLat <- -1.9
  
  #get counts per tile_id and divide into quantiles
  test <- df %>% 
    group_by(factor(tile_id)) %>% 
    dplyr::summarise(max_lon = max(lon), 
                     max_lat = max(lat), 
                     min_lon = min(lon), 
                     min_lat = min(lat), 
                     n_measure = n()) %>%
    mutate(quintile = ntile(n_measure, 5)) %>%
    dplyr::group_by(quintile) %>%
    dplyr::mutate(limit = max(n_measure))
  
  pal <- colorNumeric(brewer.pal(9, "Spectral"), test$n_measure, reverse = TRUE)
  
  #leaflet map to show frequency of measurements within rectangles
  leaflet(df) %>%
    addMapPane("streets", zIndex=560) %>%
    addMapPane("Grenze", zIndex=580) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    fitBounds(minLng, minLat, maxLng, maxLat) %>%
    addRectangles(data = test, 
                  lng1 = test$min_lon, lng2 = test$max_lon, 
                  lat1 = test$min_lat, lat2 = test$max_lat,
                  opacity = 0,
                  fillOpacity = 1,
                  fillColor = ~pal(test$n_measure)) %>%
    addLegend(pal = pal, values = ~test$n_measure, opacity = 1, 
              title = "N observations")
  
}


# Function that computes difference of predicted surface of GP and overall median surface
# and that performs the test if they are equal for all location and returns the test statistics
# for all locations and further the areas where the null hypothesis is rejected
hypo_test = function(fit, nr_locations, conf_level) {
  
  ## fit: a fitted model from fit = gpkm(Z,X)
  ## nr_locations: number of equi-spaced locations for which we test the null hypo
  ## conf_level: confidence level, e.g. most likely one of 0.1, 0.05, 0.01
  
  ## Output: For each location, we output the z-value=(predicted value-median pm25)/standard error of pred) and 
  ## whether the null-hypo is rejected.
  
  # Compute median pm 2.5 value (value under the null)
  median_pm25 = median(fit$Z)
  
  # Construct the grid of (lat, lon) where we want to generate predictions
  lat_pred = seq(min(fit$X[,1]), max(fit$X[,1]), l = sqrt(nr_locations)) 
  lon_pred = seq(min(fit$X[,2]), max(fit$X[,2]), l = sqrt(nr_locations)) 
  grid = expand.grid(lat_pred, lon_pred)
  
  # Predictions
  pred_val = fit$predict(grid, se.fit = T)$mean
  pred_se = fit$predict(grid, se.fit = T)$se
  
  # Z-values
  z_stat = (pred_val - median_pm25)/pred_se
  
  # Reject null-hypo (=1) or not (=0)
  rej_null = ifelse(abs(z_stat)>=qnorm(1-conf_level), 1, 0)
  
  # Relate z_stat and rej_null vectors back to (lat_pred, lon_pred)
  z_stat_mat = matrix(z_stat, nrow = length(lat_pred), ncol = length(lon_pred), byrow = FALSE)
  rej_null_mat = matrix(rej_null, nrow = length(lat_pred), ncol = length(lon_pred), byrow = FALSE)
  
  # Return those
  return(list(Z_values = z_stat_mat,
              Rejection_indicator = rej_null_mat))
}

dist_to_primary_road <- function(kigali_prim_rd, point){
  #Function to measure the distance between a point and a linestring
  #Idea: see how close a point is to a main road

  #Inputs:
  #point: a character vector of (lon, lat)
  #kigali_prim_rd: an sfg object. Requires 'sf' package in R

  #Outputs:
  #list of three elements: 
  #[[1]] distance of point from the road
  #[[2]] & [[3]]: lon & lat of point on kigali_prim_rd at which distance is evaluated

  closest <- geosphere::dist2Line(p = point, line = st_coordinates(kigali_prim_rd)[,1:2])
  return(closest)
}


min_dist_to_primary_road <- function(kigali_prim_rds, point){
  #Function to measure the minimum distance between a point and a set of linestrings
  #Idea: find distance from a point to the nearest primary road

  #Inputs:
  #point: a character vector of (lon, lat)
  #kigali_prim_rds: an osmdata object with osm_lines

  #Outputs: the minimum distance (metres) between point and any one of the osm_lines
  #Also, the coordinates of the line section nearest the point
  kigali_prim_rds <- st_geometry(kigali_prim_rds$osm_lines)
  distances <- lapply(X=kigali_prim_rds, FUN=dist_to_primary_road, point=point)
  dist_mat <- list.rbind(distances)
  min_ind <- match(min(dist_mat[,"distance"]),dist_mat[,"distance"])
  return(dist_mat[min_ind,])
}

mdtpr_row <- function(row, kigali_prim_rds){
  #Function applying min_dist_to_primary_road to a row of the dataframe

  #Inputs:
  #row: a row from the cleaned dataframe
  #kigali_prim_rds: an osmdata object with osm_lines

  #Outputs: 
  #Output [[1]] from min_dist_to_primary_road applied to the dataframe row
  point <- c(as.double(row["lon"]), as.double(row["lat"]))
  dist <- min_dist_to_primary_road(kigali_prim_rds,point)[[1]]
  return(dist)
}

mdtpr_df <- function(df, kigali_prim_rds, max_dist=10){
  #Function applying min_dist_to_primary_road to every row of the dataframe

  #Inputs:
  #df: the cleaned dataframe
  #kigali_prim_rds: an osmdata object with osm_lines. Example code to produce:
  #kigali_prim_rds <- kigali_bb %>%
  #                   opq() %>%
  #                   add_osm_feature("highway", "primary") %>%
  #                   osmdata_sf()
  #max_dist: maximum number of metres between an observation and a main road (default 10)

  #Outputs:
  #df: the cleaned dataframe with two new columns:
  #(i) distance_to_main_road: in metres, the distance to the nearest primary road in kigali_prim_rds
  #(ii) is_main_road: 1 if distance_to_main_road < max_dist, else 0

  distance_to_main_road <- apply(X=df, MARGIN=1 , FUN=mdtpr_row, kigali_prim_rds)
  distance_to_main_road <- unname(distance_to_main_road)
  is_main_road <- -- (distance_to_main_road <= max_dist)
  return(cbind(df, distance_to_main_road, is_main_road))
}



