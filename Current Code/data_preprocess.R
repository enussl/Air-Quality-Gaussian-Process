# Clear variables
rm(list = ls())

# Retrieve filepaths from Python
# Ignore this line if not invoking subprocess() in Python
args <- commandArgs(trailingOnly=TRUE)

# Assign filepaths
# A string ending with "alldata.csv"
alldata_filepath <- args[1]

# Or assign filepath manually:
# alldata_filepath <- "C:\Users\nial\OneDrive\Documents\ETH-Zürich-DESKTOP-9E8KBS8\AQ Data Collection\alldata.csv"

# A string ending with "data_cleaned.csv"
data_cleaned_filepath <- args[2]

#Or assign filepath manually:
# data_cleaned_filepath <- "C:\Users\nial\OneDrive\Documents\ETH-Zürich-DESKTOP-9E8KBS8\AQ Data Collection\data_cleaned_.csv"

# Install used packages
packages <- c("dplyr", "data.table", "lubridate", "zoo", "BayesGPfit"
             , "plotly", "ggplot2", "ggmap", "pacman", "slider", "sf"
             , "osmdata", "geosphere", "crs", "rlist")
install.packages(packages, repos="https://stat.ethz.ch/CRAN/")

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

# Defining the helperfunctions
data_cleanse <- function(data_alldata){

  #Description:
  #The data cleansing process as a function

  #Inputs: 
  #data_alldata: a dataframe compiled from reading in Open Seneca's raw .csv files 
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

# Apply data cleansing function
alldata <- read.csv(alldata_filepath)
data_cleaned <- data_cleanse(alldata)

# Write clean data to a .csv file
write.csv(data_cleaned, data_cleaned_filepath, row.names=FALSE)

