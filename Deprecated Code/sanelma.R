## Purpose: code that will read in all the files from a folder, and put the device that is in the top row into a colummn
# Author: Sanelma Heinonen
#Date: March 6


library(tidyverse)
library(lubridate)
library(data.table)
library(formattable)
library(writexl)
#library(ncdf4)
library(sf)
#library(raster)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(hms)
#library(openairmaps)
#library(zoo)
library(osmdata)
#library(ggmap)
#library(conflicted)



getwd()=='/Users/sanelmaheinonen/Documents/ETH Sem 2/StatsLab'
path = "~/Documents/ETH Sem 2/StatsLab/Data/"

#leaflet map default bounds
minLng <- 29.9
minLat <- -2.08 
maxLng <- 30.2
maxLat <- -1.9



#df_pickle <- fread(paste0(path, "pickel_data.csv"))

df_cleaned <- fread(paste0(path, "data_cleaned_final.csv"))

#filtering out pm2.5 values above 500
df_cleaned <- df_cleaned[pm25 < 500,]


osm.kigali<- getbb("Kigali", format_out = "data.frame")

#query roads in kigali
#wasnt sure which road types would be available so queried for all available tags under highway
# turns out the following are available c("secondary", "service", "residential", "tertiary", "pedestrian", "footway", "trunk", "unclassified" "path")
# so, could just query for these under 'value' instead, would probably be faster
highway_tags <- c(available_tags("highway"))
kigali.roads <- osm.kigali %>%
       opq(timeout = 25*100) %>%
       add_osm_feature(key = "highway", value = highway_tags) %>%
       osmdata_sf()

landuse_tags <- c(available_tags("landuse"))
kigali.landuse <- osm.kigali %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "landuse", value = landuse_tags) %>%
  osmdata_sf()
landuse.poly <- kigali.landuse$osm_polygons %>% filter(!is.na(landuse))

kigali.industrial <- osm.kigali %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "landuse", value = "industrial") %>%
  osmdata_sf()

#this is null
kigali.factory <- osm.kigali %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "manmade", value = "works") %>%
  osmdata_sf()

#looking at most common landuse types
temp <- landuse.poly %>% mutate(area = st_area(geometry))  %>% 
  group_by(landuse) %>% 
  summarise(area = sum(area)) %>% 
  arrange(desc(area))

top.ten.landuse <- temp[1:10,]$landuse


#map of kigali roads, primary roads, and industrial areas from osmdata
map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
#  addCircles(data = kigali.roads$osm_points)
  addPolylines(data = kigali.roads$osm_lines %>% filter(!highway == "primary"),
               weight = 1) %>%
  addPolylines(data = kigali.roads$osm_lines %>% filter(highway == "primary"), 
               color = "red", weight = 2) %>%
  addPolygons(data = kigali.industrial$osm_polygons, color = "red") %>%
#  addCircles(data = test %>% filter(industrial ==1), 
#             radius  = 10, 
#             fillColor = "green", 
#             opacity = 1) %>%
  addLegend(colors = c("red", "blue"), labels = c("Industrial areas & primary roads", "All roads"))

#plotting landuse types in the city - looks like majority of land area is not classified
temp <- landuse.poly %>% filter(landuse %in% top.ten.landuse)
pal <- colorFactor("Spectral", temp$landuse)
leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addPolygons(data = landuse.poly,
              fillColor = ~pal(temp$landuse), 
              fillOpacity = 1, 
              weight = 0.1) %>%
  addLegend(pal = pal, values = temp$landuse)






htmlwidgets::saveWidget(map, paste0(path, "kigali_landuse.html"))

#would be cool to try to add airquality onto the same map as the big roads and industrial areas

# want to join osm data to cleaned data to indicate whether a measurement is on a major road or industrial area


#in order to get buffer, need to transform into a flat coordinate system and then back again


#adding variable for whether in an industrial zone or not
industrial <- kigali.landuse$osm_polygons %>% filter(landuse == "industrial")

test <- head(df_cleaned, 10000)

df_geo = test
df_geo = df_geo %>% st_as_sf(coords = c("lon", "lat"))
st_crs(df_geo$geometry) = st_crs(industrial$geometry)
test$industrial <- as.numeric(lengths(st_intersects(df_geo$geometry, industrial$geometry)))


### adding variable for whether on a major road or not
roads <- kigali.roads$osm_lines  %>% 
  mutate(residential = ifelse(highway %in% c("residential", "living_street"), 1, 0), 
         through.road = ifelse(highway %in% c("primary", "primary_link", "trunk", 
                                              "trunk_link", "secondary", "secondary_link", 
                                              "tertiary", "tertiary_link", "unclassified"), 1, 0))
major.roads <- roads %>% filter(through.road == 1)

#need to add a buffer to the major roads as otherwise the intersection is empty

major.roads$geometry_trans <- st_transform(major.roads$geometry, crs = 7801)
#plot(st_geometry(major.roads$geometry_trans))

#adding a 3 meter buffer around the roads
major.roads$buffer_trans <- st_buffer(major.roads$geometry_trans, dist = 3)
#plot(st_geometry(major.roads$buffer), add = TRUE)
major.roads$buffer<- st_transform(major.roads$buffer_trans, crs = 4326)

#finding intersection
df_geo = test
df_geo = df_geo %>% st_as_sf(coords = c("lon", "lat"))
st_crs(df_geo$geometry) = st_crs(major.roads$geometry)
test$major.road <- as.numeric(lengths(st_intersects(df_geo$geometry, major.roads$buffer)))

### adding variable for residential roads
res.roads <- roads %>% filter(residential == 1)

#need to add a buffer to the major roads as otherwise the intersection is empty

res.roads$geometry_trans <- st_transform(res.roads$geometry, crs = 7801)
#plot(st_geometry(res.roadsads$geometry_trans))

#adding a 3 meter buffer around the roads
res.roads$buffer_trans <- st_buffer(res.roads$geometry_trans, dist = 3)
#plot(st_geometry(res.roadsads$buffer), add = TRUE)
res.roads$buffer<- st_transform(res.roads$buffer_trans, crs = 4326)

#finding intersection
df_geo = test
df_geo = df_geo %>% st_as_sf(coords = c("lon", "lat"))
st_crs(df_geo$geometry) = st_crs(res.roads$geometry)
test$res.roads <- as.numeric(lengths(st_intersects(df_geo$geometry, res.roads$buffer)))



st_crs(industrial$geometry) <- 4326
st_crs(df_activities$location) <- 4326

pt_stops$geometry_trans <- st_transform(pt_stops$geometry, crs = 7801)
plot(st_geometry(pt_stops$geometry_trans))


#for now, just a 20 m buffer for all stops
pt_stops$buffer_trans <- st_buffer(pt_stops$geometry_trans, dist = 20)
plot(st_geometry(pt_stops$buffer), add = TRUE)
pt_stops$buffer<- st_transform(pt_stops$buffer_trans, crs = 4326)

#check if activity happened within buffer zone of a pt stop
df_activities$at_pt_stop <- lengths(st_intersects(df_activities$location, pt_stops$buffer))







#map to show frequency of measurements within rectangles
leaflet() %>%
  addMapPane("Markers", zIndex=590)%>%
  addMapPane("MarkersGeplante", zIndex=585)%>%
  addMapPane("MarkersGeplanteOver", zIndex=999)%>%
  addMapPane("rail", zIndex=570)%>%
  addMapPane("streets", zIndex=560) %>%
  addMapPane("Grenze", zIndex=580) %>%
  #  addProviderTiles(providers$OpenStreetMap.CH, options = providerTileOptions(opacity = 0.9)) %>%
  #  addProviderTiles(providers$Esri.WorldTopoMap) %>%    #names(providers) to see options, but not all available for switzerland
  #  addProviderTiles(providers$CartoDB.Voyager) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addPolygons(data = my_grid)

leaflet() %>% 
  addPolygons(data = my_grid)
  
  
  addRectangles(data = test, 
                lng1 = test$min_lon, lng2 = test$max_lon, 
                lat1 = test$min_lat, lat2 = test$max_lat,
                opacity = 0,
                fillOpacity = 1,
                fillColor = ~pal(test$med_pm25))  %>%
  addLegend(pal = pal, values = ~test$med_pm25, opacity = 1, 
            title = "Median difference from baseline")




#trying to join raw data to pickle data on lat, lon, and datetime
test <- df_air %>% 
  dplyr::select(Latitude, Longitude, Speed, Date, Time, PM2.5, device) %>%
  mutate(Time1 = format(strptime(sprintf('%06d', Time), format='%H%M%S'), '%H:%M:%S'), 
         Date1 = as.Date(as.character(Date), format = "%Y%m%d")) %>%
  mutate(datetime.string = paste(Date1, Time1, sep = " ")) %>%
  mutate(Latitude = -1*Latitude)  #latitude seems to be off by multiple of -1?

test.pickle <- df_pickle %>% mutate(
  datetime.string = as.character(gps_datetime)
)

#try to join - variables just don't line up across the data sets??
temp <- left_join(test.pickle, 
                  test, 
                  by = c("lat" = "Latitude", 
                         "lon" = "Longitude", 
                         "datetime.string"))


# Get number of active drivers from data directly (remember to compute speed variable)  
df_pickle = df_pickle %>%
  mutate(gps_datetime = as.POSIXct(gps_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
  arrange(gps_datetime) %>%
  mutate(time = as.integer(gps_datetime)) %>%
  group_by(time) %>% 
  mutate(count = n()) %>%
  ungroup()

# Get number of active drivers from data directly (remember to compute speed variable)  
df_air = df_air %>%
  mutate(Time1 = format(strptime(sprintf('%06d', Time), format='%H%M%S'), '%H:%M:%S'), 
         Date1 = as.Date(as.character(Date), format = "%Y%m%d"), 
         datetime.string = paste(Time1, Date1, sep = " ")) %>%
 # mutate(gps_datetime = as.POSIXct(gps_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
# arrange(gps_datetime) %>%
 # mutate(time = as.integer(gps_datetime)) %>%
  group_by(datetime.string) %>% 
  mutate(count = n()) %>%
  ungroup()











#read in nasa data. It's an nc file. I'm not sure this contains info on particulate matter pollution - may be aerosals only
nasa_data <- nc_open(paste0(path, "S5P_OFFL_L2__AER_LH_20190902T103857_20190902T122026_09776_01_010302_20190908T125637.nc"))

#getting variables from the nasa data. Can't find anything about aerosol density
#not in dataframe format yet. Would want to do this to use the data
lat <- ncvar_get(nasa_data, "PRODUCT/latitude")
lon <- ncvar_get(nasa_data, "PRODUCT/longitude")
time <- ncvar_get(nasa_data, "PRODUCT/time_utc")
aerosol_index <- ncvar_get(nasa_data, "PRODUCT/SUPPORT_DATA/INPUT_DATA/aerosol_index_354_388")


#trying to read in raw data - set path for the folder
path <- "~/Documents/ETH Sem 2/StatsLab/Data/AQ Data Collection/Week 1- Sensors_ Data-Kigali/Gasago Venuste- Moto taxi/"

#create list of files to read in (all files from the folder)
files <- list.files(path = path, pattern = "*.CSV", full.names = TRUE) 

#create function that reads in data from a file, and adds top row of data as a column called "device"
my_func <- function(file){
 # fread(file, skip = 1)
  fread(file, skip = 1) %>% mutate(
    device = fread(file, nrows = 1)$V4)
  
}

#reading in all data from the folder 
df_air <- files %>%  map_df(~my_func(.)) 

#remove a few unnecessary columns
df <- df %>% select(-PM1.0, -PM4.0, -PM10)




## inspecting timing of collection

df_pickle <- df_pickle %>% mutate(time = as_hms(gps_datetime))

#distribution of time of collection throughout the day
ggplot(df_pickle, aes(x = time)) + 
  geom_histogram(bins = 96) +
  scale_x_time(breaks = c(as_hms("00:00:00"), as_hms("01:00:00"), as_hms("02:00:00"), as_hms("03:00:00"), 
               as_hms("04:00:00"), as_hms("05:00:00"), as_hms("06:00:00"), as_hms("07:00:00"), 
               as_hms("08:00:00"), as_hms("09:00:00"), as_hms("10:00:00"), as_hms("11:00:00"), 
               as_hms("12:00:00"), as_hms("13:00:00"), as_hms("14:00:00"), as_hms("15:00:00"), 
               as_hms("16:00:00"), as_hms("17:00:00"), as_hms("18:00:00"), as_hms("19:00:00"), 
               as_hms("20:00:00"), as_hms("21:00:00"), as_hms("22:00:00"), as_hms("23:00:00"),
               as_hms("24:00:00"))) +
  theme_bw() + 
  labs(title = "Number of sensor readings by hour of day")+
  theme(axis.text.x = element_text(angle = 45))

df_night <- df_pickle %>%
  filter(time < as_hms("04:00:00") | time > as_hms("23:00:00"))

#plotting observations that happened at night on the grid
ggplot(df_night, aes(x = lon, y = lat)) + 
  geom_point(alpha = 0.003, size = 0.01) + 
  scale_x_continuous(limits = c(minLng, maxLng)) + 
  scale_y_continuous(limits = c(minLat, maxLat)) + 
  theme_bw()

# some tiles have 10s of thousands of observations
#should maybe inspect some of these closer
test <- df_night %>% dplyr::count(tile_id) %>%
  arrange(desc(n))
  



top_night_tiles <- df_night %>% 
  group_by(tile_id) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) 

ids_with_many_night_readings <- (top_night_tiles %>% filter(n > 5000))$tile_id

night_tiles_to_investigate <- df_night %>%
  filter(tile_id %in% c(ids_with_many_night_readings))



#plots sensor counts per hour over time for given tile id
plot_tile_readings <- function(tile){
  ggplot(df_pickle %>% filter(tile_id == tile) %>%
           mutate(night = ifelse(time < as_hms("04:00:00") | time > as_hms("23:00:00"), 
                                 "night", "day")), 
         aes(x = gps_datetime, 
             fill = night)) + 
    geom_histogram(binwidth = 60*60) + 
    labs(title = paste("number of tile readings per hour within tile_id ", tile)) + 
    theme_bw()
}

#looks like the sensor in "hires.haggle.beads" tile was left on over night for two nights
# "marketing.vitals.frantic" for one night as well
#when have sensor ids, should confirm and then likely filter these out


########## Exploratory data analysis for initial presentation

ggplot(df_pickle %>% filter(timestamp < "2021-09-08 09:34:15 UTC"), 
       aes(x = as.factor(date), y = pm25)) + 
  geom_boxplot() + 
#  labs(x = "Datetime", y = bquote('Baseline PM2.5 concentration (\u03BCg/' ~ m^3 ~ ")"), 
 #      title = "Baseline PM2.5 concentration in Kigali over study period") +
  theme_bw()

#some weirdly outlier looking periods where the baseline is above 300,
temp <- df_pickle  %>% filter(gps_datetime < "2021-09-03 22:30:00" & gps_datetime > "2021-09-03 22:15:00")

mean(temp$pm25)
median(temp$pm25)

#15 minute baseline over time
ggplot(df_pickle, 
       aes(x = timestamp, y = pm25_baseline_15)) + 
  geom_line() + 
  labs(x = "Datetime", y = bquote('15 min baseline PM2.5 concentration (\u03BCg/' ~ m^3 ~ ")"), 
       title = "Baseline PM2.5 concentration in Kigali over study period") +
  theme_bw()

#60 minute baseline over time
ggplot(df_pickle, 
       aes(x = timestamp, y = pm25_baseline_60)) + 
  geom_line() + 
    labs(x = "Datetime", y = bquote('60 min baseline PM2.5 concentration (\u03BCg/' ~ m^3 ~ ")"), 
        title = "1 hr baseline PM2.5 concentration in Kigali over study period") +
  theme_bw()

#1 day minute baseline over time
ggplot(df_pickle, 
       aes(x = timestamp, y = pm25_baseline_1440)) + 
  geom_line() + 
  labs(x = "Datetime", y = bquote('1 day baseline PM2.5 concentration (\u03BCg/' ~ m^3 ~ ")"), 
       title = "Daily baseline PM2.5 concentration in Kigali over study period") +
  theme_bw()

minLng <- 29.9
minLat <- -2.08 
maxLng <- 30.2
maxLat <- -1.9


#function that creates leaflet map showing density of measurements per tile, given subset of df_pickle
create_measurement_density_map <- function(df){
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
  
  #color palette for map
  pal <- colorFactor(c(brewer.pal(9, "Spectral")[9:6], brewer.pal(9, "Spectral")[2]), test$limit)
  

  #leaflet map to show frequency of measurements within rectangles
  leaflet(df) %>%
    addMapPane("Markers", zIndex=590)%>%
    addMapPane("MarkersGeplante", zIndex=585)%>%
    addMapPane("MarkersGeplanteOver", zIndex=999)%>%
    addMapPane("rail", zIndex=570)%>%
    addMapPane("streets", zIndex=560) %>%
    addMapPane("Grenze", zIndex=580) %>%
    #  addProviderTiles(providers$OpenStreetMap.CH, options = providerTileOptions(opacity = 0.9)) %>%
    #  addProviderTiles(providers$Esri.WorldTopoMap) %>%    #names(providers) to see options, but not all available for switzerland
    #  addProviderTiles(providers$CartoDB.Voyager) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    fitBounds(minLng, minLat, maxLng, maxLat) %>%
    addRectangles(data = test, 
                  lng1 = test$min_lon, lng2 = test$max_lon, 
                  lat1 = test$min_lat, lat2 = test$max_lat,
                  opacity = 0,
                  fillOpacity = 1,
                  fillColor = ~pal(test$limit)) %>%
    addLegend(pal = pal, values = ~test$limit, opacity = 1, 
              title = "Quintile limit")
  
}

#function that creates leaflet map showing density of measurements per tile, given subset of df_pickle
create_measurement_density_map_cont <- function(df){
  minLng <- 29.9
  minLat <- -2.08 
  maxLng <- 30.2
  maxLat <- -1.9
  #get counts per tile_id and divide into quantiles
  test <- df %>% 
    group_by(tile_id) %>% 
    dplyr::summarise(max_lon = max(lon), 
                     max_lat = max(lat), 
                     min_lon = min(lon), 
                     min_lat = min(lat), 
                     n_measure = n()) %>%
    mutate(quintile = ntile(n_measure, 5)) %>%
    dplyr::group_by(quintile) %>%
    dplyr::mutate(limit = max(n_measure))
  
  #color palette for map
  # pal <- colorFactor(c(brewer.pal(9, "Spectral")[9:6], brewer.pal(9, "Spectral")[2]), test$limit)
  
  pal <- colorNumeric(brewer.pal(9, "Spectral"), test$n_measure, reverse = TRUE)
  #leaflet map to show frequency of measurements within rectangles
  leaflet(df) %>%
    addMapPane("Markers", zIndex=590)%>%
    addMapPane("MarkersGeplante", zIndex=585)%>%
    addMapPane("MarkersGeplanteOver", zIndex=999)%>%
    addMapPane("rail", zIndex=570)%>%
    addMapPane("streets", zIndex=560) %>%
    addMapPane("Grenze", zIndex=580) %>%
    #  addProviderTiles(providers$OpenStreetMap.CH, options = providerTileOptions(opacity = 0.9)) %>%
    #  addProviderTiles(providers$Esri.WorldTopoMap) %>%    #names(providers) to see options, but not all available for switzerland
    #  addProviderTiles(providers$CartoDB.Voyager) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    fitBounds(minLng, minLat, maxLng, maxLat) %>%
    addRectangles(data = test, 
                  lng1 = test$min_lon, lng2 = test$max_lon, 
                  lat1 = test$min_lat, lat2 = test$max_lat,
                  opacity = 0,
                  fillOpacity = 1,
                  fillColor = ~pal(test$n_measure)) %>%
    addLegend(pal = pal, values = ~test$n_measure, opacity = 1, 
              title = "Quintile limit")
  
}







test <- df_pickle %>% dplyr::group_by(date) %>%
  dplyr::summarise(baseline = median(pm25), 
            fifth_percentile = quantile(pm25, probs = 0.05), 
            ninetyfifth_percentile = quantile(pm25, probs = 0.95))

#plotting baseline, 5th percentile, and 95th percentile
ggplot(test, aes(x = date)) + 
  geom_line(aes(y = baseline, color = "Baseline (daily mean)")) + 
  geom_line(aes(y = fifth_percentile, color = "5th/95th percentile"),  linetype = "dashed") + 
  geom_line(aes(y = ninetyfifth_percentile), color = "grey40", linetype = "dashed") + 
  labs(x = "Date", y = bquote('PM2.5 concentration (\u03BCg/' ~ m^3 ~ ')'), 
       title = "PM2.5 Concentration in Kigali over time") +
  scale_color_manual(values = c("grey40", "black")) +
   theme_bw()




test <- df_cleaned %>% 
  dplyr::group_by(tile_id) %>% 
  dplyr::summarise(max_lon = max(lon), 
            max_lat = max(lat), 
            min_lon = min(lon), 
            min_lat = min(lat), 
            n_measurements = n(),
            med_pm25 = median(pm25_detrended_comb)) %>%
  mutate(quantile = ntile(med_pm25, 10)) %>%
  group_by(quantile) %>%
  mutate(lim = round(max(med_pm25))) %>%
  filter(!is.na(med_pm25))
            

pal <- colorFactor("RdYlGn", domain = test$lim, reverse = TRUE)


#map to show median difference from baseline
leaflet(test) %>%
  addMapPane("Markers", zIndex=590)%>%
  addMapPane("MarkersGeplante", zIndex=585)%>%
  addMapPane("MarkersGeplanteOver", zIndex=999)%>%
  addMapPane("rail", zIndex=570)%>%
  addMapPane("streets", zIndex=560) %>%
  addMapPane("Grenze", zIndex=580) %>%
  #  addProviderTiles(providers$OpenStreetMap.CH, options = providerTileOptions(opacity = 0.9)) %>%
  #  addProviderTiles(providers$Esri.WorldTopoMap) %>%    #names(providers) to see options, but not all available for switzerland
  #  addProviderTiles(providers$CartoDB.Voyager) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addRectangles(data = test, 
                lng1 = test$min_lon, lng2 = test$max_lon, 
                lat1 = test$min_lat, lat2 = test$max_lat,
                opacity = 0,
                fillOpacity = 1,
                fillColor = ~pal(test$lim))  %>%
  addLegend(pal = pal, values = ~test$lim, opacity = 1, 
            title = "Median difference from baseline")

## Note: In theory could read in files from subfolders by having file path point to a larger folder and adding "recursive = TRUE" into the list
#list.files(path = path, pattern = "*.CSV", recursive = TRUE, full.names = TRUE)
#However, doesn't work here because looks like data in different folders is formatted slightly differently. 


