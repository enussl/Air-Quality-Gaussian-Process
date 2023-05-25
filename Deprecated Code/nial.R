#NIAL'S WORKING SCRIPT

#0) GLOBAL PARAMETERS

#Thanks Sanelma ;)
minLng <- 29.9
minLat <- -2.08 
maxLng <- 30.2
maxLat <- -1.9


#1) INSTALL & LOAD PACKAGES
#Ocassionally had to delete & re-install package dependencies in order to forcibly update them to the necessary version

pkgs <- c("sys", "tidyverse", "lubridate", "data.table", "sqldf", "formattable", "writexl"
         , "openair", "languageserver", "httpgd", "grDevices", "ggplot2", "gganimate", "gifski"
         , "OpenStreetMap", "ggrepel", "ggmap", "dplyr", "zoo", "osmdata", "sf", "rlist", "GauPro"
         , "BayesGPfit", "plotly", "ggplot2", "pacman", "slider", "raster", "geosphere", "snow", "vctrs")
install.packages(pkgs=pkgs, dependencies=TRUE)

library(sys, quietly=TRUE)
library(tidyverse, quietly=TRUE)
library(lubridate, quietly=TRUE)
library(data.table, quietly=TRUE)
library(sqldf, quietly=TRUE)
library(formattable, quietly=TRUE)
library(writexl, quietly=TRUE)
library(openair, quietly=TRUE)
library(languageserver, quietly=TRUE)
library(httpgd, quietly=TRUE)
library(grDevices, quietly=TRUE)
library(kernlab, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(gganimate, quietly=TRUE)
library(gifski, quietly=TRUE)
library(OpenStreetMap, quietly=TRUE)
library(ggrepel, quietly=TRUE)
library(ggmap, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(slider, quietly=TRUE)
library(raster, quietly=TRUE)
library(leaflet, quietly=TRUE)
library(RColorBrewer,quietly=TRUE)
library(osmdata, quietly=TRUE)
library(geosphere, quietly=TRUE)
library(crs, quietly=TRUE)
library(sf, quietly=TRUE)
library(rlist, quietly=TRUE)
library(GauPro, quietly=TRUE)
library(snow, quietly=TRUE)
source("Code/helperfuncs.R")



#2) CREATING DYNAMIC PLOT
#A gif showing the trajectory of an individual journey

df <- read.csv("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/data_cleaned_final.csv")

device_freqs <- df %>% count(device) %>% arrange(desc(n)) #SPS30_SN:F6FEAD8A526C0870 has most observations!

df_one_driver <- filter(df, device=="SPS30_SN:F6FEAD8A526C0870") %>%
  slice_tail(n=10000)
 #filter dataframe to one driver - HTM

color_and_categorize <- function(dataframe, colors) {
  journeyid <- cumsum(c(TRUE, diff(dataframe$Date) > 0))
  color <- unlist(colors[(journeyid %% length(colors)) + 1], use.names=FALSE)
  dataframe <- transform(dataframe, journeyid = journeyid, color = color)
  return(dataframe)
}

colors <- list("dodgerblue1", "green2", "indianred4", "midnightblue", "springgreen2"
              , "pink4", "brown1", "lightskyblue", "yellow1", "slateblue")
df_cc <- color_and_categorize(df_one_driver, colors)

#Function for statistical mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
most <- Mode(df2_one_driver$journeyid) #3 is journey with most obs

#Create timestamp column and filter journey 
df_ts <- df_cc %>%
  mutate(Time_str = sprintf("%06d", Time)) %>%
  mutate(Datetime = make_datetime(year=as.numeric(substr(toString(Date), 1, 4)),
                              month=as.numeric(substr(toString(Date), 5, 6)),
                              day=as.numeric(substr(toString(Date), 7, 8)),
                              hour=as.numeric(substr(Time_str, 1, 2)),
                              min=as.numeric(substr(Time_str, 3, 4)),
                              sec=as.numeric(substr(Time_str, 5, 6)))) %>%
  mutate(Time_str=NULL) %>%
  filter(Datetime >= as.Date("2021-09-01") & Datetime <= as.Date("2021-09-30")) %>%
  #filter(journeyid==most) %>% 
  mutate(across(c("pm25"), round, 2)) %>%
  slice_head(n=1000)

#Write this filtered df to a .csv, in case I want to re-load it later
write.csv(df_ts,"C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/TEM_J3.csv"
            , row.names=FALSE)

#Dynamic plotting

minLng <- min(df_ts$lon)
maxLng <- max(df_ts$lon)
minLat <- min(df_ts$lat)
maxLat <- max(df_ts$lat)

kigali <- get_map(c(left = minLng, bottom = minLat, right = maxLng, top = maxLat)
, maptype="toner-lite", source="stamen", zoom = 12)

kig_map_plt <- ggmap(kigali, base_layer = ggplot(df_ts, 
                     aes(x = lon, y = lat))) +
            geom_point(aes(size = 2), colour = df_ts$color) +
            geom_text(aes(label = round(pm25, digits=2)), vjust = 0, hjust = 0, ) +
            labs(title = 'Time: {frame_time}', x = 'Longitude', y = 'Latitude') +
            transition_time(Datetime) +
            ease_aes('linear')

gganimate::animate(kig_map_plt, nframes=1000)
anim_save('Kigali_HTM_J0.gif')



#3) EXPLORING OSMDATA PACKAGE

#Feature: physical element that can be mapped
#Tag: key=value pair to describe a feature

#Relevant features: highway, industrial, junction, motorroad
available_features() 

#Note: available_tags() does not retrieve all available tags
#E.g., available_tags("surface") returns nothing, yet surface features for Kigali can be accessed
available_tags("highway") #Relevant tags: motorway, primary, road, secondary, tertiary
available_tags("industrial") #Relevant tags: depot
available_tags("junction") #Relevant tags: roundabout
available_tags("motorroad") #No tags

#Retrieve a bounding box to restrict our queries to Kigali
#Truncates Eastern part of the city unless format_out="data.frame" is included
kigali_bb<- getbb("Kigali", format_out = "data.frame")

#Retrieve specific Kigali features:
highway_tags <- c(available_tags("highway"))
surface_tags <- c("asphalt", "unpaved", "paved", "ground", "concrete"
                  , "paving_stones", "gravel", "dirt", "grass", "compacted"
                  , "sand", "sett")

kigali_roads <- kigali_bb %>%
  opq() %>%
  add_osm_feature("highway", highway_tags) %>% #nothing for value=motorways
  osmdata_sf()

kigali_surfaces <- kigali_bb %>%
  opq() %>%
  add_osm_feature("surface", surface_tags) %>%
  osmdata_sf()

kigali_roundabouts <- kigali_bb %>%
  opq() %>%
  add_osm_feature("junction", "roundabout") %>%
  osmdata_sf()

#Also tried (industrial, depot), but nothing was returned

#Explore one of these osmdata objects
kigali_roundabouts$osm_points #each roundabout has coordinates and an ID

#Map features
map <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  #addPolylines(data = kigali_roads$osm_lines %>% filter(highway == "primary"), 
               #color = "red", weight = 2) %>%
  #addPolylines(data = kigali_roads$osm_lines %>% filter(!highway == "primary"),
  #             weight = 1, color="blue") %>%
  addMarkers(data=kigali_roads$osm_points %>% filter(!highway == "primary")) %>%
  addPolylines(data = kigali_surfaces$osm_lines %>% filter(!surface == "unpaved"),
               weight = 1, color="chartreuse") %>%
  addPolylines(data = kigali_surfaces$osm_lines %>% filter(surface == "unpaved"),
               weight = 1, color="goldenrod") %>%
  addCircles(data = kigali_roundabouts$osm_points, radius = 3, color = "purple") %>%
  addLegend(colors = c("blue", "chartreuse", "goldenrod", "purple")
          , labels = c("All roads", "Paved surfaces", "Unpaved surfaces", "Roundabouts"))

map

#Next objective: find a way to measure the distance between a point (lat,long) and a vector (i.e., the road)

#Attempt 1: osmdata objects for roads contain an osm_points attribute
#However, there are long gaps along some roads without any points
#So measuring distance between an obs. and the nearest point would not work

#Attempt 2: 
roads_geom <- st_geometry(kigali_roads$osm_lines)
road_1 <- roads_geom[[1]]
first_obs <- df[1,]
pos <-c(first_obs$lon, first_obs$lat)
pos <- st_point(pos)
st_distance(x=roads_geom,y=pos)

dist <- geosphere::dist2Line(p = pos, line = st_coordinates(road_1)[,1:2])

#Next step: add a column 'IS_MAIN_ROAD' to the df for inclusion as a dummy variable in GP
#Let's use only 'Primary' roads for now - all roads would be computationally intensive


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


kigali_prim_rds <- kigali_bb %>%
  opq() %>%
  add_osm_feature("highway", "primary") %>%
  osmdata_sf() #79 roads



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
  #kigali_prim_rds: an osmdata object with osm_lines
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

#Create test set to test new function
test_set <- df[sample(nrow(df), 1000),]
start <- Sys.time()
test_set <- mdtpr_df(test_set, kigali_prim_rds)
end <- Sys.time()
end - start #14.05 secs for 100 rows; #1.7 mins for 1000 rows

#Diagnostic plot: do points identified as 'main road' make sense?
factpal <- colorFactor(c("darkorchid1", "chartreuse"), test_set$is_main_road)

diagnostic_map <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addPolylines(data = kigali_roads$osm_lines %>% filter(highway == "primary"), 
               color = "red", weight = 2) %>%
  addPolylines(data = kigali_roads$osm_lines %>% filter(!highway == "primary"),
               weight = 1, color="blue") %>%
  addCircleMarkers(data=test_set, lng=~lon, lat=~lat, color=~factpal(is_main_road),
  radius= ~ifelse(is_main_road==1,8,4), fillOpacity=0.6) %>%
  addLegend(colors = c("red", "blue")
          , labels = c("Primary roads", "All roads"))

diagnostic_map


#4) GAUSSIAN PROCESS FITTING

#(i) 
#fit GP to 1000 datapoints using Emi's method

# Read in data
# data  = fread("./Data/Rawdata/data_cleaned_final.csv")
# data = data %>% get_location_characteristics()
data = fread("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/data_cleaned_final.csv")

# Grid where we want to predict
lat = seq(min(data$lat), max(data$lat), l = 50)
lon = seq(min(data$lon), max(data$lon), l = 50)
grid = expand.grid(lat, lon)

#Alternative: test_set for prediction
test_set <- data %>% sample_n(1000) %>% dplyr::select(lat,lon)

#Run GP once for one subset
df = data %>% sample_n(5000) #n=2000 took ~10 mins, vs ~1 min for n=1000; n=5000 did not finish overnight
Y = as.vector(df$pm25_detrended_15)
X = as.matrix(cbind(df$lat,df$lon))
fit = gpkm(Z = Y, X = X, kernel = Exponential,
            parallel = T, useC = T)

# Return fitted value and standard error
pred = fit$predict(grid, se.fit = T)$mean
se = fit$predict(grid, se.fit = T)$se #se ~ 35 in all grid points, reflecting sparsity

#Return z value
z_val = hypo_test(fit = fit, nr_locations = 2500, conf_level = 0.05)[[1]]

#Analyse fit
fit$summary()
pred_mat = matrix(pred, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape
persp(lat, lon, pred_mat, theta = 33, phi = 15, col = "mistyrose", shade = TRUE)

#A few pronounced spikes, but no significant z-values

#(ii)
#Fit ten times and bag results

# Run the GP for 10 different subsets
subsets = 10
z_list = list()
pred_list = list()
se_list = list()


for (i in 1:subsets) {
  
  cat("Iteration: ", i, "/n")
  
  # Run the GP on subsets
  df = data %>% sample_n(1000)
  Y = as.vector(df$pm25_detrended_15)
  X = as.matrix(cbind(df$lat,df$lon))
  fit = gpkm(Z = Y, X = X, kernel = Exponential,
             parallel = T, useC = T) 
  
  # Return fitted value and standard error
  pred = fit$predict(test_set, se.fit = T)$mean
  pred_list[[i]] = pred
  se = fit$predict(test_set, se.fit = T)$se
  se_list[[i]] = se
  
  # Return z-value
  z_val = hypo_test(fit = fit, nr_locations = 2500, conf_level = 0.05)[[1]]
  z_list[[i]] = z_val
}

# Compute overall mean and variance
mean_pred = Reduce("+", pred_list) / length(pred_list)
mean_pred

var_list <- lapply(X=se_list, FUN=function(x) x^2)
pred_minus_mean_sq <- lapply(X=pred_list, FUN=function(x) (x-mean_pred)^2)
avg_pmmsq <- Reduce("+", pred_minus_mean_sq) / length(pred_minus_mean_sq)

var_pred <- Reduce("+", var_list) / length(var_list) + avg_pmmsq
se_pred <- sqrt(var_pred)

#Check for significance
median_pm25 <- median(data$pm25_detrended_15) #should be approx the same as the median of the N=10,000 sample
median_pm25

z_stats <- (mean_pred - median_pm25)/se_pred
max(z_stats)

pred_mat = matrix(pred, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape
persp(lat, lon, pred_mat, theta = 33, phi = 15, col = "mistyrose", shade = TRUE)










# X) DISUSED CODE

# ___________________________GAUSSIAN PROCESSES IN R?___________________________

#Create new dfs for testing the GP fit
keeps <- c("lat", "lon")
gp_df_x <- df_pickle_head[,..keeps]
gp_df_y <- df_pickle_head[,"pm25"]

#Fit the GP. This worked instantly with 100 datapoints, but had not finished after 10 minutes with 10,000.
gp_fit <- gausspr(x=gp_df_x, y=gp_df_y, kernel="laplacedot")

#gausspr() estimates model parameters. Which? Lambda, if the kernel is Laplacian?

alpha(gp_fit) # I have no idea what these mean

#Create test data
test_data <- df_pickle[101:150,]
X_test <- test_data[,c("lat","lon")]
Y_test <- test_data[,c("pm25")]

#Predict PM2.5 using our GP fit
Y_pred <- predict(object=gp_fit,newdata=X_test)
Y_pred - Y_test

#Conclusion? 


install.packages("magrittr") #I had problems installing tidyverse until I deleted the folders for these bois...
install.packages("purrr") #and re-installed the packages
install.packages("xfun")
install.packages(pkgs="Rcpp", dependencies=TRUE)

#reading in all data from the folder 
df <- files %>%  map_df(~my_func(.)) 

#create function that reads in data from a file, and adds top row of data as a column called "device"
my_func <- function(file){
  fread(file, skip = 1) %>% #tidyverse pipe operator
    mutate(device = fread(file, nrows = 1)$V4)
}

# __ SPEEDRUN __

start_csv_iter <- Sys.time()
df_iter <- read_csv_iter(test_files) 
end_csv_iter <- Sys.time()
end_csv_iter - start_csv_iter #59.80059 seconds

start_lapply <- Sys.time()
df_lapply <- test_files %>% lapply(read_csv_func) %>% bind_rows
end_lapply <- Sys.time()
end_lapply - start_lapply #7.454074 seconds

read_csv_iter <- function(files){
  df <- read_csv_func(files[[1]])[FALSE,]
  for (file in files){
    df <- rbind(df,read_csv_func(file))
  }
  return(df)
} #ran for 3 hours and returned nothing. I think this would work if left overnight though...

test_files <- list.files(path = test_folder, pattern = "*.CSV", full.names = TRUE, recursive=TRUE)
test_folder <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/Week 4- Sensors_ Data-Kigali/Twayigize Jean Claude-Moto taxi"
test_file <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/AQ data-Bicyclists all 4 weeks/Bernard/DEF0000.CSV"
empty_file <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/AQ data-Bicyclists all 4 weeks/Bernard/DEF0082.csv"

#create list of files to read in (all files)
files <- list.files(path = path, pattern = "*.CSV", full.names = TRUE, recursive=TRUE) 
files <- files[!(files %like% ".xlsx")] #get rid of one random .xlsx file!

#Retrieving Kigali map tiles
kig_map <- openmap(c(minLat, minLng), c(maxLat, maxLng), zoom = 10,
                  type = "osm", mergeTiles = TRUE)

kig_map_proj <- openproj(kig_map)

#Some early data filtering (usurped by Emi's)
df2 <- filter(df, Latitude != 0 & Longitude != 0 & Date != 0) %>%
  select(Counter, Latitude, Longitude, Date, Time, Millis, Speed, `PM2.5`, device) %>%
  distinct() %>%
  mutate(Latitude = -Latitude)

  #2) COMPILING THE ALLDATA.CSV FROM THE WEEK 4 FOLDER

#set path containing all .csv data
#path <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection"
week4_path <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/Week 4- Sensors_ Data-Kigali"
cyclists_path <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/AQ data-Bicyclists all 4 weeks"

#create list of files to read in
week4_files <- list.files(path = week4_path, pattern = "*.CSV", full.names = TRUE, recursive=TRUE)
cyclists_files <- list.files(path=cyclists_path, pattern = "*.CSV", full.names = TRUE, recursive=TRUE)
files <- c(cyclists_files, week4_files)
files <- files[!(files %like% ".xlsx")] #get rid of one random .xlsx file!

#Define custom function for reading in the .csv files
#ignore row 1 + create 'device' column + force column classes 
read_csv_func <- function(file){
    tryCatch(
      csv <- read.csv(file,skip=1, header=TRUE, 
      colClasses=c("integer", "numeric", "numeric", "integer", "numeric",
                   "numeric", "integer", "integer", "integer", "integer",
                   "numeric", "numeric", "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric", "numeric", "numeric",
                   "numeric", "numeric", "character")) %>%
      mutate(device = read.csv(file, header=FALSE, nrows=1)$V4)
      , #if error: print error (generally 'no lines available in input') and ignore file
      error = function(e){
          message(paste("An error occurred with file:",substr(file, nchar(file)-80, nchar(file))))
          message(e)
          message("\nThis file has been ignored")
      })
}

#Apply custom function to all files in the list and time it
start_time <- Sys.time()
df <- files %>% lapply(read_csv_func) %>% bind_rows
end_time <- Sys.time()
end_time - start_time #2.92 minutes

#Analyse the  dataframe
head(df,n=10)
nrow(df) #10.55 million
colnames(df)
df[!complete.cases(df), ] #no NAs at all in the df (?)

#Export the whole df to a .csv file
write.csv(df,"C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/alldata.csv"
            , row.names=FALSE)

#Read in the exported csv again (checkpoint)
df <- read.csv("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/alldata.csv")


#3) INITIAL DATA PROCESSING

df <- data_cleanse(df) #thanks Emi ;)

write.csv(df,"C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/cleandata.csv"
            , row.names=FALSE)

#Checkpoint: get cleaned data (as of 19/04)
df <- read.csv("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/cleandata.csv")


#4) OUTLIER ANALYSIS
#Determining which PM2.5 values are 'outliers' and can safely be filtered out from the data

#Plotting histogram of relative frequencies of PM2.5 values
upper <- 2000
lower <- 500
nbins <- 21

cutoffs <- seq(from=lower, to=upper, length.out=nbins)  #check different cutoffs
cutoff_effect <- vector()
for (c in cutoffs){cutoff_effect <- c(cutoff_effect, nrow(filter(df, pm25 > c)))}

cutoffs_effects <- cbind(cutoffs, cutoff_effect)

potential_outliers <- filter(df, pm25 <= upper & pm25 >= lower)

outliers_hist <- hist(x=potential_outliers$pm25
                      , freq=FALSE
                      , breaks = cutoffs
                      , col ="darkturquoise"
                      , main= paste("PM2.5 measurements between ",lower, " and ", upper)
                      , xlab="PM2.5 level"
                      , xlim=c(lower, upper))

#Plotting map with # observations exceeding 500 PM2.5 in 20m^2 squares

minLng <- min(pm_500_up$lon)
minLat <- min(pm_500_up$lat)
maxLng <- max(pm_500_up$lon)
maxLat <- max(pm_500_up$lat)

kigali <- get_map(c(left = minLng, bottom = minLat, right = maxLng, top = maxLat)
                  , source="stamen", zoom = 10, color="bw")

r_obj = raster(xmn=minLng, xmx=maxLng, ymn=minLat, ymx=maxLat, resolution=c(1/500,1/500))
raster_data = rasterize(x = df[,c("lon","lat")], # lon-lat data
                        y = r_obj, 
                        field = df$pm25, 
                        fun = 'count') 

pm_500_up <- filter(df, pm25 >= lower)

raster_outliers = rasterize(x = pm_500_up[,c("lon","lat")], # lon-lat data
                        y = r_obj, 
                        field = pm_500_up$pm25, 
                        fun = 'count')

raster_prop_outliers <- raster_outliers / raster_data

pal <- colorNumeric("YlOrRd", values(raster_prop_outliers),
  na.color = "transparent")

leaflet() %>% addTiles() %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addRasterImage(raster_prop_outliers, opacity = 1, colors =pal) %>%
  addLegend(values=values(raster_prop_outliers), pal=pal,
  title = "Proportion of PM2.5 readings above 500")


# 6) EXPLORING DIURNAL PATTERNS OF PM2.5
#Results: there is a morning and evening peak in PM2.5

#Load in pickle file
df_pickle <- fread(paste0(path, "/pickel_data.csv"))
df_pickle_head <- fread(paste0(path, "/pickel_data.csv"), nrow = 100)

#Rename datetime column to date - needed for timeVariation()
names(df_pickle)[names(df_pickle) == "gps_datetime"] <- "date"

#Create a plot of diurnal patterns of PM2.5
tvplot <- timeVariation(mydata=df_pickle, pollutant="pm25")

#Save plot as PNG
png("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection/diurnal_plot.png")
tvplot
dev.off()










