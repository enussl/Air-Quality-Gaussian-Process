# (I) Overall working script Emi

################################################################################

# (II) ENVIRONMENT AND PACKAGES

# Empty environment
rm(list = ls())

# Set root node 
setwd("C:/Users/eminu/OneDrive/Desktop/Air-Quality-StatsLab")
source("./Code/helperfuncs.R")

# Load used packages
library(leaflet)
library(httr)
library(ncdf4) 
library(raster) 
library(dplyr)
library(ggmap)
library(GPfit)
library(GauPro)
library(data.table)
library(fuzzyjoin)
library(lubridate)
library(sp)
library(raster)
library(zoo)
library(snow)
library(ContourFunctions)
library(lattice)
library(conleyreg)

################################################################################

# (III) DIFFERENT POSSIBLE DATA SOURCES

# Read in cleaned data
data = fread("./Data/Rawdata/pickel_data.csv")
remove(data)

# Some useful data sets for exploration (not good for memory, I know)
data.used = data %>%
  select(lat, lon, pm25, gps_datetime, pm25_baseline_15, pm25_baseline_15_removed) %>%
  mutate(gps_datetime = as.Date(gps_datetime))

data.plot = data.used %>%
  select(lat, lon, pm25) %>%
  slice_sample(prop = 0.05)

data.firstday = data.used %>%
  filter(between(gps_datetime, as.Date("2021-09-01"), as.Date("2021-09-01")))  %>%
  select(lat, lon, pm25)

data.firstweek = data.used %>%
  filter(between(gps_datetime, as.Date("2021-09-01"), as.Date("2021-09-07")))  %>%
  select(lat, lon, pm25, gps_datetime)
  
# Do some plotting to look at the trajectories
register_google("AIzaSyCJ6qkhQv12nEuTnuGtUrc9uU8F5foPFCU")

kigali = get_map(location = "kigali", zoom = 12)
kigalimap = ggmap(kigali)
kigalimap = ggmap(kigali, base_layer = ggplot(aes(x = lon, y = lat), data = data.firstweek))
                                              
traj_map = kigalimap +
              geom_point(aes(x = lon, y = lat),
              size = 0.1, alpha = 0.1, data = data.firstweek) +
              xlab("Longitude") +
              ylab("Latitude") +
              ggtitle("All observations within the first week") +
              facet_wrap(~ gps_datetime)
ggsave(filename = "Kigali_firstweek", plot = traj_map, device = "pdf", width = 25, unit = "cm")
  


qmplot(lon, lat, data = data.firstday, darken = .3, source = "stamen", maptype = "watercolor")
################################################################################

################################################################################

# (IV) GET DATA READY (merge to get driver id)

# Select the vars we need
data_pickle = fread("./Data/Rawdata/pickel_data.csv")
data_pickle = data_pickle %>% 
  select(lat, lon, pm25, gps_datetime, pm25_baseline_15, pm25_baseline_15_removed) %>%
  mutate(gps_datetime = as.character(gps_datetime))
  #mutate(gps_datetime = as.POSIXlt(gps_datetime, format = "%Y-%m-%d %H:%M:%S"))


# Data processing
data_alldata = fread("./Data/Rawdata/alldata.csv")

data_alldata = data_alldata %>%
  select(Latitude, Longitude, PM2.5, Date, Time, Millis, device, Speed) %>% # interesting variables
  distinct() %>% # remove duplicates
  rename(lat = Latitude, lon = Longitude, pm25 = PM2.5) %>%
  filter(Date != 0 & Time != 0) %>% # faulty measurements
  filter(lat != 0 & lon != 0) %>% # faulty measurements
  mutate(lat = lat*(-1)) %>% # correct latitude
  mutate(Date_ft = as.Date(as.character(Date), format = "%Y%m%d")) %>% # date stuff
  mutate(Time_ft = format(strptime(sprintf('%06d', Time), format='%H%M%S'), '%H:%M:%S')) %>% # time stuff
  filter(Date_ft >= "2021-09-01" & Date_ft <= "2021-09-20") %>% # september 21 is our sample
  mutate(datetime = paste(Date, Time),
         datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>% # make combined date to compute difference between obs. later
  group_by(device, Date, Time) %>% # pick first observation per driver, Date and Time (which corresponds to second); we dont need intra-second measurements
  slice(1) %>% # could also do summarize and compute the mean but let`s just take the first one; its within one second anyway
  ungroup() %>%
  select(-Millis) %>% # do not need milliseconds therefore
  group_by(Date, Time) %>% # compute active drivers
  mutate(count = n()) %>% # compute active drivers
  ungroup() %>%
  group_by(device) %>% # compute trajectories (drives) by looking at the differences between observations per driver, which sure might be handy later
  arrange(datetime) %>%
  mutate(time_diff = difftime(datetime, lag(datetime), units = "secs")) %>%
  mutate(trajectory_id = cumsum(time_diff > 30 | is.na(time_diff))) %>% # 30 seconds arbitrary as in in there is no measurement per driver for longer than 30 sec, this is a new traj.
  ungroup() %>%
  group_by(device, trajectory_id) %>% # per driver and trajectory, compute blocks of consecutive 0s in speed and then remove block where more than 120 cons. seconds of 0s
  mutate(
    block_id = cumsum(c(1, diff(Speed != 0) != 0)), # assign block id
    block_length = ifelse(Speed == 0, cumsum(Speed == 0), 0) # length of block id
  ) %>%
  filter(block_length <= 120 | Speed != 0) %>% # all blocks with more than 120 consecutive seconds of 0 speed are filtered out
  ungroup()
  
  

  
           
           
  
  
  


  


data_alldata = data_alldata %>%
  distinct() %>% # removes nothing, how is that possible?
  select(Latitude, Longitude, PM2.5, Date, Time, Millis, device, Speed) %>%
  rename(lat = Latitude, lon = Longitude, pm25 = PM2.5) %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) %>%
  filter(nchar(as.character(Time)) == 6) %>%
  mutate(Time = format(strptime(sprintf('%06d', Time), format='%H%M%S'), '%H:%M:%S')) %>%
  mutate(gps_datetime = paste(Date, Time)) %>%
  select(-c(Date, Time, Millis)) 
  #mutate(gps_datetime = as.POSIXlt(gps_datetime, format = "%Y-%m-%d %H:%M:%S"))

# Merge (either fuzzy or exact if it works)
data = stringdist_join(data_pickle, data_alldata, 
                       by = c("lat", "lon", "pm25", "gps_datetime"), 
                       mode = "left", 
                       method = "jw", #use jw distance metric
                       max_dist = 99, 
                       distance_col = "dist")

data = merge(data_pickle, data_alldata, by = c("lat", "lon", "pm25", "gps_datetime"), all = FALSE)
data = merge(data_pickle, data_alldata, by = c("lat", "lon", "pm25"), all = FALSE)

# Get number of active drivers from data directly (remember to compute speed variable)  
data_pickle = data_pickle %>%
  mutate(gps_datetime = as.POSIXct(gps_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
  arrange(gps_datetime) %>%
  mutate(time = as.integer(gps_datetime)) %>%
  group_by(time) %>% 
  mutate(count = n()) %>%
  ungroup()

# Rasterize data. To do so, we need to map the difference in latitude/longitude to meters as that is a more natural 
# way of specifying the area of the squares. See wikipedia: https://en.wikipedia.org/wiki/Geographic_coordinate_system#Latitude_and_longitude. We do 
# not need Haversine as the distances are very small. Add a script to input the helper functions for a good overview.

measure = function(lon1,lat1,lon2,lat2) {
  
  # (lat1, lon1) is the first coordinate
  # (lat2, lon2) is the second coordinate
  
  # Outputs the geodesic distance between (lat1,lon1) and (lat2, lon2).
  R = 6378.137                                
  dLat = (lat2-lat1)*pi/180
  dLon = (lon2-lon1)*pi/180
  a = sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  return (d * 1000)                           
}

# Compute distances north-south and east-west
distance_ns = measure(lon1 = mean(data_pickle$lon), lat1 = max(data_pickle$lat),
                      lon2 = mean(data_pickle$lon), lat2 = min(data_pickle$lat)) #29999m 

distance_ew = measure(lon1 = min(data_pickle$lon), lat1 = mean(data_pickle$lat),
                      lon2 = max(data_pickle$lon), lat2 = mean(data_pickle$lat)) #29871m


# So if we want 20m^2 squares, we now know which resolution to take as we know how big our map is.
r_obj = raster(xmn=min(data_pickle$lon), xmx=max(data_pickle$lon), ymn=min(data_pickle$lat), ymx=max(data_pickle$lat), resolution=c(1/500,1/500))
raster_data = rasterize(x = data_pickle[,c("lon","lat")], # lon-lat data
                        y = r_obj, 
                        field = data_pickle$pm25, 
                        fun = median) 
plot(raster_data)

# Convert to data frame
data = as.data.frame(raster_data, xy = TRUE)

# Apply sampling such that we do not have more than one observation per group of 5 seconds x 
data_pickle = data_pickle %>%
  mutate(five_seconds = cut(gps_datetime, "5 sec")) %>%
  mutate(five_seconds = factor(five_seconds, levels = unique(five_seconds)))






coordinates(data_pickle) = c("lon", "lat")
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(data_pickle) = CRS(wgs84)
spplot(data_pickle, zcol = "pm25", main = "dataset1 plot")

  





  
################################################################################

# (V) RANDOM STUFF

# Kigali in open street map; maybe for later stages
map = leaflet() %>%
  addTiles() %>%  
  addMarkers(lng=30.06667, lat=-1.95, popup="=")
map

# Download air quality from NASA. Might be a good way to alternatively detrend in the
# temporal and spatial dimension instead of the procedure that Peter used.

netrc_path = "/path/to/.netrc"
cookie_path = "/path/to/.urs_cookies"
downloaded_file_path = "/path/to/filename"

# Before using the script
#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
set_config(config(followlocation=1,netrc=1,netrc_file=netrc_path,cookie=cookie_path,cookiefile=cookie_path,cookiejar=cookie_path))
httr::GET(url = "https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/TRMM_3B42RT_Daily.7/2000/03/3B42RT_Daily.20000301.7.nc4",
          write_disk(downloaded_file_path, overwrite = TRUE))

# Air quality data from NASA. Downloaded on personal machine.
# Open data. This is the first hour of the first day of our period.
nc_data = nc_open("MERRA2_400.tavg1_2d_aer_Nx.20211001.nc4")

# Sink meta data into text file if desired to see the structure. There are 50 variables in total excluding
# the dimensions (lon, lat and time). We need to find the correct one that governs the particulate matter at 2.5 scale. 
{
  sink('MERRA2_400.tavg1_2d_aer_Nx.20211001.txt')
  print(nc_data)
  sink()
}

# Restructure to get rid of nc file type
lon = ncvar_get(nc_data, "lon")
lat = ncvar_get(nc_data, "lat", verbose = F)
t = ncvar_get(nc_data, "time") #hours

# I choose ducmass25 to start playing around but this is of course not final by any means. Do the standard
# NA-encoding as they use weird encoding.
pm.array = ncvar_get(nc_data, "DUCMASS25")

fillvalue = ncatt_get(nc_data, "DUCMASS25", "_FillValue")
fillvalue
pm.array[pm.array == fillvalue$value] = NA

# Now we want to slice into the array to get the data for the first hour
pm.slice = pm.array[, , 1] 
dim(pm.slice) # longitude and latitude

# Lets look at that
r = raster(t(pm.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)
writeRaster(r, "pic.tif", "jpeg", overwrite=TRUE)


################################################################################
design_mat = as.data.frame(data.plot[c("lat","lon"),])
resp = as.data.frame(data.plot["pm25",])

fit = gpkm(pm25 ~ lat + lon, data.plot)
################################################################################

# Construct double-de-trending function - not rolling
detrend = function(df, level) {
  
  # data set as pre-processed
  # level: 15 minutes, 1 hour or double de-trending as e.g. "15 min", "60 min", "1 day" or "twoway"
  
  # Output: data set with additional variables of baseline and de-trended pm 2.5 
  
  if (level %in% c("15 min", "60 min", "1 day")) {
    df = df %>%
      mutate(interval = cut(datetime, breaks = level)) %>% 
      group_by(interval) %>%
      mutate(baseline = median(pm25),
             pm25_detrended = pm25 - baseline) %>%
      rename_with(.fn = ~paste(., gsub(" ", "", level), sep = '.'), .cols = c(baseline, pm25_detrended))
    
  } else {
    df = df %>%
      mutate(interval_15 = cut(datetime, breaks = "15 min"),
             interval_day = cut(datetime, breaks = "1 day")) %>% 
      group_by(interval_15) %>%
      mutate(baseline_15 = median(pm25)) %>%
      ungroup() %>%
      group_by(interval_day) %>%
      mutate(baseline_day = median(pm25)) %>%
      ungroup() %>%
      mutate(pm25_detrended = pm25 - 1/2*(baseline_15+baseline_day)) # un-weighted average of the 15 min and daily
  }
}

#__________________ Develop the fitting process of the GP ______________________

# Read in data
data  = fread("./Data/Rawdata/data_cleaned_final.csv")
data = data %>% get_location_characteristics()
data = data %>%
  sample_n(1000)

Y = as.vector(data$pm25_detrended_day)
X = as.matrix(cbind(data$lat,data$lon))

# (I) ____ GauPro _____________________________________________________________

# Fit the GP using all standard setting
k1 = Exponential$new(beta = 0)
k2 = Exponential$new(beta = 0)
kernel_product$new(k1, k2, useC = TRUE)

k = k1 * k2
fit = gpkm(Z = Y, X = X, kernel = k,
           parallel = T, useC = T) # Exponential kernel

GauPro_kernel_model$new(X = X, Z = Y, kernel = k)

# Look at the fit 
fit$summary()

# Plot the estimated surface. Weird looking as of now.
lat = seq(min(data$lat), max(data$lat), l = 50)
lon = seq(min(data$lon), max(data$lon), l = 50)
ggrid = expand.grid(lat, lon)

pred = fit$predict(grid, se.fit = T)$mean
se = fit$predict(grid, se.fit = T)$se

pred_mat = matrix(pred, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape
persp(lat, lon, pred_mat, theta = 33, phi = 15, col = "yellow", shade = TRUE)

# Contour plot included in package
fit$plot2D(se = T)
fit$plotkernel()
fit$plotLOO()
fit$plotmarginalrandom()

# Projected plots (univariate)
plot(fit)

# ______________ Get an idea for how it is looking with n = 1000 _______________

# Read in data
# data  = fread("./Data/Rawdata/data_cleaned_final.csv")
# data = data %>% get_location_characteristics()
data = fread("./Data/Rawdata/data_cleaned_final_roads.csv")
data = data %>% filter(pm25<=500)
fwrite(data, "./Data/Rawdata/data_cleaned_final_roads.csv")

# Grid where we want to predict
lat = seq(min(data$lat), max(data$lat), l = 50)
lon = seq(min(data$lon), max(data$lon), l = 50)
grid = expand.grid(lat, lon)

# Run the GP for 10 different subsets
subsets = 10
z_list = list()
pred_list = list()
se_list = list()

for (i in 1:subsets) {
  
  cat("Iteration: ", i, "\n")
  
  # Run the GP on subsets
  df = data %>% sample_n(1000)
  Y = as.vector(df$pm25_detrended_15)
  X = as.matrix(cbind(df$lat,df$lon))
  fit = gpkm(Z = Y, X = X, kernel = Exponential,
             parallel = T, useC = T) 
  
  # Return fitted value and standard error
  pred = fit$predict(grid, se.fit = T)$mean
  pred_list[[i]] = pred
  se = fit$predict(grid, se.fit = T)$se
  se_list[[i]] = se
  
  # Return z-value
  z_val = hypo_test(fit = fit, nr_locations = 2500, conf_level = 0.05)[[1]]
  z_list[[i]] = z_val
}

# Compute overall mean
mean_z = Reduce("+", z_list) / length(z_list)
levelplot(mean_z)

# Some notes: 0.23, 0.5, 0.49, 0.73, 0.56, 0.28, 0.48, 0.47, 0.3, 0.39 are the max z values
# for these respective subsets (mean = 0.443, sd = 0.14)

# ____________ Add control vars ________________________________________________

df = data %>% sample_n(1000)
Y = as.vector(df$pm25_detrended_15)
X = as.matrix(cbind(df$lat, df$lon, df$industrial, df$major.road, df$res.road))
fit_cont = gpkm(Z = Y, X = X, kernel = Exponential,
           parallel = T, useC = T) 

# Create a df of observations that we use for the final decision. They should be equispaced and
# we need to have knowledge about the roadstuff. The equispace stuff is clear. Now lets add the street knowledge.
# Maybe add higher resolution for the final plots, i.e. maybe l = 500 or something.
lat = seq(min(data$lat), max(data$lat), l = 50)
lon = seq(min(data$lon), max(data$lon), l = 50)
grid = expand.grid(lat, lon)
df_evaluate = data.frame(lat = grid$Var1, lon = grid$Var2)
df_evaluate = df_evaluate %>% get_location_characteristics()

# Return fitted value and standard error
pred_cont = fit_cont$predict(df_evaluate, se.fit = T)$mean
se_cont = fit_cont$predict(df_evaluate, se.fit = T)$se

# Plot
pred_mat = matrix(pred_cont, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape
se_mat = matrix(se_cont, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape

persp(lat, lon, pred_mat, theta = 33, phi = 15, col = "yellow", shade = TRUE)
persp(lat, lon, se_mat, theta = 33, phi = 15, col = "yellow", shade = TRUE)

# Diagnostics: variance even bigger
z_val = (pred-median(fit[["Z"]]))/se
hist(z_val, breaks = 30)

# Plot
pal <- colorNumeric("RdYlGn", domain = grid$pred, reverse = TRUE)
leaflet(grid) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(min(grid$Var2), min(grid$Var1), max(grid$Var2), max(grid$Var1)) %>%
  addCircleMarkers(data = grid, 
                   lng = grid$Var2, 
                   lat = grid$Var1, 
                   radius = 0.001, 
                   opacity = 0.5,
                   color = ~pal(grid$pred)) %>%
  addLegend(pal = pal, values = ~grid$pred, opacity = 1, 
            title = "Gaussian process prediction PM2.5")

# Run the GP for 10 different subsets
subsets = 10
z_list = list()
pred_list = list()
se_list = list()

for (i in 1:subsets) {
  
  cat("Iteration: ", i, "\n")
  
  # Run the GP on subsets
  df = data %>% sample_n(1000)
  Y = as.vector(df$pm25_detrended_15)
  X = as.matrix(cbind(df$lat, df$lon, df$industrial, df$major.road, df$res.road))
  fit = gpkm(Z = Y, X = X, kernel = Exponential,
             parallel = T, useC = T) 
  
  # Return fitted value and standard error
  pred = fit$predict(df_evaluate, se.fit = T)$mean
  pred_list[[i]] = pred
  se = fit$predict(df_evaluate, se.fit = T)$se
  se_list[[i]] = se
  
  # Return z-value
  z_val = hypo_test(fit = fit, nr_locations = 2500, conf_level = 0.05)[[1]]
  z_list[[i]] = z_val
}

# ___________ More obs: n = 5000 _______________________________________________

# With n = 5000, We get a max z_val of 0.8, which is way higher than the mean we get 
# for the 10 cases with n = 1000 (0.443). It is 2.5 standard deviations away from the mean with n = 1000 so I think we can use that as evidence.
load(file = "./Data/Rawdata/n5000_gp.rda")

pred = fit$predict(fit[["X"]], se.fit = T)$mean
se = fit$predict(fit[["X"]], se.fit = T)$se

# Diagnostics
z_val = (pred-median(fit[["Z"]]))/se
hist(z_val, breaks = 30)
plot(pred, fit[["Z"]]-pred)
mean((fit[["Z"]]-pred)^2)/nrow(fit[["X"]]) # much smaller than with the n = 1000 (0.59)
fit$summary()

# ____________ Create X_test for python and in general _________________________

lat = seq(min(data$lat), max(data$lat), l = 200)
lon = seq(min(data$lon), max(data$lon), l = 200)
grid = expand.grid(lat, lon)
df_evaluate = data.frame(lat = grid$Var1, lon = grid$Var2)
df_evaluate = df_evaluate %>% get_location_characteristics()
fwrite(df_evaluate, "./Data/Rawdata/X_test.csv")

# ___________ Create plot for presentation _____________________________________
df = fread("./Data/Rawdata/Z_val_test.csv")
X_test = fread("./Data/Rawdata/X_test.csv")

# Plot
pal = colorNumeric("RdYlGn", domain = df$z_val, reverse = TRUE)
pal = colorNumeric("YlGnBu", domain = df$z_val, reverse = TRUE)

min_lat = -1.993033
max_lat = -1.937483
min_lon = 29.5
max_lon = 30.50

map = leaflet(df) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(29.9, -2.01, 30.2 , -1.99) %>%
  addCircleMarkers(data = df, 
                   lng = df$longitude, 
                   lat = df$latitude, 
                   radius = 0.001, 
                   opacity = 0.5,
                   color = ~pal(df$z_val)) %>%
  addLegend(pal = pal, values = ~df$z_val, opacity = 1, 
            title = "Z Values")
mapshot(map, file = "zval_map.png")


# Create the contour plot
ggplot(df, aes(x = longitude, y = latitude, z = z_val)) + 
  geom_contour(aes(color = ..level..), binwidth = 0.1) +
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Longitude", y = "Latitude", color = "Z Value")


# Set the color palette for the contour lines

my_palette <- colorRampPalette(c("#2C7BB6", "#00A6CA", "#00CCBC", "#90EB9D", "#FFFF8C", "#F9D057", "#F29E2E", "#E76818", "#D7191C"))

# Create the contour plot
cont = ggplot(df, aes(x = longitude, y = latitude, z = z_val)) + 
  geom_contour(aes(color = ..level..), binwidth = 0.75) +
  geom_contour(breaks = 2.575,
               binwidth = 0.1, color = "black", size = 1.2) +
  scale_color_gradientn(colours = my_palette(100)) +
  labs(x = "Longitude", y = "Latitude", color = "Z Value",
       fill = "Z Value") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        legend.position = "bottom")
cont + guides(fill = guide_colorbar(title = "Z Value", nbin = 100, 
                                 barwidth = 10, barheight = 0.5,
                                 ticks = FALSE, draw.ulim = TRUE, 
                                 draw.llim = TRUE, direction = "horizontal",
                                 label.position = "bottom", label.theme = element_text(size = 10)))
cont
ggsave("contour.png", cont, width = 10, height = 8, dpi = 300)


p <- ggplot(df, aes(x = longitude, y = latitude, z = z_val)) +
  geom_contour(aes(z = ..level..), breaks = c(1.96), col = "red", lwd = 2) +
  geom_raster(aes(fill = z_val)) +
  scale_fill_gradientn(colors = my_palette, na.value = "transparent") +
  labs(fill = "Z Value")

# Add legend to ggplot object
p + guides(fill = guide_colorbar(title = "Z Value", nbin = 100, 
                                 barwidth = 10, barheight = 0.5,
                                 ticks = FALSE, draw.ulim = TRUE, 
                                 draw.llim = TRUE, direction = "horizontal",
                                 label.position = "bottom", label.theme = element_text(size = 10)))
p

# Spatial autocorrelation robust standard errors
df_estim = cbind(X_test, df)
fit = conleyreg(z_val ~ industrial + major.road + res.road, lat = "latitude", lon = "longitude",
                data = df_estim, dist_cutoff = 3, model = "ols",  crs = 4326) 
