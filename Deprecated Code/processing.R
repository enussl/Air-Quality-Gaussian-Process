# (I) Data Preprocessing

################################################################################

# (II) ENVIRONMENT AND PACKAGES

# Empty environment
rm(list = ls())

# Set root node 
setwd("C:/Users/eminu/OneDrive/Desktop/Air-Quality-StatsLab")
source("./Code/helperfuncs.R")

# Load used packages
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)

################################################################################


################################################################################
# Combine the raw data; thanks a lot Nial!

#set path containing all .csv data
#path <- "C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/AQ Data Collection"
week4_path <- "./Data/Rawdata/Week 4- Sensors_ Data-Kigali"
cyclists_path <- "./Data/Rawdata/AQ data-Bicyclists all 4 weeks"

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
df <- read.csv("C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/alldata.csv")
write.csv(df,"C:/Users/nial/OneDrive/Documents/ETH-Zürich-DESKTOP-9E8KBS8/AQ Data Collection/alldata.csv"
          , row.names=FALSE)
fwrite(df, "./Data/Rawdata/alldata_new.csv")

#New df omitting rows where lat, long or date is missing
df2<-df[!(df$Latitude==0 | df$Longitude==0 | df$Date==0),] #Kigali is beneath the equator and far East of the Meridian line
nrow(df2) #8.72 million
df2 <- df2[c("Counter","Latitude","Longitude","Date", "Time", "Millis", "Speed", "PM2.5", "device")] #subset columns
df2 <- distinct(df2)
nrow(df2) #8.04 million
df2$Latitude <- df2$Latitude * (-1)


#Can we now drop measurements where speed = 0?
nrow(df2[df2$Speed==0,]) #4.4 million measurements have speed = 0... could this be possible?

df2_BB <- df2[df2$device=="SPS30_SN:3EB2A712168D6DEF",]

#Creating a dynamic plot to test whether speed is reliable
df2_BB <- cbind(df2_BB, journeyid=NA, color=NA)
colors <- list("dodgerblue1", "green2", "indianred4", "midnightblue", "springgreen2"
               , "pink4", "brown1", "lightskyblue", "yellow1", "slateblue")
j <- 1
for (i in 2:nrow(df2_BB)){
  if (df2_BB$Counter[i] < df2_BB$Counter[i-1]){
    j <- j+1
  }
  df2_BB$journeyid[i] <- j
  df2_BB$color[i] <- colors[[(j %% 10) + 1]]
}

df2_BB <- df2_BB %>%
  mutate(Time_str = sprintf("%06d", Time)) %>%
  mutate(Datetime = make_datetime(year=as.numeric(substr(toString(Date), 1, 4)),
                                  month=as.numeric(substr(toString(Date), 5, 6)),
                                  day=as.numeric(substr(toString(Date), 7, 8)),
                                  hour=as.numeric(substr(Time_str, 1, 2)),
                                  min=as.numeric(substr(Time_str, 3, 4)),
                                  sec=as.numeric(substr(Time_str, 5, 6)))) %>%
  mutate(Time_str=NULL)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(df2_BB$journeyid) #6 is journey with most obs
df2_BB_journey_six <- df2_BB[df2_BB$journeyid==6,] 

#Dynamic plotting
df2_BB_stationary <- df2_BB[df2_BB$Speed==0,]

minLng <- min(df2_BB$Longitude)
maxLng <- max(df2_BB$Longitude)
minLat <- min(df2_BB$Latitude)
maxLat <- max(df2_BB$Latitude)

kig_map <- openmap(c(minLat, minLng), c(maxLat, maxLng), zoom = 10,
                   type = "osm", mergeTiles = TRUE)

kig_map_proj <- openproj(kig_map)

kig_map_plt <- OpenStreetMap::autoplot.OpenStreetMap(kig_map_proj) +
  geom_point(data = df2_BB_journey_six[1:1000,],
             aes(x = Longitude, y = Latitude),
             colour = df2_BB_journey_six$color, size =  2.5) +
  labs(title = 'Time: {frame_time}', x = 'Longitude', y = 'Latitude') + #TODO: add speed subtitle
  transition_time(Datetime) +
  ease_aes('linear')

animate(kig_map_plt, nframes=nrow(df2_BB_journey_six))
anim_save('Kigali_BB_J6_.gif')
# appears that speed may not be reliable... plot the little journey (rows 1 - 21)

################################################################################


# I perform some data processing. Quickly dewscribed what is done here. Niál, maybe add 
# your part in here as well. I quickly did it using your old compiled data (rawdata) and I`ll
# compare it to your new methodology (only using week 4).

# (I) Data processing for the old data that Nial compiled
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
  mutate(datetime = paste(Date_ft, Time_ft),
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
  mutate(trajectory_id = cumsum(time_diff > 30 | is.na(time_diff))) %>% # 30 seconds arbitrary as in if there is no measurement per driver for longer than 30 sec, this is a new traj.
  ungroup() %>%
  group_by(device, trajectory_id) %>% # per driver and trajectory, compute blocks of consecutive 0s in speed and then remove block where more than 120 cons. seconds of 0s
  mutate(
    block_id = cumsum(c(1, diff(Speed != 0) != 0)), # assign block id
    block_length = ifelse(Speed == 0, cumsum(Speed == 0), 0) # length of block id
  ) %>%
  filter(block_length <= 120 | Speed != 0) %>% # all blocks with more than 120 consecutive seconds of 0 speed are filtered out
  ungroup()

# Write to csv
fwrite(data_alldata, file = "./Data/Rawdata/data_cleaned.csv")
data_alldata = fread("./Data/Rawdata/data_cleaned.csv")

# (II) Data processcing for the new data Nial compiled
data_alldata_new = fread("./Data/Rawdata/alldata_new.csv")

data_alldata_new = data_alldata_new %>%
  select(Latitude, Longitude, PM2.5, Date, Time, Millis, device, Speed) %>% # interesting variables
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

# Write to csv
fwrite(data_alldata_new, file = "./Data/Rawdata/data_cleaned_new.csv")
data_alldata_new = fread("./Data/Rawdata/data_cleaned_new.csv")


# (III) Data processcing new (09.04.23; sorry do not have a lot of time today so I`ll do it like this)
data_alldata_final = fread("./Data/Rawdata/alldata_new.csv")

data_alldata_final = data_alldata_final %>%
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

# Write to csv
fwrite(data_alldata_final, file = "./Data/Rawdata/data_cleaned_final.csv")
data_alldata_final = fread("./Data/Rawdata/data_cleaned_final.csv")

#Investigate the differences. Adjust datatypes to use anti_join (see comment later)
data_alldata = data_alldata %>%
  mutate(Date_ft = as.Date(Date_ft))
data_alldata_new = data_alldata_new %>%
  mutate(time_diff = as.integer(time_diff))

# There are some differences but when we only use lat, lon, pm25, datetime and device, we get that almost all observations of data_alldata_new 
# (only using last week; new approach) are within data_alldata and only 199238 observations are not in common, which is exactly 
# nrow(data_alldata)-nrow(data_alldata_new) so there are just 199238 additional observations in data_alldata. If we join on all columns,
# there are 1120972 observations not in common. I suspect the differences that make them not in common are small and maybe due some small differences in 
# processing (see e.g.) difference in data type. So with a little care, we might get that data_alldata_new is a subset of data_alldata even when we do
# join using all columns. Not really necessary though, I suggest we just use the approach where we only use the data from the last week (new approach Nial).
data_antij = anti_join(data_alldata, data_alldata_new, by = c("lat", "lon", "pm25", "datetime", "device"))
data_antij_full = anti_join(data_alldata, data_alldata_new)
