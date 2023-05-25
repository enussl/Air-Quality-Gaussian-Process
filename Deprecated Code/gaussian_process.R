# (I) Model fitting

################################################################################

# (II) ENVIRONMENT AND PACKAGES

# Empty environment
rm(list = ls())

# Set root node and source helperfunctions
setwd("C:/Users/eminu/OneDrive/Desktop/Air-Quality-StatsLab")
source("./Code/helperfuncs.R")

# Load used packages
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(BayesGPfit)
library(plotly)
library(ggplot2)
library(ggmap)
library(GauPro)
library(FastGP)
library(GPfit)
library(leaflet)
library(leaflet.extras)

################################################################################

# Read in cleaned data and sub-sample for the testing phase. TODO: Include these things
# into the processing step.
data  = fread("./Data/Rawdata/data_cleaned_new.csv")
data = data %>%
  sample_frac(0.01) %>%
  filter(pm25 <= 500) %>% # removes 705 observations in sub-sampled data if we do not remove the baseline first. Quite a lot!
  detrend(level = "1 day") # de-trends data

data = df_cleaned %>% sample_frac(0.1)

Y = as.vector(data$pm25_detrended_comb)
X = as.matrix(cbind(data$lat,data$lon))

# BELOW: TRYING DIFFERENT PACKAGES TO FIT GP. NOT SURE WHICH IS BEST YET. THE BAYESGPFIT
# SURELY NOT HOWEVER, AS WE CANNOT CHANGE THE KERNEL, BUT IT IS FAST SO NICE FOR INITIAL 
# EXPLORATION.

# (I) ____ Bayes GP ____________________________________________________________

# Fit the GP using all standard setting
start_time = Sys.time()
fit = GP.fast.Bayes.fit(y = Y, x = X, b = 1000) # b is smoothness parameter of GP
end_time = Sys.time()
end_time - start_time 
# Around 30 seconds (depending on b) but complexity if not linear in N, remember. Naive 
# approach is O(n^3). This fast thing surely improves but I do not know by how much (what is does). Can look it up 
# though.

# Plot the estimated surface. Weird looking as of now.
lat = seq(min(data$lat), max(data$lat), l = 100)
lon = seq(min(data$lon), max(data$lon), l = 100)
grid = expand.grid(lat, lon)

y_grid = GP.predict(fit, newx = data.frame(x1 = grid[,1], x2 = grid[,2]), CI = FALSE)$f # mean surface
z_grid = matrix(y_grid, nrow = length(lat), ncol = length(lon), byrow = FALSE) # reshape
persp(lat, lon, z_grid, theta = 33, phi = 15, col = "firebrick", shade = TRUE)

grid <- cbind(grid, y_grid)
# Examine the estimated variances is cumbersome for this package, they only report the 95% CI but we ofc. 
# have a 1:1 mapping. ¨
pred = GP.predict(fit, newx = data.frame(x1 = grid[,1], x2 = grid[,2]))
var = ((pred[["lci"]][["f"]]-pred[["mean"]][["f"]])/qnorm(0.975))^2
hist(var, breaks = 10, prob = T)

# Plot the data on the plane to see if that spike might be justified.

kigali = get_map(location = "kigali", zoom = 12)
kigalimap = ggmap(kigali)
kigalimap = ggmap(kigali, base_layer = ggplot(aes(x = lon, y = lat), data = data))

traj_map = kigalimap +
  geom_point(aes(x = lon, y = lat, color = pm25_detrended_comb),
             size = 0.1, alpha = 0.1, data = data) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", color = "PM 2.5")
traj_map


# plotting gaussian process prediction on kigali grid
pal <- colorNumeric("RdYlGn", domain = grid$y_grid, reverse = TRUE)
leaflet(grid) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  fitBounds(minLng, minLat, maxLng, maxLat) %>%
  addCircleMarkers(data = grid, 
             lng = grid$Var2, 
             lat = grid$Var1, 
             radius = 0.001, 
             opacity = 0.5,
             color = ~pal(grid$y_grid)) %>%
  addLegend(pal = pal, values = ~grid$y_grid, opacity = 1, 
            title = "Gaussian process prediction PM2.5")





# Look at the distribution of pm2.5
hist(data$pm25, breaks = 20, prob = T)
lines(density(data$pm25), col = "firebrick", lwd = 1.5)
max(data$pm25)

# (II) ____ GauPro _____________________________________________________________

# Fit the GP using all standard setting
start_time = Sys.time()
fit = gpkm(Z = Y, X = X, kernel = Exponential,
           parallel = T, useC = T) # b is smoothness parameter of GP
end_time = Sys.time()
end_time - start_time 
# Expected run time: 578075986 seconds LOL. Also dont see a way of really making it faster
# other than setting the options useC and parallel to TRUE, which is not sufficient. This is 
# with 10% of the total data, ups.

# (III) ____ FastGP ____________________________________________________________

# Only really a package that contains function that optimize the operations needed to 
# fit a Gaussian process. Could use that, not very user-friendly however.

# (IV) ____ GPfit ______________________________________________________________

X_scaled = cbind(scale_norm(data$lat), scale_norm(data$lon))
fit = GP_fit(X = X_scaled, Y = Y)

