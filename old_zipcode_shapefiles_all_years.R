# How to create the zip code sf file
# but in reality the zip codes are only changed every 5 years, so we realy are getting a repeated number of obs

# load the library
library(tigris)
library(sf)

# if you have cached data, use it!
options(tigris_use_cache = TRUE)

# make a loop for all the years from 2011 to 2023
zip_sf <- NULL
for(y in 2011:2023){
  if(y < 2015){
  z1 <- zctas(year = 2010)
  if(y >= 2015 & y < 2020){
    z1 <- zctas(year = 2015)
    }
  if(y >= 2020){
    z1 <- zctas(year = 2022)
    }
  z1[,2] <- as.numeric(z1[,2])
  z1$year <- y
  z1 <- z1[c(2,)]
  zip_sf <- rbind(zip_sf,z1)
}

# export the shapefile
location <- "C:/Users/jesus/Box/energy/net_metering/shapefiles/zipcode_polygons.shp"

st_write(zip_sf, location, append = F)