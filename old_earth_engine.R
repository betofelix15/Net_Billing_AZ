# collect data from earth engine in R

install.packages("rgee")
library(rgee)

# install python using reticulate
install.packages("reticulate")
library(reticulate)

# check if python is installed
reticulate::py_available()

# install python
reticulate::py_discover_config()

# we need the following below for the environment:
#  C:/Users/jesus/anaconda3/python.exe
# this was found in the python: ........ 

# install and set python environment
rgee::ee_install_set_pyenv(
  py_path =  "C:/Users/jesus/anaconda3/python.exe",
  py_env = "rgee"
)


# check if you have everything you need
library(rgee)
ee_check()
ee_install_upgrade()

# initialize earth engine connection
ee_Initialize(
  user = "j.felix@usu.edu"
)

# using a dataset

# libraries you need
library(sf)
library(giscoR)

mex_sf <- gisco_get_countries(country = "Mexico")
us_sf <- gisco_get_countries(country = "United States of America")
plot(mex_sf$geometry)
plot(us_sf$geometry)

mex_buff <- st_buffer(mex_sf,50000) # 50 kilometers

# visualize the buffer
library(ggplot2)
library(dplyr)
ggplot() + 
  geom_sf(data = mex_buff$geometry, color = "darkred") +
  geom_sf(data = mex_sf$geometry, color = "black") +
  theme_minimal()

plot(mex_buff$geometry)

# coerce mex_buff into earth engine object
mex_bounds <- ee$Geometry$Polygon(list(mex_buff$geometry))

# get sea surface temperature
sst_data <- ee$ImageCollection('NOAA/CDR/OISST/V2_1')$select("sst")

sst_data <- ee$FeatureCollection("sst")


