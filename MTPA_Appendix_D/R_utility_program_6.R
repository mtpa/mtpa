# Utilities for Spatial Data Analysis (R)

# user-defined function to convert degrees to radians
# needed for lat.long.distance function
degrees.to.radians <- function(x) { 
  (pi/180)*x
  } # end degrees.to.radians function 

# user-defined function to convert distance between two points in miles
# when the two points (a and b) are defined by longitude and latitude
lat.long.distance <- function(longitude.a,latitude.a,longitude.b,latitude.b) {
  radius.of.earth <- 24872/(2*pi)
  c <- sin((degrees.to.radians(latitude.a) - 
    degrees.to.radians(latitude.b))/2)^2 + 
    cos(degrees.to.radians(latitude.a)) * 
    cos(degrees.to.radians(latitude.b)) * 
    sin((degrees.to.radians(longitude.a) -
    degrees.to.radians(longitude.b))/2)^2
  2 * radius.of.earth * (asin(sqrt(c)))
  } # end lat.long.distance function
  
save(degrees.to.radians,
  lat.long.distance, 
  file = "mtpa_spatial_distance_utilities.R")  
