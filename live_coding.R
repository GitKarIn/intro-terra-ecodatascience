
library(terra)

ras <- rast(xmin = 0, xmax = 10, ymin = 0 , ymax = 10, resolution = 2)

ras

plot(ras)

#why no plot, because no values
values(ras) <- 1:25

ras

inMemory(ras)

setwd("C:/Users/KAlstad/Github/intro-terra-ecodatascience")

sst <- rast("data/gbr_temp_2023_05_31.tif") #load the data

sst #view the properties

plot(sst, colNA = "black")

#load the park boundary
gbr_boundary <- vect("data/gbr_MPA_boundary.gpkg")

#plot the SST data and the boundary on top
plot(sst)


#get the marine park boundary extent and convert it into a SpatVector so we can plot it
gbr_boundary_extent <- ext(gbr_boundary) |>
  vect()
plot(sst)
lines(gbr_boundary)
lines(gbr_boundary_extent, col = "blue")

# Every raster has to be a rectangle!
# Can crop to teh extent of the rectangle
#crop
sst_cropped <- crop(sst, gbr_boundary)

#plot
plot(sst_cropped)
lines(gbr_boundary)

# masking out values outline the marine park
sst_cropped_masked <- mask(sst_cropped, gbr_boundary)

plot(sst_cropped_masked)
# All the values outside cropped area are NA

# both steps at once:
#mask
sst_cropped_masked <- crop(sst_cropped, gbr_boundary, mask = TRUE)

#plot
plot(sst_cropped_masked)
hist(sst_cropped_masked)  # bars represent frequency of particular value
hist(sst_cropped_masked, breaks = 100)
freq(sst_cropped_masked, value = NA)
freq(sst_cropped_masked, digits = 1) |>
  head()

ncell(sst_cropped_masked) # number cells in whole raster

# basic stats
summary(sst_cropped_masked)
global(sst_cropped_masked,"mean") # default not to remove NA
global(sst_cropped_masked,"mean",na.rm = TRUE)
sst_mean <- as.numeric(global(sst_cropped_masked,"mean",na.rm = TRUE))

# classify values!
#less than mean temp is going to be cold; greater than mean is hot
#first we create a matrix that will be used for the classification
# all values >= 0 and less than the mean become 1
# all values greater than the mean become 2
reclass_matrix <- c(0, sst_mean, 1,
                    sst_mean, Inf, 2) |>
  matrix(ncol = 3, byrow = TRUE)

#now we classify our raster using this matrix
sst_reclassed <- classify(sst_cropped_masked, reclass_matrix)

#plot the result
plot(sst_reclassed)

plot(sst_reclassed, col = c("blue", "red"), plg = list(legend = c("Cooler", "Warmer")),
     las = 1) #the las = 1 argument just rotates the y-axis labels so that they are horizontal

#do the conversion
sst_fahrenheit <- (sst_cropped_masked*1.8) + 32

#plot our new raster
plot(sst_fahrenheit)


#load the zones data
zones <- vect("data/gbr_habitat_protection_zones.gpkg")

#take a look
zones

crs(zones, describe = TRUE)
crs(sst_cropped_masked, describe = TRUE)

# EPSG 4326 is for the globe!

sst_cropped_masked_projected <- project(sst_cropped_masked, crs(zones))
summary(sst_cropped_masked)
summary(sst_cropped_masked_projected)
plot(sst_cropped_masked_projected)

# Rasterlayers
sst_monthly <- rast("data/gbr_monthly_temp.tif")

sst_monthly
# dimensions  : 301, 261, 36  (nrow, ncol, nlyr)  ; has 36 layers!
plot(sst_monthly)
plot(sst_monthly[[33:36]])
sst_monthly_fahrenheit <- (sst_monthly * 1.8) + 32

plot(sst_monthly_fahrenheit)

# Zonal statistics

zones
plot(zones)
zones_projected <- project(zones, crs(sst_monthly))
plot(sst_monthly_cropped_masked[[1:4]], fun = function()lines(zones_projected))

#what is the mean temp in each of the zones?
zones_mean_temp <- zonal(sst_monthly, zones_projected)
zones_mean_temp

#transpose the data so that the rows become columns, then make it into a data frame
zones_mean_temp_t <- t(zones_mean_temp) |>
  as.data.frame()

#add date column in proper format - use 1st day of month as the day for all values
zones_mean_temp_t$ym <- gsub("ym_", "",  rownames(zones_mean_temp_t)) |>
  paste0("01") |>
  as.Date(format = "%Y%m%d")

plot(zones_mean_temp_t$ym, zones_mean_temp_t$V1, type = "l",
     ylim = c(min(zones_mean_temp_t$V2), max(zones_mean_temp_t$V1)),
     col = "red",
     xlab = "Date",
     ylab = "Sea surface temperature (Celsius)")
lines(zones_mean_temp_t$ym, zones_mean_temp_t$V2, type = "l", col = "blue")


plot(sst_cropped_masked, col = hcl.colors(50, palette = "RdYlBu", rev = TRUE), las = 1) #see ?hcl.colors for more info on colour palettes. rev= TRUE because we want to reverse the palette: red colours are the highest values and blue the lowest
lines(gbr_boundary)
sbar(d = 400, type = "bar", divs = 2, below = "km") #400km scale bar with 2 divisions and "km" written below


plet(sst_cropped_masked, col = hcl.colors(50, palette = "RdYlBu", rev = TRUE), tiles = "Streets")

# time to learn T-MAP

