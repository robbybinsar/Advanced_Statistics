library(sf)
library(sp)
library(adehabitatHR)
library(rgdal)
library(raster)
library(rgeos)
library(terra)
library(amt)

runKDE <- function(filename) {
# Data Preparation
#load the dataset
dataset <- readOGR(paste0("~/My R/Advanced_Statistics/GISandEnvironment/dataset/",filename,".shp"))
fence <- readOGR("~/My R/Advanced_Statistics/GISandEnvironment/fencer11.shp")
ENP <- readOGR("~/My R/Advanced_Statistics/GISandEnvironment/Fence.shp")

#KDE
kernel.ref <- kernelUD(dataset, h = 1550, grid = 700, boundary = fence)

# Contour
contour50 <- getverticeshr(kernel.ref, percent = 50, ida = "kde50")
contour95 <- getverticeshr(kernel.ref, percent = 95, ida = "kde95")
contour99 <- getverticeshr(kernel.ref, percent = 99, ida = "kde99")

output <- rbind(contour50, contour95, contour99, makeUniqueIDs = TRUE)

writeOGR(output, "~/My R/Advanced_Statistics/GISandEnvironment/output",
         layer = paste0("KDE",filename), driver = "ESRI Shapefile")
}




runKDE_new <- function(filename) {
  # Data Preparation
  #load the dataset
  dataset <- read.csv(paste0("~/My R/Advanced_Statistics/GISandEnvironment/dataset/",filename,".csv"))
  datanew <- dataset[c(8,4,5)]
  names(datanew)[c(1,2,3)] <- c("id","x","y")
  coordinates(datanew) <- c("x", "y")
  proj4string(datanew) <- CRS( "+proj=longlat +datum=WGS84 +no_defs" )
  mydatasppr <- spTransform(datanew, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs")
  
  #read fence and ENP boundary
  fence <- readOGR("~/My R/Advanced_Statistics/GISandEnvironment/fencer11.shp")
  ENP <- readOGR("~/My R/Advanced_Statistics/GISandEnvironment/Fence.shp")
  
  #KDE
  kernel.ref <- kernelUD(mydatasppr, h = 1550, grid = 700, boundary = fence)
  
  # Contour
  contour50 <- getverticeshr(kernel.ref, percent = 50, unout = "km2")
  contour50@data["contour"] <- 50
  contour95 <- getverticeshr(kernel.ref, percent = 95, unout = "km2")
  contour95@data["contour"] <- 95
  contour99 <- getverticeshr(kernel.ref, percent = 99, unout = "km2")
  contour99@data["contour"] <- 99
  
  output <- rbind(contour50, contour95, contour99, makeUniqueIDs = TRUE)
  
  write.csv(output@data, paste0("~/My R/Advanced_Statistics/GISandEnvironment/output/",filename, "KDEtable.csv"))
  writeOGR(output, "~/My R/Advanced_Statistics/GISandEnvironment/output",
           layer = paste0("KDE_tag",filename), driver = "ESRI Shapefile")
}
