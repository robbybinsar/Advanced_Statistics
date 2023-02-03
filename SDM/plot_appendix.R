library(climateStability)

aceh <- st_read("~/My R/Advanced_Statistics/summerproject/BATAS_PROVINSI_ACEH.shp")

px <- raster("~/My R/Advanced_Statistics/summerproject/species_modelling/results/GLM/predicted_GLM.tif")
px <- raster ("~/My R/Advanced_Statistics/summerproject/species_modelling/results/maxent/predicted_maxent.tif")
px <- raster ("~/My R/Advanced_Statistics/summerproject/species_modelling/results/bioclim/Historical_presence_absence.tif")

plot(px, main='Bioclim, raw values', cex.main = 2)
plot(aceh$geometry, add=TRUE, border='dark grey')

lst <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent")

for (i in lst[2:5]) {
  #read data
  px <- raster(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/", i,"/future_predicted_maxent.tif"))
  png(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/", i,".png"))
  plot(px, main=  i, cex.main = 2)
  plot(aceh$geometry, add=TRUE, border='dark grey')
  dev.off()
}
