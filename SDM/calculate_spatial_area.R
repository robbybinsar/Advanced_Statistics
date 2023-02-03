library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library("sf")
library("terra")
library("geodata")
library(bnspatial)
library(dplyr)

lst <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/regression")
df_output <- data.frame(matrix(nrow = 0,ncol = 0))

for (i in lst) {
#read data
input <- raster(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/regression/", i,"/future_predicted_GLM_new1_spec_sens.tif"))
mask_data_pa <- st_read("C:/Users/robby/Documents/MSc Biodiversity and Conservation/summer project/summer_project/BPKH/SK580_AR_250K_ACEH_pr_cons_area.shp")
mask_data_pa <- st_zm(mask_data_pa)
mask_data_pockets <- st_read("C:/Users/robby/Documents/MSc Biodiversity and Conservation/summer project/summer_project/BKSDA/pockets.shp")
mask_data_pockets <- st_zm(mask_data_pockets)
#extract by mask
extr_pa <- mask(input, mask_data_pa)
extr_po <- mask(input, mask_data_pockets)
#calculate area
t <- (sum(!is.na(values(input))))*(prod(res(input))/1000000)
#pa
total_area_pa <- (sum(!is.na(values(extr_pa))))*(prod(res(extr_pa))/1000000)
#po
total_area_po <- (sum(!is.na(values(extr_po))))*(prod(res(extr_po))/1000000)

output <- data.frame(model = i, areakm2 = t,areakm2_pa = total_area_pa,areakm2_po = total_area_po, 
                     percentage_pa = total_area_pa/t*100, percentage_po = total_area_po/t*100)
df_output <- bind_rows(df_output, output)
.GlobalEnv$df_output <- df_output
}

# menghitung area historical predicted dalam ulu masen dan leuser
df_output <- data.frame(matrix(nrow = 0,ncol = 0))

  #read data
  input <- raster("~/My R/Advanced_Statistics/summerproject/species_modelling/results/maxent/predicted_maxent_spec_sens.tif")
  mask_data_ulu_leu <- st_read("~/MSc Biodiversity and Conservation/summer project/summer_project/leuser/Leuser_and_Ulu_Masen.shp")
  mask_data_ulu_leu <- st_zm(mask_data_ulu_leu)
  #extract by mask
  extr_ulu_leu <- mask(input, mask_data_ulu_leu)
  #calculate area
  t <- (sum(!is.na(values(input))))*(prod(res(input))/1000000)
  #Ulu_leuser
  total_area <- (sum(!is.na(values(extr_ulu_leu))))*(prod(res(extr_ulu_leu))/1000000)
  
  output <- data.frame(areakm2 = t,areakm2_ulu_leuser = total_area, 
                       percentage = total_area/t*100)
  df_output <- bind_rows(df_output, output)
  .GlobalEnv$df_output <- df_output


# menghitung area future predicted dalam ulu masen dan leuser
lst <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent")
lst <- lst[c(3,5,7,9)]
df_output <- data.frame(matrix(nrow = 0,ncol = 0))

for (i in lst) {
  #read data
  input <- raster(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/",i,"/future_predicted_maxent_spec_sens.tif"))
  mask_data_ulu_leu <- st_read("~/MSc Biodiversity and Conservation/summer project/summer_project/leuser/Leuser_and_Ulu_Masen.shp")
  mask_data_ulu_leu <- st_zm(mask_data_ulu_leu)
  #extract by mask
  extr_ulu_leu <- mask(input, mask_data_ulu_leu)
  #calculate area
  t <- (sum(!is.na(values(input))))*(prod(res(input))/1000000)
  #Ulu_leuser
  total_area <- (sum(!is.na(values(extr_ulu_leu))))*(prod(res(extr_ulu_leu))/1000000)
  
  output <- data.frame(model = i, areakm2 = t,areakm2_ulu_leuser = total_area, 
                       percentage = total_area/t*100)
  df_output <- bind_rows(df_output, output)
  .GlobalEnv$df_output <- df_output
}

# menghitung area predicted habitat suitability in  PA in  ulu masen dan leuser
df_output <- data.frame(matrix(nrow = 0,ncol = 0))

#read data
input <- raster("~/MSc Biodiversity and Conservation/summer project/summer_project/RESULTS/SDM/interpolation/MAXENT/maxent_within_SK.tif")
mask_data_ulu_leu <- st_read("~/MSc Biodiversity and Conservation/summer project/summer_project/leuser/Leuser_and_Ulu_Masen.shp")
mask_data_ulu_leu <- st_zm(mask_data_ulu_leu)
#extract by mask
extr_ulu_leu <- mask(input, mask_data_ulu_leu)
#calculate area
t <- (sum(!is.na(values(input))))*(prod(res(input))/1000000)
#Ulu_leuser
total_area <- (sum(!is.na(values(extr_ulu_leu))))*(prod(res(extr_ulu_leu))/1000000)

output <- data.frame(areakm2 = t,areakm2_ulu_leuser = total_area, 
                     percentage = total_area/t*100)
df_output <- bind_rows(df_output, output)
.GlobalEnv$df_output <- df_output
