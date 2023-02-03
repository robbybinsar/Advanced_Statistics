library(SDMtune)
library(dismo)
library(zeallot)
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library("sf")
library("terra")
library("geodata")
library("rJava")
library(bnspatial)

?#https://cran.r-project.org/web/packages/SDMtune/vignettes/var-selection.html

#Extrapolation
#[1] maxent_historical_126.tif"                             
#[2] maxent_historical_370.tif"                             
#[3] wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2021-2040_masked.tif"
#[4] wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2041-2060_masked.tif"
#[5] wc2.1_2.5m_bioc_CNRM-CM6-1_ssp370_2021-2040_masked.tif"
#[6] wc2.1_2.5m_bioc_CNRM-CM6-1_ssp370_2041-2060_masked.tif"
# Load environmental variables
  predictors <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/future",pattern = "*.tif$", full.names = TRUE)
  predictors <- brick(predictors[6])
  names(predictors) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8",
                         "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
                         "bio16", "bio17", "bio18", "bio19")
  historical <- raster("C:/Users/robby/Documents/My R/Advanced_Statistics/summerproject/species_modelling/future/maxent_historical_126.tif")
  names(historical) <- "Historical_prediction"
  listraster <- list(predictors, historical)
  predictors <- stack(listraster)
  names(predictors)

#interpolation
predictors <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/historical/20182019",pattern = "*.tif$", full.names = TRUE)
predictors <- predictors[-c(8)]
predictors <- lapply(predictors, raster)
predictors <- stack(predictors)
predictors$Tree_cover  <- extractByMask(predictors$Tree_cover, predictors$Distance_to_Crops, spatial = T)
matr <- c(100, predictors@layers[[8]]@data@max, 0)
rec_matrix <- matrix(matr, ncol = 3)
predictors$Tree_cover <- reclassify(predictors$Tree_cover, rec_matrix)

# Load observation data and Aceh Boundary
obs.data <- readOGR("~/My R/Advanced_Statistics/summerproject/species_modelling/all_dataset_new.shp")
names(obs.data)[2]<- "latitude"
names(obs.data)[3] <- "longitude"

aceh <- st_read("~/My R/Advanced_Statistics/summerproject/BATAS_PROVINSI_ACEH.shp")
colnames(obs.data@coords)[1] <- "longitude"
colnames(obs.data@coords)[2] <- "latitude"

#Determine geographic extent of our data
ext <- extent(predictors)
# Create random points
# Use the bioclim data files for sampling resolution
bil.files <- list.files(path = "~/My R/Advanced_Statistics/summerproject/species_modelling/historical/20182019", 
                        pattern = "*.tif$", 
                        full.names = TRUE)
# We only need one file, so use the first one in the list of .bil files
mask <- raster(bil.files[1])
# Set the seed for the random-number generator to ensure results are similar
set.seed(20210707)
# Randomly sample points (same number as our observed points)
backgr <- randomPoints(mask = predictors,     # Provides resolution of sampling points
                       n = 10000,      # Number of random points
                       ext = ext, # Spatially restricts sampling
                       extf = 1.25)             # Expands sampling a little bit
colnames(backgr) = c('longitude', 'latitude')


# Prepare data

data <- prepareSWD(species = "Elephants", p = obs.data@coords,
                   a = backgr, env = predictors) #categorical = "Slope"


c(train, test) %<-% trainValTest(data, test = 0.2, only_presence = T,
                                 seed = 25)

# Train model
model <- train("Maxent", data = train)

# Train cross validation model
folds <- randomFolds(data, k = 4, only_presence = TRUE, seed = 25)
cv_model <- train("Maxent", data = data, folds = folds)

#Variable importance
model@model@results
vi <- maxentVarImp(model)
vi <- data.frame(vi)
plotVarImp(vi[, 1:2])
plotVarImp(vi[, c(1,3)])

#Permutation importance
maxnet_model <- train("Maxnet", data = train)
vi_maxnet <- varImp(maxnet_model, permut = 5)
vi_maxnet
plotVarImp(vi_maxnet)

# Compute the permutation importance
vi_maxent <- varImp(model, permut = 10)
# Print it
vi_maxent
# Compare with Maxent output
maxentVarImp(model)

#Response curves
plotResponse(model, var = "bio4", type = NULL, only_presence = T,
             marginal = F, rug = T)
plotResponse(cv_model, var = "bio4", type = "cloglog", only_presence = TRUE,
             marginal = TRUE, fun = mean, rug = TRUE)

#Model Report (should be after variable selection and reduction)
library(kableExtra)
library(rasterVis)
modelReport(model, type = "clolog", 
            folder = "maxent_historical_selected_var", 
            test = test,
            response_curves = TRUE, only_presence = TRUE, jk = TRUE,
            env = predictors)

#Data-driven variable selection
set.seed(25)
bg <- dismo::randomPoints(predictors, 10000)
bg <- prepareSWD(species = "Bgs", a = bg, env = predictors)#categorical = "Slope"
plotCor(bg, method = "spearman", cor_th = 0.8)
corVar(bg, method = "spearman", cor_th = 0.8)

#Remove highly correlated variables
selected_variables_model <- varSel(maxnet_model, metric = "auc", test = test,
                                   bg4cor = bg, method = "spearman",
                                   cor_th = 0.7, permut = 1)

selected_variables_model <- varSel(model, metric = "aicc", bg4cor = bg,
                                   method = "spearman", cor_th = 0.8,
                                   env = predictors, use_pc = TRUE, permut = 3)

#Remove variables with low importance
varImp(model, permut = 1)

cat("Testing AUC before: ", auc(model, test = test))
reduced_variables_model <- reduceVar(model, th = 6, metric = "auc",
                                     test = test, permut = 1)
cat("Testing AUC after: ", auc(reduced_variables_model, test = test))
