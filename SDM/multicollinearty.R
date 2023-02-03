#check correlation personal touch
library(SDMtune)
#saveRDS(predictors, "~/My R/Advanced_Statistics/summerproject/species_modelling/predictors.rds")
#saveRDS(pres_train, "~/My R/Advanced_Statistics/summerproject/species_modelling/pres_train.rds")
#saveRDS(backg_train, "~/My R/Advanced_Statistics/summerproject/species_modelling/backg_train.rds")

#interpolation
predictors <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/predictors.rds")
pres_train <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/pres_train.rds")
backg_train <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/backg_train.rds")
data <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/envtrain.rds")

swd_ob <- prepareSWD("Elephants", predictors, p = pres_train, a = backg_train, categorical = "Slope")
corr <- corVar(swd_ob, method = "pearson", remove_diagonal = T)

#extrapolation
predictors <- list.files("~/My R/Advanced_Statistics/summerproject/species_modelling/future",pattern = "*.tif$", full.names = TRUE)
predictors <- brick(predictors[4])
names(predictors) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8",
                       "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
                       "bio16", "bio17", "bio18", "bio19")

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

# Randomly sample points (same number as our observed points)
set.seed(20210707)
backgr <- randomPoints(mask = predictors,     # Provides resolution of sampling points
                       n = nrow(obs.data),      # Number of random points
                       ext = ext, # Spatially restricts sampling
                       extf = 1.25)             # Expands sampling a little bit
colnames(backgr) = c('longitude', 'latitude')


# Arbitrarily assign group 1 as the testing data group
testing.group <- 1

# Create vector of group memberships
set.seed(0)
group <- kfold(x = obs.data@coords, k = 5) # kfold is in dismo package

# Separate observations into training and testing groups
pres_train <- obs.data@coords[group != testing.group, ]
pres_test <- obs.data@coords[group == testing.group, ]

#create train and test data for absence
# Repeat the process for pseudo-absence points
set.seed(10)
group.background <- kfold(x = backgr, k = 5)
backg_train <- backgr[group.background != testing.group, ]
backg_test <- backgr[group.background == testing.group, ]

#Extract environmental values based on presence and absence train coordinates
train <- rbind(pres_train, backg_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
head(envtrain)
data <- envtrain

#check correlation https://datascienceplus.com/multicollinearity-in-r/
X<-data[,-c(1)]
library(GGally)
ggpairs(X)

library(corpcor)
cor2pcor(cov(X))

gm1 <- glm(pa ~ .-Distance_to_Waterbodies,
                 family = binomial(link = "logit"), data=data)  # with all the independent variables in the dataframe
summary(gm1)

library(mctest)
omcdiag(gm1)
imcdiag(gm1)

library(ppcor)
partial_corr <- pcor(X, method = "pearson")
print(partial_corr)

# https://www.projectpro.io/recipes/check-multicollinearity-r

##Step 1 - Install necessary packages

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)

##Step 2 - Define a Dataframe

data <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/envtrain.rds")

head(data)

##Step 3 - Create a linear regression model

model_all <- glm(pa ~ .-Distance_to_Waterbodies,
                family = binomial(link = "logit"), data=data)  # with all the independent variables in the dataframe

summary(model_all)

##Step 4 - Use the vif() function

vif(model_all)

##Step 5 - Visualize VIF Values

vif_values <- vif(model_all)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation

#After plotting the graph, user can does decide which variable to remove i.e not include in model building and check whether the coreesponding R squared value improves.

##Step 6 - Multicollinearity test can be checked by

data_x <- data[,-c(1,7,8)]                                       # independent variables 

var <- cor(data_x)                                         # independent variables correlation matrix 

var_inv <- ginv(var)                                       # independent variables inverse correlation matrix 

colnames(var_inv) <- colnames(data_x)                      # rename the row names and column names
rownames(var_inv) <- colnames(data_x)

corrplot(var_inv,method='number',is.corr = F)              # visualize the multicollinearity



#colleration

library("ggpubr")
my_data <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/envtrain.rds")
my_data <- my_data[,2:3]
ggscatter(my_data, x = "DEM", y = "Distance_to_Crops", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "DEM (Meter asl)", ylab = "Distance to Crops (Meter)")
