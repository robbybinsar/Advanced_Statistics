library(sdm)
library(SDMtune)
library(plotROC)
library(maxnet)
library(stringr)

e <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/maxent_eval.rds")
bc.eval <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/bioclim_eval.rds")
ge1 <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/ge1_eval.rds")


par(mfrow = c(1, 3))
plot(bc.eval, "ROC")
plot(ge1 , "ROC")
plot(e, "ROC")


# plotting response variables interpolation

xm <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/maxent/maxent_model.rds")
cname <- colnames(xm@presence)

for (i in cname) {
png(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/maxent/", i,".png"))
  gr <- response(xm, var = i)
plot(gr, type = "b", lwd = 2, col = "red", xlab = str_replace_all(i, "[^[:alnum:]]", " "), 
     ylab = "Predicted Value", ylim = c(0,1), cex.lab = 1.6, cex.axis = 1)
dev.off()
}


# extrapolation
xm <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp370_2021-2040/xm_370_2021.rds")
cname <- colnames(xm@presence)[c(4,14)]


for (i in cname) {
  if (i ==  "bio4") {
    xlabel <- "Temperature Seasonality"
  } else {
    xlabel <- "Precipitation of driest month"
  }
  png(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp370_2021-2040/", i,".png"))
  gr <- response(xm, var = i)
  plot(gr, type = "b", lwd = 2, col = "red", xlab = xlabel, 
       ylab = "Predicted Value", ylim = c(0,1), cex.lab = 1.6, cex.axis = 1)
  dev.off()
}



#axis(side=1, at=seq(0, 25000, by=1000))
#axis(side=2, at=seq(0, 1, by=0.1))


#from the SDM tune models


png(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp370_2021-2040/", i,".png"))
gg <- plotResponse(model, var = "bio4", type = NULL, only_presence = T,
             marginal = F, rug = T)
gg + xlab("dfd") + theme(
  axis.title.x = element_text(color="#993333", size=12, face="bold"),
  axis.title.y = element_text(color="#993333", size=12, face="bold")
)
dev.off()
