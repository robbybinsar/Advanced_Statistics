# extrapolation
xm_126_2021 <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp126_2021-2040/xm_126_2021.rds")
xm_126_2041 <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp126_2041-2060/xm_126_2041.rds")
xm_370_2021 <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp370_2021-2040/xm_370_2021.rds")
xm_370_2041 <- readRDS("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/CNRM-CM6-1_ssp370_2041-2060/xm_370_2041.rds")
xm <- list(xm_126_2021, xm_126_2041, xm_370_2021, xm_370_2041)
cname <- colnames(xm_126_2021@presence)


for (i in cname) {
  out <- data.frame(matrix(data = rep(0, 200),nrow = 100,ncol = 2))
  colnames(out) <- c("V1", "p")
  for (j in xm){
    gr <- response(j, var = i)
    out <- out + gr
    .GlobalEnv$out <- out
  }
  out <- out/4
  png(paste0("~/My R/Advanced_Statistics/summerproject/species_modelling/results/future/maxent/mean_resp/", i,".png"))
  plot(out, type = "b", lwd = 2, col = "red", xlab = i, 
       ylab = "Predicted Value", ylim = c(0,1), cex.lab = 1.6, cex.axis = 1)
  dev.off()
}
