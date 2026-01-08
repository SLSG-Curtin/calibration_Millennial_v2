#clusters

cluster_ode_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_driving_993.RDS")

names(cluster_ode_993)[4:9] <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim")
rangelands_cluster_map <- cluster_ode_993[,c(1,4:9,13:15)]
names(rangelands_cluster_map)[8:10] <- c("SOM.obs","MAOM.obs","POM.AGG.obs")
#rangelands_eqm_map[, c(4:12)] <- lapply(rangelands_eqm_map[, c(4:12)], as.numeric)
rangelands_cluster_map[, c(2:10)] <- lapply(rangelands_cluster_map[, c(2:10)], function(x){x/100})



sitebysite_993     <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/sitebysite_benchmark_sim_outs_0320_init.rds")
rangelands_eqm_map <- sitebysite_993[,c(1:9)]
names(rangelands_eqm_map) <- c("SOM.obs","MAOM.obs","POM.AGG.obs","SOM.sim","MAOM.sim","POM.sim","AGG.sim","MIC.sim","LMWC.sim")
rangelands_eqm_map[, c(1:9)] <- lapply(rangelands_eqm_map[, c(1:9)], function(x){x/100})


IQR5 <- function(x)  { quantile(x, 0.05)}
IQR95 <- function(x) { quantile(x, 0.95)}

#1km^2 = 100 ha
#GtC   = MgC / 10^9
#gC/m2 = 0.01 MgC/ha

# som obs --- Gt
mean(rangelands_cluster_map$SOM.obs)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$SOM.obs)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$SOM.obs)*0.75*7673138*100/1000000000

# maom obs --- Gt
mean(rangelands_cluster_map$MAOM.obs)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$MAOM.obs)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$MAOM.obs)*0.75*7673138*100/1000000000

# pom obs --- Gt
mean(rangelands_cluster_map$POM.AGG.obs)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$POM.AGG.obs)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$POM.AGG.obs)*0.75*7673138*100/1000000000

################################################################################
# som cluster --- Gt 
mean(rangelands_cluster_map$SOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$SOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$SOM.sim)*0.75*7673138*100/1000000000


# maom cluster --- Gt 
mean(rangelands_cluster_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$MAOM.sim)*0.75*7673138*100/1000000000


# POM cluster --- Gt 
mean(rangelands_cluster_map$POM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$POM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$POM.sim)*0.75*7673138*100/1000000000

# AGG cluster --- Gt 
mean(rangelands_cluster_map$AGG.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$AGG.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$AGG.sim)*0.75*7673138*100/1000000000

# LMWC cluster --- Gt 
mean(rangelands_cluster_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$LMWC.sim)*0.75*7673138*100/1000000000

# MIC cluster --- Gt 
mean(rangelands_cluster_map$MIC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_cluster_map$MIC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_cluster_map$MIC.sim)*0.75*7673138*100/1000000000

######################

# som eqm --- Gt 
mean(rangelands_eqm_map$SOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$SOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$SOM.sim)*0.75*7673138*100/1000000000

# maom eqm --- Gt 
mean(rangelands_eqm_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$MAOM.sim)*0.75*7673138*100/1000000000

# pom eqm --- Gt 
mean(rangelands_eqm_map$POM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$POM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$POM.sim)*0.75*7673138*100/1000000000

# agg eqm --- Gt 
mean(rangelands_eqm_map$AGG.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$AGG.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$AGG.sim)*0.75*7673138*100/1000000000

# lmwc eqm --- Gt 
mean(rangelands_eqm_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$LMWC.sim)*0.75*7673138*100/1000000000

# mic eqm --- Gt 
mean(rangelands_eqm_map$MIC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_eqm_map$MIC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_eqm_map$MIC.sim)*0.75*7673138*100/1000000000

############################

# som onebatch --- Gt 
mean(rangelands_steadystate_map$SOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$SOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$SOM.sim)*0.75*7673138*100/1000000000

# maom onebatch --- Gt 
mean(rangelands_steadystate_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$MAOM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$MAOM.sim)*0.75*7673138*100/1000000000

# pom onebatch --- Gt 
mean(rangelands_steadystate_map$POM.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$POM.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$POM.sim)*0.75*7673138*100/1000000000

# agg onebatch --- Gt 
mean(rangelands_steadystate_map$AGG.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$AGG.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$AGG.sim)*0.75*7673138*100/1000000000

# lmwc onebatch --- Gt 
mean(rangelands_steadystate_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$LMWC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$LMWC.sim)*0.75*7673138*100/1000000000

# mic onebatch --- Gt 
mean(rangelands_steadystate_map$MIC.sim)*0.75*7673138*100/1000000000
IQR5(rangelands_steadystate_map$MIC.sim)*0.75*7673138*100/1000000000
IQR95(rangelands_steadystate_map$MIC.sim)*0.75*7673138*100/1000000000



