#library(foreach)
#library(doParallel)
library(readr)
library(sf)
library(dplyr)
library(tmap)
library(tmaptools)
library(maptools)
library(RColorBrewer)

aust  <- st_read("/media/DATADRIVE1/Data/Australia_boundary/Aust.shp")

rangelands  <- st_read("/media/DATADRIVE1/Data/Australia_boundary/rangeland_main.shp")


rangelands_eqm_benchmark_sim_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/sitebysite_benchmark_sim_outs_0320_init.rds")
rangelands_eqm_benchmark_sim_993$site <- row.names(rangelands_eqm_benchmark_sim_993)

newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites_update.txt")
newinputs <- newinputs[!(newinputs$site %in% paste0("S", 51:193)),]
inputs <- newinputs[,c("site","x","y","POM.AGG","MAOM","SOM")]

inputs <- inputs[which(inputs$site %in% rangelands_eqm_benchmark_sim_993$site),]

Obs.pools <- inputs

names(Obs.pools)[4:6] <- c("POM.AGG.obs","MAOM.obs","SOM.obs")

Obs.pools[, c(4:6)] <- lapply(Obs.pools[, c(4:6)], function(x){x/100})


df_sf <- st_as_sf(Obs.pools, coords = c("x", "y"), crs = 4326)

st_write(df_sf, "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/map/rangeland_obs_993.shp", driver = "ESRI Shapefile", delete_layer = TRUE)

tmap_mode("plot")
# Basic syntax
obs_sombreaks <- c(0, 10, 20, 
                   30, 40, 60)

obs_SOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(df_sf) +
  tm_dots(col = "SOM.obs",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlGnBu", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = obs_sombreaks,
          title = expression(paste("SOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_compass(size=5,position = c("right", "top"))+
  tm_scale_bar(text.size = 2, breaks = c(0,500,1000), position = c("left", "bottom"))+
  tm_layout(panel.labels=c("Measured SOM"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

obs_SOM_tm

obs_maombreaks <- c(0, 10, 20, 30, 40, 50)

obs_MAOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(df_sf) +
  tm_dots(col = "MAOM.obs",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlOrBr", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = obs_maombreaks,
          title = expression(paste("MAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("Measured MAOM"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

obs_MAOM_tm


obs_pomaggbreaks <- c(0,3,6,9,12,15)

obs_POMAGG_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(df_sf) +
  tm_dots(col = "POM.AGG.obs",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("PuBuGn", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = obs_pomaggbreaks,
          title = expression(paste("POM.AGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("Measured POM.AGG"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

obs_POMAGG_tm
###
rangelands_eqm_benchmark_sim_993 <- rangelands_eqm_benchmark_sim_993[,c("POM1000","LMWC1000","AGG1000","MIC1000","MAOM1000","SOM1000","site")]
names(rangelands_eqm_benchmark_sim_993)[1:6] <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim")
rangelands_eqm_map <- merge(Obs.pools,rangelands_eqm_benchmark_sim_993,by="site")

rangelands_eqm_map[, c(7:12)] <- lapply(rangelands_eqm_map[, c(7:12)], as.numeric)
rangelands_eqm_map[, c(7:12)] <- lapply(rangelands_eqm_map[, c(7:12)], function(x){x/100})

rangelands_eqm_map$POM.AGG.sim  <- with(rangelands_eqm_map,POM.sim+AGG.sim)
rangelands_eqm_map$SOM_diff  <- with(rangelands_eqm_map,SOM.obs-SOM.sim)
rangelands_eqm_map$MAOM_diff <- with(rangelands_eqm_map,MAOM.obs-MAOM.sim)
rangelands_eqm_map$POMAGG_diff <- with(rangelands_eqm_map,POM.AGG.obs-POM.sim-AGG.sim)

eqm_df_sf <- st_as_sf(rangelands_eqm_map, coords = c("x", "y"), crs = 4326)

st_write(eqm_df_sf, "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/map/rangeland_eqm_993.shp", driver = "ESRI Shapefile", delete_layer = TRUE)

aust  <- st_read("/media/DATADRIVE1/Data/Australia_boundary/Aust.shp")

rangelands  <- st_read("/media/DATADRIVE1/Data/Australia_boundary/rangeland_main.shp")

###

tmap_mode("plot")

eqm_sombreaks <- c(0, 10, 20, 
                   30, 40, 60)

eqm_SOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "SOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette =  get_brewer_pal("YlGnBu", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = eqm_sombreaks,
          title = expression(paste("SOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("SOM at equilibrium"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_SOM_tm

eqm_somdiffbreaks <- c(-1.5, -0.5, 0, 1, 1.5)
#eqm_somdiffbreaks <- c(-30, -15, 0, 15, 30)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

eqm_SOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "SOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = eqm_somdiffbreaks,
          title = expression(paste("ΔSOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("ΔSOM (Measured-Equilibrium)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_SOMdiff_tm


eqm_maom_breaks <- c(0, 10, 20, 30, 40, 50)

eqm_MAOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "MAOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlOrBr", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = eqm_maom_breaks,
          title = expression(paste("MAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("MAOC at equilibrium"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_MAOM_tm

eqm_maomdiffbreaks <- c(-1.5, -0.5, 0, 1, 1.5)
#eqm_maomdiffbreaks <- c(-20, -10, 0, 10, 20)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

eqm_MAOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "MAOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = eqm_maomdiffbreaks,
          title = expression(paste("ΔMAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔMAOM (Measured-Equilibrium)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_MAOMdiff_tm

eqm_pomagg_breaks <- c(0,3,6,9,15)

eqm_POMAGG_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "POM.AGG.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("PuBuGn", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = eqm_pomagg_breaks,
          title = expression(paste("POM.AGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("POM.AGG at equilibrium"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_POMAGG_tm

eqm_pomaggdiffbreaks <- c(-1.5, -0.5, 0, 0.5, 1.5)
#eqm_pomaggdiffbreaks <- c(-20, -10, 0, 10, 20)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

eqm_POMAGGdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(eqm_df_sf) +
  tm_dots(col = "POMAGG_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = eqm_pomaggdiffbreaks,
          title = expression(paste("ΔPOMAGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔPOMAGG (Measured-Equilibrium)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

eqm_POMAGGdiff_tm
#
rangelands_steadystate_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/onebatch/LBFGSB_default_rangelands_stode_993.RDS")
#rangelands_odestate_993    <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/rangelands_odestate_993.RDS")

rangelands_steadystate_993[, c(3:8)] <- lapply(rangelands_steadystate_993[, c(3:8)], function(x){x/100})
#rangelands_odestate_993[, c(3:8)]    <- lapply(rangelands_odestate_993[, c(3:8)], function(x){x/100})

names(rangelands_steadystate_993)[3:8] <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim")
#names(rangelands_odestate_993)[3:8]    <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim")

rangelands_steadystate_map <- merge(Obs.pools,rangelands_steadystate_993[,c(1,3:8)],by="site")
#rangelands_odestate_map <- merge(Obs.pools,rangelands_odestate_993[,c(1,3:8)],by="site")

rangelands_steadystate_map$POM.AGG.sim  <- with(rangelands_steadystate_map,POM.sim+AGG.sim)
rangelands_steadystate_map$SOM_diff  <- with(rangelands_steadystate_map,SOM.obs-SOM.sim)
rangelands_steadystate_map$MAOM_diff <- with(rangelands_steadystate_map,MAOM.obs-MAOM.sim)
rangelands_steadystate_map$POMAGG_diff <- with(rangelands_steadystate_map,POM.AGG.obs-POM.AGG.sim)

steadystate_df_sf <- st_as_sf(rangelands_steadystate_map, coords = c("x", "y"), crs = 4326)

st_write(steadystate_df_sf, "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/map/rangeland_steadystate_993.shp", driver = "ESRI Shapefile", delete_layer = TRUE)


#tmap_mode("plot")

steadystate_sombreaks <- c(0, 10, 20, 
                           30, 40, 60)

steadystate_SOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "SOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlGnBu", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = steadystate_sombreaks,
          title = expression(paste("SOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("SOM at steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_SOM_tm

steadystate_somdiffbreaks <- c(-30, -15, 0, 15, 30)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

steadystate_SOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "SOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = steadystate_somdiffbreaks,
          title = expression(paste("ΔSOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("ΔSOM (Measured-SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_SOMdiff_tm

steadystate_maomdiff_breaks <- c(-20, -10, 0, 10, 20)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

steadystate_MAOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "MAOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = steadystate_maomdiff_breaks,
          title = expression(paste("ΔMAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔMAOM (Measured-SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_MAOMdiff_tm

steadystate_maom_breaks <- c(0, 10, 20, 30, 40, 50)

steadystate_MAOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "MAOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlOrBr", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = steadystate_maom_breaks,
          title = expression(paste("MAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("MAOC at steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_MAOM_tm

steadystate_pomaggdiff_breaks <- c(-10, -5, 0, 5, 10)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

steadystate_POMAGGdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "POMAGG_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          #n = 5,  # number of classes
          breaks = steadystate_pomaggdiff_breaks,
          title = expression(paste("ΔPOMAGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔPOMAGG (Measured-SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_POMAGGdiff_tm

steadystate_POMAGG_breaks <- c(0,3,6,9,15)

steadystate_POMAGG_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(steadystate_df_sf) +
  tm_dots(col = "POM.AGG.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("PuBuGn", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = steadystate_POMAGG_breaks,
          title = expression(paste("MAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("MAOC at steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

steadystate_POMAGG_tm

##############################################################
rangelands_cluster_ode_sim_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_driving_993.RDS")

names(rangelands_cluster_ode_sim_993)[4:9] <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim")
rangelands_cluster_map <- merge(Obs.pools,rangelands_cluster_ode_sim_993,by="site")

#rangelands_eqm_map[, c(4:12)] <- lapply(rangelands_eqm_map[, c(4:12)], as.numeric)
rangelands_cluster_map[, c(9:14)] <- lapply(rangelands_cluster_map[, c(9:14)], function(x){x/100})

rangelands_cluster_map$POM.AGG.sim  <- with(rangelands_cluster_map,POM.sim+AGG.sim)
rangelands_cluster_map$SOM_diff  <- with(rangelands_cluster_map,SOM.obs-SOM.sim)
rangelands_cluster_map$MAOM_diff <- with(rangelands_cluster_map,MAOM.obs-MAOM.sim)
rangelands_cluster_map$POMAGG_diff <- with(rangelands_cluster_map,POM.AGG.obs-POM.AGG.sim)

cluster_df_sf <- st_as_sf(rangelands_cluster_map, coords = c("x", "y"), crs = 4326)

st_write(cluster_df_sf, "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/map/rangeland_cluster_stode_992.shp", driver = "ESRI Shapefile", delete_layer = TRUE)

cluster_sombreaks <- c(0, 10, 20, 30, 40, 60)

cluster_SOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "SOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette =  get_brewer_pal("YlGnBu", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = cluster_sombreaks,
          title = expression(paste("SOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("SOM at cluster steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_SOM_tm

#cluster_somdiffbreaks <- c(-20, -10, 0, 10, 20)
cluster_somdiffbreaks <- c(-30, -15, 0, 15, 30)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

cluster_SOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "SOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          breaks = cluster_somdiffbreaks,
          title = expression(paste("ΔSOM (MgC ", ha^-1,")")),
          title.size = 6)+
  #tm_compass(size=2,position = c("right", "top"))+
  #tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(panel.labels=c("ΔSOM (Measured-Cluster_SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_SOMdiff_tm

cluster_maom_breaks <- c(0, 10, 20, 30, 40, 50)

cluster_MAOM_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "MAOM.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("YlOrBr", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = cluster_maom_breaks,
          title = expression(paste("MAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("MAOC at cluster steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_MAOM_tm

#cluster_maomdiff_breaks <- c(-15, -5, 0, 5, 15)
cluster_maomdiff_breaks <- c(-20, -10, 0, 10, 20)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

cluster_MAOMdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "MAOM_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          breaks = cluster_maomdiff_breaks,
          title = expression(paste("ΔMAOM (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔMAOM (Measured-Cluster_SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_MAOMdiff_tm



cluster_pomagg_breaks <- c(0,3,6,9,15)

cluster_POMAGG_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "POM.AGG.sim",
          style = "fixed",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = get_brewer_pal("PuBuGn", n = 5, contrast = c(0, 1)),
          n = 5,  # number of classes
          breaks = cluster_pomagg_breaks,
          title = expression(paste("POMAGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("POMAGG at cluster steadystate"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_POMAGG_tm

cluster_pomaggdiff_breaks <- c(-10, -5, 0, 5, 10)

brewerpal <- c('#b2182b','#d6604d','#f7f7f7','#92c5de','#0571b0')

cluster_POMAGGdiff_tm <- tm_shape(aust) +
  tm_polygons(alpha = 0.2)+
  tm_shape(rangelands) +
  tm_polygons(alpha = 0.1)+
  tm_shape(cluster_df_sf) +
  tm_dots(col = "POMAGG_diff",
          style = "cont",  # or "quantile", "equal", "pretty"
          size = 0.5,
          palette = brewerpal,
          midpoint = 0,
          breaks = cluster_pomaggdiff_breaks,
          title = expression(paste("ΔPOMAGG (MgC ", ha^-1,")")),
          title.size = 6)+
  tm_layout(panel.labels=c("ΔPOMAGG (Measured-Cluster_SteadyState)"),
            panel.label.size = 1.5,         # Increase panel label size
            panel.label.fontface = "bold",  # Make panel labels bold
            legend.position = c("left", "top"),
            legend.title.size = 1.5,        # Increased from 1.5 to 2.5
            legend.text.size = 1.5,         # Increased from 1 to 1.5
            legend.title.fontface = "bold")

cluster_POMAGGdiff_tm


empty_map <- tm_shape(rangelands) + tm_polygons(alpha = 0, border.col = NA, border.alpha = 0) + tm_layout(frame = FALSE)



png(file = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/map/ranglands_comparison_993_newmap_smart.png", bg = "transparent",width = 8000, height=10000,res=300)

# tmap_arrange(obs_SOM_tm, obs_MAOM_tm, empty_map, empty_map,
#              eqm_SOM_tm, eqm_MAOM_tm, eqm_SOMdiff_tm, eqm_MAOMdiff_tm,
#              cluster_SOM_tm,cluster_MAOM_tm, cluster_SOMdiff_tm, cluster_MAOMdiff_tm,
#              steadystate_SOM_tm,steadystate_MAOM_tm,steadystate_SOMdiff_tm, steadystate_MAOMdiff_tm,
#              ncol=4,nrow=4)

tmap_arrange(obs_SOM_tm, obs_MAOM_tm, obs_POMAGG_tm,
             eqm_SOM_tm, cluster_SOM_tm, steadystate_SOM_tm,
             eqm_SOMdiff_tm, cluster_SOMdiff_tm, steadystate_SOMdiff_tm, 
             eqm_MAOM_tm, cluster_MAOM_tm,steadystate_MAOM_tm,
             eqm_MAOMdiff_tm, cluster_MAOMdiff_tm,steadystate_MAOMdiff_tm,
             ncol=3,nrow=5)

tmap_arrange(obs_SOM_tm, obs_MAOM_tm, obs_POMAGG_tm,
             eqm_SOM_tm, cluster_SOM_tm, steadystate_SOM_tm,
             eqm_SOMdiff_tm, cluster_SOMdiff_tm, steadystate_SOMdiff_tm, 
             eqm_MAOMdiff_tm, cluster_MAOMdiff_tm,steadystate_MAOMdiff_tm,
             eqm_POMAGGdiff_tm, cluster_POMAGGdiff_tm,steadystate_POMAGGdiff_tm,
             ncol=3,nrow=5)

tmap_arrange(obs_SOM_tm, obs_MAOM_tm, obs_POMAGG_tm,
             eqm_SOMdiff_tm, eqm_MAOMdiff_tm, eqm_POMAGGdiff_tm,
             cluster_SOMdiff_tm,cluster_MAOMdiff_tm,cluster_POMAGGdiff_tm,
             steadystate_SOMdiff_tm, steadystate_MAOMdiff_tm,steadystate_POMAGGdiff_tm,
             ncol=3,nrow=4)

tmap_arrange(eqm_SOM_tm, cluster_SOM_tm, steadystate_SOM_tm,
             eqm_SOMdiff_tm, cluster_SOMdiff_tm, steadystate_SOMdiff_tm, 
             eqm_MAOMdiff_tm, cluster_MAOMdiff_tm,steadystate_MAOMdiff_tm,
             eqm_POMAGGdiff_tm, cluster_POMAGGdiff_tm,steadystate_POMAGGdiff_tm,
             ncol=3,nrow=4)

dev.off()


#
