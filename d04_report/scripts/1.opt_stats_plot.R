library(ggplot2)
library(Cairo)
library(tibble)
library(jsonlite)
library(ggtext)
library(patchwork)
library(reshape2)
library(ggpubr)
library(tools)
library(gridExtra)
#import Obs.pools first
newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites_update.txt")
flist <- list.files("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/rangeland_driving/",full.names = TRUE,recursive = TRUE)
basefile_names <- basename(flist)
isite <- file_path_sans_ext(basefile_names)
Obs.pools <- newinputs[which(newinputs$site %in% isite),]
Obs.pools <- Obs.pools[,c("site","SOM","MAOM","POM.AGG")]
Obs.pools$id <- 1:nrow(Obs.pools)

names(Obs.pools)[2:4] <- c("TOC.obs","MAOM.obs","POM.AGG.obs")

onebatch.pools.sim = readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/onebatch/LBFGSB_default_rangelands_stode_993.RDS")
rownames(onebatch.pools.sim) <- onebatch.pools.sim$site

colnames(onebatch.pools.sim)[3:8] <- c(paste0(colnames(onebatch.pools.sim)[3:8], ".sim"))

onebatch.pools.sim <- merge(Obs.pools,onebatch.pools.sim,by="site")

onebatch.pools.sim$"POM.AGG.sim" <- with(onebatch.pools.sim,POM.sim+LMWC.sim+AGG.sim+MIC.sim)

names(onebatch.pools.sim)[12] <- "TOC.sim"

targets_3frac <- c("POM.AGG", "MAOM", "TOC")


fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- onebatch.pools.sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_a1 <- fracs_df3

pltdf_a1$target <- factor(
  pltdf_a1$target,
  levels = c("POM.AGG", "MAOM", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


line_df_a1 <- NULL
#print(unique(pltdf_b$target))
for (target in unique(pltdf_a1$target)) {
  print(target)
  tar_df <- pltdf_a1[pltdf_a1$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  tar_max <- max(tar_df) * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_a1 <- rbind(line_df_a1, line_rec)
}

line_df_a1$target <- factor(
  line_df_a1$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

#SOM statistic
fit.TOC <- summary(lm(onebatch.pools.sim$TOC.obs ~ onebatch.pools.sim$TOC.sim))
TOC.bias <- onebatch.pools.sim$TOC.obs/100 - onebatch.pools.sim$TOC.sim/100
dim.TOC <- 993
RMSE_TOC <- sqrt(sum((onebatch.pools.sim$TOC.obs/100- onebatch.pools.sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC <- mean(TOC.bias, na.rm=T)
MAE_TOC <- mean(abs(TOC.bias), na.rm=T)

#R square 0.19, RMSE of 7.99, MAE of 5.82

#MAOM statistic
fit.MAOM <- summary(lm(onebatch.pools.sim$MAOM.obs ~ onebatch.pools.sim$MAOM.sim))
MAOM.bias <- onebatch.pools.sim$MAOM.obs/100 - onebatch.pools.sim$MAOM.sim/100
dim.MAOM <- 993
RMSE_MAOM <- sqrt(sum((onebatch.pools.sim$MAOM.obs/100- onebatch.pools.sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM <- mean(MAOM.bias, na.rm=T)
MAE_MAOM <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.18, RMSE of 6.81, MAE of 5.16

#POM.AGG statistic
fit.POM.AGG <- summary(lm(onebatch.pools.sim$POM.AGG.obs ~ onebatch.pools.sim$POM.AGG.sim))
POM.AGG.bias <- onebatch.pools.sim$POM.AGG.obs/100 - onebatch.pools.sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((onebatch.pools.sim$POM.AGG.obs/100- onebatch.pools.sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.11, RMSE of 2.94, MAE of 2.35

rho_toc     <- cor(onebatch.pools.sim$TOC.obs, onebatch.pools.sim$TOC.sim, method = "pearson")
rho_maom    <- cor(onebatch.pools.sim$MAOM.obs, onebatch.pools.sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(onebatch.pools.sim$POM.AGG.obs, onebatch.pools.sim$POM.AGG.sim, method = "pearson")

#0.44, 0.43, 0.34

text_df_a1 <- data.frame(
  target = c("POC", "MAOC", "TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(19, 38, 50),
  text = c(
    "RMSE = 2.94<br>MAE = 2.35<br>&rho;<sub>c</sub> = 0.34",
    "RMSE = 6.81<br>MAE = 5.16<br>&rho;<sub>c</sub> = 0.43",
    "RMSE = 7.99<br>MAE = 5.82<br>&rho;<sub>c</sub> = 0.44"
  )
)

text_df_a1$target <- factor(
  text_df_a1$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


p_a1 <- ggplot() +
  geom_point(
    data = pltdf_a1,
    mapping = aes(x = x, y = y)
  ) +
  geom_line(
    data = line_df_a1,
    mapping = aes(x = x, y = y), colour = "grey40"
  ) +
  geom_richtext(
    data = text_df_a1,
    mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
    label.color = NA
  ) +
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Global calibration with default parameters, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )

p_a1
#-------------------------------------------------------------------------------
################################################################################
#cluster optim
sim_all <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_stode_993.RDS")
clusters_optim_sim <- sim_all
rownames(clusters_optim_sim) <- clusters_optim_sim$site
clusters_optim_sim <- clusters_optim_sim[,c(8,9,10,13,14,15)]

names(clusters_optim_sim) <- c("MAOM.sim","TOC.sim","POM.AGG.sim","TOC.obs","MAOM.obs","POM.AGG.obs")

fit.TOC  <- summary(lm(clusters_optim_sim$TOC.obs ~ clusters_optim_sim$TOC.sim))
TOC.bias <- clusters_optim_sim$TOC.obs/100 - clusters_optim_sim$TOC.sim/100
dim.TOC  <- 993
RMSE_TOC <- sqrt(sum((clusters_optim_sim$TOC.obs/100- clusters_optim_sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC  <- mean(TOC.bias, na.rm=T)
MAE_TOC  <- mean(abs(TOC.bias), na.rm=T)

#R square 0.3688, RMSE of 6.15, MAE of 4.20

#MAOM statistic
fit.MAOM  <- summary(lm(clusters_optim_sim$MAOM.obs ~ clusters_optim_sim$MAOM.sim))
MAOM.bias <- clusters_optim_sim$MAOM.obs/100 - clusters_optim_sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((clusters_optim_sim$MAOM.obs/100 - clusters_optim_sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.3538, RMSE of 5.15, MAE of 3.63

#POM.AGG statistic
fit.POM.AGG <- summary(lm(clusters_optim_sim$POM.AGG.obs ~ clusters_optim_sim$POM.AGG.sim))
POM.AGG.bias <- clusters_optim_sim$POM.AGG.obs/100 - clusters_optim_sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((clusters_optim_sim$POM.AGG.obs/100- clusters_optim_sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.1623, RMSE of 2.23, MAE of 1.69

rho_toc     <- cor(clusters_optim_sim$TOC.obs, clusters_optim_sim$TOC.sim, method = "pearson")
rho_maom    <- cor(clusters_optim_sim$MAOM.obs, clusters_optim_sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(clusters_optim_sim$POM.AGG.obs, clusters_optim_sim$POM.AGG.sim, method = "pearson")


text_df_a <- data.frame(
  target = c("POC", "MAOC","TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.23<br>MAE = 1.69<br>&rho;<sub>c</sub> = 0.40",
    "RMSE = 5.15<br>MAE = 3.63<br>&rho;<sub>c</sub> = 0.60",
    "RMSE = 6.15<br>MAE = 4.20<br>&rho;<sub>c</sub> = 0.61"
  )
)

text_df_a$target <- factor(
  text_df_a$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


targets_3frac <- c("POM.AGG", "MAOM","TOC")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- clusters_optim_sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_a <- fracs_df3

pltdf_a$target <- factor(
  pltdf_a$target,
  levels = c("POM.AGG", "MAOM", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


line_df_a <- NULL
tar_max_values <- c("POC" = 25, "MAOC" = 50, "TOC" = 60)
#print(unique(pltdf_b$target))
for (target in unique(pltdf_a$target)) {
  print(target)
  tar_df <- pltdf_a[pltdf_a$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  #tar_max <- max(tar_df) * 1.05
  tar_max <- tar_max_values[target] * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_a <- rbind(line_df_a, line_rec)
}

line_df_a$target <- factor(
  line_df_a$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

p_a <- ggplot() +
  geom_point(
    data = pltdf_a,
    mapping = aes(x = x, y = y)
  ) +
  geom_line(
    data = line_df_a,
    mapping = aes(x = x, y = y), colour = "grey40"
  ) +
  geom_richtext(
    data = text_df_a,
    mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
    label.color = NA
  ) +
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Bioregional calibration with optimised parameters, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_a


################################################################################
#cluster
lbfgsb_opt_clusters.sim <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_stode_993.RDS")
rownames(lbfgsb_opt_clusters.sim) <- lbfgsb_opt_clusters.sim$site
lbfgsb_opt_clusters.sim <- lbfgsb_opt_clusters.sim[,c(8,9,10,13,14,15)]
names(lbfgsb_opt_clusters.sim) <- c("MAOM.sim","TOC.sim","POM.AGG.sim","TOC.obs","MAOM.obs","POM.AGG.obs")

#names(lbfgsb_opt_clusters.sim)[8]  <- c("TOC.sim")
#names(lbfgsb_opt_clusters.sim)[12] <- c("TOC.obs")

fit.TOC  <- summary(lm(lbfgsb_opt_clusters.sim$TOC.obs ~ lbfgsb_opt_clusters.sim$TOC.sim))
TOC.bias <- lbfgsb_opt_clusters.sim$TOC.obs/100 - lbfgsb_opt_clusters.sim$TOC.sim/100
dim.TOC  <- 993
RMSE_TOC <- sqrt(sum((lbfgsb_opt_clusters.sim$TOC.obs/100- lbfgsb_opt_clusters.sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC  <- mean(TOC.bias, na.rm=T)
MAE_TOC  <- mean(abs(TOC.bias), na.rm=T)

#R square 0.37, RMSE of 6.15, MAE of 4.20

#MAOM statistic
fit.MAOM  <- summary(lm(lbfgsb_opt_clusters.sim$MAOM.obs ~ lbfgsb_opt_clusters.sim$MAOM.sim))
MAOM.bias <- lbfgsb_opt_clusters.sim$MAOM.obs/100 - lbfgsb_opt_clusters.sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((lbfgsb_opt_clusters.sim$MAOM.obs/100 - lbfgsb_opt_clusters.sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.35, RMSE of 5.15, MAE of 3.63

#POM.AGG statistic
fit.POM.AGG <- summary(lm(lbfgsb_opt_clusters.sim$POM.AGG.obs ~ lbfgsb_opt_clusters.sim$POM.AGG.sim))
POM.AGG.bias <- lbfgsb_opt_clusters.sim$POM.AGG.obs/100 - lbfgsb_opt_clusters.sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((lbfgsb_opt_clusters.sim$POM.AGG.obs/100- lbfgsb_opt_clusters.sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.16, RMSE of 2.23, MAE of 1.69

rho_toc     <- cor(lbfgsb_opt_clusters.sim$TOC.obs, lbfgsb_opt_clusters.sim$TOC.sim, method = "pearson")
rho_maom    <- cor(lbfgsb_opt_clusters.sim$MAOM.obs, lbfgsb_opt_clusters.sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(lbfgsb_opt_clusters.sim$POM.AGG.obs, lbfgsb_opt_clusters.sim$POM.AGG.sim, method = "pearson")

#0.61, 0.60, 0.40

text_df_b <- data.frame(
  target = c("POC", "MAOC", "TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.23<br>MAE = 1.69<br>&rho;<sub>c</sub> = 0.40",
    "RMSE = 5.15<br>MAE = 3.63<br>&rho;<sub>c</sub> = 0.60",
    "RMSE = 6.15<br>MAE = 4.20<br>&rho;<sub>c</sub> = 0.61"
  )
)

text_df_b$target <- factor(
  text_df_b$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


targets_3frac <- c("POM.AGG", "MAOM", "TOC")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- lbfgsb_opt_clusters.sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_b <- fracs_df3

pltdf_b$target <- factor(
  pltdf_b$target,
  levels = c("POM.AGG", "MAOM", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


line_df_b <- NULL
tar_max_values <- c("POC" = 25, "MAOC" = 50, "TOC" = 60)
#print(unique(pltdf_b$target))
for (target in unique(pltdf_b$target)) {
  print(target)
  tar_df <- pltdf_b[pltdf_b$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  #tar_max <- max(tar_df) * 1.05
  tar_max <- tar_max_values[target] * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_b <- rbind(line_df_b, line_rec)
}

line_df_b$target <- factor(
  line_df_b$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

p_b <- ggplot() +
  geom_point(
    data = pltdf_b,
    mapping = aes(x = x, y = y)
  ) +
  geom_line(
    data = line_df_b,
    mapping = aes(x = x, y = y), colour = "grey40"
  ) +
  geom_richtext(
    data = text_df_b,
    mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
    label.color = NA
  ) +
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Bioregional calibration with optimised parameters, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_b


################################################################################

#cluster
#lbfgsb_opt_clusters.sim <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_rangelands_clusterstode_sim_993.RDS")
lbfgsb_opt_init_clusters.sim <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_993.RDS")
rownames(lbfgsb_opt_init_clusters.sim) <- lbfgsb_opt_init_clusters.sim$site
lbfgsb_opt_init_clusters.sim <- lbfgsb_opt_init_clusters.sim[,c(8,9,10,13,14,15)]
names(lbfgsb_opt_init_clusters.sim) <- c("MAOM.sim","TOC.sim","POM.AGG.sim","TOC.obs","MAOM.obs","POM.AGG.obs")


fit.toc  <- summary(lm(lbfgsb_opt_init_clusters.sim$TOC.obs ~ lbfgsb_opt_init_clusters.sim$TOC.sim))
TOC.bias <- lbfgsb_opt_init_clusters.sim$TOC.obs/100 - lbfgsb_opt_init_clusters.sim$TOC.sim/100
dim.TOC  <- 993
RMSE_TOC <- sqrt(sum((lbfgsb_opt_init_clusters.sim$TOC.obs/100- lbfgsb_opt_init_clusters.sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC  <- mean(TOC.bias, na.rm=T)
MAE_TOC  <- mean(abs(TOC.bias), na.rm=T)

#R square 0.39, RMSE of 6.04, MAE of 4.11

#MAOM statistic
fit.MAOM  <- summary(lm(lbfgsb_opt_init_clusters.sim$MAOM.obs ~ lbfgsb_opt_init_clusters.sim$MAOM.sim))
MAOM.bias <- lbfgsb_opt_init_clusters.sim$MAOM.obs/100 - lbfgsb_opt_init_clusters.sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((lbfgsb_opt_init_clusters.sim$MAOM.obs/100 - lbfgsb_opt_init_clusters.sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.37, RMSE of 5.06, MAE of 3.55

#POM.AGG statistic
fit.POM.AGG <- summary(lm(lbfgsb_opt_init_clusters.sim$POM.AGG.obs ~ lbfgsb_opt_init_clusters.sim$POM.AGG.sim))
POM.AGG.bias <- lbfgsb_opt_init_clusters.sim$POM.AGG.obs/100 - lbfgsb_opt_init_clusters.sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((lbfgsb_opt_init_clusters.sim$POM.AGG.obs/100- lbfgsb_opt_init_clusters.sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.19, RMSE of 2.13, MAE of 1.63

rho_toc     <- cor(lbfgsb_opt_init_clusters.sim$TOC.obs, lbfgsb_opt_init_clusters.sim$TOC.sim, method = "pearson")
rho_maom    <- cor(lbfgsb_opt_init_clusters.sim$MAOM.obs, lbfgsb_opt_init_clusters.sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(lbfgsb_opt_init_clusters.sim$POM.AGG.obs, lbfgsb_opt_init_clusters.sim$POM.AGG.sim, method = "pearson")

#0.62, 0.61, 0.44

text_df_c <- data.frame(
  target = c("POC", "MAOC", "TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.13<br>MAE = 1.63<br>&rho;<sub>c</sub> = 0.44",
    "RMSE = 5.06<br>MAE = 3.55<br>&rho;<sub>c</sub> = 0.61",
    "RMSE = 6.04<br>MAE = 4.11<br>&rho;<sub>c</sub> = 0.62"
  )
)

text_df_c$target <- factor(
  text_df_c$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


targets_3frac <- c("POM.AGG", "MAOM", "TOC")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- lbfgsb_opt_init_clusters.sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_c <- fracs_df3

pltdf_c$target <- factor(
  pltdf_c$target,
  levels = targets_3frac,
  labels = c("POC", "MAOC", "TOC")
)


line_df_c <- NULL
tar_max_values <- c("POC" = 25, "MAOC" = 50, "TOC" = 60)
#print(unique(pltdf_b$target))
for (target in unique(pltdf_c$target)) {
  print(target)
  tar_df <- pltdf_c[pltdf_c$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  #tar_max <- max(tar_df) * 1.05
  tar_max <- tar_max_values[target] * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_c <- rbind(line_df_c, line_rec)
}

line_df_c$target <- factor(
  line_df_c$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

p_c <- ggplot() +
  geom_point(
    data = pltdf_c,
    mapping = aes(x = x, y = y)
  ) +
  geom_line(
    data = line_df_c,
    mapping = aes(x = x, y = y), colour = "grey40"
  ) +
  geom_richtext(
    data = text_df_c,
    mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
    label.color = NA
  ) +
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Bioregional calibration with optimised parameters and initialisation, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_c

################################################################################

lbfgsb_opt_init_driving_clusters.sim <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_driving_993.RDS")
rownames(lbfgsb_opt_init_driving_clusters.sim) <- lbfgsb_opt_init_driving_clusters.sim$site
lbfgsb_opt_init_driving_clusters.sim <- lbfgsb_opt_init_driving_clusters.sim[,c(8,9,10,13,14,15)]
names(lbfgsb_opt_init_driving_clusters.sim) <- c("MAOM.sim","TOC.sim","POM.AGG.sim","TOC.obs","MAOM.obs","POM.AGG.obs")


fit.toc  <- summary(lm(lbfgsb_opt_init_driving_clusters.sim$TOC.obs ~ lbfgsb_opt_init_driving_clusters.sim$TOC.sim))
TOC.bias <- lbfgsb_opt_init_driving_clusters.sim$TOC.obs/100 - lbfgsb_opt_init_driving_clusters.sim$TOC.sim/100
dim.TOC  <- 993
RMSE_TOC <- sqrt(sum((lbfgsb_opt_init_driving_clusters.sim$TOC.obs/100- lbfgsb_opt_init_driving_clusters.sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC  <- mean(TOC.bias, na.rm=T)
MAE_TOC  <- mean(abs(TOC.bias), na.rm=T)

#R square 0.38, RMSE of 6.10, MAE of 4.15

#MAOM statistic
fit.MAOM  <- summary(lm(lbfgsb_opt_init_driving_clusters.sim$MAOM.obs ~ lbfgsb_opt_init_driving_clusters.sim$MAOM.sim))
MAOM.bias <- lbfgsb_opt_init_driving_clusters.sim$MAOM.obs/100 - lbfgsb_opt_init_driving_clusters.sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((lbfgsb_opt_init_driving_clusters.sim$MAOM.obs/100 - lbfgsb_opt_init_driving_clusters.sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.36, RMSE of 5.10, MAE of 3.55

#POM.AGG statistic
fit.POM.AGG <- summary(lm(lbfgsb_opt_init_driving_clusters.sim$POM.AGG.obs ~ lbfgsb_opt_init_driving_clusters.sim$POM.AGG.sim))
POM.AGG.bias <- lbfgsb_opt_init_driving_clusters.sim$POM.AGG.obs/100 - lbfgsb_opt_init_driving_clusters.sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((lbfgsb_opt_init_driving_clusters.sim$POM.AGG.obs/100- lbfgsb_opt_init_driving_clusters.sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.21, RMSE of 2.13, MAE of 1.62

rho_toc     <- cor(lbfgsb_opt_init_driving_clusters.sim$TOC.obs, lbfgsb_opt_init_driving_clusters.sim$TOC.sim, method = "pearson")
rho_maom    <- cor(lbfgsb_opt_init_driving_clusters.sim$MAOM.obs, lbfgsb_opt_init_driving_clusters.sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(lbfgsb_opt_init_driving_clusters.sim$POM.AGG.obs, lbfgsb_opt_init_driving_clusters.sim$POM.AGG.sim, method = "pearson")

#0.61, 0.60, 0.46

text_df_cd <- data.frame(
  target = c("POC", "MAOC", "TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.13<br>MAE = 1.62<br>&rho;<sub>c</sub> = 0.46",
    "RMSE = 5.10<br>MAE = 3.55<br>&rho;<sub>c</sub> = 0.60",
    "RMSE = 6.10<br>MAE = 4.15<br>&rho;<sub>c</sub> = 0.61"
  )
)

text_df_cd$target <- factor(
  text_df_cd$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)


targets_3frac <- c("POM.AGG", "MAOM", "TOC")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- lbfgsb_opt_init_driving_clusters.sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_cd <- fracs_df3

pltdf_cd$target <- factor(
  pltdf_cd$target,
  levels = targets_3frac,
  labels = c("POC", "MAOC", "TOC")
)


line_df_cd <- NULL
tar_max_values <- c("POC" = 25, "MAOC" = 50, "TOC" = 60)
#print(unique(pltdf_b$target))
for (target in unique(pltdf_cd$target)) {
  print(target)
  tar_df <- pltdf_cd[pltdf_cd$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  #tar_max <- max(tar_df) * 1.05
  tar_max <- tar_max_values[target] * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_cd <- rbind(line_df_cd, line_rec)
}

line_df_cd$target <- factor(
  line_df_cd$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

p_cd <- ggplot() +
  geom_point(
    data = pltdf_cd,
    mapping = aes(x = x, y = y)
  ) +
  geom_line(
    data = line_df_cd,
    mapping = aes(x = x, y = y), colour = "grey40"
  ) +
  geom_richtext(
    data = text_df_cd,
    mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
    label.color = NA
  ) +
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Bioregional calibration with optimised parameters, initialisation, and forcing, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_cd

################################################################################
#benchmark_sim_outs_old <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/rangelands_eqm_benchmark_sim_993.rds")
benchmark_sim_outs <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/sitebysite_benchmark_sim_outs_0320_init.rds")
benchmark_sim_outs[, c(1:12)] <- lapply(benchmark_sim_outs[, c(1:12)], as.numeric)
benchmark_sim_outs$site <- row.names(benchmark_sim_outs)
benchmark_sim_outs <- benchmark_sim_outs[,c(1:9,13)]
names(benchmark_sim_outs)[4:9] <- c("TOC.sim","MAOM.sim","POM.sim","AGG.sim","MIC.sim","LMWC.sim")
benchmark_sim_outs$POM.AGG.sim <- with(benchmark_sim_outs,POM.sim+AGG.sim)
#benchmark_sim <- merge(benchmark_sim_outs,Obs.pools,by="site")

#-------------------------------------------------------------------------------
benchmark_sim_outs <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/sitebysite_benchmark_sim_outs_0609.rds")
colnames(benchmark_sim_outs) <- c('TOC','MAOM','POM.AGG','TOC1000', 'MAOM1000', 'POM1000', 'AGG1000' ,'TOC_dev', 'MAOM_dev','POMAGG_dev')
benchmark_sim_outs <- as.data.frame(benchmark_sim_outs)
benchmark_sim_outs$site <- row.names(benchmark_sim_outs)
benchmark_sim_outs <- benchmark_sim_outs[,c(1:7,11)]
names(benchmark_sim_outs)[4:7] <- c("TOC.sim","MAOM.sim","POM.sim","AGG.sim")
benchmark_sim_outs$POM.AGG.sim <- with(benchmark_sim_outs,POM.sim+AGG.sim)
#-------------------------------------------------------------------------------
benchmark_sim <- benchmark_sim_outs
names(benchmark_sim)[1:3] <- c("TOC.obs","MAOM.obs","POM.AGG.obs")
targets_3frac <- c("POM.AGG", "MAOM", "TOC")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- benchmark_sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_d <- fracs_df3

pltdf_d$target <- factor(
  pltdf_d$target,
  levels = targets_3frac,
  labels = c("POC", "MAOC", "TOC")
)


line_df_d <- NULL
tar_max_values <- c("POC" = 25, "MAOC" = 50, "TOC" = 60)
#print(unique(pltdf_b$target))
for (target in unique(pltdf_d$target)) {
  print(target)
  tar_df <- pltdf_d[pltdf_d$target == target, c("x", "y")]
  tar_min <- 0.0  # min(tar_df) * 0.95
  tar_max <- tar_max_values[target] * 1.05
  
  line_rec <- data.frame(
    target = target,
    x = c(tar_min, tar_max),
    y = c(tar_min, tar_max)
  )
  
  line_df_d <- rbind(line_df_d, line_rec)
}

line_df_d$target <- factor(
  line_df_d$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

#TOC statistic

fit.toc <- summary(lm(benchmark_sim$TOC.obs ~ benchmark_sim$TOC.sim))
TOC.bias <- benchmark_sim$TOC.obs/100 - benchmark_sim$TOC.sim/100
dim.TOC  <- 992
RMSE_TOC <- sqrt(sum((benchmark_sim$TOC.obs/100- benchmark_sim$TOC.sim/100)^2, na.rm=T)/dim.TOC)
MBE_TOC  <- mean(TOC.bias, na.rm=T)
MAE_TOC  <- mean(abs(TOC.bias), na.rm=T)

#R square 0.88, RMSE of 2.79, MAE of 1.21

#MAOM statistic
fit.MAOM  <- summary(lm(benchmark_sim$MAOM.obs ~ benchmark_sim$MAOM.sim))
MAOM.bias <- benchmark_sim$MAOM.obs/100 - benchmark_sim$MAOM.sim/100
dim.MAOM  <- 992
RMSE_MAOM   <- sqrt(sum((benchmark_sim$MAOM.obs/100- benchmark_sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.84, RMSE of 2.42, MAE of 1.18

#POM.AGG statistic
fit.POM.AGG  <- summary(lm(benchmark_sim$POM.AGG.obs ~ benchmark_sim$POM.AGG.sim))
POM.AGG.bias <- benchmark_sim$POM.AGG.obs/100 - benchmark_sim$POM.AGG.sim/100
dim.POM.AGG  <- 992
RMSE_POM.AGG <- sqrt(sum((benchmark_sim$POM.AGG.obs/100- benchmark_sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG  <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG  <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.61, RMSE of 2.05, MAE of 1.03

rho_toc     <- cor(benchmark_sim$TOC.obs, benchmark_sim$TOC.sim, method = "pearson")
rho_maom    <- cor(benchmark_sim$MAOM.obs, benchmark_sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(benchmark_sim$POM.AGG.obs, benchmark_sim$POM.AGG.sim, method = "pearson")

#site-by-site, rho_TOC 0.94, rho_maom 0.92, rho_pomagg 0.78


text_df_d <- data.frame(
  target = c("POC", "MAOC", "TOC"),
  x = c(0.0, 0.0, 0.0),
  y = c(24, 45, 63),
  text = c(
    "RMSE = 2.05<br>MAE = 1.03<br>&rho;<sub>c</sub> = 0.78",
    "RMSE = 2.42<br>MAE = 1.18<br>&rho;<sub>c</sub> = 0.92",
    "RMSE = 2.79<br>MAE = 1.21<br>&rho;<sub>c</sub> = 0.94"
  )
)

text_df_d$target <- factor(
  text_df_d$target,
  levels = c("POC", "MAOC", "TOC"),
  labels = c("POC", "MAOC", "TOC")
)

p_d <- ggplot() +
    geom_point(
      data = pltdf_d,
      mapping = aes(x = x, y = y)
    ) +
    geom_line(
      data = line_df_d,
      mapping = aes(x = x, y = y), colour = "grey40"
    ) +
    geom_richtext(
      data = text_df_d,
      mapping = aes(x = x, y = y, label = text), hjust = 0, size = 3.0,
      label.color = NA
    ) +
    labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Site-specific calibration with optimised parameters, (MgC ha"^{-1}*")")) +
    facet_wrap(~target, ncol = 1, scale = "free") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      aspect.ratio = 1.0,
      strip.background = element_blank()
    )+
    theme(legend.position = "none")


p_d

# combined_plot <-  p_a + p_b + p_c + p_d
# # Add a overall title if desired
# combined_plot <- combined_plot +
#   #plot_annotation(title = "Combined Plots")
#   plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
# combined_plot
# # Save the combined plot
# ggsave("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/predictions_all.png", combined_plot, width = 12, height = 12)

CairoPNG("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/predictions_all_schemes_new.png", dpi = 300, width = 3000, height = 2300)
#gridExtra::grid.arrange(p_a, p_b, p_c, p_d, nrow = 1)
gridExtra::grid.arrange(p_a, p_c, p_d, nrow = 1)
dev.off()


CairoPNG("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/predictions_all_schemes_new.png", dpi = 300, width = 2700, height = 2700)
#grid.arrange(p_a1, p_a, p_d, nrow = 1)
gridExtra::grid.arrange(p_a1, p_a, p_d, nrow = 1)
dev.off()

###############################################
dev.new()
#new_plot_all
grid.arrange(p_a1, p_a, p_d, nrow = 1)
grid.arrange(p_a, p_c, p_cd, nrow = 1)
dev.copy2pdf(file = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/predictions_all_schemes_new.pdf",width = 9, height = 9)
dev.off()
###############################################
CairoPNG("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/predictions_all_schemes_cluster.png", dpi = 300, width = 2700, height = 2700)
#gridExtra::grid.arrange(p_a, p_b, p_c, p_d, nrow = 1)
gridExtra::grid.arrange(p_a, p_c, p_cd, nrow = 1)
dev.off()