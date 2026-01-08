#clusters_init_driving
library(readr)
library(tools)
library(dplyr)
library(lubridate)
library(tools)
library(tidyr)
library(tibble)
library(jsonlite)
library(ggtext)
library(patchwork)
library(reshape2)
library(cowplot)
library(gridExtra)


file_list <- list.files(path = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_drive/optim_sets/", pattern = "*.csv", full.names = TRUE)
#library(dplyr)
# Read and combine all CSV files into a single data frame
combined_data <- file_list %>%
  lapply(read.csv) %>%        # Read each file into a list
  bind_rows()                 # Combine the list into a single data frame

# View the combined data
#print(combined_data)
combined_data$POM.AGG <- with(combined_data,POM+AGG)
colnames(combined_data)[4:10] <- c("POM1000","LMWC1000","AGG1000","MIC1000","MAOM1000","SOM1000","POM.AGG1000")
sim_all <- merge(combined_data,Obs.clusters,by="site")
#colnames(sim_all)[13:15] <- c("SOM","MAOM","POM.AGG")
fit.SOM <- summary(lm(sim_all$SOM ~ sim_all$SOM1000))
RMSE_tha <- sqrt(sum((sim_all$SOM/100- sim_all$SOM1000/100)^2, na.rm=T)/993)
cor(sim_all$SOM, sim_all$SOM1000, method = "pearson")
saveRDS(sim_all,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_driving_993.RDS")
#clusters_init

file_list <- list.files(path = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_init/optim_sets/", pattern = "*.csv", full.names = TRUE)
#library(dplyr)
# Read and combine all CSV files into a single data frame
combined_data <- file_list %>%
  lapply(read.csv) %>%        # Read each file into a list
  bind_rows()                 # Combine the list into a single data frame

# View the combined data
#print(combined_data)
combined_data$POM.AGG <- with(combined_data,POM+AGG)
colnames(combined_data)[4:10] <- c("POM1000","LMWC1000","AGG1000","MIC1000","MAOM1000","SOM1000","POM.AGG1000")
sim_all <- merge(combined_data,Obs.clusters,by="site")
#colnames(sim_all)[13:15] <- c("SOM","MAOM","POM.AGG")
fit.SOM <- summary(lm(sim_all$SOM ~ sim_all$SOM1000))
RMSE_tha <- sqrt(sum((sim_all$SOM/100- sim_all$SOM1000/100)^2, na.rm=T)/993)
cor(sim_all$SOM, sim_all$SOM1000, method = "pearson")

saveRDS(sim_all,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_ode_993.RDS")
#clusters_withoutinit

file_list <- list.files(path = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13/optim_sets/", pattern = "*.csv", full.names = TRUE)
#library(dplyr)
# Read and combine all CSV files into a single data frame
combined_data <- file_list %>%
  lapply(read.csv) %>%        # Read each file into a list
  bind_rows()                 # Combine the list into a single data frame

# View the combined data
#print(combined_data)
combined_data$POM.AGG <- with(combined_data,POM+AGG)
colnames(combined_data)[4:10] <- c("POM1000","LMWC1000","AGG1000","MIC1000","MAOM1000","SOM1000","POM.AGG1000")
sim_all <- merge(combined_data,Obs.clusters,by="site")
#colnames(sim_all)[13:15] <- c("SOM","MAOM","POM.AGG")
fit.SOM <- summary(lm(sim_all$SOM ~ sim_all$SOM1000))
RMSE_tha <- sqrt(sum((sim_all$SOM/100- sim_all$SOM1000/100)^2, na.rm=T)/993)
cor(sim_all$SOM, sim_all$SOM1000, method = "pearson")

saveRDS(sim_all,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_stode_993.RDS")

sim_all <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_opt_stode_993.RDS")
##########################################

#cluster optim
clusters_optim_sim <- sim_all
rownames(clusters_optim_sim) <- clusters_optim_sim$site
clusters_optim_sim <- clusters_optim_sim[,c(8,9,10,13,14,15)]

names(clusters_optim_sim) <- c("MAOM.sim","SOM.sim","POM.AGG.sim","SOM.obs","MAOM.obs","POM.AGG.obs")


fit.SOM  <- summary(lm(clusters_optim_sim$SOM.obs ~ clusters_optim_sim$SOM.sim))
SOM.bias <- clusters_optim_sim$SOM.obs/100 - clusters_optim_sim$SOM.sim/100
dim.SOM  <- 993
RMSE_SOM <- sqrt(sum((clusters_optim_sim$SOM.obs/100- clusters_optim_sim$SOM.sim/100)^2, na.rm=T)/dim.SOM)
MBE_SOM  <- mean(SOM.bias, na.rm=T)
MAE_SOM  <- mean(abs(SOM.bias), na.rm=T)

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

rho_som     <- cor(clusters_optim_sim$SOM.obs, clusters_optim_sim$SOM.sim, method = "pearson")
rho_maom    <- cor(clusters_optim_sim$MAOM.obs, clusters_optim_sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(clusters_optim_sim$POM.AGG.obs, clusters_optim_sim$POM.AGG.sim, method = "pearson")

#0.608, 0.595, 0.404

text_df_a <- data.frame(
  target = c("POM.AGG", "MAOM", "SOM"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.23<br>ME = 1.69<br>&rho;<sub>c</sub> = 0.40",
    "RMSE = 5.15<br>ME = 3.63<br>&rho;<sub>c</sub> = 0.60",
    "RMSE = 6.15<br>ME = 4.20<br>&rho;<sub>c</sub> = 0.61"
  )
)

text_df_a$target <- factor(
  text_df_a$target,
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
)


targets_3frac <- c("POM.AGG", "MAOM", "SOM")

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
  levels = targets_3frac,
  labels = c("POM.AGG", "MAOM", "SOM")
)


line_df_a <- NULL
tar_max_values <- c("POM.AGG" = 25, "MAOM" = 50, "SOM" = 60)
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
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
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
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Cluster-based simulation with optimized parameters, (MgC ha"^{-1}*")")) +
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

#cluster_init
clusters_optim__init_sim <- sim_all

rownames(clusters_optim__init_sim) <- clusters_optim__init_sim$site
clusters_optim__init_sim <- clusters_optim__init_sim[,c(8,9,10,13,14,15)]

names(clusters_optim__init_sim) <- c("MAOM.sim","SOM.sim","POM.AGG.sim","SOM.obs","MAOM.obs","POM.AGG.obs")


fit.SOM  <- summary(lm(clusters_optim__init_sim$SOM.obs ~ clusters_optim__init_sim$SOM.sim))
SOM.bias <- clusters_optim__init_sim$SOM.obs/100 - clusters_optim__init_sim$SOM.sim/100
dim.SOM  <- 993
RMSE_SOM <- sqrt(sum((clusters_optim__init_sim$SOM.obs/100- clusters_optim__init_sim$SOM.sim/100)^2, na.rm=T)/dim.SOM)
MBE_SOM  <- mean(SOM.bias, na.rm=T)
MAE_SOM  <- mean(abs(SOM.bias), na.rm=T)

#R square 0.3855, RMSE of 6.03, MAE of 4.11

#MAOM statistic
fit.MAOM  <- summary(lm(clusters_optim__init_sim$MAOM.obs ~ clusters_optim__init_sim$MAOM.sim))
MAOM.bias <- clusters_optim__init_sim$MAOM.obs/100 - clusters_optim__init_sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((clusters_optim__init_sim$MAOM.obs/100 - clusters_optim__init_sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.37, RMSE of 5.06, MAE of 3.55

#POM.AGG statistic
fit.POM.AGG <- summary(lm(clusters_optim__init_sim$POM.AGG.obs ~ clusters_optim__init_sim$POM.AGG.sim))
POM.AGG.bias <- clusters_optim__init_sim$POM.AGG.obs/100 - clusters_optim__init_sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((clusters_optim__init_sim$POM.AGG.obs/100- clusters_optim__init_sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.19, RMSE of 2.13, MAE of 1.63

rho_som     <- cor(clusters_optim__init_sim$SOM.obs, clusters_optim__init_sim$SOM.sim, method = "pearson")
rho_maom    <- cor(clusters_optim__init_sim$MAOM.obs, clusters_optim__init_sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(clusters_optim__init_sim$POM.AGG.obs, clusters_optim__init_sim$POM.AGG.sim, method = "pearson")

#0.62, 0.61, 0.44

text_df_b <- data.frame(
  target = c("POM.AGG", "MAOM", "SOM"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.13<br>ME = 1.63<br>&rho;<sub>c</sub> = 0.44",
    "RMSE = 5.09<br>ME = 3.55<br>&rho;<sub>c</sub> = 0.61",
    "RMSE = 6.04<br>ME = 4.11<br>&rho;<sub>c</sub> = 0.62"
  )
)

text_df_b$target <- factor(
  text_df_b$target,
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
)


targets_3frac <- c("POM.AGG", "MAOM", "SOM")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- clusters_optim__init_sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_b <- fracs_df3

pltdf_b$target <- factor(
  pltdf_b$target,
  levels = targets_3frac,
  labels = c("POM.AGG", "MAOM", "SOM")
)


line_df_b <- NULL
tar_max_values <- c("POM.AGG" = 25, "MAOM" = 50, "SOM" = 60)
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
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
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
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Cluster-based with optimal parameters and initialization, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_b


###########################################
#cluster_init_driving
clusters_optim_init_driving_sim <- sim_all

rownames(clusters_optim_init_driving_sim) <- clusters_optim_init_driving_sim$site
clusters_optim_init_driving_sim <- clusters_optim_init_driving_sim[,c(8,9,10,13,14,15)]

names(clusters_optim_init_driving_sim) <- c("MAOM.sim","SOM.sim","POM.AGG.sim","SOM.obs","MAOM.obs","POM.AGG.obs")


fit.SOM  <- summary(lm(clusters_optim_init_driving_sim$SOM.obs ~ clusters_optim_init_driving_sim$SOM.sim))
SOM.bias <- clusters_optim_init_driving_sim$SOM.obs/100 - clusters_optim_init_driving_sim$SOM.sim/100
dim.SOM  <- 993
RMSE_SOM <- sqrt(sum((clusters_optim_init_driving_sim$SOM.obs/100- clusters_optim_init_driving_sim$SOM.sim/100)^2, na.rm=T)/dim.SOM)
MBE_SOM  <- mean(SOM.bias, na.rm=T)
MAE_SOM  <- mean(abs(SOM.bias), na.rm=T)

#R square 0.38, RMSE of 6.10, MAE of 4.15

#MAOM statistic
fit.MAOM  <- summary(lm(clusters_optim_init_driving_sim$MAOM.obs ~ clusters_optim_init_driving_sim$MAOM.sim))
MAOM.bias <- clusters_optim_init_driving_sim$MAOM.obs/100 - clusters_optim_init_driving_sim$MAOM.sim/100
dim.MAOM  <- 993
RMSE_MAOM <- sqrt(sum((clusters_optim_init_driving_sim$MAOM.obs/100 - clusters_optim_init_driving_sim$MAOM.sim/100)^2, na.rm=T)/dim.MAOM)
MBE_MAOM  <- mean(MAOM.bias, na.rm=T)
MAE_MAOM  <- mean(abs(MAOM.bias), na.rm=T)

#R square 0.36, RMSE of 5.10, MAE of 3.55

#POM.AGG statistic
fit.POM.AGG <- summary(lm(clusters_optim_init_driving_sim$POM.AGG.obs ~ clusters_optim_init_driving_sim$POM.AGG.sim))
POM.AGG.bias <- clusters_optim_init_driving_sim$POM.AGG.obs/100 - clusters_optim_init_driving_sim$POM.AGG.sim/100
dim.POM.AGG <- 993
RMSE_POM.AGG <- sqrt(sum((clusters_optim_init_driving_sim$POM.AGG.obs/100- clusters_optim_init_driving_sim$POM.AGG.sim/100)^2, na.rm=T)/dim.POM.AGG)
MBE_POM.AGG <- mean(POM.AGG.bias, na.rm=T)
MAE_POM.AGG <- mean(abs(POM.AGG.bias), na.rm=T)

#R square 0.21, RMSE of 2.13, MAE of 1.62

rho_som     <- cor(clusters_optim_init_driving_sim$SOM.obs, clusters_optim_init_driving_sim$SOM.sim, method = "pearson")
rho_maom    <- cor(clusters_optim_init_driving_sim$MAOM.obs, clusters_optim_init_driving_sim$MAOM.sim, method = "pearson")
rho_pomagg  <- cor(clusters_optim_init_driving_sim$POM.AGG.obs, clusters_optim_init_driving_sim$POM.AGG.sim, method = "pearson")

#0.61, 0.60, 0.46

text_df_c <- data.frame(
  target = c("POM.AGG", "MAOM", "SOM"),
  x = c(0.0, 0.0, 0.0),
  y = c(22, 45, 54),
  text = c(
    "RMSE = 2.13<br>ME = 1.62<br>&rho;<sub>c</sub> = 0.46",
    "RMSE = 5.10<br>ME = 3.55<br>&rho;<sub>c</sub> = 0.60",
    "RMSE = 6.10<br>ME = 4.15<br>&rho;<sub>c</sub> = 0.61"
  )
)

text_df_c$target <- factor(
  text_df_c$target,
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
)


targets_3frac <- c("POM.AGG", "MAOM", "SOM")

fracs_df3 <- NULL
for (target in targets_3frac) {
  tar_df <- clusters_optim_init_driving_sim[,c(paste0(target, ".obs"), paste0(target, ".sim"))]/100
  colnames(tar_df)[1:2] <- c("x", "y")
  tar_df <- add_column(tar_df, target = target, .before = 1)
  
  fracs_df3 <- rbind(fracs_df3, tar_df)
}

pltdf_c <- fracs_df3

pltdf_c$target <- factor(
  pltdf_c$target,
  levels = targets_3frac,
  labels = c("POM.AGG", "MAOM", "SOM")
)


line_df_c <- NULL
tar_max_values <- c("POM.AGG" = 25, "MAOM" = 50, "SOM" = 60)
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
  levels = c("POM.AGG", "MAOM", "SOM"),
  labels = c("POM.AGG", "MAOM", "SOM")
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
  labs(x = expression("Observation, (MgC ha"^{-1}*")"), y = expression("Cluster-based with optimal parameters, initialization, and driving, (MgC ha"^{-1}*")")) +
  facet_wrap(~target, ncol = 1, scale = "free", labeller = label_parsed) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    aspect.ratio = 1.0,
    strip.background = element_blank()
  )+
  theme(legend.position = "none")

p_c


library(ggtext)
library(patchwork)
library(reshape2)
library(Cairo)

CairoPNG("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/rplot-pred-clusters.png", dpi = 300, width = 2700, height = 2700)
grid.arrange(p_a, p_b, p_c, nrow = 1)
dev.off()



