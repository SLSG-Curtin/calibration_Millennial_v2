library(caTools)
library(calibrater)
library(SAFER)
library(ggplot2)
library(gridExtra)
library(matrixStats)
require(graphics)
library(extrafont)

boxplot2_dummy <- function(mu, lb = NULL, ub = NULL, prnam = NULL){
  
  mu1 <- mu[1:(length(mu)-1)]
  lb1 <- lb[1:(length(lb)-1)]
  ub1 <- ub[1:(length(ub)-1)]
  mu2 <- tail(mu,n=1) 
  lb2 <- tail(lb,n=1)
  ub2 <- tail(ub,n=1)
  
  dat <- data.frame(x = factor( prnam, levels = prnam ),mu = mu1)
  
  .pl <- ggplot(data = dat, mapping = aes(x = x, y = mu1))
  
  if( !is.null(lb1) && !is.null(ub1) ){
    dat$lb = lb1 
    dat$ub = ub1
    .pl <- .pl + geom_errorbar(mapping = aes(ymin = lb1, ymax = ub1), width = 0.5) 
  }
  
  .pl <- .pl + geom_point(color = 'red', size = 3) + 
    theme_bw() + 
    xlab(NULL) + ylab("mvd") +
    #scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), limits = c(0, 0.5)) +
    scale_y_continuous(breaks = c(0,0.09,0.18,0.27,0.36,0.5), limits = c(0, 0.5)) +
    geom_hline(yintercept=mu2, size = 0.5, color = "grey49") +
    geom_hline(yintercept=lb2, size = 0.5, linetype = "dashed", color = "grey49") +
    geom_hline(yintercept=ub2, size = 0.5, linetype = "dashed", color = "grey49") +
    annotate("text", x = length(dat$x)/2, y = 0, label = "Threshold for non-influential input factors",
             size = 16, color = "grey49")
  
  return( .pl )
  
}

sxlabels <- c("P[i]",     "P[a]",       "K[pl]",  "alpha[pl]", "plain(E)*alpha[pl]",  "K[pa]",
              "K[b]",       "K[l]",       "K[ld]",  "P[1]",        "P[2]",        "K[lb]",
              "alpha[lb]",  "plain(E)*alpha[lb]", "K[bd]",  "K[ma]",       "CUE[ref]",    "CUE[t]",
              "T[ae-ref]",  "phi",        "lambda", "Porosity",    "K['a,min']",    "P[b]"
             )

X        <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sensitivity_outs/samples_5000/PAWN_Sens_X.RDS")
Y.result <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sensitivity_outs/samples_5000/PAWN_Sens_Y.RDS")



#
n = 10
Nboot <- 1000
#
n = 9
Nboot <- 3000
################################################################################
################################################################################
#################SOM

som_pawn_ind <- pawn_indices(X, Y.result[,6], n, Nboot, dummy = TRUE)

som_KS_max   <- som_pawn_ind$KS_max # KS_max has dim (Nboot, M)
som_KS_dummy <- som_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

som_KS_stat     <- som_KS_max
som_KS_max_m    <- colMeans(som_KS_stat) # mean
som_KS_max_lb   <- colQuantiles(som_KS_stat,probs=alfa/2) # Lower bound
som_KS_max_ub   <- colQuantiles(som_KS_stat,probs=1-alfa/2) # Upper bound

som_KS_stat         <- som_KS_dummy
som_KS_dummy_m      <- mean(som_KS_stat) # mean
som_KS_dummy_lb     <- quantile(som_KS_dummy,alfa/2) # Lower bound
som_KS_dummy_ub     <- quantile(som_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
som_KS_max_d_m  <- c(som_KS_max_m,som_KS_dummy_m) 
som_KS_max_d_lb <- c(som_KS_max_lb,som_KS_dummy_lb)
som_KS_max_d_ub <- c(som_KS_max_ub,som_KS_dummy_ub)

som_KS_summary <- data.frame(
  Parameter = sxlabels,
  mean = som_KS_max_d_m[-length(som_KS_max_d_m)],
  lower = som_KS_max_d_lb[-length(som_KS_max_d_lb)],
  upper = som_KS_max_d_ub[-length(som_KS_max_d_ub)]
)

som_KS_summary$range <- som_KS_summary$upper - som_KS_summary$lower

print("SOM PAWN summary (mean and range):")
print(som_KS_summary[, c("Parameter", "mean", "range")])

dev.new()

som_bboxplot <- boxplot2_dummy(mu = som_KS_max_d_m, lb = som_KS_max_d_lb, ub = som_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
                               #scale_x_discrete(labels = parse(text = sxlabels)) +
                               scale_x_discrete(labels = function(x) parse(text = paste0("bold(", x, ")")))+
                               theme(axis.text.x = element_text(angle = 45,hjust = 1, face = "bold"),
                                      axis.text = element_text(size = 36,face="bold"),
                                      axis.text.y = element_text(size = 36,face="bold"),
                                      axis.title.y = element_text(size = 36,face = "bold")
                               )
som_bboxplot
dev.copy2pdf(file = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/boxplot2_SOC_KS.pdf",width = 24, height = 20)
dev.off()

################################################################################
################################################################################
#################MAOM

maom_pawn_ind <- pawn_indices(X, Y.result[,5], n, Nboot, dummy = TRUE)

maom_KS_max   <- maom_pawn_ind$KS_max # KS_max has dim (Nboot, M)
maom_KS_dummy <- maom_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

maom_KS_stat     <- maom_KS_max
maom_KS_max_m    <- colMeans(maom_KS_stat) # mean
maom_KS_max_lb   <- colQuantiles(maom_KS_stat,probs=alfa/2) # Lower bound
maom_KS_max_ub   <- colQuantiles(maom_KS_stat,probs=1-alfa/2) # Upper bound

maom_KS_stat         <- maom_KS_dummy
maom_KS_dummy_m      <- mean(maom_KS_stat) # mean
maom_KS_dummy_lb     <- quantile(maom_KS_dummy,alfa/2) # Lower bound
maom_KS_dummy_ub     <- quantile(maom_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
maom_KS_max_d_m  <- c(maom_KS_max_m,maom_KS_dummy_m) 
maom_KS_max_d_lb <- c(maom_KS_max_lb,maom_KS_dummy_lb)
maom_KS_max_d_ub <- c(maom_KS_max_ub,maom_KS_dummy_ub)

#dev.new()

maom_bboxplot <- boxplot2_dummy(mu = maom_KS_max_d_m, lb = maom_KS_max_d_lb, ub = maom_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
  )


################################################################################
################################################################################
#################POM

pom_pawn_ind <- pawn_indices(X, Y.result[,1], n, Nboot, dummy = TRUE)

pom_KS_max   <- pom_pawn_ind$KS_max # KS_max has dim (Nboot, M)
pom_KS_dummy <- pom_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

pom_KS_stat     <- pom_KS_max
pom_KS_max_m    <- colMeans(pom_KS_stat) # mean
pom_KS_max_lb   <- colQuantiles(pom_KS_stat,probs=alfa/2) # Lower bound
pom_KS_max_ub   <- colQuantiles(pom_KS_stat,probs=1-alfa/2) # Upper bound

pom_KS_stat         <- pom_KS_dummy
pom_KS_dummy_m      <- mean(pom_KS_stat) # mean
pom_KS_dummy_lb     <- quantile(pom_KS_dummy,alfa/2) # Lower bound
pom_KS_dummy_ub     <- quantile(pom_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
pom_KS_max_d_m  <- c(pom_KS_max_m,pom_KS_dummy_m) 
pom_KS_max_d_lb <- c(pom_KS_max_lb,pom_KS_dummy_lb)
pom_KS_max_d_ub <- c(pom_KS_max_ub,pom_KS_dummy_ub)

#dev.new()

pom_bboxplot <- boxplot2_dummy(mu = pom_KS_max_d_m, lb = pom_KS_max_d_lb, ub = pom_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
  )


################################################################################
################################################################################
#################LMWC

lmwc_pawn_ind <- pawn_indices(X, Y.result[,2], n, Nboot, dummy = TRUE)

lmwc_KS_max   <- lmwc_pawn_ind$KS_max # KS_max has dim (Nboot, M)
lmwc_KS_dummy <- lmwc_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

lmwc_KS_stat     <- lmwc_KS_max
lmwc_KS_max_m    <- colMeans(lmwc_KS_stat) # mean
lmwc_KS_max_lb   <- colQuantiles(lmwc_KS_stat,probs=alfa/2) # Lower bound
lmwc_KS_max_ub   <- colQuantiles(lmwc_KS_stat,probs=1-alfa/2) # Upper bound

lmwc_KS_stat         <- lmwc_KS_dummy
lmwc_KS_dummy_m      <- mean(lmwc_KS_stat) # mean
lmwc_KS_dummy_lb     <- quantile(lmwc_KS_dummy,alfa/2) # Lower bound
lmwc_KS_dummy_ub     <- quantile(lmwc_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
lmwc_KS_max_d_m  <- c(lmwc_KS_max_m,lmwc_KS_dummy_m) 
lmwc_KS_max_d_lb <- c(lmwc_KS_max_lb,lmwc_KS_dummy_lb)
lmwc_KS_max_d_ub <- c(lmwc_KS_max_ub,lmwc_KS_dummy_ub)

#dev.new()

lmwc_bboxplot <- boxplot2_dummy(mu = lmwc_KS_max_d_m, lb = lmwc_KS_max_d_lb, ub = lmwc_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
  )

################################################################################
################################################################################
#################AGG

agg_pawn_ind <- pawn_indices(X, Y.result[,3], n, Nboot, dummy = TRUE)

agg_KS_max   <- agg_pawn_ind$KS_max # KS_max has dim (Nboot, M)
agg_KS_dummy <- agg_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

agg_KS_stat     <- agg_KS_max
agg_KS_max_m    <- colMeans(agg_KS_stat) # mean
agg_KS_max_lb   <- colQuantiles(agg_KS_stat,probs=alfa/2) # Lower bound
agg_KS_max_ub   <- colQuantiles(agg_KS_stat,probs=1-alfa/2) # Upper bound

agg_KS_stat         <- agg_KS_dummy
agg_KS_dummy_m      <- mean(agg_KS_stat) # mean
agg_KS_dummy_lb     <- quantile(agg_KS_dummy,alfa/2) # Lower bound
agg_KS_dummy_ub     <- quantile(agg_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
agg_KS_max_d_m  <- c(agg_KS_max_m,agg_KS_dummy_m) 
agg_KS_max_d_lb <- c(agg_KS_max_lb,agg_KS_dummy_lb)
agg_KS_max_d_ub <- c(agg_KS_max_ub,agg_KS_dummy_ub)

#dev.new()

agg_bboxplot <- boxplot2_dummy(mu = agg_KS_max_d_m, lb = agg_KS_max_d_lb, ub = agg_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
  )

agg_bboxplot


################################################################################
################################################################################
#################MIC

mic_pawn_ind <- pawn_indices(X, Y.result[,4], n, Nboot, dummy = TRUE)

mic_KS_max   <- mic_pawn_ind$KS_max # KS_max has dim (Nboot, M)
mic_KS_dummy <- mic_pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

mic_KS_stat     <- mic_KS_max
mic_KS_max_m    <- colMeans(mic_KS_stat) # mean
mic_KS_max_lb   <- colQuantiles(mic_KS_stat,probs=alfa/2) # Lower bound
mic_KS_max_ub   <- colQuantiles(mic_KS_stat,probs=1-alfa/2) # Upper bound

mic_KS_stat         <- mic_KS_dummy
mic_KS_dummy_m      <- mean(mic_KS_stat) # mean
mic_KS_dummy_lb     <- quantile(mic_KS_dummy,alfa/2) # Lower bound
mic_KS_dummy_ub     <- quantile(mic_KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
mic_KS_max_d_m  <- c(mic_KS_max_m,mic_KS_dummy_m) 
mic_KS_max_d_lb <- c(mic_KS_max_lb,mic_KS_dummy_lb)
mic_KS_max_d_ub <- c(mic_KS_max_ub,mic_KS_dummy_ub)

#dev.new()

mic_bboxplot <- boxplot2_dummy(mu = mic_KS_max_d_m, lb = mic_KS_max_d_lb, ub = mic_KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
  )
mic_bboxplot

################################################################################
library(ggplot2)
library(reshape2)
library(dplyr)
# Sample data creation
#response_vars <- c("N2O", "NO3-", "NH4+", "SOM_CN", "SOM", "CO2", "ENZ", "MBA_CN", "MB_CN", "MBA", "MB")
#parameters <- paste0("param", 1:30)

response_vars <- c("SOM","MAOM","AGG","POM","LMWC","MIC")
parameters <- sxlabels
# Create sample sensitivity matrix
set.seed(123)

sensitivity_matrix <- matrix(rbind(t(som_KS_max_m),
                                   t(maom_KS_max_m),
                                   t(agg_KS_max_m),
                                   t(pom_KS_max_m),
                                   t(lmwc_KS_max_m),
                                   t(mic_KS_max_m)),
                             nrow = length(response_vars),
                             dimnames = list(response_vars, parameters)
                             )
saveRDS(sensitivity_matrix,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sensitivity_matrix_pawn.rds")
#sensitivity_matrix <- matrix(runif(length(response_vars) * length(parameters), 0, 0.7),
#                             nrow = length(response_vars),
#                             dimnames = list(response_vars, parameters))


sensitivity_matrix <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sensitivity_matrix_pawn.rds")
# Convert matrix to long format
row.names(sensitivity_matrix)[1] <- "TOC"
data_long <- melt(sensitivity_matrix)
names(data_long) <- c("Response", "Parameter", "Value")

# Define exactly 9 breaks and 9 colors
breaks <- seq(0, 0.8, length.out = 10)  # 10 breaks to create 9 intervals
labels <- round(breaks, 2)

library(circlize)
library("scales")
library("ggsci")
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(dplyr)
#col_fun <- colorRamp2(c(0, 0.4, 0.8), c("blue", "yellow", "red"))
#cols = col_fun(breaks)

#show_col(pal_frontiers("default", alpha = 0.6)(9))
# Create 9 distinct colors from blue to red

# Cut the values into discrete categories
data_long$Value_discrete <- cut(data_long$Value, 
                                breaks = breaks, 
                                labels = head(labels, -1),
                                include.lowest = TRUE)

sxlabels <- c("P[i]",     "P[a]",       "K[pl]",  "alpha[pl]", "plain(E)*alpha[pl]",  "K[pa]",
              "K[b]",       "K[l]",       "K[ld]",  "P[1]",        "P[2]",        "K[lb]",
              "alpha[lb]",  "plain(E)*alpha[lb]", "K[bd]",  "K[ma]",       "CUE[ref]",    "CUE[t]",
              "T[ae-ref]",  "phi",        "lambda", "Porosity",    "K['a,min']",    "P[b]"
)

tmplables <- c("alpha[pl]", "plain(E)*alpha[pl]", "alpha[lb]",  
               "plain(E)*alpha[lb]", "CUE[ref]",  "CUE[t]", "T[ae-ref]")

smlables  <- c("P[a]", "K[pa]", "K[b]", 
               "K[l]", "K[ma]", "K['a,min']",
               "lambda","Porosity","phi")

langmulabels <- c("P[1]","P[2]","K[ld]")

reactlabels <- c("K[pl]","K[lb]")

otherlabels <- c("P[i]", "P[b]", "K[bd]")

label_lists <- list(
  "Water effects" = smlables,
  "Temperature effects" = tmplables,
  "Langmuir-type saturation" = langmulabels,
  "Reaction kinetics" = reactlabels,
  "Other" = otherlabels
)

label_lists <- list(
  facet1 = smlables,
  facet2 = tmplables,
  facet3 = langmulabels,
  facet4 = reactlabels,
  facet5 = otherlabels
)
#data_long$classes <- ifelse(data_long$Parameter %in% tmplables, "Temperature effects", "Other1")
#data_long$classes <- ifelse(data_long$Parameter %in% smlables,  "Water effects", "Other2")
#data_long$classes <- ifelse(data_long$Parameter %in% langmulabels, "Langmuir-type saturation", "Other3")
#data_long$classes <- ifelse(data_long$Parameter %in% reactlabels, "Reaction kinetics", "Other")

# Assign values based on multiple conditions
data_long <- data_long %>%
  mutate(classes = case_when(
    Parameter %in% tmplables ~ "Temperature effects",
    Parameter %in% smlables ~ "Water effects",
    Parameter %in% langmulabels ~ "Langmuir\nmodel",
    Parameter %in% reactlabels ~ "Reaction\nkinetics",
    TRUE ~ "Other",
  ))


data_long$classes <- factor(data_long$classes, 
                               levels=c("Water effects",
                                        "Temperature effects",
                                        "Langmuir\nmodel",
                                        "Reaction\nkinetics",
                                        "Other"))

# Create the heatmap
original_palette <- brewer.pal(9, "Set1")
# Reverse the palette
reversed_palette <- rev(original_palette)


custom_labeller2 <- function(variable) {
   parse(text = label_lists[[variable]])
}

# facet_mapping <- setNames(
#   c("facet1", "facet2", "facet3", "facet4", "facet5"),
#   c("Water effects",
#     "Temperature effects",
#     "Langmuir-type saturation",
#     "Reaction kinetics",
#     "Other")
# )

# custom_labeller <- function(x) {
#   parsed_labels <- label_lists[[facet_mapping[as.character(x)]]]
#   parse(text = parsed_labels)
# }


plot_class_all <- ggplot(data_long, aes(x = Parameter, y = Response, fill = Value_discrete)) +
  geom_tile(color = "black", linewidth = 0.1) +
  #scale_x_discrete(labels = parse(text = sxlabels)) +
  scale_x_discrete(labels = function(x) parse(text = paste0("bold(", x, ")")))+
  scale_fill_manual(values = reversed_palette,
                    name = "PAWN",
                    guide = guide_legend(
                      frame.colour = "black",
                      frame.linewidth = 1,
                      ticks.colour = "black",
                      ticks.linewidth = 1,
                      barwidth = 2,
                      barheight = 3,
                      reverse = TRUE)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 36, face = "bold"),
    axis.text.y = element_text(size = 36, face = "bold"),
    #panel.grid.major.x = element_line(color = "gray"),
    #panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = "black", size = 0.5),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(8, "pt"),
    panel.grid = element_blank(),
    panel.spacing = unit(-0.1, "lines"),
    legend.title = element_text(size = 36, face = "bold"),
    legend.text = element_text(size = 36, face = "bold"),
    legend.position = "right"
  )+ 
  #theme(aspect.ratio = 1) +
  theme(panel.spacing = unit(0.1, "lines"))+
  #facet_wrap(~classes, scales = "free_x") +
  #facet_wrap(~classes, labeller = as_labeller(custom_labeller)) +
  facet_grid(~ classes,space = "free_x", scales = "free_x") +
  #facet_wrap(~classes, labeller = function(x) {parse(text = label_lists[[as.character(x)]])})+
  #scale_x_discrete(labels = function(x) lapply(label_lists[x], function(y) parse(text = y)))+
  theme(strip.placement = "outside",
        strip.text = element_text(size = 30, face = "bold"))+
  labs(title = "",
       x = "",
       y = "")

plot_class_all

new_plot_all <- ggarrange(
  plot_class_all,
  ncol = 1.0,
  common.legend = TRUE,
  legend = "right"  # This places the legend on the right and makes it common for all facets
)

new_plot_all

require(graphics)
library(extrafont)
library(Cairo)
library(tibble)
library(jsonlite)
library(ggtext)

dev.new()
#new_plot_all
plot_class_all
dev.copy2pdf(file = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/boxplot2_cfractions_KS_5000.pdf",width = 24, height = 16)
dev.off()

CairoPNG("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/boxplot2_cfractions_KS.png", dpi = 300, width = 2600*5, height = 1080*5)
#CairoPDF("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/boxplot2_cfractions_KS.pdf", width = 18, height = 6)
print(plot_class_all)
dev.off()

library(cowplot)

# Arrange the plots in a grid with equal width and height
combined_plot <- plot_grid(plot_class_a,plot_class_b,plot_class_c,
                           plot_class_d,plot_class_e, 
                           ncol = 5, align = "hv", axis = "tb", 
                           rel_widths = rep(1, 5))

# Display the combined plot
combined_plot

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example data structure similar to the provided image
# You would replace this with your actual dataset
data <- expand.grid(
  Reason = c("Insufficient enrollment", "Business or administrative", "Logistics or resources",
             "Negative", "Study design", "Invalid reason", "Study staff moved",
             "COVID-19", "Regulatory", "Safety or side effects", "Another study",
             "No context", "Interim analysis", "Insufficient data", "Success"),
  StudyStartDate = seq(1995, 2021, by = 1),
  Phase = c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", 
            "Phase 2/Phase 3", "Phase 3", "Phase 4", "Unknown"),
  TherapeuticArea = c("Oncology", "Neurological", "Infectious", "Respiratory", "Other"),
  StoppedTrials = sample(1:500, 500, replace = TRUE) # Random data for illustration
)

# Define color scale for Stopped Trials count
stopped_trials_colors <- c(
  "1-4" = "purple", "5-9" = "dark purple", "10-24" = "blue",
  "25-99" = "green", "100-249" = "light green", "250-499" = "yellow", "500+" = "orange"
)

# Define color mapping for Reason implications
reason_colors <- c(
  "Negative" = "red", "Safety" = "peachpuff", "Suspicious" = "purple",
  "Neutral" = "blue", "Success" = "green", "Invalid" = "brown"
)

# Plotting the heatmap with facets
ggplot(data, aes(x = StudyStartDate, y = Reason, fill = StoppedTrials)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("purple", "peachpuff", "blue", "green", "brown", "yellow", "orange"),
                       limits = c(1, 500),
                       name = "Stopped Trials") +
  facet_grid(Phase ~ TherapeuticArea) +
  scale_y_discrete(limits = rev(unique(data$Reason))) + # Reverse order for y-axis
  theme_minimal() +
  labs(
    title = "Stopped Clinical Trials by Reason, Study Start Date, Phase, and Therapeutic Area",
    x = "Study Start Date",
    y = "Reason for Stopping"
  ) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

