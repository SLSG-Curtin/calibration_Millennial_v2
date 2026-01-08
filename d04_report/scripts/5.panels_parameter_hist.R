rm(list = ls())

library(FME)
library(MASS)
library(pracma)
library(ggplot2)
library(corrplot)
library(deSolve)
library(lattice)
library(graphics)
library(rsq)
library(ppcor)
library(caret)
library(gbm)
library(rtop)
library(EnvStats)
library(readr)
library(tools)
library(extrafont)
#-------------------------------------------------------------------------------
#import scripts

rootdir <- "/media/DATADRIVE1/Model/Millennial/"
source(paste0(rootdir, "R/simulation/run_functions.R"))
source(paste0(rootdir, "R/models/derivs_V2_MM.R"))

average_year_col_xy  <- readRDS("/media/DATADRIVE1/Model/Millennial/R/data/average_year_col.RDS")

#continental_input_sites_update.csv
newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites_update.txt")

#continental_input_sites.csv
#newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites.txt")
newinputs <- newinputs[!(newinputs$site %in% paste0("S", 51:193)),]

flist <- list.files("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/rangeland_driving/",full.names = TRUE,recursive = TRUE)
basefile_names <- basename(flist)



parToFit.VR.opt <- c(
  param_pa = 0.4,
  eact_pl = 63339,
  rate_pa = 0.012,
  rate_break = 0.026,
  rate_leach = 1.5e-03,
  kaff_des = 0.02,
  param_p1 = 0.078,
  kaff_lb = 710.8,
  eact_lb = 60428,
  rate_ma = 0.0052,
  cue_ref = 0.53,
  tae_ref = 15,
  matpot = 15,
  lambda = 2.1e-04,
  porosity = 0.62,
  kamin = 0.2
)


#opt low values
parToFit.lower.opt = c(
  param_pa = 0,
  eact_pl = 5.0e+04,
  rate_pa = 0,
  rate_break = 0,
  rate_leach = 7.5e-04,
  kaff_des = 1.2e-02,
  param_p1 = 5.0e-02,
  kaff_lb = 1.0e+02,
  eact_lb = 5.0e+04,
  rate_ma = 1.0e-03,
  cue_ref = 3.0e-01,
  tae_ref = 7.5,
  matpot = 7.5,
  lambda = 1.05e-04,
  porosity = 0.3,
  kamin = 0.1
)

#opt upper values
parToFit.upper.opt <- c(
  param_pa = 1,
  eact_pl = 7.0e+04,
  rate_pa = 8.5e-01,
  rate_break = 0.1,
  rate_leach = 2.25e-03,
  kaff_des = 3.6e-02,
  param_p1 = 5.0e-01,
  kaff_lb = 1.0e+03,
  eact_lb = 7.5e+04,
  rate_ma = 1.0e-01,
  cue_ref = 9.0e-01,
  tae_ref = 22.5,
  matpot = 22.5,
  lambda = 3.15e-04,
  porosity = 0.9,
  kamin = 0.3
)


ParaName    = c('param_pi'  ,'param_pa'  ,'kaff_pl' ,'alpha_pl','eact_pl' ,'rate_pa',
                'rate_break','rate_leach','kaff_des','param_p1','param_p2','kaff_lb',
                'alpha_lb'  ,'eact_lb'   ,'rate_bd' ,'rate_ma' ,'cue_ref' ,'cue_t'  ,
                'tae_ref'   ,'matpot'    ,'lambda'  ,'porosity','kamin'   ,'param_pb')

ParaNameNew = c('param_pi'  ,'param_pa'  ,'kaff_pl' ,'alpha_pl','eact_pl' ,'rate_pa',
                'rate_break','rate_leach','kaff_des','param_p1','param_p2','kaff_lb',
                'alpha_lb'  ,'eact_lb'   ,'rate_bd' ,'rate_ma' ,'cue_ref' ,'cue_t'  ,
                'tae_ref'   ,'matpot'    ,'lambda'  ,'porosity','kamin'   ,'param_pb', 'param_qmax','param_pH')


nUpper  = c(1,            1,      15000,    3.75e12,   70000,    0.85,
            0.1,     0.00225,      0.036,        0.5,   0.324,    1000,
            3.9e12,   75000,     0.0054,        0.1,     0.9,   0.018,
            22.5,        22.5,    3.15e-4,        0.9,       0.3,       1)


nLower  = c(0,            0,       5000,    1.25e12,   50000,     0.0,
            0,       0.00075,      0.012,       0.05,   0.108,     100,
            1.0e12,   50000,     0.0018,      0.001,     0.3,   0.006,
            7.5,         7.5,    1.05e-4,        0.3,     0.1,       0)

par0    = c(0.66,      0.4,      10000,     2.6e+12,   63339,    0.012,
            0.026,   0.0015,       0.02,      0.078,   0.216,    710.8,
            1.2e+12,  60428,     0.0044,     0.0052,    0.53,    0.012,
            15,          15,    0.00021,       0.62,     0.2,     0.52)


logor   = c(FALSE, FALSE,  TRUE,   TRUE,  TRUE,  FALSE,
            FALSE, FALSE,  FALSE,  FALSE, FALSE, TRUE,
            TRUE,  TRUE,   FALSE,  FALSE, FALSE, FALSE,
            FALSE, FALSE,  FALSE,  FALSE, FALSE, FALSE)

params_df <- data.frame(
  Parameter = ParaName,
  Lower = nLower,
  Initial = par0,
  Upper = nUpper,
  Logor = logor
)

important_params_set6 <- c('param_pa', 'rate_pa', 'rate_break', 'rate_leach',
                           'rate_ma',  'matpot',  'eact_pl',    'eact_lb',
                           'cue_ref',  'tae_ref', 'kaff_des',   'param_p1',
                           'param_pi')

selected_params <- params_df[params_df$Parameter %in% important_params_set6, ]

selected_init <- setNames(selected_params$Initial, selected_params$Parameter)
selected_lower <- setNames(selected_params$Lower, selected_params$Parameter)
selected_upper <- setNames(selected_params$Upper, selected_params$Parameter)

isite <- file_path_sans_ext(basefile_names)

inputs <- newinputs[,c("site","POM.AGG","MAOM","SOM","SoilTMP.C","SoilMoi.m3m3","NPP.gC.m2.d","qmax.gC.m2","pH_CaCl2","BD.mg.cm3","depth")]

inputs <- inputs[which(inputs$site %in% isite),]


derivs.rangeland.MMRMM <- function(step.num,state,parameters) {
  
  default_params <- list(
    param_pi   = 0.66,
    param_pa   = 0.40,
    kaff_pl    = 10000,
    alpha_pl   = 2.6e+12,
    eact_pl    = 63339,
    rate_pa    = 0.012,
    rate_break = 0.026,
    rate_leach = 0.0015,
    kaff_des   = 0.02,
    param_p1   = 0.078,
    param_p2   = 0.216,
    kaff_lb    = 710.8,
    alpha_lb   = 1.2e+12,
    eact_lb    = 60428,
    rate_bd    = 0.0044,
    rate_ma    = 0.0052,
    cue_ref    = 0.53,
    cue_t      = 0.012,
    tae_ref    = 15,
    matpot     = 15,
    lambda     = 0.00021,
    porosity   = 0.62,
    kamin      = 0.2,
    param_pb   = 0.52,
    param_qmax = 6000,
    param_pH   = 6.2,
    forc_st    = 25,
    forc_sw    = 0.19,
    forc_npp   = 0.65
  )
  
  parameters <- modifyList(default_params, parameters)
  
  with(as.list(c(state,parameters)), {
    
    # Soil type properties  
    #Equation 10
    kaff_lm = exp(-parameters$param_p1 * parameters$param_pH - parameters$param_p2) * parameters$kaff_des
    
    #Equation 111
    #param_qmax = parameters$param_bulkd * parameters$param_pc * parameters$param_claysilt 
    
    # Hydrological properties
    
    #Equation 4
    scalar_wd = (parameters$forc_sw / parameters$porosity)^0.5
    
    #Equation 15
    scalar_wb = exp(parameters$lambda * -parameters$matpot) * (parameters$kamin + (1 - parameters$kamin) * ((parameters$porosity - parameters$forc_sw) / parameters$porosity)^0.5) * scalar_wd
    
    # Decomposition
    
    gas_const <- 8.31446
    
    #Equation 3
    vmax_pl = parameters$alpha_pl * exp(-parameters$eact_pl / (gas_const * (parameters$forc_st + 273.15))) 
    
    #Equation 2
    # POM -> LMWC
    if(POM>0){
      f_PO_LM = POM * vmax_pl * MIC * scalar_wd / (parameters$kaff_pl + MIC)
    }else{
      f_PO_LM=0
    }
    
    #Equation 5
    # POM -> AGG
    if(POM>0){
      f_PO_AG = parameters$rate_pa * scalar_wd * POM
    }else{
      f_PO_AG=0
    }
    
    #Equation 6
    # AGG -> MAOM + POM
    if(AGG>0){
      f_AG_break = parameters$rate_break * scalar_wd * AGG
    }else{
      f_AG_break=0
    }
    
    #Equation 8
    # LMWC -> out of system leaching
    if(LMWC>0){
      f_LM_leach = parameters$rate_leach * scalar_wd * LMWC
    }else{
      f_LM_leach=0
    }
    
    #Equation 9
    # LMWC -> MAOM
    if(LMWC>0 && MAOM>0){
      f_LM_MA = scalar_wd * kaff_lm * LMWC * (1 - MAOM / parameters$param_qmax)
    }else{
      f_LM_MA=0
    }
    
    #Equation 12
    # MAOM -> LMWC
    if(MAOM>0){
      f_MA_LM = parameters$kaff_des * MAOM / parameters$param_qmax
    }else{
      f_MA_LM=0
    }
    
    #Equation 14
    vmax_lb = parameters$alpha_lb * exp(-parameters$eact_lb / (gas_const * (parameters$forc_st + 273.15)))
    
    #Equation 13
    # LMWC -> MIC
    if(LMWC>0){
      f_LM_MB = vmax_lb * scalar_wb * LMWC * MIC / (parameters$kaff_lb + LMWC)
    }else{
      f_LM_MB=0
    }
    
    #Equation 16
    # MIC -> MAOM + LMWC
    if(MIC>0){
      f_MB_turn = parameters$rate_bd * MIC^2.0
    }else{
      f_MB_turn=0
    }
    
    #Equation 18
    # MAOM -> AGG
    if(MAOM>0){  
      f_MA_AG = parameters$rate_ma * scalar_wd * MAOM
    }else{
      f_MA_AG=0
    }
    
    #Equation 22
    # microbial growth flux, but is not used in mass balance
    
    #Equation 21
    # MIC -> atmosphere
    if(MIC>0){  
      f_MB_atm = f_LM_MB * (1 - (parameters$cue_ref - parameters$cue_t * (parameters$forc_st - parameters$tae_ref) ) )
    }else{
      f_MB_atm=0
    }
    
    #Equation 1
    dPOM = parameters$forc_npp * parameters$param_pi + f_AG_break * parameters$param_pa - f_PO_AG - f_PO_LM
    
    #Equation 7
    dLMWC = parameters$forc_npp * (1. - parameters$param_pi) - f_LM_leach + f_PO_LM - f_LM_MA - f_LM_MB + f_MB_turn * (1. - parameters$param_pb) + f_MA_LM
    
    #Equation 17
    dAGG = f_MA_AG + f_PO_AG - f_AG_break
    
    #Equation 20
    dMIC = f_LM_MB - f_MB_turn - f_MB_atm
    
    #Equation 19
    dMAOM = f_LM_MA - f_MA_LM + f_MB_turn * parameters$param_pb - f_MA_AG + f_AG_break * (1. - parameters$param_pa)
    
    return(list(c(dPOM, dLMWC, dAGG, dMIC, dMAOM)))
  })
}

state.SS <- matrix(nrow=dim(inputs)[1], ncol=5)

SS.Model.pools <- function(parsin,inputs) {
  parsin <- as.list(parsin)
  for(i in 1:dim(inputs)[1]){
    tryCatch({
      parsin$forc_st     <- inputs[i,5]
      parsin$forc_sw     <- inputs[i,6]
      parsin$forc_npp    <- inputs[i,7]
      parsin$param_qmax  <- inputs[i,8]
      parsin$param_pH    <- inputs[i,9]
      
      obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      
      state.SS[i,]      <- stode(y = obs.state, func = derivs.rangeland.MMRMM, parms = parsin, positive=TRUE)$y
      
    }, error=function(e){}
    )
  }
  
  modeled.pools <- as.data.frame(cbind(1:dim(inputs)[1], state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.pools) <- c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  
  return(modeled.pools)
}

pars <- selected_init

Obs.pools <- newinputs[which(newinputs$site %in% isite),]
Obs.pools <- Obs.pools[,c("site","SOM","MAOM","POM.AGG")]
Obs.pools$id <- 1:nrow(Obs.pools)
Obs.pools <- Obs.pools[,c("id","SOM","MAOM","POM.AGG")]


matrix_inf <- matrix(100, nrow = 993, ncol = 7)
df_inf <- as.data.frame(matrix_inf)
colnames(df_inf) <- c("POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
df_inf$"id" <- 1:993
df_inf <- df_inf[, c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")]

Objective.pools <- function(x, parset = names(x)) {
  #assign the values from x to parameters pars 
  pars[parset] <- x
  # a model to get the steady state for all the sites
  # it returns dataframe with column names of ("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  out <- SS.Model.pools(parsin = pars, inputs = inputs)
  # calculate the proportions of fraction
  out$rate_lw <- with(out,LMWC/SOM)
  out$rate_mc <- with(out, MIC/SOM)
  #using modCost as the loss function
  J_result <- tryCatch({
    if (any(out$rate_lw > 0.03) || any(out$rate_mc > 0.03)) {
      stop("invalid parameters!")
    } else{
      modCost(out, Obs.pools, x="id", weight = "none")
    }
  }, error = function(e) {
    # Handle the error, e.g., by returning a large penalty value or NA
    modCost(df_inf, Obs.pools, x="id", weight = "none")
    #J_result <- 1e10
  })
  return(J_result)
}



set.seed(123)
# Fit the model
Fit.pools <- modFit(
  f = Objective.pools,
  p = selected_init,
  lower = selected_lower,
  upper = selected_upper,
  method = "L-BFGS-B",
  #method = "Marq",
  control = list(
    ftol = 1e-06,
    ptol = 1e-06,
    gtol = 1e-06,
    nprint = 1
  ),
  jac = NULL,
  hessian = TRUE
)


saveRDS(Fit.pools,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/LBFGSB_opt_pars_rangeland_993_1125.RDS")
pars[names(selected_init)] <- Fit.pools$par
optim.pools.default <- SS.Model.pools(pars,inputs)

Obs.pools <- newinputs[which(newinputs$site %in% isite),]
Obs.pools <- Obs.pools[,c("site","SOM","MAOM","POM.AGG")]
Obs.pools$id <- 1:nrow(Obs.pools)
names(Obs.pools)[2:4] <- c("SOM.obs","MAOM.obs","POM.AGG.obs")
optim.pools.sim <- merge(optim.pools.default,Obs.pools,by="id")
optim.pools.sim <- optim.pools.sim[order(optim.pools.sim$site), ]

fit.SOM  <- summary(lm(optim.pools.sim$SOM.obs ~ optim.pools.sim$SOM))
SOM.bias <- optim.pools.sim$SOM.obs/993 - optim.pools.sim$SOM/993
dim.SOM  <- 993
RMSE_SOM <- sqrt(sum((optim.pools.sim$SOM.obs/993- optim.pools.sim$SOM/993)^2, na.rm=T)/dim.SOM)
MBE_SOM  <- mean(SOM.bias, na.rm=T)
MAE_SOM  <- mean(abs(SOM.bias), na.rm=T)



file_list <- list.files(path = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_drive/parsets/", pattern = "*.csv", full.names = TRUE)

combined_data <- file_list %>%
  lapply(read.csv) %>%        # Read each file into a list
  bind_rows() 

pars_result <- do.call(rbind, lapply(file_list, function(f) {
  df <- read.csv(f)
  df_t <- data.frame(t(df))[c("X", "FitPars"), ]
  colnames(df_t) <- df_t["X", ]
  df_out <- df_t[2, ]
  as.data.frame(lapply(df_out, as.numeric))
}))

lbfgsb_pars_global <- pars

lbfgsb_pars_clusters_39 <- pars_result

sitebysite_benchmark_pars <- sitebysite_benchmark_pars_outs_ode_100y_0526_init[,c(1:13)]

#-----------------------------------------------------------------------
# Parameter comparison across calibration strategies
common_param_names <- Reduce(
  intersect,
  list(
    names(lbfgsb_pars_global),
    colnames(lbfgsb_pars_clusters_39),
    colnames(sitebysite_benchmark_pars)
  )
)

if (length(common_param_names) != length(lbfgsb_pars_global)) {
  warning("Not all parameters found across datasets; plotting only common parameters.")
}

if (!length(common_param_names)) {
  stop("No overlapping parameter names available for plotting.")
}

param_levels <- common_param_names

x_labels  <- c("param_pi",       "param_pa",          "eact_pl",      "rate_pa",
         "rate_break",     "rate_leach",        "kaff_des",     "param_p1", 
         "eact_lb",        "rate_ma",           "cue_ref",      "tae_ref", 
         "matpot")


sxlabels <- c("P[i]",     "P[a]",       "plain(E)*alpha[pl]",  "K[pa]",
        "K[b]",     "K[l]",       "K[ld]",                "P[1]",      
  "plain(E)*alpha[lb]", "K[ma]",      "CUE[ref]",             "T[ae-ref]",  
  "phi")

param_label_map <- setNames(sxlabels, x_labels)

param_labeller <- as_labeller(
  setNames(param_label_map[param_levels], param_levels),
  label_parsed
)

reshape_params_long <- function(df, label) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(Parameter = character(), Value = numeric(), Source = character()))
  }
  subset_df <- df[, param_levels, drop = FALSE]
  data.frame(
    Parameter = rep(param_levels, each = nrow(subset_df)),
    Value = as.numeric(as.matrix(subset_df)),
    Source = label,
    stringsAsFactors = FALSE
  )
}

global_params_df <- data.frame(
  Parameter = param_levels,
  Value = as.numeric(lbfgsb_pars_global[param_levels]),
  Source = "Global",
  stringsAsFactors = FALSE
)

clusters_params_df <- reshape_params_long(lbfgsb_pars_clusters_39, "Bioregional")
benchmark_params_df <- reshape_params_long(sitebysite_benchmark_pars, "Site-specific")

param_boxplot_df <- rbind(global_params_df, clusters_params_df, benchmark_params_df)

param_boxplot_df$Parameter <- factor(param_boxplot_df$Parameter, levels = param_levels)
param_boxplot_df$Source <- factor(
  param_boxplot_df$Source,
  levels = c("Global", "Bioregional", "Site-specific")
)

param_boxplot <- ggplot(param_boxplot_df, aes(x = Parameter, y = Value, fill = Source)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.6,
    outlier.shape = 21,
    outlier.size = 1.5,
    outlier.stroke = 0.2
  ) +
  scale_x_discrete(
    breaks = param_levels,
    labels = parse(text = param_label_map[param_levels])
  ) +
  labs(
    title = "Parameter distributions across calibration sources",
    x = "Parameter",
    y = "Value",
    fill = "Dataset"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

param_boxplot

param_boxplot_faceted <- ggplot(param_boxplot_df, aes(x = Source, y = Value, fill = Source)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.stroke = 0.2) +
  facet_wrap(
    ~Parameter,
    scales = "free_y",
    labeller = labeller(Parameter = param_labeller)
  ) +
  labs(
    title = "Parameter distributions by calibration approach",
    x = NULL,
    y = "Value",
    fill = "Calibration"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.justification = c(0, 1),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 5, unit = "mm"),
    legend.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 11, face = "bold")
  )

param_boxplot_faceted
