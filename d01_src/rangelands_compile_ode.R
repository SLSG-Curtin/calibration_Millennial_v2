library(rtop)
library(deSolve)
library(FME)
library(rsq)
library(foreach)
library(doParallel)
library(zoo)

#source("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d02_script/MM_v3.R")
source("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d02_script/functions/MM_v3.1.R")
################################################################################
#parameters        <- NULL
#parameters.file   <- read.table("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/soilpara_in_dyna_new.txt")
#parameters.file   <- read.table(paste0(rootdir,"Fortran/MillennialV2/simulationv2/soilpara_in_vf.txt"))
#parameters        <- as.list(parameters.file$V2)
#names(parameters) <- parameters.file$V1
#parsin            <- parameters
#par0              = array(parsin,dim = c(24, 1))

################################################################################
#'param_pa', 'eact_pl', 'eact_lb', 'rate_ma', 'cue_ref'

ParaName    = c('param_pi'  ,'param_pa'  ,'kaff_pl' ,'alpha_pl','eact_pl' ,'rate_pa',
                'rate_break','rate_leach','kaff_des','param_p1','param_p2','kaff_lb',
                'alpha_lb'  ,'eact_lb'   ,'rate_bd' ,'rate_ma' ,'cue_ref' ,'cue_t'  ,
                'tae_ref'   ,'matpot'    ,'lambda'  ,'porosity','kamin'   ,'param_pb')

ParaNameNew = c('param_pi'  ,'param_pa'  ,'kaff_pl' ,'alpha_pl','eact_pl' ,'rate_pa',
                'rate_break','rate_leach','kaff_des','param_p1','param_p2','kaff_lb',
                'alpha_lb'  ,'eact_lb'   ,'rate_bd' ,'rate_ma' ,'cue_ref' ,'cue_t'  ,
                'tae_ref'   ,'matpot'    ,'lambda'  ,'porosity','kamin'   ,'param_pb', 'param_qmax','param_pH')

# I try six parameters: param_pi, rate_leach, tae_ref, matpot, lambda, kamin
# but actual sens parameters: kaff_pl, param_p2, matpot, kamin, lambda



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

#excluded_params <- c('kaff_pl', 'param_p2', 'matpot', 'kamin', 'lambda')

important_params_set1 <- c('param_pa', 'eact_pl', 'eact_lb', 'rate_ma', 'cue_ref')

important_params_set2 <- c('param_pa', 'kaff_pl', 'alpha_pl', 'eact_pl', 'rate_pa', 
                           'rate_break', 'param_p1', 'kaff_lb', 'alpha_lb','eact_lb', 
                           'rate_bd', 'rate_ma', 'cue_ref', 'porosity', 'param_pb')

important_params_set3 <- c('param_pa', 'rate_pa', 'rate_break', 'rate_leach',
                           'rate_ma',  'matpot',  'lambda',     'porosity', 
                           'kamin',    'eact_pl', 'eact_lb',    'cue_ref', 
                           'tae_ref',  'kaff_des','param_p1',  'kaff_lb')

important_params_set5 <- c('param_pa', 'rate_pa', 'rate_break', 'rate_leach',
                           'rate_ma',   'matpot',   'porosity',    'eact_pl', 
                           'eact_lb',  'cue_ref',    'tae_ref',   'kaff_des',
                           'kaff_lb', 'param_p1',   'param_pi',    'rate_bd')

important_params_set6 <- c('param_pa', 'rate_pa', 'rate_break', 'rate_leach',
                           'rate_ma',  'matpot',  'eact_pl',    'eact_lb',
                           'cue_ref',  'tae_ref', 'kaff_des',   'param_p1',
                           'param_pi')
#selected_params <- params_df[!params_df$Parameter %in% excluded_params, ]

selected_params <- params_df[params_df$Parameter %in% important_params_set6, ]
#
inputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites_update.txt")


################################################################################

derivs.rangeland.MV3 <- function(step.num,state,params_sceua,forc_st,forc_sw,forc_npp) {
  
  MV3_params <- list(
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
    param_pH   = 6.2
  )
  
  parameters <- modifyList(MV3_params, params_sceua)
  
  with(as.list(c(state,parameters)), {
    
    #POM <- max(state[1], 0)
    #LMWC <- max(state[2], 0)
    #AGG <- max(state[3], 0)
    #MIC <- max(state[4], 0)
    #MAOM <- max(state[5], 0)
    # Soil type properties  
    #Equation 10
    kaff_lm = exp(-parameters$param_p1 * parameters$param_pH - parameters$param_p2) * parameters$kaff_des
    
    # Hydrological properties
    
    #Equation 4
    scalar_wd = (forc_sw(step.num) / parameters$porosity)^0.5
    
    #Equation 15
    scalar_wb = exp(parameters$lambda * -parameters$matpot) * (parameters$kamin + (1 - parameters$kamin) * ((parameters$porosity - forc_sw(step.num)) / parameters$porosity)^0.5) * scalar_wd
    
    # Decomposition
    
    gas_const <- 8.31446
    
    #Equation 3
    vmax_pl = parameters$alpha_pl * exp(-parameters$eact_pl / (gas_const * (forc_st(step.num) + 273.15)))
    
    #Equation 2
    # POM -> LMWC
    if(POM>0 && MIC>0){
      f_PO_LM = vmax_pl * scalar_wd * POM * MIC / (parameters$kaff_pl + MIC)
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
    
    #Equation 4
    vmax_lb = parameters$alpha_lb * exp(-parameters$eact_lb / (gas_const * (forc_st(step.num) + 273.15)))
    
    #Equation 13
    # LMWC -> MIC
    if(LMWC>0 && MIC>0){
      f_LM_MB = vmax_lb * scalar_wb * MIC * LMWC / (parameters$kaff_lb + LMWC)
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
    if(MIC>0 && LMWC>0){ 
      f_MB_atm = f_LM_MB * (1 - (parameters$cue_ref - parameters$cue_t * (forc_st(step.num) - parameters$tae_ref) ) )
    }else{
      f_MB_atm=0
    }
    
    # Update state variables
    
    #Equation 1
    dPOM = forc_npp(step.num) * parameters$param_pi + f_AG_break * parameters$param_pa - f_PO_AG - f_PO_LM
    
    #Equation 7
    dLMWC = forc_npp(step.num) * (1. - parameters$param_pi) - f_LM_leach + f_PO_LM - f_LM_MA - f_LM_MB + f_MB_turn * (1. - parameters$param_pb) + f_MA_LM
    
    #Equation 17
    dAGG = f_MA_AG + f_PO_AG - f_AG_break
    
    #Equation 20
    dMIC = f_LM_MB - f_MB_turn - f_MB_atm
    
    #Equation 19
    dMAOM = f_LM_MA - f_MA_LM + f_MB_turn * parameters$param_pb - f_MA_AG + f_AG_break * (1. - parameters$param_pa)
    
    #if(POM <= 1e-6 && dPOM < 0) dPOM <- 0
    #if(LMWC <= 1e-6 && dLMWC < 0) dLMWC <- 0
    #if(AGG <= 1e-6 && dAGG < 0) dAGG <- 0
    #if(MIC <= 1e-6 && dMIC < 0) dMIC <- 0
    #if(MAOM <= 1e-6 && dMAOM < 0) dMAOM <- 0
    outputs_delta <- c(dPOM, dLMWC, dAGG, dMIC, dMAOM)
    
    if (any(abs(unlist(outputs_delta)) > 100)){
      stop("something is wrong!")
    }
    
    new_values <- state + outputs_delta
    
    if (any(unlist(new_values) < 0)){
      stop("something is wrong!")
    }
    
    return(list(c(dPOM, dLMWC, dAGG, dMIC, dMAOM)))
  })
}

derivs_Aus_wrapper <- function(step.num,state,params_sceua,forc_st,forc_sw,forc_npp) {
  output <- derivs.rangeland.MV3(step.num,state,params_sceua,forc_st,forc_sw,forc_npp)
  return(list(output[[1]][1:5]))
}

SStime     <- 36500
run.steps  <- seq(1,36500)

sce_compile_ode <- function(params,sitename,dat) {
  
  obs_row  <- inputs[which(inputs$site == sitename),]
  
  initial_values <- c(POM = obs_row$POM.AGG, LMWC = 6, AGG = 6, MIC = 6, MAOM=obs_row$MAOM)  
  
  #100years
  forc_st_fun  <- approxfun(1:SStime, rep(dat$forc_st, 100))
  forc_sw_fun  <- approxfun(1:SStime, rep(dat$forc_sw, 100))
  forc_npp_fun <- approxfun(1:SStime, rep(dat$forc_npp,100))
  
  #25years
  #forc_st_fun  <- approxfun(1:SStime, rep(dat$forc_st, 1))
  #forc_sw_fun  <- approxfun(1:SStime, rep(dat$forc_sw, 1))
  #forc_npp_fun <- approxfun(1:SStime, rep(dat$forc_npp,1))
  #forc_st_fun  <- approxfun(1:SStime, rep(obs_row$SoilTMP.C,   SStime))
  #forc_sw_fun  <- approxfun(1:SStime, rep(obs_row$SoilMoi.m3m3,SStime))
  #forc_npp_fun <- approxfun(1:SStime, rep(obs_row$NPP.gC.m2.d, SStime))
  #forc_st  <- forc_st_fun(1:SStime)
  #forc_sw  <- forc_sw_fun(1:SStime)
  #forc_npp <- forc_npp_fun(1:SStime)
  
  param_qmax  <- obs_row$qmax
  param_pH    <- obs_row$pH
  
  #pars_matrix <- c(params,param_qmax,param_pH)
  #pars_matrix <- unlist(pars_matrix)
  
  #params_sceua <- data.frame(Names = c(selected_params$Parameter,"param_qmax","param_pH"),
  #                           Parameters = c(params,param_qmax,param_pH))
  
  params_sceua <- c(as.list(setNames(params, selected_params$Parameter)),
                    param_qmax = param_qmax,
                    param_pH = param_pH)
  
  
  ODE.MMRMM <- ode(y = initial_values, times=run.steps,  func = derivs_Aus_wrapper, parms = params_sceua, forc_st=forc_st_fun, forc_sw=forc_sw_fun, forc_npp=forc_npp_fun, method="rk4")
  #ODE.MMRMM <- forcing_delta_MM(initial_values,params_sceua,forcing_dat)
  
  ODE.MMRMM <- as.data.frame(ODE.MMRMM)
  # get the mean of equlibrium state, suggesting 120 rows
  ODE.MMRMM$SOM <- with(ODE.MMRMM,POM+LMWC+AGG+MIC+MAOM)
  
  # ODE.MMRMM  <- cbind(dat[,1:3],ODE.MMRMM)
  # 
  # sim_update <- aggregate(
  #   cbind(POM, LMWC, AGG, MIC, MAOM, SOM) ~ year,
  #   data = ODE.MMRMM,
  #   FUN  = function(x) mean(x, na.rm = TRUE)
  # )
  sim_update <- rollapply(ODE.MMRMM, width = 365, by = 365, FUN = mean)
  sim_update <- as.data.frame(sim_update)
  #sim_update$time <- 1:100
  
  POM10000       <- mean(tail(sim_update$POM, 1))
  LMWC10000      <- mean(tail(sim_update$LMWC, 1))
  AGG10000       <- mean(tail(sim_update$AGG, 1))
  MIC10000       <- mean(tail(sim_update$MIC, 1))
  MAOM10000      <- mean(tail(sim_update$MAOM,1))
  SOM10000       <- mean(tail(sim_update$SOM,1))
  
  
  #POMAGG_dev  <- abs(POM10000 + AGG10000  - obs_row$POM.AGG)
  #MAOM_dev    <- abs(MAOM10000 - obs_row$MAOM)
  #SOM_dev     <- abs(SOM10000  - obs_row$SOM)
  
  
  return(list(c(obs_row$SOM,obs_row$MAOM,obs_row$POM.AGG,SOM10000, MAOM10000, POM10000, AGG10000, MIC10000, LMWC10000)))
  
}

sce_compile_ts  <- function(params,sitename,dat) {
  
  obs_row  <- inputs[which(inputs$site == sitename),]
  
  initial_values <- c(POM = obs_row$POM.AGG, LMWC = 6, AGG = 6, MIC = 6, MAOM=obs_row$MAOM)  
  
  #100years
  forc_st_fun  <- approxfun(1:SStime, rep(dat$forc_st, 100))
  forc_sw_fun  <- approxfun(1:SStime, rep(dat$forc_sw, 100))
  forc_npp_fun <- approxfun(1:SStime, rep(dat$forc_npp,100))
  
  #25years
  #forc_st_fun  <- approxfun(1:SStime, rep(dat$forc_st, 1))
  #forc_sw_fun  <- approxfun(1:SStime, rep(dat$forc_sw, 1))
  #forc_npp_fun <- approxfun(1:SStime, rep(dat$forc_npp,1))
  #forc_st_fun  <- approxfun(1:SStime, rep(obs_row$SoilTMP.C,   SStime))
  #forc_sw_fun  <- approxfun(1:SStime, rep(obs_row$SoilMoi.m3m3,SStime))
  #forc_npp_fun <- approxfun(1:SStime, rep(obs_row$NPP.gC.m2.d, SStime))
  #forc_st  <- forc_st_fun(1:SStime)
  #forc_sw  <- forc_sw_fun(1:SStime)
  #forc_npp <- forc_npp_fun(1:SStime)
  
  param_qmax  <- obs_row$qmax
  param_pH    <- obs_row$pH
  
  #pars_matrix <- c(params,param_qmax,param_pH)
  #pars_matrix <- unlist(pars_matrix)
  
  #params_sceua <- data.frame(Names = c(selected_params$Parameter,"param_qmax","param_pH"),
  #                           Parameters = c(params,param_qmax,param_pH))
  
  params_sceua <- c(as.list(setNames(params, selected_params$Parameter)),
                    param_qmax = param_qmax,
                    param_pH = param_pH)
  
  
  ODE.MMRMM <- ode(y = initial_values, times=run.steps,  func = derivs_Aus_wrapper, parms = params_sceua, forc_st=forc_st_fun, forc_sw=forc_sw_fun, forc_npp=forc_npp_fun, method="rk4")
  #ODE.MMRMM <- forcing_delta_MM(initial_values,params_sceua,forcing_dat)
  
  ODE.MMRMM <- as.data.frame(ODE.MMRMM)
  # get the mean of equlibrium state, suggesting 120 rows
  ODE.MMRMM$SOM <- with(ODE.MMRMM,POM+LMWC+AGG+MIC+MAOM)
  
  
  ODE.MMRMM <- as.data.frame(ODE.MMRMM[1:365,])
  # ODE.MMRMM  <- cbind(dat[,1:3],ODE.MMRMM)
  # 
  # sim_update <- aggregate(
  #   cbind(POM, LMWC, AGG, MIC, MAOM, SOM) ~ year,
  #   data = ODE.MMRMM,
  #   FUN  = function(x) mean(x, na.rm = TRUE)
  # )
  #sim_update <- rollapply(ODE.MMRMM, width = 365, by = 365, FUN = mean)
  #sim_update <- as.data.frame(sim_update)
  #sim_update$time <- 1:100
  
  #POM10000       <- mean(tail(sim_update$POM, 1))
  #LMWC10000      <- mean(tail(sim_update$LMWC, 1))
  #AGG10000       <- mean(tail(sim_update$AGG, 1))
  #MIC10000       <- mean(tail(sim_update$MIC, 1))
  #MAOM10000      <- mean(tail(sim_update$MAOM,1))
  #SOM10000       <- mean(tail(sim_update$SOM,1))
  #
  #
  #POMAGG_dev  <- abs(POM10000 + AGG10000  - obs_row$POM.AGG)
  #MAOM_dev    <- abs(MAOM10000 - obs_row$MAOM)
  #SOM_dev     <- abs(SOM10000  - obs_row$SOM)
  
  #return(list(c(obs_row$SOM,obs_row$MAOM,obs_row$POM.AGG,SOM10000, MAOM10000, POM10000, AGG10000, SOM_dev, MAOM_dev,POMAGG_dev)))
  
  return(ODE.MMRMM)
  
}

safe_compile_fn <- function(params,sitename,dat) {
  J_result <- tryCatch({
    # Replace 'actual_objective_function' with your actual function
    sce_compile_ode(params,sitename,dat)
    #sce_compile_ts(params,sitename,dat)
  }, error = function(e) {
    # Handle the error, e.g., by returning a large penalty value or NA
    return(Inf)  # or NA, depending on how sceua() handles it
  })
  
  return(J_result)
}


set.seed(123)

flist <- list.files("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/rangeland_daily_driving/",full.names = TRUE,recursive = TRUE)

flists <- sub(".txt","",basename(flist))

#sites_recalib <- readRDS("/projects/mingxi/calibration_mm2/d03_output/sites_recalibrate.rds")

#flist_extra <- flist[flists %in% unique(sites_recalib$site)]

fnames <- sub(".txt","",basename(flist))

pars_set <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sitebysite_benchmark_pars_outs_ode_100y_0526_init.rds")
pars_set <- pars_set[is.finite(rowSums(pars_set)), ]
pars_set <- pars_set[,1:13]
names(pars_set) <- selected_params$Parameter

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)

#flist <- flist[1:2]
# for equilibrium
pars.list <- foreach(i=seq(flist),.packages = c("dplyr","magrittr","doParallel","deSolve","zoo","rsq")) %dopar% {
  
  set.seed(123)
  
  driving_input <- read.csv(flist[i])
  
  dates <- as.Date(with(driving_input, ISOdate(Year, Month, Day)), tz = "UTC")
  
  # compute doy and drop Feb 29
  doy <- as.POSIXlt(dates)$yday + 1
  keep <- !(format(dates, "%m-%d")=="02-29")
  
  # aggregate each variable
  agg_NPP  <- tapply(driving_input$forc_npp[keep],  doy[keep], mean, na.rm=TRUE)
  agg_moist<- tapply(driving_input$forc_sw[keep], doy[keep], mean, na.rm=TRUE)
  agg_temp <- tapply(driving_input$forc_st[keep],doy[keep], mean, na.rm=TRUE)
  
  # combine into a data.frame
  # "forc_st", "forc_sw", "forc_npp"
  agg <- data.frame(
    doy             = as.integer(names(agg_NPP)),
    forc_npp        = as.numeric(agg_NPP),
    forc_sw         = as.numeric(agg_moist),
    forc_st         = as.numeric(agg_temp)
  )
  agg$avg_date <- as.Date(agg$doy, origin = as.Date("2000-12-31"))
  agg <- agg[1:365,]
  
  sitename <- gsub("*.txt","",basename(flist[i]))
  
  selected_row <- pars_set[sitename, ]
  
  sim_output <-  safe_compile_fn(selected_row,sitename,agg)
  
  #sim_output = sceua(safe_ode_fn, pars = selected_params$Initial, lower = selected_params$Lower, upper = selected_params$Upper,maxn = 1000, sitename =sitename, dat = agg, plog = selected_params$Logor)
  
  sim_output <- t(as.data.frame(unlist(sim_output)))
  
  rownames(sim_output) <- sitename
  
  return(sim_output)
  
}

# for time-series
pars.list <- foreach(i=seq(flist),.packages = c("dplyr","magrittr","doParallel","deSolve","zoo","rsq")) %dopar% {
  
  set.seed(123)
  
  driving_input <- read.csv(flist[i])
  
  dates <- as.Date(with(driving_input, ISOdate(Year, Month, Day)), tz = "UTC")
  
  # compute doy and drop Feb 29
  doy <- as.POSIXlt(dates)$yday + 1
  keep <- !(format(dates, "%m-%d")=="02-29")
  
  # aggregate each variable
  agg_NPP  <- tapply(driving_input$forc_npp[keep],  doy[keep], mean, na.rm=TRUE)
  agg_moist<- tapply(driving_input$forc_sw[keep], doy[keep], mean, na.rm=TRUE)
  agg_temp <- tapply(driving_input$forc_st[keep],doy[keep], mean, na.rm=TRUE)
  
  # combine into a data.frame
  # "forc_st", "forc_sw", "forc_npp"
  agg <- data.frame(
    doy             = as.integer(names(agg_NPP)),
    forc_npp        = as.numeric(agg_NPP),
    forc_sw         = as.numeric(agg_moist),
    forc_st         = as.numeric(agg_temp)
  )
  agg$avg_date <- as.Date(agg$doy, origin = as.Date("2000-12-31"))
  agg <- agg[1:365,]
  
  sitename <- gsub("*.txt","",basename(flist[i]))
  
  selected_row <- pars_set[sitename, ]
  
  sim_output <-  safe_compile_fn(selected_row,sitename,agg)
  
  #sim_output = sceua(safe_ode_fn, pars = selected_params$Initial, lower = selected_params$Lower, upper = selected_params$Upper,maxn = 1000, sitename =sitename, dat = agg, plog = selected_params$Logor)
  
  #sim_output <-  sim_output[,c(2:12)]
  
  sim_output$site <- sitename
  
  return(sim_output)
  
}

filtered_list <- Filter(function(x) !identical(x, pars.list[[48]]), pars.list)

new_sim_outs <- do.call(rbind, filtered_list)

new_sim_outs <- as.data.frame(new_sim_outs)

colnames(new_sim_outs) <- c('SOM','MAOM','POM.AGG','SOM1000', 'MAOM1000', 'POM1000', 'AGG1000' ,'MIC10000', 'LMWC10000')

#
saveRDS(new_sim_outs,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/VR_new_benchmark_sim_0615.rds")

parallel::stopCluster(cl)


