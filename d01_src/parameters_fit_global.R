rm(list = ls())

library(FME)
library(MASS)
#library(pracma)
library(ggplot2)
#library(corrplot)
library(deSolve)
library(lattice)
library(rsq)
#library(ppcor)
library(caret)
#library(gbm)
library(rtop)
#library(EnvStats)
library(readr)
library(tools)
#-------------------------------------------------------------------------------
#continental_input_sites_update.csv
newinputs <- read.csv("/projects/mingxi/calibration_mm2/d01_data/input/continental_input_sites_update.txt")

#continental_input_sites.csv
#newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/continental_input_sites.txt")
newinputs <- newinputs[!(newinputs$site %in% paste0("S", 51:193)),]
#set.seed(123)

flist <- list.files("/projects/mingxi/calibration_mm2/d01_data/rangeland_driving/",full.names = TRUE,recursive = TRUE)
basefile_names <- basename(flist)


parToFit.pools <- c(#param_pi = parameters$param_pi,
  param_pa = parameters$param_pa,
  kaff_pl = parameters$kaff_pl,
  alpha_pl = parameters$alpha_pl,
  eact_pl = parameters$eact_pl,
  rate_pa = parameters$rate_pa,
  rate_break = parameters$rate_break,
  #rate_leach = parameters$rate_leach,
  #kaff_des = parameters$kaff_des,
  param_p1 = parameters$param_p1,
  #param_p2 = parameters$param_p2,
  kaff_lb = parameters$kaff_lb,
  alpha_lb = parameters$alpha_lb, 
  eact_lb = parameters$eact_lb, 
  rate_bd = parameters$rate_bd,
  rate_ma = parameters$rate_ma,
  cue_ref = parameters$cue_ref,
  #cue_t = parameters$cue_t,
  #tae_ref = parameters$tae_ref,
  #matpot = parameters$matpot,
  #lambda = parameters$lambda,
  porosity = parameters$porosity,
  #kamin = parameters$kamin,
  param_pb = parameters$param_pb
) 

parToFit.lower <- c(#param_pi = 0,
  param_pa = 0,
  kaff_pl = 0,
  alpha_pl = 0,
  eact_pl = 0,
  rate_pa = 0,
  rate_break = 0,
  #rate_leach = 0,
  #kaff_des = 0,
  param_p1 = 0,
  #param_p2 = 0,
  kaff_lb = 0,
  alpha_lb = 0, 
  eact_lb = 0, 
  rate_bd = 0,
  rate_ma = 0,
  cue_ref = 0,
  #cue_t = -Inf,
  #tae_ref = -Inf
  #matpot = 0,
  #lambda = 0,
  porosity = 0,
  #kamin = 0,
  param_pb = 0
)

parToFit.upper <- c(#param_pi = 1,
  param_pa = 1,
  kaff_pl = Inf,
  alpha_pl = Inf,
  eact_pl = Inf,
  rate_pa = Inf,
  rate_break = Inf,
  #rate_leach = Inf,
  #kaff_des = Inf,
  param_p1 = Inf,
  #param_p2 = Inf,
  kaff_lb = Inf,
  alpha_lb = Inf, 
  eact_lb = Inf, 
  rate_bd = Inf,
  rate_ma = Inf,
  cue_ref = 1,
  #cue_t = Inf,
  #tae_ref = Inf
  #matpot = Inf,
  #lambda = Inf,
  porosity = 1,
  #kamin = Inf,
  param_pb = 1
)

parToFit.sets <- c(#param_pi = parameters$param_pi,
  param_pa = parameters$param_pa,
  kaff_pl = parameters$kaff_pl,
  alpha_pl = parameters$alpha_pl,
  eact_pl = parameters$eact_pl,
  rate_pa = parameters$rate_pa,
  rate_break = parameters$rate_break,
  #rate_leach = parameters$rate_leach,
  #kaff_des = parameters$kaff_des,
  param_p1 = parameters$param_p1,
  #param_p2 = parameters$param_p2,
  kaff_lb = parameters$kaff_lb,
  alpha_lb = parameters$alpha_lb, 
  eact_lb = parameters$eact_lb, 
  rate_bd = parameters$rate_bd,
  rate_ma = parameters$rate_ma,
  cue_ref = parameters$cue_ref,
  #cue_t = parameters$cue_t,
  #tae_ref = parameters$tae_ref,
  #matpot = parameters$matpot,
  #lambda = parameters$lambda,
  porosity = parameters$porosity,
  #kamin = parameters$kamin,
  param_pb = parameters$param_pb
) 

parToFit.lower <- c(#param_pi = 0,
  param_pa = 0,
  kaff_pl = 5000,
  alpha_pl = 1.25e12,
  eact_pl = 60000,
  rate_pa = 0.01,
  rate_break = 0.0095,
  #rate_leach = 0,
  #kaff_des = 0,
  param_p1 = 0.05,
  #param_p2 = 0,
  kaff_lb = 100,
  alpha_lb = 1.0e12, 
  eact_lb = 54000, 
  rate_bd = 0.0018,
  rate_ma = 0.003,
  cue_ref = 0.3,
  #cue_t = -Inf,
  #tae_ref = -Inf
  #matpot = 0,
  #lambda = 0,
  porosity = 0.3,
  #kamin = 0,
  param_pb = 0
)

parToFit.upper <- c(#param_pi = 1,
  param_pa = 1,
  kaff_pl = 15000,
  alpha_pl = 3.75e12,
  eact_pl = 70000,
  rate_pa = 0.03,
  rate_break = 0.0285,
  #rate_leach = Inf,
  #kaff_des = Inf,
  param_p1 = 0.5,
  #param_p2 = Inf,
  kaff_lb = 1000,
  alpha_lb = 3.9e12, 
  eact_lb = 61000, 
  rate_bd = 0.0054,
  rate_ma = 0.03,
  cue_ref = 0.9,
  #cue_t = Inf,
  #tae_ref = Inf
  #matpot = Inf,
  #lambda = Inf,
  porosity = 0.9,
  #kamin = Inf,
  param_pb = 1
)


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


isite <- file_path_sans_ext(basefile_names)

inputs <- newinputs[,c("site","POM.AGG","MAOM","SOM","SoilTMP.C","SoilMoi.m3m3","NPP.gC.m2.d","qmax.gC.m2","pH_CaCl2","BD.mg.cm3","depth")]
#inputs <- newinputs[,c("site","POM.AGG","MAOM","SOM","SoilTMP.C","SoilMoi.m3m3","NPP.gC.m2.d","qmax.gC.m2","pH.CaCl2","BD.mg.cm3","depth")]

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

derivs.rangeland.dyna <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
  
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
    
    #Equation 11
    #param_qmax = 1 * 1000 * 0.86 * 80   
    
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
    
    return(list(c(dPOM, dLMWC, dAGG, dMIC, dMAOM)))
  })
}

derivs_SS_wrapper <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
  output <- derivs.rangeland.dyna(step.num,state,parameters,forc_st,forc_sw,forc_npp)
  return(list(output[[1]][1:5]))
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

#SStime <- 10000
#run.steps <- seq(1,10000)

SS.Model.states.old <- function(parsin,inputs) {
  state.SS <- matrix(nrow=dim(inputs)[1], ncol=5)
  parsin <- as.list(parsin)
  for(i in 1:dim(inputs)[1]){
    tryCatch({

      parsin$param_qmax  <- inputs[i,8]
      parsin$param_pH    <- inputs[i,9]
      
      obs.state = c(POM = inputs[i,2], LMWC = 1, AGG = 1, MIC = 1, MAOM=inputs[i,3])
      
      forc_st  <- approxfun(1:SStime, rep(inputs[i,5],SStime))
      forc_sw  <- approxfun(1:SStime, rep(inputs[i,6],SStime))
      forc_npp <- approxfun(1:SStime, rep(inputs[i,7],SStime))
      ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
      ode.sss  <- as.data.frame(ode.sss)
      
      POM10000   <- tail(ode.sss$POM, 1)
      AGG10000   <- tail(ode.sss$AGG, 1)
      MAOM10000  <- tail(ode.sss$MAOM, 1)
      MIC10000   <- tail(ode.sss$MIC, 1)
      LMWC10000  <- tail(ode.sss$LMWC, 1)
      
      
      state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
      
    }, error=function(e){}
    )
  }
  
  modeled.pools <- as.data.frame(cbind(1:dim(inputs)[1], state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.pools) <- c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  
  return(modeled.pools)
}

SStime <- 36500
run.steps <- seq(1,36500)

SS.Model.states <- function(parsin,inputs) {
  state.SS <- matrix(nrow=dim(inputs)[1], ncol=5)
  parsin <- as.list(parsin)
  for(i in 1:dim(inputs)[1]){
    tryCatch({
      
      parsin$param_qmax  <- inputs[i,8]
      parsin$param_pH    <- inputs[i,9]
      sitename           <- inputs[i,1]
      
      driving_avg        <- read.csv(paste0("/projects/mingxi/calibration_mm2/d01_data/rangeland_avg_driving/",sitename,".txt"))
      
      obs.state = c(POM = inputs[i,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs[i,3])
      
      forc_st  <- approxfun(1:SStime, rep(driving_avg$forc_st,100))
      forc_sw  <- approxfun(1:SStime, rep(driving_avg$forc_st,100))
      forc_npp <- approxfun(1:SStime, rep(driving_avg$forc_st,100))
      ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
      ode.sss  <- as.data.frame(ode.sss)
      
      POM10000   <- tail(ode.sss$POM, 1)
      AGG10000   <- tail(ode.sss$AGG, 1)
      MAOM10000  <- tail(ode.sss$MAOM, 1)
      MIC10000   <- tail(ode.sss$MIC, 1)
      LMWC10000  <- tail(ode.sss$LMWC, 1)
      
      
      state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
      
    }, error=function(e){}
    )
  }
  
  modeled.pools <- as.data.frame(cbind(1:dim(inputs)[1], state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.pools) <- c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  
  return(modeled.pools)
}
#parameters        <- NULL
#parameters.file   <- read.table("/media/DATADRIVE1/Model/Millennial/Fortran/MillennialV2/simulationv2/soilpara_in_dyna_new.txt")
#parameters        <- as.list(parameters.file$V2)
#names(parameters) <- parameters.file$V1
#pars <- unlist(parameters)

pars <- parToFit.VR.opt

Obs.pools <- newinputs[which(newinputs$site %in% isite),]
Obs.pools <- Obs.pools[,c("site","SOM","MAOM","POM.AGG")]
Obs.pools$id <- 1:nrow(Obs.pools)
Obs.pools <- Obs.pools[,c("id","SOM","MAOM","POM.AGG")]

# matrix_inf <- matrix(100, nrow = 1120, ncol = 7)
# df_inf <- as.data.frame(matrix_inf)
# colnames(df_inf) <- c("POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
# df_inf$"id" <- 1:1120
# df_inf <- df_inf[, c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")]

#For each observed variable present in Obs.pools (which are SOM, MAOM, and POM.AGG), 
#it will find the corresponding column in out (by matching column names).
#For each matching pair, it computes the residual:
#residual=simulated value from out − observed value from Obs.pools
#Then it squares the residuals, sums them per variable, and finally sums the variable costs
#to obtain a single cost value.

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

Objective.states <- function(x, parset = names(x)) {
  #assign the values from x to parameters pars 
  pars[parset] <- x
  # a model to get the steady state for all the sites
  # it returns dataframe with column names of ("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  out <- SS.Model.states(parsin = pars, inputs = inputs)
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
  p = parToFit.VR.opt,
  lower = parToFit.lower.opt,
  upper = parToFit.upper.opt,
  #method = "L-BFGS-B",
  method = "Marq",
  jac = NULL,
  hessian = TRUE
)

set.seed(123)

Fit.pools <- modFit(
  f = Objective.states,
  p = parToFit.VR.opt,
  lower = parToFit.lower.opt,
  upper = parToFit.upper.opt,
  method = "L-BFGS-B",
  #method = "Marq",
  jac = NULL,
  hessian = TRUE
)
saveRDS(Fit.pools,"/projects/mingxi/calibration_mm2/d03_output/lbfgsb_opt_pars_rangeland_init.RDS")
#pars[names(parToFit.sets)] <- Fit.pools$par
#pars[names(parToFit.sets)] <- parToFit.VR
optim.pools.VR <- SS.Model.pools(Fit.pools$par,inputs)

#optim_steadystate_20250205 <- SS.Model.pools(Fit.pools$par,inputs)
#optim_steadystate_20250205 <- optim_steadystate_20250205[,c(1:7)]
#names(optim_steadystate_20250205)[6:7] <- c("MAOM","SOM")
LBFGSB_opt_ode_993 <- merge(optim.pools.VR,Obs.pools,by="id")
#optim_steadystate_993 <- optim_steadystate_993[order(optim_steadystate_993$site),c(8,1:7)]
#names(optim_steadystate_993)[7:8] <- c("MAOM","SOM")

saveRDS(LBFGSB_opt_ode_993,"/projects/mingxi/calibration_mm2/d03_output/lbfgsb_opt_ode_rangeland_init.RDS")



LBFGSB_fitted      <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/L-BFGS-B-fitted-pars-rangeland-933.RDS")
optim.pools.LBFGSB <- SS.Model.pools(LBFGSB_fitted$par,inputs)
optim.pools.LBFGSB <- merge(optim.pools.LBFGSB,Obs.pools,by="id")
optim.pools.LBFGSB.993 <- optim.pools.LBFGSB[!(optim.pools.LBFGSB$site %in% paste0("S", 51:193)),]

optim.pools.VR$ratio_ma <- with(optim.pools.VR,MAOM/(POM+LMWC+AGG+MIC+MAOM))
optim.pools.VR$ratio_lw <- with(optim.pools.VR,LMWC/(POM+LMWC+AGG+MIC+MAOM))
optim.pools.VR$ratio_mc <- with(optim.pools.VR,MIC/(POM+LMWC+AGG+MIC+MAOM))


Obs.pools <- newinputs[which(newinputs$site %in% isite),]
Obs.pools <- Obs.pools[,c("site","SOM","MAOM","POM.AGG")]
Obs.pools$id <- 1:nrow(Obs.pools)
names(Obs.pools)[2:4] <- c("SOM.obs","MAOM.obs","POM.AGG.obs")
optim.pools.sim <- merge(optim.pools.VR,Obs.pools,by="id")
optim.pools.sim <- optim.pools.sim[order(optim.pools.sim$site), ]
saveRDS(optim.pools.sim,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim.pools.rangeland.933.RDS.VR")


#TEST THE r-SQUARE
fit.SOM <- summary(lm(optim.pools.sim$SOM.obs ~ optim.pools.sim$SOM))

################################################################################
################################################################################
################################################################################
################################################################################

derivs.rangeland.MV3 <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
  
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
  
  parameters <- modifyList(MV3_params, parameters)
  
  with(as.list(c(state,parameters)), {
    
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
    
    return(list(c(dPOM, dLMWC, dAGG, dMIC, dMAOM)))
  })
}

derivs_SS_wrapper <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
  output <- derivs.rangeland.MV3(step.num,state,parameters,forc_st,forc_sw,forc_npp)
  return(list(output[[1]][1:5]))
}


SStime <- 10000
run.steps <- seq(1,10000)

library(foreach)
library(doParallel)

c1 <- makeCluster(10)
registerDoParallel(c1)

SS.Model.steadystate <- function(parsin,inputs) {
  parsin <- as.list(parsin)
  results <- foreach(i = 1:dim(inputs)[1],.packages = c("doParallel","FME","dplyr","lubridate")) %dopar% {
  #for(i in 1:dim(inputs)[1]){
    tryCatch({
      
      parsin$param_qmax  <- inputs[i,8]
      parsin$param_pH    <- inputs[i,9]
      
      obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      
      forc_st  <- approxfun(1:SStime, rep(inputs[i,5],SStime))
      forc_sw  <- approxfun(1:SStime, rep(inputs[i,6],SStime))
      forc_npp <- approxfun(1:SStime, rep(inputs[i,7],SStime))
      
      ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
      
      ode.sss       <- as.data.frame(ode.sss)
      #POM1000       <- ode.sss$POM[nrow(ode.sss)]
      #AGG1000       <- ode.sss$AGG[nrow(ode.sss)]
      #MAOM1000      <- ode.sss$MAOM[nrow(ode.sss)]
      #MIC1000       <- ode.sss$MIC[nrow(ode.sss)]
      #LMWC1000      <- ode.sss$LMWC[nrow(ode.sss)]
      
      POM_final   <- tail(ode.sss$POM, 1)
      LMWC_final  <- tail(ode.sss$LMWC, 1)
      AGG_final   <- tail(ode.sss$AGG, 1)
      MIC_final   <- tail(ode.sss$MIC, 1)
      MAOM_final  <- tail(ode.sss$MAOM, 1)
      
      # Return a named vector for this site
      c(POM = POM_final, LMWC = LMWC_final, AGG = AGG_final, MIC = MIC_final, MAOM = MAOM_final)
      
      #state.SS[i,]  <- c(POM1000,LMWC1000,AGG1000,MIC1000,MAOM1000)
    
    }, error=function(e){
      c(POM = NA, LMWC = NA, AGG = NA, MIC = NA, MAOM = NA)
    }
    )
  }
  
  ode_outs <- do.call(rbind, results)
  state.SS <- as.data.frame(ode_outs)
  
  modeled.pools <- as.data.frame(cbind(1:dim(inputs)[1], state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.pools) <- c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  
  return(modeled.pools)
}

SS.Model.odestates   <- function(parsin,inputs) {
  
  parsin <- as.list(parsin)
  results <- foreach(i = 1:dim(inputs)[1],.packages = c("doParallel","FME","dplyr","lubridate")) %dopar% {
    #for(i in 1:dim(inputs)[1]){
    tryCatch({
      
      parsin$param_qmax  <- inputs[i,8]
      parsin$param_pH    <- inputs[i,9]
      
      obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      
      forc_st  <- approxfun(1:SStime, rep(inputs[i,5],SStime))
      forc_sw  <- approxfun(1:SStime, rep(inputs[i,6],SStime))
      forc_npp <- approxfun(1:SStime, rep(inputs[i,7],SStime))
      
      ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
      
      ode.sss  <- as.data.frame(ode.sss)
      
      ode.sss$id <- i
      
      ode.sss
      
      
    }, error=function(e){
      data.frame(time = NA, POM = NA, LMWC = NA, AGG = NA, MIC = NA, MAOM = NA, id = i)
    }
    )}
  
  ode_outs <- do.call(rbind, results)
  #ode_outs <- as.data.frame(ode_outs)
  
  return(ode_outs)
}


stopCluster(c1)

#to get the steady state data frame
optim_odestate_20250205 <- SS.Model.steadystate(Fit.pools$par,inputs)

optim_odestate_20250205_combined <- merge(optim_odestate_20250205[,c(1:7)],Obs.pools,by="id")

optim_odestate_993 <- optim_odestate_20250205_combined[order(optim_odestate_20250205_combined$site),c(8,1:7)]
names(optim_odestate_993)[7:8] <- c("MAOM","SOM")

saveRDS(optim_odestate_993,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/rangelands_odestate_993.RDS")


#to get the time series dataframe
odestates_20250205 <- SS.Model.odestates(Fit.pools$par,inputs)
odestates_20250205 <- ode_outs




all_MAOM_sim_plot <- odestates_20250205[,c(1,6,7)]

colors <- rainbow(length(unique(all_MAOM_sim_plot$id)), alpha=0.5)
plot(range(1:10000), range(all_MAOM_sim_plot$MAOM), type = "n", xlab = "Months", ylab = "Value")
for(i in seq_along(unique(all_MAOM_sim_plot$id))) {
  site <- unique(all_MAOM_sim_plot$id)[i]
  site_data <- all_MAOM_sim_plot[all_MAOM_sim_plot$id == site,]
  #lines(site_data$months, site_data$MAOM, col = rgb(0,0,0,0.2))
  lines(site_data$time, site_data$MAOM, col = colors[i])
}

#plot

plot.ode <- function(output){
  par(mar = c(5, 4, 4, 4) + 0.3)  # Adjust margins to fit the second axis
  plot(output[,"time"], output[,"POM"], xlab="time (month)", ylab="C fractions for POM, AGG and MAOM (gC m-2)", 
       col=2, type="l", ylim=c(0, max(output[,-1], na.rm=T)))
  lines(output[,"time"], output[,"AGG"], col=4)
  lines(output[,"time"], output[,"MAOM"], col=6)
  
  par(new = TRUE)  # Allow a second plot on the same graph
  plot(output[,"time"], output[,"LMWC"], axes=FALSE, xlab="", ylab="", 
       col=3, type="l", ylim=c(0, max(output[,"LMWC"], output[,"MIC"], na.rm=T)))
  lines(output[,"time"], output[,"MIC"], col=5)
  axis(4)  # Add the secondary y-axis
  mtext("C fractions for LMWC and MIC (gC m-2)", side=4, line=3)
  
  legend("top", c("POM", "AGG", "MAOM", "LMWC", "MIC"), col=c(2,4,6,3,5), lwd=1)
}


plot.ode(output)



