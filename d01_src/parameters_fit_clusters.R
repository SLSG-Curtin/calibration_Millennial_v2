library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(deSolve)
library(lattice)
library(FME)
library(readr)
library(tools)
library(dplyr)
library(lubridate)
library(tools)
################################################################################
# fit the model, and get optimized parameters
source("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d02_script/functions/derivs_V3_MMRMM.R")
################################################################################

important_params_set6 <- c('param_pa', 'rate_pa', 'rate_break', 'rate_leach',
                           'rate_ma',  'matpot',  'eact_pl',    'eact_lb',
                           'cue_ref',  'tae_ref', 'kaff_des',   'param_p1',
                           'param_pi')

parToFit.VR.opt <- c(
  param_pa = 0.4,
  eact_pl = 63339,
  rate_pa = 0.012,
  rate_break = 0.026,
  rate_leach = 1.5e-03,
  kaff_des = 0.02,
  param_p1 = 0.078,
  #kaff_lb = 710.8,
  eact_lb = 60428,
  rate_ma = 0.0052,
  cue_ref = 0.53,
  tae_ref = 15,
  matpot = 15,
  param_pi = 0.66
  #lambda = 2.1e-04,
  #porosity = 0.62,
  #kamin = 0.2
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
  #kaff_lb = 1.0e+02,
  eact_lb = 5.0e+04,
  rate_ma = 1.0e-03,
  cue_ref = 3.0e-01,
  tae_ref = 7.5,
  matpot = 7.5,
  param_pi = 0
  #lambda = 1.05e-04,
  #porosity = 0.3,
  #kamin = 0.1
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
  #kaff_lb = 1.0e+03,
  eact_lb = 7.5e+04,
  rate_ma = 1.0e-01,
  cue_ref = 9.0e-01,
  tae_ref = 22.5,
  matpot = 22.5,
  param_pi = 1
  #lambda = 3.15e-04,
  #porosity = 0.9,
  #kamin = 0.3
)

#Defines time variables
# num.years = 100 
# run.steps <- seq(1,num.years*365)
# run.steps.minus.one <- run.steps[1:length(run.steps)-1]
# SStime <- 100*365

#newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/rangelands_input_sites.csv")
#newinputs$site <- gsub("\\+AF8-", "_", newinputs$site)
#newinputs$y <- gsub("\\+AC0", "", newinputs$y)
#newinputs$y <- as.numeric(newinputs$y)
pars <- parToFit.VR.opt

newinputs <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/input/bioclimatic_clusters_new.txt")
flist <- list.files("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/rangeland_driving/",full.names = TRUE,recursive = TRUE)
basefile_names <- basename(flist)
isite <- file_path_sans_ext(basefile_names)
names(newinputs)[4:12] <- c("SOM","MAOM","POM.AGG","SoilTMP.C","SoilMoi.m3m3","NPP.gC.m2.d","qmax.gC.m2","pH_CaCl2","BD.mg.cm3")
inputs <- newinputs[,c("site","POM.AGG","MAOM","SOM","SoilTMP.C","SoilMoi.m3m3","NPP.gC.m2.d","qmax.gC.m2","pH_CaCl2","BD.mg.cm3","depth","REC.ID")]
inputs <- inputs[which(inputs$site %in% isite),]
count_result <- inputs %>% count(REC.ID)

Obs.clusters <- inputs[,c("site","SOM","MAOM","POM.AGG","REC.ID")]

Obs.clusters.25 <- Obs.clusters[which(Obs.clusters$REC.ID == 25),]

sample_dataframe <- function(df, sample_size = 39) {
  if (nrow(df) < sample_size) {
    stop("Sample size cannot be larger than the number of rows in the dataframe")
  }
  
  set.seed(12345)
  # Randomly sample row indices
  sampled_rows <- sample(1:nrow(df), size = sample_size, replace = FALSE)
  
  # Return the sampled rows
  return(df[sampled_rows, ])
}

create_five_samples <- function(df, sample_size = 39) {

  set.seed(12345)
  # Create a random permutation of all row indices
  all_indices <- sample(1:nrow(df), size = nrow(df), replace = FALSE)
  
  # Create a list to store the five samples
  samples <- list()
  
  # Split the indices into five equal parts
  for (i in 1:5) {
    start_idx <- (i-1) * sample_size + 1
    end_idx <- i * sample_size
    group_indices <- all_indices[start_idx:end_idx]
    samples[[i]] <- df[group_indices, ]
  }
  
  return(samples)
}

Obs.clusters.samples <- create_five_samples(Obs.clusters.25, 39)


#length(unique(sample_25))
#length(unique(Obs.clusters.25$site))
Obs.clusters.25_sample1 <- Obs.clusters.samples[[1]]
Obs.clusters.25_sample2 <- Obs.clusters.samples[[2]]
Obs.clusters.25_sample3 <- Obs.clusters.samples[[3]]
Obs.clusters.25_sample4 <- Obs.clusters.samples[[4]]
Obs.clusters.25_sample5 <- Obs.clusters.samples[[5]]

Obs.clusters.25_sample1$REC.ID <- 251
Obs.clusters.25_sample2$REC.ID <- 252
Obs.clusters.25_sample3$REC.ID <- 253
Obs.clusters.25_sample4$REC.ID <- 254
Obs.clusters.25_sample5$REC.ID <- 255

Obs.cluster.minus <- Obs.clusters[which(Obs.clusters$REC.ID != 25),]

Obs.clusters.update <- rbind(Obs.cluster.minus,
                             Obs.clusters.25_sample1,
                             Obs.clusters.25_sample2,
                             Obs.clusters.25_sample3,
                             Obs.clusters.25_sample4,
                             Obs.clusters.25_sample5)

length(unique(Obs.clusters.update$site))

inputs_sample1 <- inputs[which(inputs$site %in% Obs.clusters.25_sample1$site),]
inputs_sample2 <- inputs[which(inputs$site %in% Obs.clusters.25_sample2$site),]
inputs_sample3 <- inputs[which(inputs$site %in% Obs.clusters.25_sample3$site),]
inputs_sample4 <- inputs[which(inputs$site %in% Obs.clusters.25_sample4$site),]
inputs_sample5 <- inputs[which(inputs$site %in% Obs.clusters.25_sample5$site),]

inputs_sample1$REC.ID <- 251
inputs_sample2$REC.ID <- 252
inputs_sample3$REC.ID <- 253
inputs_sample4$REC.ID <- 254
inputs_sample5$REC.ID <- 255


inputs.update <- rbind(inputs[which(inputs$REC.ID != 25),],
                       inputs_sample1,
                       inputs_sample2,
                       inputs_sample3,
                       inputs_sample4,
                       inputs_sample5)



length(unique(inputs.update$site))

#Obs.clusters <- Obs.clusters[which(Obs.clusters$REC.ID != 62),]
flists <- sub(".txt","",basename(flist))

first_data <- read.csv(flist[1])

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
    param_pH   = 6.2
    #forc_st    = 25,
    #forc_sw    = 0.19,
    #forc_npp   = 0.65
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

derivs_SS_wrapper <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
   output <- derivs.rangeland.dyna(step.num,state,parameters,forc_st,forc_sw,forc_npp)
   return(list(output[[1]][1:5]))
}

SS.Model.clusters <- function(parsin,inputs.cluster) {
  state.SS <- matrix(nrow=dim(inputs.cluster)[1], ncol=5)
  parsin_stode <- as.list(parsin)
  parsin_ode   <- as.list(parsin)
  for(j in 1:dim(inputs.cluster)[1]){
    
    tryCatch({
      parsin_stode$forc_st     <- inputs.cluster[j,5]
      parsin_stode$forc_sw     <- inputs.cluster[j,6]
      parsin_stode$forc_npp    <- inputs.cluster[j,7]
      parsin_stode$param_qmax  <- inputs.cluster[j,8]
      parsin_stode$param_pH    <- inputs.cluster[j,9]
      parsin_ode$param_qmax    <- inputs.cluster[j,8]
      parsin_ode$param_pH      <- inputs.cluster[j,9]
      
      #obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      obs.state = c(POM = inputs.cluster[j,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs.cluster[j,3])
      
      ss_state      <- stode(y = obs.state, func = derivs.rangeland.MMRMM, 
                             parms = parsin_stode, positive=TRUE)
      
      if(!attr(ss_state,"steady")){
        run.steps <- seq(1,10000)
        
        forc_st  <- approxfun(1:10000, rep(inputs.cluster[j,5],10000))
        forc_sw  <- approxfun(1:10000, rep(inputs.cluster[j,6],10000))
        forc_npp <- approxfun(1:10000, rep(inputs.cluster[j,7],10000))
        ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin_ode, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
        ode.sss  <- as.data.frame(ode.sss)
        
        POM10000   <- mean(tail(ode.sss$POM, 12))
        AGG10000   <- mean(tail(ode.sss$AGG, 12))
        MAOM10000  <- mean(tail(ode.sss$MAOM, 12))
        MIC10000   <- mean(tail(ode.sss$MIC, 12))
        LMWC10000  <- mean(tail(ode.sss$LMWC, 12))
        
        state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
        #stop("invalid parameters!")
        print("ode running")
      }else{
        state.SS[j,] <- ss_state$y
      }
        
    }, error=function(e){}
    )
  }
  
  modeled.sets <- as.data.frame(cbind(inputs.cluster$site,1:dim(inputs.cluster)[1],state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.sets) <- c("site","id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  modeled.sets <- modeled.sets %>% mutate(across(-1, as.numeric))
  return(modeled.sets)
}

SS.Model.sets <- function(parsin,inputs.cluster) {
  
  state.SS  <- matrix(nrow=dim(inputs.cluster)[1], ncol=5)
  parsin_stode <- as.list(parsin)
  parsin_ode   <- as.list(parsin)
  
  for(j in 1:dim(inputs.cluster)[1]){
    
    tryCatch({
      
      parsin_stode$forc_st     <- inputs.cluster[j,5]
      parsin_stode$forc_sw     <- inputs.cluster[j,6]
      parsin_stode$forc_npp    <- inputs.cluster[j,7]
      parsin_stode$param_qmax  <- inputs.cluster[j,8]
      parsin_stode$param_pH    <- inputs.cluster[j,9]
      
      parsin_ode$param_qmax    <- inputs.cluster[j,8]
      parsin_ode$param_pH      <- inputs.cluster[j,9]
      
      #obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      obs.state = c(POM = inputs.cluster[j,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs.cluster[j,3])
      
      ss_state      <- stode(y = obs.state, func = derivs.rangeland.MMRMM, 
                             parms = parsin_stode, positive=TRUE)
      
      if(!attr(ss_state,"steady")){
        run.steps <- seq(1,10000)
        
        forc_st  <- approxfun(1:10000, rep(inputs.cluster[j,5],10000))
        forc_sw  <- approxfun(1:10000, rep(inputs.cluster[j,6],10000))
        forc_npp <- approxfun(1:10000, rep(inputs.cluster[j,7],10000))
        ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin_ode, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
        ode.sss  <- as.data.frame(ode.sss)
        
        POM10000   <- mean(tail(ode.sss$POM, 12))
        AGG10000   <- mean(tail(ode.sss$AGG, 12))
        MAOM10000  <- mean(tail(ode.sss$MAOM, 12))
        MIC10000   <- mean(tail(ode.sss$MIC, 12))
        LMWC10000  <- mean(tail(ode.sss$LMWC, 12))
        
        state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
        #stop("invalid parameters!")
        print("ode running")
      }else{
        state.SS[j,] <- ss_state$y
      }
      
    }, error=function(e){}
    )
  }
  modeled.sets <- as.data.frame(cbind(inputs.cluster$site,1:dim(inputs.cluster)[1],state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.sets) <- c("site","id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  modeled.sets <- modeled.sets %>% mutate(across(-1, as.numeric))
  return(modeled.sets)
}

SStime <- 36500
run.steps <- seq(1,36500)


SS.Model.clusters.drive <- function(parsin,inputs.cluster,climate_driving) {
  
  state.SS <- matrix(nrow=dim(inputs.cluster)[1], ncol=5)
  parsin_stode <- as.list(parsin)
  parsin_ode   <- as.list(parsin)
  for(j in 1:dim(inputs.cluster)[1]){
    
    tryCatch({
      
      parsin_stode$forc_st     <- inputs.cluster[j,5]
      parsin_stode$forc_sw     <- inputs.cluster[j,6]
      parsin_stode$forc_npp    <- inputs.cluster[j,7]
      parsin_stode$param_qmax  <- inputs.cluster[j,8]
      parsin_stode$param_pH    <- inputs.cluster[j,9]
      
      parsin_ode$param_qmax    <- inputs.cluster[j,8]
      parsin_ode$param_pH      <- inputs.cluster[j,9]
      
      #obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      obs.state = c(POM = inputs.cluster[j,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs.cluster[j,3])
      
      ss_state      <- stode(y = obs.state, func = derivs.rangeland.MMRMM, 
                             parms = parsin_stode, positive=TRUE)
      
      if(!attr(ss_state,"steady")){
        run.steps <- seq(1,10800)
        
        forc_st  <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_st,50))
        forc_sw  <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_sw,50))
        forc_npp <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_npp,50))
        ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin_ode, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
        ode.sss  <- as.data.frame(ode.sss)
        
        POM10000   <- mean(tail(ode.sss$POM, 12))
        AGG10000   <- mean(tail(ode.sss$AGG, 12))
        MAOM10000  <- mean(tail(ode.sss$MAOM, 12))
        MIC10000   <- mean(tail(ode.sss$MIC, 12))
        LMWC10000  <- mean(tail(ode.sss$LMWC, 12))
        
        state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
        #stop("invalid parameters!")
        print("ode running")
      }else{
        state.SS[j,] <- ss_state$y
      }
      
    }, error=function(e){}
    )
  }
  
  modeled.sets <- as.data.frame(cbind(inputs.cluster$site,1:dim(inputs.cluster)[1],state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.sets) <- c("site","id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  modeled.sets <- modeled.sets %>% mutate(across(-1, as.numeric))
  return(modeled.sets)
}

SS.Model.sets.drive <- function(parsin,inputs.cluster,climate_driving) {
  
  state.SS  <- matrix(nrow=dim(inputs.cluster)[1], ncol=5)
  parsin_stode <- as.list(parsin)
  parsin_ode   <- as.list(parsin)
  
  for(j in 1:dim(inputs.cluster)[1]){
    
    tryCatch({
      
      parsin_stode$forc_st     <- inputs.cluster[j,5]
      parsin_stode$forc_sw     <- inputs.cluster[j,6]
      parsin_stode$forc_npp    <- inputs.cluster[j,7]
      parsin_stode$param_qmax  <- inputs.cluster[j,8]
      parsin_stode$param_pH    <- inputs.cluster[j,9]
      
      parsin_ode$param_qmax    <- inputs.cluster[j,8]
      parsin_ode$param_pH      <- inputs.cluster[j,9]
      
      #obs.state = c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
      obs.state = c(POM = inputs.cluster[j,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs.cluster[j,3])
      
      ss_state      <- stode(y = obs.state, func = derivs.rangeland.MMRMM, 
                             parms = parsin_stode, positive=TRUE)
      
      if(!attr(ss_state,"steady")){
        
        #run.steps <- seq(1,10800)
        #forc_st  <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_st,50))
        #forc_sw  <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_sw,50))
        #forc_npp <- approxfun(1:10800, rep(climate_driving[[inputs.cluster[j,"site"]]]$forc_npp,50))
        #ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin_ode, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
        #ode.sss  <- as.data.frame(ode.sss)
        #
        #POM10000   <- mean(tail(ode.sss$POM, 12))
        #AGG10000   <- mean(tail(ode.sss$AGG, 12))
        #MAOM10000  <- mean(tail(ode.sss$MAOM, 12))
        #MIC10000   <- mean(tail(ode.sss$MIC, 12))
        #LMWC10000  <- mean(tail(ode.sss$LMWC, 12))
        
        sitename           <- inputs.cluster[j,1]
        
        driving_avg        <- read.csv(paste0("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d01_data/rangeland_avg_driving/",sitename,".txt"))
        
        obs.state = c(POM = inputs.cluster[j,2], LMWC = 6, AGG = 6, MIC = 6, MAOM=inputs.cluster[j,3])
        
        forc_st  <- approxfun(1:SStime, rep(driving_avg$forc_st,100))
        forc_sw  <- approxfun(1:SStime, rep(driving_avg$forc_sw,100))
        forc_npp <- approxfun(1:SStime, rep(driving_avg$forc_npp,100))
        
        ode.sss  <- ode(y = obs.state, times=run.steps, func= derivs_SS_wrapper, parms = parsin, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
        ode.sss  <- as.data.frame(ode.sss)
        
        sim_update <- rollapply(ode.sss, width = 365, by = 365, FUN = mean)
        sim_update <- as.data.frame(sim_update)
        
        
        POM10000   <- tail(sim_update$POM, 1)
        AGG10000   <- tail(sim_update$AGG, 1)
        MAOM10000  <- tail(sim_update$MAOM, 1)
        MIC10000   <- tail(sim_update$MIC, 1)
        LMWC10000  <- tail(sim_update$LMWC, 1)
        
        
        state.SS[j,]  <- c(POM10000,LMWC10000,AGG10000,MIC10000,MAOM10000)
        #stop("invalid parameters!")
        print("ode running")
      }else{
        state.SS[j,] <- ss_state$y
      }
      
    }, error=function(e){}
    )
  }
  modeled.sets <- as.data.frame(cbind(inputs.cluster$site,1:dim(inputs.cluster)[1],state.SS, rowSums(state.SS),(state.SS[,1]+state.SS[,2]+state.SS[,3]+state.SS[,4])))
  names(modeled.sets) <- c("site","id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
  modeled.sets <- modeled.sets %>% mutate(across(-1, as.numeric))
  return(modeled.sets)
}

average.cluster.climate <- function(flist_cluster,first_data){
  
  #sum data
  sum_data           <- matrix(0, nrow = 216, ncol = 3)
  colnames(sum_data) <- c("forc_npp","forc_sw","forc_st")
  # Counter for number of files
  file_count <- 0
  
  # Read all files and sum the values
  for (file in flist_cluster) {
    # Read the file (adjust read function based on your file type)
    data <- read.csv(file)
    data <- data[,c(4:6)]
    # Make sure the dimensions match
    if (all(dim(data) == dim(sum_data))) {
      sum_data <- sum_data + as.matrix(data)
      file_count <- file_count + 1
    } else {
      warning(paste("Skipping file", file, "due to dimension mismatch"))
    }
  }
  
  # Calculate averages across files
  average_data <- sum_data / file_count
  
  # If you need to convert back to a data frame
  average_df <- as.data.frame(average_data)
  
  average_drive <- cbind(first_data[,c(2:3)],average_df)
  
  return(average_drive)
  
}

store.cluster.climate <- function(flist_cluster){
  
  climate_list <- list()

  # Read all files and sum the values
  for (i in seq_along(flist_cluster)) {
    # Read the file (adjust read function based on your file type)
    data <- read.csv(flist_cluster[i],header = TRUE)
    
    drive_site <- sub(".txt","",basename(flist_cluster[i]))
    
    data <- data[,c(4:6)]
    climate_list[[drive_site]] <- data
    
  }
  

  return(climate_list)
  
}
#modCost <- 3244954
#default_modCost <- 29706087
Objective.clusters <- function(x, parset = names(x)) {
  pars[parset] <- x
  out <- SS.Model.clusters(parsin = pars, inputs = inputs.cluster)
  #out <- SS.Model.sets(parsin = pars, inputs = inputs.cluster)
  out$rate_ma <- with(out,MAOM/SOM)
  out$rate_lw <- with(out,LMWC/SOM)
  out$rate_mc <- with(out, MIC/SOM)
  J_result <- tryCatch({
    if (any(out$rate_ma < 0.04) || any(out$rate_lw > 0.03) || any(out$rate_mc > 0.03)) {
      stop("invalid parameters!")
    } else{
      #modCost(out, Obs.cluster, x="id", weight = "none")
      modCost(out[,c(2:11)], Obs.cluster[,c(2:5)], x="id", weight = "none")
    }
  }, error = function(e) {
    # Handle the error, e.g., by returning a large penalty value or NA
    matrix_inf <- matrix(100, nrow = nrow(Obs.cluster), ncol = 7)
    df_inf <- as.data.frame(matrix_inf)
    colnames(df_inf) <- c("POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
    df_inf$"id" <- 1:nrow(Obs.cluster)
    df_inf <- df_inf[, c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")]
    modCost(df_inf, Obs.cluster[,c(2:5)], x="id", weight = "none")
  })
  return(J_result)
}

Objective.clusters.drive <- function(x, parset = names(x)) {
  pars[parset] <- x
  out <- SS.Model.clusters.drive(parsin = pars, inputs = inputs.cluster,climate_driving)
  #out <- SS.Model.sets(parsin = pars, inputs = inputs.cluster)
  out$rate_ma <- with(out,MAOM/SOM)
  out$rate_lw <- with(out,LMWC/SOM)
  out$rate_mc <- with(out, MIC/SOM)
  J_result <- tryCatch({
    if (any(out$rate_ma < 0.04) || any(out$rate_lw > 0.03) || any(out$rate_mc > 0.03)) {
      stop("invalid parameters!")
    } else{
      #modCost(out, Obs.cluster, x="id", weight = "none")
      modCost(out[,c(2:11)], Obs.cluster[,c(2:5)], x="id", weight = "none")
    }
  }, error = function(e) {
    # Handle the error, e.g., by returning a large penalty value or NA
    matrix_inf <- matrix(100, nrow = nrow(Obs.cluster), ncol = 7)
    df_inf <- as.data.frame(matrix_inf)
    colnames(df_inf) <- c("POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")
    df_inf$"id" <- 1:nrow(Obs.cluster)
    df_inf <- df_inf[, c("id","POM","LMWC","AGG","MIC","MAOM","SOM","POM.AGG")]
    modCost(df_inf, Obs.cluster[,c(2:5)], x="id", weight = "none")
  })
  return(J_result)
}


library(foreach)
library(doParallel)
c1 <- makeCluster(10)
registerDoParallel(c1)

clusters <- unique(Obs.clusters.update$REC.ID)

foreach(i = 1:39,.packages = c("doParallel","FME","dplyr","lubridate")) %dopar% {
  
  set.seed(123)
  cluster = clusters[i]
  inputs.cluster <- inputs[which(inputs$REC.ID == cluster),]
  Obs.cluster    <- Obs.clusters[which(Obs.clusters$REC.ID == cluster),]
  Obs.cluster$id <- 1:nrow(Obs.cluster)
  Obs.cluster    <- Obs.cluster[,c("site","id","SOM","MAOM","POM.AGG","REC.ID")]
  
  pars <- parToFit.VR.opt
  
  #first_data <- read.csv(flist[1])
  
  flist_cluster <- flist[flists %in% unique(Obs.cluster$site)]
  
  #average_driving <- average.cluster.climate(flist_cluster,first_data)
  
  climate_driving <- store.cluster.climate(flist_cluster)

  Fit.sets <- modFit(
    f = Objective.clusters.drive,
    p = parToFit.VR.opt,
    lower = parToFit.lower.opt,
    upper = parToFit.upper.opt,
    method = "L-BFGS-B",
    jac = NULL,
    # control = list(
    #   ftol = 1e-06,
    #   ptol = 1e-06,
    #   gtol = 1e-06,
    #   nprint = 1
    # ),
    hessian = TRUE
  )

  # optim_func<-optim(prior_params,Jparam_new, method = "L-BFGS-B",
  #                   lower = parToFit.lower, 
  #                   upper = parToFit.upper)
  
  outtab <- cbind(Fit.sets$par, parToFit.lower.opt, parToFit.VR.opt, parToFit.upper.opt)
  colnames(outtab) <- c("FitPars","Lower","default","Upper")
  write.csv(outtab, file= paste0("/projects/mingxi/calibration_mm2/d03_output/clusters_par13_drive/",cluster,"_parset.csv"))
  #pars[names(parToFit.sets)] <- Fit.sets$par
  #optim.sets <- SS.Model.pools(Fit.sets$par, inputs.cluster)
  #optim.sets <- SS.Model.sets(Fit.sets$par, inputs.cluster)
  
  outtab <- read.csv("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_drive/parsets/5_parset.csv")
  
  Fit.sets <- outtab$FitPars
  names(Fit.sets) <- outtab$X
  
  optim.sets <- SS.Model.sets.drive(Fit.sets, inputs.cluster,climate_driving)
  optim.sets$rate_lw <- with(optim.sets,LMWC/SOM)
  optim.sets$rate_mc <- with(optim.sets, MIC/SOM)
  write.csv(optim.sets, file= paste0("/projects/mingxi/calibration_mm2/d03_output/clusters_par13_drive/",cluster,"_optim_sets.csv"))
  
}

#flist

foreach(i = 1:43,.packages = c("doParallel","FME","dplyr","lubridate")) %dopar% {
  
  set.seed(123)
  cluster = clusters[i]
  inputs.cluster <- inputs.update[which(inputs.update$REC.ID == cluster),]
  Obs.cluster    <- Obs.clusters.update[which(Obs.clusters.update$REC.ID == cluster),]
  Obs.cluster$id <- 1:nrow(Obs.cluster)
  Obs.cluster    <- Obs.cluster[,c("site","id","SOM","MAOM","POM.AGG","REC.ID")]
  
  pars <- parToFit.VR.opt
  
  flist_cluster <- flist[flists %in% unique(Obs.cluster$site)]
  
  climate_driving <- store.cluster.climate(flist_cluster)
  
  opt_filename <- paste0("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_drive/parsets/",cluster,"_parset.csv")
  
  outtab <- read.csv(opt_filename)
  
  Fit.sets <- outtab$FitPars
  names(Fit.sets) <- outtab$X
  
  optim.sets <- SS.Model.sets.drive(Fit.sets, inputs.cluster,climate_driving)
  optim.sets$rate_lw <- with(optim.sets,LMWC/SOM)
  optim.sets$rate_mc <- with(optim.sets, MIC/SOM)
  write.csv(optim.sets, file= paste0("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_par13_drive/optim_sets/",cluster,"_optim_sets.csv"))
  
}


stopCluster(c1)

# file_list <- list.files(path = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/clusters_modFit_default_LBFGSB_15/clusters_stode_set/", pattern = "*.csv", full.names = TRUE)
# #library(dplyr)
# # Read and combine all CSV files into a single data frame
# combined_data <- file_list %>%
#   lapply(read.csv) %>%        # Read each file into a list
#   bind_rows()                 # Combine the list into a single data frame
# 
# # View the combined data
# #print(combined_data)
# combined_data$POM.AGG <- with(combined_data,POM+AGG)
# colnames(combined_data)[4:10] <- c("POM1000","LMWC1000","AGG1000","MIC1000","MAOM1000","SOM1000","POM.AGG1000")
# sim_all <- merge(combined_data,Obs.clusters,by="site")
# #colnames(sim_all)[13:15] <- c("SOM","MAOM","POM.AGG")
# #fit.SOM <- summary(lm(sim_all$SOM.x ~ sim_all$SOM.y))
# #RMSE_kg <- sqrt(sum((sim_all$SOM.y/1000- sim_all$SOM.x/1000)^2, na.rm=T)/993)
# #cor(sim_all$SOM.y, sim_all$SOM.x, method = "pearson")
# sim_all <- sim_all[,c(1,3:16)]
# names(sim_all)[3:9] <- c("POM.sim","LMWC.sim","AGG.sim","MIC.sim","MAOM.sim","SOM.sim","POM.AGG.sim")
# names(sim_all)[12:14] <- c("SOM.obs","MAOM.obs","POM.AGG.obs")
# 
# saveRDS(sim_all,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/clusters/lbfgsb_default_rangelands_stode_sim_993.RDS")
# 
# 
# output <- ode.sss
# 
# plot.states <- function(output){
#   par(mar = c(5, 4, 4, 4) + 0.3)  # Adjust margins to fit the second axis
#   plot(output[,"time"], output[,"POM"], xlab="time (month)", ylab="C fractions for POM, AGG and MAOM (gC m-2)", 
#        col=2, type="l", ylim=c(0, max(output[,-1], na.rm=T)))
#   lines(output[,"time"], output[,"AGG"], col=4)
#   lines(output[,"time"], output[,"MAOM"], col=6)
#   
#   par(new = TRUE)  # Allow a second plot on the same graph
#   plot(output[,"time"], output[,"LMWC"], axes=FALSE, xlab="", ylab="", 
#        col=3, type="l", ylim=c(0, max(output[,"LMWC"], output[,"MIC"], na.rm=T)))
#   lines(output[,"time"], output[,"MIC"], col=5)
#   axis(4)  # Add the secondary y-axis
#   mtext("C fractions for LMWC and MIC (gC m-2)", side=4, line=3)
#   
#   legend("top", c("POM", "AGG", "MAOM", "LMWC", "MIC"), col=c(2,4,6,3,5), lwd=1)
# }
# 
# 
# plot.states(output)
