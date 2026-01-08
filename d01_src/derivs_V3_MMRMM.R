
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