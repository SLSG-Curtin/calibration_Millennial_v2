library(FME)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(corrplot)
library(deSolve)
library(lattice)
library(raster)
library(rsq)
library(ppcor)
library(caret)
library(gbm)
library(rtop)
library(EnvStats)

library(caTools)
library(calibrater)
library(SAFER)
library(ggplot2)
library(gridExtra)
library(matrixStats)

# setup the model
# also new 15 important parameters
parameters$param_p1   = parameters[10]
#parameters$param_p2   = parameters[11]
parameters$kaff_des   = parameters[9]
parameters$porosity   = parameters[22]
#parameters$lambda     = parameters[21]
#parameters$matpot     = parameters[20]
#parameters$kamin      = parameters[23]
#parameters$alpha_pl   = parameters[4]
parameters$eact_pl    = parameters[5]
#parameters$kaff_pl    = parameters[3]
parameters$rate_pa    = parameters[6]
parameters$rate_break = parameters[7]
parameters$rate_leach = parameters[8]
#parameters$alpha_lb   = parameters[13]
parameters$eact_lb    = parameters[14]
#parameters$kaff_lb    = parameters[12]
parameters$rate_bd    = parameters[15]
parameters$rate_ma    = parameters[16]
parameters$cue_ref    = parameters[17]
#parameters$cue_t      = parameters[18]
parameters$tae_ref    = parameters[19]
parameters$param_pi   = parameters[1]
parameters$param_pa   = parameters[2]
parameters$param_pb   = parameters[24]

derivs.Aus.pool.MMRMM <- function(step.num,state,parameters,forc_st,forc_sw,forc_npp) {
  with(as.list(c(state,parameters)), {
    
    param_p1   = parameters[10]
    param_p2   = parameters[11]
    kaff_des   = parameters[9]
    porosity   = parameters[22]
    lambda     = parameters[21]
    matpot     = parameters[20]
    kamin      = parameters[23]
    alpha_pl   = parameters[4]
    eact_pl    = parameters[5]
    kaff_pl    = parameters[3]
    rate_pa    = parameters[6]
    rate_break = parameters[7]
    rate_leach = parameters[8]
    alpha_lb   = parameters[13]
    eact_lb    = parameters[14]
    kaff_lb    = parameters[12]
    rate_bd    = parameters[15]
    rate_ma    = parameters[16]
    cue_ref    = parameters[17]
    cue_t      = parameters[18]
    tae_ref    = parameters[19]
    param_pi   = parameters[1]
    param_pa   = parameters[2]
    param_pb   = parameters[24]
    
    
    # Soil type properties  
    #Equation 10
    kaff_lm = exp(-param_p1 * 7 - param_p2) * kaff_des
    
    #Equation 11
    param_qmax = 1 * 1000 * 0.86 * 80   
    
    # Hydrological properties
    
    #Equation 4
    scalar_wd = (forc_sw(step.num) / porosity)^0.5
    
    #Equation 15
    scalar_wb = exp(lambda * -matpot) * (kamin + (1 - kamin) * ((porosity - forc_sw(step.num)) / porosity)^0.5) * scalar_wd
    
    # Decomposition
    
    gas_const <- 8.31446
    
    #Equation 3
    vmax_pl = alpha_pl * exp(-eact_pl / (gas_const * (forc_st(step.num) + 273.15)))
    
    #Equation 2
    # POM -> LMWC
    if(POM>0 && MIC>0){
      f_PO_LM = vmax_pl * scalar_wd * POM * MIC / (kaff_pl + MIC)
    }else{
      f_PO_LM=0
    }
    
    #Equation 5
    # POM -> AGG
    if(POM>0){
      f_PO_AG = rate_pa * scalar_wd * POM
    }else{
      f_PO_AG=0
    }
    
    #Equation 6
    # AGG -> MAOM + POM
    if(AGG>0){
      f_AG_break = rate_break * scalar_wd * AGG
    }else{
      f_AG_break=0
    }
    
    #Equation 8
    # LMWC -> out of system leaching
    if(LMWC>0){
      f_LM_leach = rate_leach * scalar_wd * LMWC
    }else{
      f_LM_leach=0
    }
    
    #Equation 9
    # LMWC -> MAOM
    if(LMWC>0 && MAOM>0){
      f_LM_MA = scalar_wd * kaff_lm * LMWC * (1 - MAOM / param_qmax)
    }else{
      f_LM_MA=0
    }
    
    #Equation 12
    # MAOM -> LMWC
    if(MAOM>0){
      f_MA_LM = kaff_des * MAOM / param_qmax
    }else{
      f_MA_LM=0
    }
    
    #Equation 4
    vmax_lb = alpha_lb * exp(-eact_lb / (gas_const * (forc_st(step.num) + 273.15)))
    
    #Equation 13
    # LMWC -> MIC
    if(LMWC>0 && MIC>0){
      f_LM_MB = vmax_lb * scalar_wb * MIC * LMWC / (kaff_lb + LMWC)
    }else{
      f_LM_MB=0
    }
    
    #Equation 16
    # MIC -> MAOM + LMWC
    if(MIC>0){
      f_MB_turn = rate_bd * MIC^2.0
    }else{
      f_MB_turn=0
    }
    
    #Equation 18
    # MAOM -> AGG
    if(MAOM>0){  
      f_MA_AG = rate_ma * scalar_wd * MAOM
    }else{
      f_MA_AG=0
    }
    
    #Equation 22
    # microbial growth flux, but is not used in mass balance
    
    #Equation 21
    # MIC -> atmosphere
    if(MIC>0 && LMWC>0){ 
      f_MB_atm = f_LM_MB * (1 - (cue_ref - cue_t * (forc_st(step.num) - tae_ref) ) )
    }else{
      f_MB_atm=0
    }
    
    # Update state variables
    
    #Equation 1
    dPOM = forc_npp(step.num) * param_pi + f_AG_break * param_pa - f_PO_AG - f_PO_LM
    
    #Equation 7
    dLMWC = forc_npp(step.num) * (1. - param_pi) - f_LM_leach + f_PO_LM - f_LM_MA - f_LM_MB + f_MB_turn * (1. - param_pb) + f_MA_LM
    
    #Equation 17
    dAGG = f_MA_AG + f_PO_AG - f_AG_break
    
    #Equation 20
    dMIC = f_LM_MB - f_MB_turn - f_MB_atm
    
    #Equation 19
    dMAOM = f_LM_MA - f_MA_LM + f_MB_turn * param_pb - f_MA_AG + f_AG_break * (1. - param_pa)
    
    return(list(c(dPOM, dLMWC, dAGG, dMIC, dMAOM)))
    
    return(list(output[[1]][1:5]))
    
  })
}

Model.Aus.pool.MMRMM <- function (parameters, times=run.steps) {
  output <- ode(y = state.V2.5pool.MMRMM, times=run.steps, func=derivs.Aus.pool.MMRMM, parms = parameters, method="rk4")
  #return(as.data.frame(cbind(time = output[run.steps.minus.one,"time"], 
  #                           POM  = output[run.steps.minus.one,"POM"], 
  #                           LMWC = output[run.steps.minus.one,"LMWC"], 
  #                           AGG  = output[run.steps.minus.one,"AGG"], 
  #                           MIC  = output[run.steps.minus.one,"MIC"], 
  #                           MAOM = output[run.steps.minus.one,"MAOM"])))
  return(as.data.frame(cbind(POM  = output[run.steps.minus.one,"POM"], 
                             LMWC = output[run.steps.minus.one,"LMWC"], 
                             AGG  = output[run.steps.minus.one,"AGG"], 
                             MIC  = output[run.steps.minus.one,"MIC"], 
                             MAOM = output[run.steps.minus.one,"MAOM"])))
}
################################################################################
# define inputs and sample inputs space
# Define inputs:
#DistrFun  <- "unif" # Parameter distribution
#DistrPar  <- list(c(0, 400), c(0, 2), c(0, 1), c(0, 0.1), c(0.1, 1))
#x_labels  <- c("Sm", "beta", "alfa", "Rs", "Rf")
#-------------------------------------------------------------------------------
# define inputs
DistrFun  <- "unif"

#param_pi,       param_pa,          kaff_pl,           alpha_pl,          eact_pl,       rate_pa,
#rate_break,     rate_leach,        kaff_des,          param_p1,          param_p2,      kaff_lb,
#alpha_lb,       eact_lb,           rate_bd,           rate_ma,           cue_ref,       cue_t,
#tae_ref,        matpot,            lambda,            porosity,          kamin,         param_pb

DistrPar_old  <- list(c(0,1),      c(0,1),            c(5000,15e4),      c(1.25e12,3.75e12),c(32160,96480),c(0.01,0.03),
                  c(0.0095,0.0285),c(0.00075,0.00225),c(0,10),           c(0.05,0.4),       c(0.108,0.324),c(100,1000),
                  c(1.3e12,3.9e12),c(30130,90390),    c(0.0018,0.0054),  c(0.003,0.03),     c(0.3,0.9),    c(0.006,0.018),
                  c(7.5,22.5),     c(7.5,22.5),       c(1.05e-4,3.15e-4),c(0.3,0.9),        c(0.1,0.3),    c(0,1))

DistrPar  <- list(c(0,1),          c(0,1),            c(5000,15e4),      c(1.25e12,3.75e12),c(32160,96480),c(0.01,0.03),
                  c(0.0095,0.0285),c(0.00075,0.00225),c(0,10),           c(0.05,0.4),       c(0.108,0.324),c(100,1000),
                  c(1.3e12,3.9e12),c(30130,90390),    c(0.0018,0.0054),  c(0.003,0.03),     c(0.3,0.9),    c(0.006,0.018),
                  c(7.5,22.5),     c(7.5,22.5),       c(1.05e-4,3.15e-4),c(0.3,0.9),        c(0.1,0.3),    c(0,1))

x_labels  <- c("param_pi",       "param_pa",          "kaff_pl",         "alpha_pl",        "eact_pl",      "rate_pa",
               "rate_break",     "rate_leach",        "kaff_des",        "param_p1",        "param_p2",     "kaff_lb",
               "alpha_lb",       "eact_lb",           "rate_bd",         "rate_ma",         "cue_ref",      "cue_t",
               "tae_ref",        "matpot",            "lambda",          "porosity",        "kamin",        "param_pb")


sxlabels <- c("P[i]",     "P[a]",       "K[pl]",  "alpha[pl]", "plain(E)*alpha[pl]",  "K[pa]",
            "K[b]",       "K[l]",       "K[ld]",  "P[1]",        "P[2]",        "K[lb]",
            "alpha[lb]",  "plain(E)*alpha[lb]", "K[bd]",  "K[ma]",       "CUE[ref]",    "CUE[t]",
            "T[ae-ref]",  "phi",        "lambda", "Porosity",    "K[a,min]",    "P[b]"
            )
#scale_x_discrete(labels = parse(text = labels)) +


# sample inputs space
set.seed(123)
SampStrategy <- "lhs" # Latin Hypercube
N            <- 3000 # Number of samples
M            <- length(DistrPar) # Number of inputs
X            <- AAT_sampling(SampStrategy, M, DistrFun, DistrPar, N)
colnames(X)  <- x_labels

state  <- c(POM = 1, LMWC = 1, AGG = 1, MIC = 1, MAOM=1)
SStime <- 1000*365

run.steps           <- seq(1,100*365)
run.steps.minus.one <- run.steps[1:length(run.steps)-1]

inputdata <- read.table(paste0("/media/DATADRIVE1/Model/Millennial/Fortran/MillennialV2/simulationv2/","globalaverage.txt"))
#degC, m3/m3, gC/m2/d
names(inputdata) <- c("forc_st","forc_sw","forc_npp") 

SS.Model.pools <- function(X, dat) {
  
  forc_st <- approxfun(1:SStime,  rep(mean(dat$forc_st),SStime))
  forc_sw <- approxfun(1:SStime,  rep(mean(dat$forc_sw),SStime))
  forc_npp <- approxfun(1:SStime, rep(mean(dat$forc_npp),SStime))
  
  derivs_Aus_wrapper <- function(step.num,state,X,forc_st,forc_sw,forc_npp) {
    output <- derivs.Aus.pool.MMRMM(step.num,state,X,forc_st,forc_sw,forc_npp)
    return(list(output[[1]][1:5]))
  }
  
  #Solve for steady state
  SS.V2.5pool.MMRMM <- stode(y = state, time = SStime, func = derivs_Aus_wrapper, parms = X, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, positive=TRUE)
  
  #Calculate the eigenvalues
  eigens <- eigen(jacobian.full(y=SS.V2.5pool.MMRMM$y, func=derivs.Aus.pool.MMRMM, parms=X, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, time=1))
  
  #Define new initial states
  state.V2.5pool.MMRMM = SS.V2.5pool.MMRMM$y
  
  #state.SS return c(dPOM, dLMWC, dAGG, dMIC, dMAOM)
  #rowSums to get SOM
  forc_st <- approxfun(run.steps, rep(dat$forc_st,100))
  forc_sw <- approxfun(run.steps, rep(dat$forc_sw,100))
  forc_npp <- approxfun(run.steps,rep(dat$forc_npp,100))
  
  output <- ode(y = state.V2.5pool.MMRMM, times=run.steps, func=derivs.Aus.pool.MMRMM, parms = X, forc_st=forc_st, forc_sw=forc_sw, forc_npp=forc_npp, method="rk4")
  
  output_ode <- as.data.frame(cbind(POM  = output[36499,"POM"], 
                                    LMWC = output[36499,"LMWC"], 
                                    AGG  = output[36499,"AGG"], 
                                    MIC  = output[36499,"MIC"], 
                                    MAOM = output[36499,"MAOM"]))
  return(output_ode)
}

# run the model

Y <- model_execution("SS.Model.pools", X, dat = inputdata)

Y.result <- do.call(rbind, Y)

Y.result$SOM <- with(Y.result,POM+LMWC+AGG+MIC+MAOM)


#Y.result <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/PAWN_Sens.RDS")
# compute PAWN sensitivity indices
# n maybe set as log(n), here we try 8 as input

n = 8
Nboot <- 1000

pawn_ind <- pawn_indices(X, Y.result[,6], n, Nboot, dummy = TRUE)

saveRDS(X,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/PAWN_Sens_X.RDS")
saveRDS(Y.result,"/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/PAWN_Sens_Y.RDS")

X        <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/PAWN_Sens_X.RDS")
Y.result <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/PAWN_Sens_Y.RDS")

pawn_ind <- pawn_indices(X, Y.result[,6], n, Nboot, dummy = TRUE)

KS_max   <- pawn_ind$KS_max # KS_max has dim (Nboot, M)
KS_dummy <- pawn_ind$KS_dummy # KS_dummy has dim (Nboot, 1)

# Compute mean and confidence intervals of the sensitivity indices across the
# bootstrap resamples:
alfa <- 0.05 # Significance level for the confidence intervals estimated by bootstrapping 

KS_stat     <- KS_max
KS_max_m    <- colMeans(KS_stat) # mean
KS_max_lb   <- colQuantiles(KS_stat,probs=alfa/2) # Lower bound
KS_max_ub   <- colQuantiles(KS_stat,probs=1-alfa/2) # Upper bound

KS_stat     <- KS_dummy
KS_dummy_m  <- mean(KS_stat) # mean
KS_dummy_lb <-  quantile(KS_dummy,alfa/2) # Lower bound
KS_dummy_ub <- quantile(KS_dummy,1-alfa/2) # Upper bound

# Combine KS max for all inputs and for dummy to plot
KS_max_d_m  <- c(KS_max_m,KS_dummy_m) 
KS_max_d_lb <- c(KS_max_lb,KS_dummy_lb)
KS_max_d_ub <- c(KS_max_ub,KS_dummy_ub)

# Plot bootstrapping results:

require(graphics)
library(extrafont)

dev.new()
boxplot1_dummy(mu = KS_max_d_m, lb = KS_max_d_lb, ub = KS_max_d_ub, prnam = x_labels) + ylab("KS") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 8))
  
boxplot2_dummy(mu = KS_max_d_m, lb = KS_max_d_lb, ub = KS_max_d_ub, prnam = sxlabels) + ylab("PAWN indices") +
  scale_x_discrete(labels = parse(text = sxlabels)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 16,face="bold"),
        axis.title.y = element_text(size = 16,face = "bold")
        )

dev.copy2pdf(file = "/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/boxplot2_SOC_KS.pdf",width = 1080/72, height = 800/72)
dev.off()

#boxplot1_dummy(mu = KS_max_d, prnam = x_labels) + ylab("KS")

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
    scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), limits = c(0, 0.5)) +
    geom_hline(yintercept=mu2, size = 0.5, color = "grey49") +
    geom_hline(yintercept=lb2, size = 0.5, linetype = "dashed", color = "grey49") +
    geom_hline(yintercept=ub2, size = 0.5, linetype = "dashed", color = "grey49") +
    annotate("text", x = length(dat$x)/2, y = 0, label = "Threshold for non-influential input factors",
             size = 6, color = "grey49")
  
  return( .pl )
  
}

################################################################################

set.seed(123)

y <- Y.result[,6]
x <- as.matrix(X)

rank_kfilterfused    <- Kfilter_fused(x, y, nsis = (dim(x)[1])/log(dim(x)[1]))

kfilter_rank <- data.frame(names=colnames(X),values=unlist(rank_kfilterfused$measurement))

kfilter_rank <- kfilter_rank[order(kfilter_rank$values,decreasing = TRUE),]
kfilter_rank_filter <- kfilter_rank[1:dim(x)[1],]

