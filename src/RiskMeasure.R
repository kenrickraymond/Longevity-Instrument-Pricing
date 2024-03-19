forwardCDF = function(maturity, control, lambda, model, premium, pi, nsim=1000){
  model = toString(model)
  premium = toString(premium)
  
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = maturity)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = maturity)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = maturity)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = maturity)
  }
  survival_probability = (1-mod_sim$rates)[6+control, control,] # control = 9,
  
  if (premium == "Wang") {
    S_t = ( 1 - pnorm( qnorm( 1 - survival_probability) - LCWanglambda) )
    K_t = mean( 1 - pnorm( qnorm( 1 - survival_probability) - 0) )
  }
  if (premium == "Proportional") {
    S_t = ( survival_probability^(1/lambda) )
    K_t =  mean( survival_probability )
  }
  if (premium == "Dual") { 
    S_t = (1 -  (1 - survival_probability)^(lambda) )
    K_t =  mean(1 -  (1 - survival_probability) )
  }
  if (premium == "Gini") {
    S_t = ( ( (1 + lambda) * survival_probability ) - lambda * mean( survival_probability^2 ) )
    K_t =  mean( (1) * survival_probability )
  }
  if (premium == "Exponential") {
    S_t = ( 1 - exp( - lambda * survival_probability ) )/(1-exp(- lambda) )
    K_t =  mean( 1 - exp( -1 * survival_probability ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = ( ( survival_probability ) + lambda * sd( survival_probability ) )
    K_t = mean( survival_probability )
  }
  if (premium == "Var") {
    S_t = ( ( survival_probability ) + lambda * var( survival_probability ) )
    K_t = mean( survival_probability )
  }
  if (premium == "Mad") {
    S_t = ( (survival_probability ) + lambda * mad( survival_probability ) )
    K_t = mean(survival_probability)
  }
  
  contract_value = S_t - (1+pi) * K_t
  return(contract_value)
}

# Control = 5 for 5-year VaR and control = 9 for 1-year VaR
SwapCDF = function(forward_years, control, lambda, model, premium, pi, nsim=100){
  model = toString(model)
  premium = toString(premium)
  
  maturity = forward_years+x
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = maturity)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = maturity)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = maturity)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = maturity)
  }
  survival_probability = (1-mod_sim$rates)
  survival_df = matrix(nrow=nsimCDF,ncol=control)
  for (i in 1:control){
    survival_df[,i] = survival_probability[6+i, i,]  # For year i  
  }
  
  if (premium == "Wang") {
    S_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - LCWanglambda) )
    K_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - 0) )
  }
  if (premium == "Proportional") {
    S_t = rowSums( (survival_df)^(1/lambda) )
    K_t =  rowSums( survival_df ) 
  }
  if (premium == "Dual") {
    S_t = rowSums(1 -  (1 - (survival_df))^(lambda) )
    K_t = rowSums(1 -  (1 - (survival_df)) )
  }
  if (premium == "Gini") {
    S_t = rowSums( ( (1 + lambda) * (survival_df) ) - lambda * ( (survival_df)^2 ) )
    K_t = rowSums( (1) * (survival_df) )
  }
  if (premium == "Exponential") { 
    S_t = rowSums( 1 - exp( - lambda * (survival_df) ) )/(1-exp(- lambda) )
    K_t = rowSums( 1 - exp( -1 * (survival_df) ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = ( ( rowSums(survival_df) ) + lambda * sd( rowSums(survival_df) ) )
    K_t = ( rowSums(survival_df) )
  }
  if (premium == "Var") {
    S_t = ( ( rowSums(survival_df) ) + lambda * var( rowSums(survival_df) ) )
    K_t = ( rowSums(survival_df) )
  }
  if (premium == "Mad") {
    S_t = (rowSums(survival_df) ) + lambda * mad( rowSums(survival_df) ) # Recheck this
    K_t = rowSums(survival_df)
  }
  
  contract_value = S_t - (1+pi) * K_t
  return(contract_value)
}

forward_years=10
nsimCDF = 5000
x = 5 # Indicates Age 65 + x

LCWangForwardCDF = forwardCDF(forward_years,control=9, LCWanglambda, "LC", "Wang", pi=LC_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
RHWangForwardCDF = forwardCDF(forward_years,control=9, RHWanglambda, "RH", "Wang", pi=RH_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
CBDWangForwardCDF = forwardCDF(forward_years,control=9, CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
M6WangForwardCDF = forwardCDF(forward_years,control=9, M6Wanglambda, "M6", "Wang", pi=M6_Wang_Forward_Premium[forward_years], nsim=nsimCDF)

LCPropForwardCDF = forwardCDF(forward_years,control=9, LCProplambda, "LC", "Proportional", pi=LC_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
RHPropForwardCDF = forwardCDF(forward_years,control=9, RHProplambda, "RH", "Proportional", pi=RH_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
CBDPropForwardCDF = forwardCDF(forward_years,control=9, CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
M6PropForwardCDF = forwardCDF(forward_years,control=9, M6Proplambda, "M6", "Proportional", pi=M6_Prop_Forward_Premium[forward_years], nsim=nsimCDF)

LCDualForwardCDF = forwardCDF(forward_years,control=9, LCDuallambda, "LC", "Dual", pi=LC_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
RHDualForwardCDF = forwardCDF(forward_years,control=9, RHDuallambda, "RH", "Dual", pi=RH_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
CBDDualForwardCDF = forwardCDF(forward_years,control=9, CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
M6DualForwardCDF = forwardCDF(forward_years,control=9, M6Duallambda, "M6", "Dual", pi=M6_Dual_Forward_Premium[forward_years], nsim=nsimCDF)

LCGiniForwardCDF = forwardCDF(forward_years,control=9, LCGinilambda, "LC", "Gini", pi=LC_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
RHGiniForwardCDF = forwardCDF(forward_years,control=9, RHGinilambda, "RH", "Gini", pi=RH_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
CBDGiniForwardCDF = forwardCDF(forward_years,control=9, CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
M6GiniForwardCDF = forwardCDF(forward_years,control=9, M6Ginilambda, "M6", "Gini", pi=M6_Gini_Forward_Premium[forward_years], nsim=nsimCDF)

LCExponentialForwardCDF = forwardCDF(forward_years,control=9, LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
RHExponentialForwardCDF = forwardCDF(forward_years,control=9, RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
CBDExponentialForwardCDF = forwardCDF(forward_years,control=9, CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
M6ExponentialForwardCDF = forwardCDF(forward_years,control=9, M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)

LCStDevForwardCDF = forwardCDF(forward_years,control=9, LCStDevlambda, "LC", "StDev", pi=LC_Std_Forward_Premium[forward_years], nsim=nsimCDF)
RHStDevForwardCDF = forwardCDF(forward_years,control=9, RHStDevlambda, "RH", "StDev", pi=RH_Std_Forward_Premium[forward_years], nsim=nsimCDF)
CBDStDevForwardCDF = forwardCDF(forward_years,control=9, CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Forward_Premium[forward_years], nsim=nsimCDF)
M6StDevForwardCDF = forwardCDF(forward_years,control=9, M6StDevlambda, "M6", "StDev", pi=M6_Std_Forward_Premium[forward_years], nsim=nsimCDF)

LCVarForwardCDF = forwardCDF(forward_years,control=9, LCVarlambda, "LC", "Var", pi=LC_Var_Forward_Premium[forward_years], nsim=nsimCDF)
RHVarForwardCDF = forwardCDF(forward_years,control=9, RHVarlambda, "RH", "Var", pi=RH_Var_Forward_Premium[forward_years], nsim=nsimCDF)
CBDVarForwardCDF = forwardCDF(forward_years,control=9, CBDVarlambda, "CBD", "Var", pi=CBD_Var_Forward_Premium[forward_years], nsim=nsimCDF)
M6VarForwardCDF = forwardCDF(forward_years,control=9, M6Varlambda, "M6", "Var", pi=M6_Var_Forward_Premium[forward_years], nsim=nsimCDF)

LCMadForwardCDF = forwardCDF(forward_years,control=9, LCMadlambda, "LC", "Mad", pi=LC_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
RHMadForwardCDF = forwardCDF(forward_years,control=9, RHMadlambda, "RH", "Mad", pi=RH_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
CBDMadForwardCDF = forwardCDF(forward_years,control=9, CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
M6MadForwardCDF = forwardCDF(forward_years,control=9, M6Madlambda, "M6", "Mad", pi=M6_Mad_Forward_Premium[forward_years], nsim=nsimCDF)

############ SWAP CDF ###############
LCWangSwapCDF = SwapCDF(forward_years, control=9, lambda= LCWanglambda, "LC", "Wang", pi=LC_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
RHWangSwapCDF = SwapCDF(forward_years, control=9, lambda= RHWanglambda, "RH", "Wang", pi=RH_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
CBDWangSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
M6WangSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Wanglambda, "M6", "Wang", pi=M6_Wang_Swap_Premium[forward_years], nsim=nsimCDF)

LCPropSwapCDF = SwapCDF(forward_years, control=9, lambda= LCProplambda, "LC", "Proportional", pi=LC_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
RHPropSwapCDF = SwapCDF(forward_years, control=9, lambda= RHProplambda, "RH", "Proportional", pi=RH_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
CBDPropSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
M6PropSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Proplambda, "M6", "Proportional", pi=M6_Prop_Swap_Premium[forward_years], nsim=nsimCDF)

LCDualSwapCDF = SwapCDF(forward_years, control=9, lambda= LCDuallambda, "LC", "Dual", pi=LC_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
RHDualSwapCDF = SwapCDF(forward_years, control=9, lambda= RHDuallambda, "RH", "Dual", pi=RH_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
CBDDualSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
M6DualSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Duallambda, "M6", "Dual", pi=M6_Dual_Swap_Premium[forward_years], nsim=nsimCDF)

LCGiniSwapCDF = SwapCDF(forward_years, control=9, lambda= LCGinilambda, "LC", "Gini", pi=LC_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
RHGiniSwapCDF = SwapCDF(forward_years, control=9, lambda= RHGinilambda, "RH", "Gini", pi=RH_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
CBDGiniSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
M6GiniSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Ginilambda, "M6", "Gini", pi=M6_Gini_Swap_Premium[forward_years], nsim=nsimCDF)

LCExponentialSwapCDF = SwapCDF(forward_years, control=9, lambda= LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
RHExponentialSwapCDF = SwapCDF(forward_years, control=9, lambda= RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
CBDExponentialSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
M6ExponentialSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)

LCStDevSwapCDF = SwapCDF(forward_years, control=9, lambda= LCStDevlambda, "LC", "StDev", pi=LC_Std_Swap_Premium[forward_years], nsim=nsimCDF)
RHStDevSwapCDF = SwapCDF(forward_years, control=9, lambda= RHStDevlambda, "RH", "StDev", pi=RH_Std_Swap_Premium[forward_years], nsim=nsimCDF)
CBDStDevSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Swap_Premium[forward_years], nsim=nsimCDF)
M6StDevSwapCDF = SwapCDF(forward_years, control=9, lambda= M6StDevlambda, "M6", "StDev", pi=M6_Std_Swap_Premium[forward_years], nsim=nsimCDF)

LCVarSwapCDF = SwapCDF(forward_years, control=9, lambda= LCVarlambda, "LC", "Var", pi=LC_Var_Swap_Premium[forward_years], nsim=nsimCDF)
RHVarSwapCDF = SwapCDF(forward_years, control=9, lambda= RHVarlambda, "RH", "Var", pi=RH_Var_Swap_Premium[forward_years], nsim=nsimCDF)
CBDVarSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDVarlambda, "CBD", "Var", pi=CBD_Var_Swap_Premium[forward_years], nsim=nsimCDF)
M6VarSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Varlambda, "M6", "Var", pi=M6_Var_Swap_Premium[forward_years], nsim=nsimCDF)

LCMadSwapCDF = SwapCDF(forward_years, control=9, lambda= LCMadlambda, "LC", "Mad", pi=LC_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
RHMadSwapCDF = SwapCDF(forward_years, control=9, lambda= RHMadlambda, "RH", "Mad", pi=RH_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
CBDMadSwapCDF = SwapCDF(forward_years, control=9, lambda= CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
M6MadSwapCDF = SwapCDF(forward_years, control=9, lambda= M6Madlambda, "M6", "Mad", pi=M6_Mad_Swap_Premium[forward_years], nsim=nsimCDF)

# Quantile Calculations

alpha = 0.005 # PLEASE VERIFY
LCWangVaR = quantile(LCWangForwardCDF, probs=alpha)
RHWangVaR = quantile(RHWangForwardCDF, probs=alpha)
CBDWangVaR = quantile(CBDWangForwardCDF, probs=alpha)
M6WangVaR = quantile(M6WangForwardCDF, probs=alpha)

LCPropVaR = quantile(LCPropForwardCDF, probs=alpha)
RHPropVaR = quantile(RHPropForwardCDF, probs=alpha)
CBDPropVaR = quantile(CBDPropForwardCDF, probs=alpha)
M6PropVaR = quantile(M6PropForwardCDF, probs=alpha)

LCDualVaR = quantile(LCDualForwardCDF, probs=alpha)
RHDualVaR = quantile(RHDualForwardCDF, probs=alpha)
CBDDualVaR = quantile(CBDDualForwardCDF, probs=alpha)
M6DualVaR = quantile(M6DualForwardCDF, probs=alpha)

LCGiniVaR = quantile(LCGiniForwardCDF, probs=alpha)
RHGiniVaR = quantile(RHGiniForwardCDF, probs=alpha)
CBDGiniVaR = quantile(CBDGiniForwardCDF, probs=alpha)
M6GiniVaR = quantile(M6GiniForwardCDF, probs=alpha)

LCExponentialVaR = quantile(LCExponentialForwardCDF, probs=alpha)
RHExponentialVaR = quantile(RHExponentialForwardCDF, probs=alpha)
CBDExponentialVaR = quantile(CBDExponentialForwardCDF, probs=alpha)
M6ExponentialVaR = quantile(M6ExponentialForwardCDF, probs=alpha)

LCStDevVaR = quantile(LCStDevForwardCDF, probs=alpha)
RHStDevVaR = quantile(RHStDevForwardCDF, probs=alpha)
CBDStDevVaR = quantile(CBDStDevForwardCDF, probs=alpha)
M6StDevVaR = quantile(M6StDevForwardCDF, probs=alpha)

LCVarVaR = quantile(LCVarForwardCDF, probs=alpha)
RHVarVaR = quantile(RHVarForwardCDF, probs=alpha)
CBDVarVaR = quantile(CBDVarForwardCDF, probs=alpha)
M6VarVaR = quantile(M6VarForwardCDF, probs=alpha)

LCMadVaR = quantile(LCMadForwardCDF, probs=alpha)
RHMadVaR = quantile(RHMadForwardCDF, probs=alpha)
CBDMadVaR = quantile(CBDMadForwardCDF, probs=alpha)
M6MadVaR = quantile(M6MadForwardCDF, probs=alpha)

LCWangSwapVaR = quantile(LCWangSwapCDF, probs=alpha)
RHWangSwapVaR = quantile(RHWangSwapCDF, probs=alpha)
CBDWangSwapVaR = quantile(CBDWangSwapCDF, probs=alpha)
M6WangSwapVaR = quantile(M6WangSwapCDF, probs=alpha)

LCPropSwapVaR = quantile(LCPropSwapCDF, probs=alpha)
RHPropSwapVaR = quantile(RHPropSwapCDF, probs=alpha)
CBDPropSwapVaR = quantile(CBDPropSwapCDF, probs=alpha)
M6PropSwapVaR = quantile(M6PropSwapCDF, probs=alpha)

LCDualSwapVaR = quantile(LCDualSwapCDF, probs=alpha)
RHDualSwapVaR = quantile(RHDualSwapCDF, probs=alpha)
CBDDualSwapVaR = quantile(CBDDualSwapCDF, probs=alpha)
M6DualSwapVaR = quantile(M6DualSwapCDF, probs=alpha)

LCGiniSwapVaR = quantile(LCGiniSwapCDF, probs=alpha)
RHGiniSwapVaR = quantile(RHGiniSwapCDF, probs=alpha)
CBDGiniSwapVaR = quantile(CBDGiniSwapCDF, probs=alpha)
M6GiniSwapVaR = quantile(M6GiniSwapCDF, probs=alpha)

LCExponentialSwapVaR = quantile(LCExponentialSwapCDF, probs=alpha)
RHExponentialSwapVaR = quantile(RHExponentialSwapCDF, probs=alpha)
CBDExponentialSwapVaR = quantile(CBDExponentialSwapCDF, probs=alpha)
M6ExponentialSwapVaR = quantile(M6ExponentialSwapCDF, probs=alpha)

LCStDevSwapVaR = quantile(LCStDevSwapCDF, probs=alpha)
RHStDevSwapVaR = quantile(RHStDevSwapCDF, probs=alpha)
CBDStDevSwapVaR = quantile(CBDStDevSwapCDF, probs=alpha)
M6StDevSwapVaR = quantile(M6StDevSwapCDF, probs=alpha)

LCVarSwapVaR = quantile(LCVarSwapCDF, probs=alpha)
RHVarSwapVaR = quantile(RHVarSwapCDF, probs=alpha)
CBDVarSwapVaR = quantile(CBDVarSwapCDF, probs=alpha)
M6VarSwapVaR = quantile(M6VarSwapCDF, probs=alpha)

LCMadSwapVaR = quantile(LCMadSwapCDF, probs=alpha)
RHMadSwapVaR = quantile(RHMadSwapCDF, probs=alpha)
CBDMadSwapVaR = quantile(CBDMadSwapCDF, probs=alpha)
M6MadSwapVaR = quantile(M6MadSwapCDF, probs=alpha)


################## EXPECTED SHORTFALL ################
LCWangES = mean( sort(LCWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHWangES = mean( sort(RHWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangES = mean( sort(CBDWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6WangES = mean( sort(M6WangForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCPropES = mean( sort(LCPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHPropES = mean( sort(RHPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropES = mean( sort(CBDPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6PropES = mean( sort(M6PropForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCDualES = mean( sort(LCDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHDualES = mean( sort(RHDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualES = mean( sort(CBDDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6DualES = mean( sort(M6DualForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniES = mean( sort(LCGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniES = mean( sort(RHGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniES = mean( sort(CBDGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniES = mean( sort(M6GiniForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialES = mean( sort(LCExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialES = mean( sort(RHExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES = mean( sort(CBDExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES = mean( sort(M6ExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevES = mean( sort(LCStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevES = mean( sort(RHStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevES = mean( sort(CBDStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevES = mean( sort(M6StDevForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCVarES = mean( sort(LCVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHVarES = mean( sort(RHVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarES = mean( sort(CBDVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6VarES = mean( sort(M6VarForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCMadES = mean( sort(LCMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHMadES = mean( sort(RHMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadES = mean( sort(CBDMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6MadES = mean( sort(M6MadForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES = mean( sort(LCWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES = mean( sort(RHWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES = mean( sort(CBDWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES = mean( sort(M6WangSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES = mean( sort(LCPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES = mean( sort(RHPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES = mean( sort(CBDPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES = mean( sort(M6PropSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES = mean( sort(LCDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES = mean( sort(RHDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES = mean( sort(CBDDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES = mean( sort(M6DualSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES = mean( sort(LCGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES = mean( sort(RHGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES = mean( sort(CBDGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES = mean( sort(M6GiniSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES = mean( sort(LCExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES = mean( sort(RHExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES = mean( sort(CBDExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES = mean( sort(M6ExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES = mean( sort(LCStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES = mean( sort(RHStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES = mean( sort(CBDStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES = mean( sort(M6StDevSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES = mean( sort(LCVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES = mean( sort(RHVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES = mean( sort(CBDVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES = mean( sort(M6VarSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES = mean( sort(LCMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES = mean( sort(RHMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES = mean( sort(CBDMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES = mean( sort(M6MadSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCWangES = mean( sort(LCWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHWangES = mean( sort(RHWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangES = mean( sort(CBDWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6WangES = mean( sort(M6WangForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCPropES = mean( sort(LCPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHPropES = mean( sort(RHPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropES = mean( sort(CBDPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6PropES = mean( sort(M6PropForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCDualES = mean( sort(LCDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHDualES = mean( sort(RHDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualES = mean( sort(CBDDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6DualES = mean( sort(M6DualForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniES = mean( sort(LCGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniES = mean( sort(RHGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniES = mean( sort(CBDGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniES = mean( sort(M6GiniForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialES = mean( sort(LCExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialES = mean( sort(RHExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES = mean( sort(CBDExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES = mean( sort(M6ExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevES = mean( sort(LCStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevES = mean( sort(RHStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevES = mean( sort(CBDStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevES = mean( sort(M6StDevForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCVarES = mean( sort(LCVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHVarES = mean( sort(RHVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarES = mean( sort(CBDVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6VarES = mean( sort(M6VarForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCMadES = mean( sort(LCMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHMadES = mean( sort(RHMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadES = mean( sort(CBDMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6MadES = mean( sort(M6MadForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES = mean( sort(LCWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES = mean( sort(RHWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES = mean( sort(CBDWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES = mean( sort(M6WangSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES = mean( sort(LCPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES = mean( sort(RHPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES = mean( sort(CBDPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES = mean( sort(M6PropSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES = mean( sort(LCDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES = mean( sort(RHDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES = mean( sort(CBDDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES = mean( sort(M6DualSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES = mean( sort(LCGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES = mean( sort(RHGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES = mean( sort(CBDGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES = mean( sort(M6GiniSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES = mean( sort(LCExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES = mean( sort(RHExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES = mean( sort(CBDExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES = mean( sort(M6ExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES = mean( sort(LCStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES = mean( sort(RHStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES = mean( sort(CBDStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES = mean( sort(M6StDevSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES =  mean( sort(LCVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES = mean( sort(RHVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES = mean( sort(CBDVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES = mean( sort(M6VarSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES = mean( sort(LCMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES = mean( sort(RHMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES = mean( sort(CBDMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES = mean( sort(M6MadSwapCDF)[1:(nsimCDF*alpha - 1)] )


Forward_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangVaR, LCPropVaR, LCDualVaR, LCGiniVaR, LCExponentialVaR, LCStDevVaR, LCVarVaR, LCMadVaR,
                                                            RHWangVaR, RHPropVaR, RHDualVaR, RHGiniVaR, RHExponentialVaR, RHStDevVaR, RHVarVaR, RHMadVaR,
                                                            CBDWangVaR, CBDPropVaR, CBDDualVaR, CBDGiniVaR, CBDExponentialVaR, CBDStDevVaR, CBDVarVaR, CBDMadVaR,
                                                            M6WangVaR, M6PropVaR, M6DualVaR, M6GiniVaR, M6ExponentialVaR, M6StDevVaR, M6VarVaR, M6MadVaR)
))
rownames(Forward_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Forward_VaR_table) = c("LC","RH","CBD","M6")

Swap_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapVaR, LCPropSwapVaR, LCDualSwapVaR, LCGiniSwapVaR, LCExponentialSwapVaR, LCStDevSwapVaR, LCVarSwapVaR, LCMadSwapVaR,
                                                         RHWangSwapVaR, RHPropSwapVaR, RHDualSwapVaR, RHGiniSwapVaR, RHExponentialSwapVaR, RHStDevSwapVaR, RHVarSwapVaR, RHMadSwapVaR,
                                                         CBDWangSwapVaR, CBDPropSwapVaR, CBDDualSwapVaR, CBDGiniSwapVaR, CBDExponentialSwapVaR, CBDStDevSwapVaR, CBDVarSwapVaR, CBDMadSwapVaR,
                                                         M6WangSwapVaR, M6PropSwapVaR, M6DualSwapVaR, M6GiniSwapVaR, M6ExponentialSwapVaR, M6StDevSwapVaR, M6VarSwapVaR, M6MadSwapVaR)
))
rownames(Swap_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Swap_VaR_table) = c("LC","RH","CBD","M6")

Forward_ES_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangES, LCPropES, LCDualES, LCGiniES, LCExponentialES, LCStDevES, LCVarES, LCMadES,
                                                           RHWangES, RHPropES, RHDualES, RHGiniES, RHExponentialES, RHStDevES, RHVarES, RHMadES,
                                                           CBDWangES, CBDPropES, CBDDualES, CBDGiniES, CBDExponentialES, CBDStDevES, CBDVarES, CBDMadES,
                                                           M6WangES, M6PropES, M6DualES, M6GiniES, M6ExponentialES, M6StDevES, M6VarES, M6MadES)
))
rownames(Forward_ES_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Forward_ES_table) = c("LC","RH","CBD","M6")

Swap_ES_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapES, LCPropSwapES, LCDualSwapES, LCGiniSwapES, LCExponentialSwapES, LCStDevSwapES, LCVarSwapES, LCMadSwapES,
                                                        RHWangSwapES, RHPropSwapES, RHDualSwapES, RHGiniSwapES, RHExponentialSwapES, RHStDevSwapES, RHVarSwapES, RHMadSwapES,
                                                        CBDWangSwapES, CBDPropSwapES, CBDDualSwapES, CBDGiniSwapES, CBDExponentialSwapES, CBDStDevSwapES, CBDVarSwapES, CBDMadSwapES,
                                                        M6WangSwapES, M6PropSwapES, M6DualSwapES, M6GiniSwapES, M6ExponentialSwapES, M6StDevSwapES, M6VarSwapES, M6MadSwapES)
))
rownames(Swap_ES_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Swap_ES_table) = c("LC","RH","CBD","M6")

#  RUN AFTER HERE
forwardCDF_2Y = function(maturity, control=8, lambda, model, premium, pi, nsim=1000){
  model = toString(model)
  premium = toString(premium)
  
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = maturity)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = maturity)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = maturity)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = maturity)
  }
  survival_probability = (1-mod_sim$rates)[7+control, control,] # control = 8,
  
  if (premium == "Wang") {
    S_t = ( 1 - pnorm( qnorm( 1 - survival_probability) - LCWanglambda) )
    K_t = mean( 1 - pnorm( qnorm( 1 - survival_probability) - 0) )
  }
  if (premium == "Proportional") {
    S_t = ( survival_probability^(1/lambda) )
    K_t =  mean( survival_probability )
  }
  if (premium == "Dual") { 
    S_t = (1 -  (1 - survival_probability)^(lambda) )
    K_t =  mean(1 -  (1 - survival_probability) )
  }
  if (premium == "Gini") { 
    S_t = ( ( (1 + lambda) * survival_probability ) - lambda * mean( survival_probability^2 ) )
    K_t =  mean( (1) * survival_probability )
  }
  if (premium == "Exponential") {
    S_t = ( 1 - exp( - lambda * survival_probability ) )/(1-exp(- lambda) )
    K_t =  mean( 1 - exp( -1 * survival_probability ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = ( ( survival_probability ) + lambda * sd( survival_probability ) )
    K_t = mean( survival_probability )
  }
  if (premium == "Var") {
    S_t = ( ( survival_probability ) + lambda * var( survival_probability ) )
    K_t = mean( survival_probability )
  }
  if (premium == "Mad") {
    S_t = ( (survival_probability ) + lambda * mad( survival_probability ) ) 
    K_t = mean(survival_probability)
  }
  
  contract_value = S_t - (1+pi) * K_t
  return(contract_value)
}

SwapCDF_2Y = function(forward_years, control=8, lambda, model, premium, pi, nsim=100){
  model = toString(model)
  premium = toString(premium)
  
  maturity = forward_years+x
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = maturity)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = maturity)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = maturity)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = maturity)
  }
  survival_probability = (1-mod_sim$rates)
  survival_df = matrix(nrow=nsimCDF,ncol=control)
  for (i in 1:control){
    survival_df[,i] = survival_probability[7+i, i,]  # For year i  
  }
  
  if (premium == "Wang") {
    S_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - LCWanglambda) )
    K_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - 0) )
  }
  if (premium == "Proportional") {
    S_t = rowSums( (survival_df)^(1/lambda) )
    K_t =  rowSums( survival_df ) 
  }
  if (premium == "Dual") { 
    S_t = rowSums(1 -  (1 - (survival_df))^(lambda) )
    K_t = rowSums(1 -  (1 - (survival_df)) )
  }
  if (premium == "Gini") { 
    S_t = rowSums( ( (1 + lambda) * (survival_df) ) - lambda * ( (survival_df)^2 ) )
    K_t = rowSums( (1) * (survival_df) )
  }
  if (premium == "Exponential") {
    S_t = rowSums( 1 - exp( - lambda * (survival_df) ) )/(1-exp(- lambda) )
    K_t = rowSums( 1 - exp( -1 * (survival_df) ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = ( ( rowSums(survival_df) ) + lambda * sd( rowSums(survival_df) ) )
    K_t = ( rowSums(survival_df) )
  }
  if (premium == "Var") {
    S_t = ( ( rowSums(survival_df) ) + lambda * var( rowSums(survival_df) ) )
    K_t = ( rowSums(survival_df) )
  }
  if (premium == "Mad") {
    S_t = (rowSums(survival_df) ) + lambda * mad( rowSums(survival_df) ) 
    K_t = rowSums(survival_df)
  }
  
  contract_value = S_t - (1+pi) * K_t
  return(contract_value)
}

# 5 Y
LCWangforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCWanglambda, "LC", "Wang", pi=LC_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
RHWangforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHWanglambda, "RH", "Wang", pi=RH_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
CBDWangforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
M6WangforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Wanglambda, "M6", "Wang", pi=M6_Wang_Forward_Premium[forward_years], nsim=nsimCDF)

LCPropforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCProplambda, "LC", "Proportional", pi=LC_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
RHPropforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHProplambda, "RH", "Proportional", pi=RH_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
CBDPropforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
M6PropforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Proplambda, "M6", "Proportional", pi=M6_Prop_Forward_Premium[forward_years], nsim=nsimCDF)

LCDualforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCDuallambda, "LC", "Dual", pi=LC_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
RHDualforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHDuallambda, "RH", "Dual", pi=RH_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
CBDDualforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
M6DualforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Duallambda, "M6", "Dual", pi=M6_Dual_Forward_Premium[forward_years], nsim=nsimCDF)

LCGiniforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCGinilambda, "LC", "Gini", pi=LC_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
RHGiniforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHGinilambda, "RH", "Gini", pi=RH_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
CBDGiniforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
M6GiniforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Ginilambda, "M6", "Gini", pi=M6_Gini_Forward_Premium[forward_years], nsim=nsimCDF)

LCExponentialforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
RHExponentialforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
CBDExponentialforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
M6ExponentialforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)

LCStDevforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCStDevlambda, "LC", "StDev", pi=LC_Std_Forward_Premium[forward_years], nsim=nsimCDF)
RHStDevforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHStDevlambda, "RH", "StDev", pi=RH_Std_Forward_Premium[forward_years], nsim=nsimCDF)
CBDStDevforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Forward_Premium[forward_years], nsim=nsimCDF)
M6StDevforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6StDevlambda, "M6", "StDev", pi=M6_Std_Forward_Premium[forward_years], nsim=nsimCDF)

LCVarforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCVarlambda, "LC", "Var", pi=LC_Var_Forward_Premium[forward_years], nsim=nsimCDF)
RHVarforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHVarlambda, "RH", "Var", pi=RH_Var_Forward_Premium[forward_years], nsim=nsimCDF)
CBDVarforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDVarlambda, "CBD", "Var", pi=CBD_Var_Forward_Premium[forward_years], nsim=nsimCDF)
M6VarforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Varlambda, "M6", "Var", pi=M6_Var_Forward_Premium[forward_years], nsim=nsimCDF)

LCMadforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, LCMadlambda, "LC", "Mad", pi=LC_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
RHMadforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, RHMadlambda, "RH", "Mad", pi=RH_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
CBDMadforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
M6MadforwardCDF_2Y = forwardCDF_2Y(forward_years,control = 8, M6Madlambda, "M6", "Mad", pi=M6_Mad_Forward_Premium[forward_years], nsim=nsimCDF)

############ SWAP CDF ###############
LCWangSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCWanglambda, "LC", "Wang", pi=LC_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
RHWangSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHWanglambda, "RH", "Wang", pi=RH_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
CBDWangSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
M6WangSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Wanglambda, "M6", "Wang", pi=M6_Wang_Swap_Premium[forward_years], nsim=nsimCDF)

LCPropSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCProplambda, "LC", "Proportional", pi=LC_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
RHPropSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHProplambda, "RH", "Proportional", pi=RH_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
CBDPropSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
M6PropSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Proplambda, "M6", "Proportional", pi=M6_Prop_Swap_Premium[forward_years], nsim=nsimCDF)

LCDualSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCDuallambda, "LC", "Dual", pi=LC_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
RHDualSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHDuallambda, "RH", "Dual", pi=RH_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
CBDDualSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
M6DualSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Duallambda, "M6", "Dual", pi=M6_Dual_Swap_Premium[forward_years], nsim=nsimCDF)

LCGiniSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCGinilambda, "LC", "Gini", pi=LC_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
RHGiniSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHGinilambda, "RH", "Gini", pi=RH_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
CBDGiniSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
M6GiniSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Ginilambda, "M6", "Gini", pi=M6_Gini_Swap_Premium[forward_years], nsim=nsimCDF)

LCExponentialSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
RHExponentialSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
CBDExponentialSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
M6ExponentialSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)

LCStDevSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCStDevlambda, "LC", "StDev", pi=LC_Std_Swap_Premium[forward_years], nsim=nsimCDF)
RHStDevSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHStDevlambda, "RH", "StDev", pi=RH_Std_Swap_Premium[forward_years], nsim=nsimCDF)
CBDStDevSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Swap_Premium[forward_years], nsim=nsimCDF)
M6StDevSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6StDevlambda, "M6", "StDev", pi=M6_Std_Swap_Premium[forward_years], nsim=nsimCDF)

LCVarSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCVarlambda, "LC", "Var", pi=LC_Var_Swap_Premium[forward_years], nsim=nsimCDF)
RHVarSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHVarlambda, "RH", "Var", pi=RH_Var_Swap_Premium[forward_years], nsim=nsimCDF)
CBDVarSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDVarlambda, "CBD", "Var", pi=CBD_Var_Swap_Premium[forward_years], nsim=nsimCDF)
M6VarSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Varlambda, "M6", "Var", pi=M6_Var_Swap_Premium[forward_years], nsim=nsimCDF)

LCMadSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= LCMadlambda, "LC", "Mad", pi=LC_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
RHMadSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= RHMadlambda, "RH", "Mad", pi=RH_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
CBDMadSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
M6MadSwapCDF_2Y = SwapCDF_2Y(forward_years, control = 8, lambda= M6Madlambda, "M6", "Mad", pi=M6_Mad_Swap_Premium[forward_years], nsim=nsimCDF)

# Quantile Calculations

alpha = 0.005 # PLEASE VERIFY
LCWangVaR_2Y = quantile(LCWangforwardCDF_2Y, probs=alpha)
RHWangVaR_2Y = quantile(RHWangforwardCDF_2Y, probs=alpha)
CBDWangVaR_2Y = quantile(CBDWangforwardCDF_2Y, probs=alpha)
M6WangVaR_2Y = quantile(M6WangforwardCDF_2Y, probs=alpha)

LCPropVaR_2Y = quantile(LCPropforwardCDF_2Y, probs=alpha)
RHPropVaR_2Y = quantile(RHPropforwardCDF_2Y, probs=alpha)
CBDPropVaR_2Y = quantile(CBDPropforwardCDF_2Y, probs=alpha)
M6PropVaR_2Y = quantile(M6PropforwardCDF_2Y, probs=alpha)

LCDualVaR_2Y = quantile(LCDualforwardCDF_2Y, probs=alpha)
RHDualVaR_2Y = quantile(RHDualforwardCDF_2Y, probs=alpha)
CBDDualVaR_2Y = quantile(CBDDualforwardCDF_2Y, probs=alpha)
M6DualVaR_2Y = quantile(M6DualforwardCDF_2Y, probs=alpha)

LCGiniVaR_2Y = quantile(LCGiniforwardCDF_2Y, probs=alpha)
RHGiniVaR_2Y = quantile(RHGiniforwardCDF_2Y, probs=alpha)
CBDGiniVaR_2Y = quantile(CBDGiniforwardCDF_2Y, probs=alpha)
M6GiniVaR_2Y = quantile(M6GiniforwardCDF_2Y, probs=alpha)

LCExponentialVaR_2Y = quantile(LCExponentialforwardCDF_2Y, probs=alpha)
RHExponentialVaR_2Y = quantile(RHExponentialforwardCDF_2Y, probs=alpha)
CBDExponentialVaR_2Y = quantile(CBDExponentialforwardCDF_2Y, probs=alpha)
M6ExponentialVaR_2Y = quantile(M6ExponentialforwardCDF_2Y, probs=alpha)

LCStDevVaR_2Y = quantile(LCStDevforwardCDF_2Y, probs=alpha)
RHStDevVaR_2Y = quantile(RHStDevforwardCDF_2Y, probs=alpha)
CBDStDevVaR_2Y = quantile(CBDStDevforwardCDF_2Y, probs=alpha)
M6StDevVaR_2Y = quantile(M6StDevforwardCDF_2Y, probs=alpha)

LCVarVaR_2Y = quantile(LCVarforwardCDF_2Y, probs=alpha)
RHVarVaR_2Y = quantile(RHVarforwardCDF_2Y, probs=alpha)
CBDVarVaR_2Y = quantile(CBDVarforwardCDF_2Y, probs=alpha)
M6VarVaR_2Y = quantile(M6VarforwardCDF_2Y, probs=alpha)

LCMadVaR_2Y = quantile(LCMadforwardCDF_2Y, probs=alpha)
RHMadVaR_2Y = quantile(RHMadforwardCDF_2Y, probs=alpha)
CBDMadVaR_2Y = quantile(CBDMadforwardCDF_2Y, probs=alpha)
M6MadVaR_2Y = quantile(M6MadforwardCDF_2Y, probs=alpha)

LCWangSwapVaR_2Y = quantile(LCWangSwapCDF_2Y, probs=alpha)
RHWangSwapVaR_2Y = quantile(RHWangSwapCDF_2Y, probs=alpha)
CBDWangSwapVaR_2Y = quantile(CBDWangSwapCDF_2Y, probs=alpha)
M6WangSwapVaR_2Y = quantile(M6WangSwapCDF_2Y, probs=alpha)

LCPropSwapVaR_2Y = quantile(LCPropSwapCDF_2Y, probs=alpha)
RHPropSwapVaR_2Y = quantile(RHPropSwapCDF_2Y, probs=alpha)
CBDPropSwapVaR_2Y = quantile(CBDPropSwapCDF_2Y, probs=alpha)
M6PropSwapVaR_2Y = quantile(M6PropSwapCDF_2Y, probs=alpha)

LCDualSwapVaR_2Y = quantile(LCDualSwapCDF_2Y, probs=alpha)
RHDualSwapVaR_2Y = quantile(RHDualSwapCDF_2Y, probs=alpha)
CBDDualSwapVaR_2Y = quantile(CBDDualSwapCDF_2Y, probs=alpha)
M6DualSwapVaR_2Y = quantile(M6DualSwapCDF_2Y, probs=alpha)

LCGiniSwapVaR_2Y = quantile(LCGiniSwapCDF_2Y, probs=alpha)
RHGiniSwapVaR_2Y = quantile(RHGiniSwapCDF_2Y, probs=alpha)
CBDGiniSwapVaR_2Y = quantile(CBDGiniSwapCDF_2Y, probs=alpha)
M6GiniSwapVaR_2Y = quantile(M6GiniSwapCDF_2Y, probs=alpha)

LCExponentialSwapVaR_2Y = quantile(LCExponentialSwapCDF_2Y, probs=alpha)
RHExponentialSwapVaR_2Y = quantile(RHExponentialSwapCDF_2Y, probs=alpha)
CBDExponentialSwapVaR_2Y = quantile(CBDExponentialSwapCDF_2Y, probs=alpha)
M6ExponentialSwapVaR_2Y = quantile(M6ExponentialSwapCDF_2Y, probs=alpha)

LCStDevSwapVaR_2Y = quantile(LCStDevSwapCDF_2Y, probs=alpha)
RHStDevSwapVaR_2Y = quantile(RHStDevSwapCDF_2Y, probs=alpha)
CBDStDevSwapVaR_2Y = quantile(CBDStDevSwapCDF_2Y, probs=alpha)
M6StDevSwapVaR_2Y = quantile(M6StDevSwapCDF_2Y, probs=alpha)

LCVarSwapVaR_2Y = quantile(LCVarSwapCDF_2Y, probs=alpha)
RHVarSwapVaR_2Y = quantile(RHVarSwapCDF_2Y, probs=alpha)
CBDVarSwapVaR_2Y = quantile(CBDVarSwapCDF_2Y, probs=alpha)
M6VarSwapVaR_2Y = quantile(M6VarSwapCDF_2Y, probs=alpha)

LCMadSwapVaR_2Y = quantile(LCMadSwapCDF_2Y, probs=alpha)
RHMadSwapVaR_2Y = quantile(RHMadSwapCDF_2Y, probs=alpha)
CBDMadSwapVaR_2Y = quantile(CBDMadSwapCDF_2Y, probs=alpha)
M6MadSwapVaR_2Y = quantile(M6MadSwapCDF_2Y, probs=alpha)


################## EXPECTED SHORTFALL ################
LCWangES_2Y = mean( sort(LCWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHWangES_2Y = mean( sort(RHWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDWangES_2Y = mean( sort(CBDWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6WangES_2Y = mean( sort(M6WangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCPropES_2Y = mean( sort(LCPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHPropES_2Y = mean( sort(RHPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDPropES_2Y = mean( sort(CBDPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6PropES_2Y = mean( sort(M6PropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCDualES_2Y = mean( sort(LCDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHDualES_2Y = mean( sort(RHDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDDualES_2Y = mean( sort(CBDDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6DualES_2Y = mean( sort(M6DualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCGiniES_2Y = mean( sort(LCGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHGiniES_2Y = mean( sort(RHGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDGiniES_2Y = mean( sort(CBDGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6GiniES_2Y = mean( sort(M6GiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCExponentialES_2Y = mean( sort(LCExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHExponentialES_2Y = mean( sort(RHExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES_2Y = mean( sort(CBDExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES_2Y = mean( sort(M6ExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCStDevES_2Y = mean( sort(LCStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHStDevES_2Y = mean( sort(RHStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDStDevES_2Y = mean( sort(CBDStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6StDevES_2Y = mean( sort(M6StDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCVarES_2Y = mean( sort(LCVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHVarES_2Y = mean( sort(RHVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDVarES_2Y = mean( sort(CBDVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6VarES_2Y = mean( sort(M6VarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCMadES_2Y = mean( sort(LCMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHMadES_2Y = mean( sort(RHMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDMadES_2Y = mean( sort(CBDMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6MadES_2Y = mean( sort(M6MadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES_2Y = mean( sort(LCWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES_2Y = mean( sort(RHWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES_2Y = mean( sort(CBDWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES_2Y = mean( sort(M6WangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES_2Y = mean( sort(LCPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES_2Y = mean( sort(RHPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES_2Y = mean( sort(CBDPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES_2Y = mean( sort(M6PropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES_2Y = mean( sort(LCDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES_2Y = mean( sort(RHDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES_2Y = mean( sort(CBDDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES_2Y = mean( sort(M6DualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES_2Y = mean( sort(LCGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES_2Y = mean( sort(RHGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES_2Y = mean( sort(CBDGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES_2Y = mean( sort(M6GiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES_2Y = mean( sort(LCExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES_2Y = mean( sort(RHExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES_2Y = mean( sort(CBDExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES_2Y = mean( sort(M6ExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES_2Y = mean( sort(LCStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES_2Y = mean( sort(RHStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES_2Y = mean( sort(CBDStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES_2Y = mean( sort(M6StDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES_2Y = mean( sort(LCVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES_2Y = mean( sort(RHVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES_2Y = mean( sort(CBDVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES_2Y = mean( sort(M6VarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES_2Y = mean( sort(LCMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES_2Y = mean( sort(RHMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES_2Y = mean( sort(CBDMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES_2Y = mean( sort(M6MadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCWangES_2Y = mean( sort(LCWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHWangES_2Y = mean( sort(RHWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDWangES_2Y = mean( sort(CBDWangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6WangES_2Y = mean( sort(M6WangforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCPropES_2Y = mean( sort(LCPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHPropES_2Y = mean( sort(RHPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDPropES_2Y = mean( sort(CBDPropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6PropES_2Y = mean( sort(M6PropforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCDualES_2Y = mean( sort(LCDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHDualES_2Y = mean( sort(RHDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDDualES_2Y = mean( sort(CBDDualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6DualES_2Y = mean( sort(M6DualforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCGiniES_2Y = mean( sort(LCGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHGiniES_2Y = mean( sort(RHGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDGiniES_2Y = mean( sort(CBDGiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6GiniES_2Y = mean( sort(M6GiniforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCExponentialES_2Y = mean( sort(LCExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHExponentialES_2Y = mean( sort(RHExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES_2Y = mean( sort(CBDExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES_2Y = mean( sort(M6ExponentialforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCStDevES_2Y = mean( sort(LCStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHStDevES_2Y = mean( sort(RHStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDStDevES_2Y = mean( sort(CBDStDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6StDevES_2Y = mean( sort(M6StDevforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCVarES_2Y = mean( sort(LCVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHVarYES_2Y = mean( sort(RHVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDVarES_2Y = mean( sort(CBDVarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6VarES_2Y = mean( sort(M6VarforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCMadES_2Y = mean( sort(LCMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHMadES_2Y = mean( sort(RHMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDMadES_2Y = mean( sort(CBDMadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6MadES_2Y = mean( sort(M6MadforwardCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES_2Y = mean( sort(LCWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES_2Y = mean( sort(RHWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES_2Y = mean( sort(CBDWangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES_2Y = mean( sort(M6WangSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES_2Y = mean( sort(LCPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES_2Y = mean( sort(RHPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES_2Y = mean( sort(CBDPropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES_2Y = mean( sort(M6PropSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES_2Y = mean( sort(LCDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES_2Y = mean( sort(RHDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES_2Y = mean( sort(CBDDualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES_2Y = mean( sort(M6DualSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES_2Y = mean( sort(LCGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES_2Y = mean( sort(RHGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES_2Y = mean( sort(CBDGiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES_2Y = mean( sort(M6GiniSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES_2Y = mean( sort(LCExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES_2Y = mean( sort(RHExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES_2Y = mean( sort(CBDExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES_2Y = mean( sort(M6ExponentialSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES_2Y = mean( sort(LCStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES_2Y = mean( sort(RHStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES_2Y = mean( sort(CBDStDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES_2Y = mean( sort(M6StDevSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES_2Y =  mean( sort(LCVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES_2Y = mean( sort(RHVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES_2Y = mean( sort(CBDVarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES_2Y = mean( sort(M6VarSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES_2Y = mean( sort(LCMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES_2Y = mean( sort(RHMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES_2Y = mean( sort(CBDMadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES_2Y = mean( sort(M6MadSwapCDF_2Y)[1:(nsimCDF*alpha - 1)] )


Forward_VaR_table_2Y = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangVaR_2Y, LCPropVaR_2Y, LCDualVaR_2Y, LCGiniVaR_2Y, LCExponentialVaR_2Y, LCStDevVaR_2Y, LCVarVaR_2Y, LCMadVaR_2Y,
                                                               RHWangVaR_2Y, RHPropVaR_2Y, RHDualVaR_2Y, RHGiniVaR_2Y, RHExponentialVaR_2Y, RHStDevVaR_2Y, RHVarVaR_2Y, RHMadVaR_2Y,
                                                               CBDWangVaR_2Y, CBDPropVaR_2Y, CBDDualVaR_2Y, CBDGiniVaR_2Y, CBDExponentialVaR_2Y, CBDStDevVaR_2Y, CBDVarVaR_2Y, CBDMadVaR_2Y,
                                                               M6WangVaR_2Y, M6PropVaR_2Y, M6DualVaR_2Y, M6GiniVaR_2Y, M6ExponentialVaR_2Y, M6StDevVaR_2Y, M6VarVaR_2Y, M6MadVaR_2Y)
))
rownames(Forward_VaR_table_2Y) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Forward_VaR_table_2Y) = c("LC","RH","CBD","M6")

Swap_VaR_table_2Y = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapVaR_2Y, LCPropSwapVaR_2Y, LCDualSwapVaR_2Y, LCGiniSwapVaR_2Y, LCExponentialSwapVaR_2Y, LCStDevSwapVaR_2Y, LCVarSwapVaR_2Y, LCMadSwapVaR_2Y,
                                                            RHWangSwapVaR_2Y, RHPropSwapVaR_2Y, RHDualSwapVaR_2Y, RHGiniSwapVaR_2Y, RHExponentialSwapVaR_2Y, RHStDevSwapVaR_2Y, RHVarSwapVaR_2Y, RHMadSwapVaR_2Y,
                                                            CBDWangSwapVaR_2Y, CBDPropSwapVaR_2Y, CBDDualSwapVaR_2Y, CBDGiniSwapVaR_2Y, CBDExponentialSwapVaR_2Y, CBDStDevSwapVaR_2Y, CBDVarSwapVaR_2Y, CBDMadSwapVaR_2Y,
                                                            M6WangSwapVaR_2Y, M6PropSwapVaR_2Y, M6DualSwapVaR_2Y, M6GiniSwapVaR_2Y, M6ExponentialSwapVaR_2Y, M6StDevSwapVaR_2Y, M6VarSwapVaR_2Y, M6MadSwapVaR_2Y)
))
rownames(Swap_VaR_table_2Y) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Swap_VaR_table_2Y) = c("LC","RH","CBD","M6")

Forward_ES_table_2Y = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangES_2Y, LCPropES_2Y, LCDualES_2Y, LCGiniES_2Y, LCExponentialES_2Y, LCStDevES_2Y, LCVarES_2Y, LCMadES_2Y,
                                                              RHWangES_2Y, RHPropES_2Y, RHDualES_2Y, RHGiniES_2Y, RHExponentialES_2Y, RHStDevES_2Y, RHVarES_2Y, RHMadES_2Y,
                                                              CBDWangES_2Y, CBDPropES_2Y, CBDDualES_2Y, CBDGiniES_2Y, CBDExponentialES_2Y, CBDStDevES_2Y, CBDVarES_2Y, CBDMadES_2Y,
                                                              M6WangES_2Y, M6PropES_2Y, M6DualES_2Y, M6GiniES_2Y, M6ExponentialES_2Y, M6StDevES_2Y, M6VarES_2Y, M6MadES_2Y)
))
rownames(Forward_ES_table_2Y) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Forward_ES_table_2Y) = c("LC","RH","CBD","M6")

Swap_ES_table_2Y = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapES_2Y, LCPropSwapES_2Y, LCDualSwapES_2Y, LCGiniSwapES_2Y, LCExponentialSwapES_2Y, LCStDevSwapES_2Y, LCVarSwapES_2Y, LCMadSwapES_2Y,
                                                           RHWangSwapES_2Y, RHPropSwapES_2Y, RHDualSwapES_2Y, RHGiniSwapES_2Y, RHExponentialSwapES_2Y, RHStDevSwapES_2Y, RHVarSwapES_2Y, RHMadSwapES_2Y,
                                                           CBDWangSwapES_2Y, CBDPropSwapES_2Y, CBDDualSwapES_2Y, CBDGiniSwapES_2Y, CBDExponentialSwapES_2Y, CBDStDevSwapES_2Y, CBDVarSwapES_2Y, CBDMadSwapES_2Y,
                                                           M6WangSwapES_2Y, M6PropSwapES_2Y, M6DualSwapES_2Y, M6GiniSwapES_2Y, M6ExponentialSwapES_2Y, M6StDevSwapES_2Y, M6VarSwapES_2Y, M6MadSwapES_2Y)
))
rownames(Swap_ES_table_2Y) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
colnames(Swap_ES_table_2Y) = c("LC","RH","CBD","M6")