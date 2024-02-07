forwardCDF = function(maturity, x, lambda, model, premium, pi, nsim=1000){
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
  survival_probability = (1-mod_sim$rates)
    
  if (premium == "Wang") {
    S_t = ( 1 - pnorm( qnorm( 1 - survival_probability[x, maturity, ]) - LCWanglambda) )
    K_t = mean( 1 - pnorm( qnorm( 1 - survival_probability[x, maturity, ]) - 0) )
  }
  if (premium == "Proportional") {
    S_t = ( survival_probability[x, maturity, ]^(1/lambda) )
    K_t =  mean( survival_probability[x, maturity, ] )
  }
  if (premium == "Dual") { # Check
    S_t = (1 -  (1 - survival_probability[x, maturity, ])^(lambda) )
    K_t =  mean(1 -  (1 - survival_probability[x, maturity, ]) )
  }
  if (premium == "Gini") { # Check
    S_t = ( ( (1 + lambda) * survival_probability[x, maturity, ] ) - lambda * mean( survival_probability[x, maturity, ]^2 ) )
    K_t =  mean( (1) * survival_probability[x, maturity, ] )
  }
  if (premium == "Exponential") { # I don't know if this K_t is correct
    S_t = ( 1 - exp( - lambda * survival_probability[x, maturity, ] ) )/(1-exp(- lambda) )
    K_t =  mean( 1 - exp( -1 * survival_probability[x, maturity, ] ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = ( ( survival_probability[x, maturity, ] ) + lambda * sd( survival_probability[x, maturity, ] ) )
    K_t = mean( survival_probability[x, maturity, ] )
  }
  if (premium == "Var") {
    S_t = ( ( survival_probability[x, maturity, ] ) + lambda * var( survival_probability[x, maturity, ] ) )
    K_t = mean( survival_probability[x, maturity, ] )
  }
  if (premium == "Mad") {
    S_t = ( (survival_probability[x, maturity, ] ) + lambda * mad( survival_probability[x, maturity, ] ) ) # Recheck this
    K_t = mean(survival_probability[x, maturity, ])
  }
  
  contract_value = S_t - (1+pi) * K_t
  return(contract_value)
}

# Control = 5 for 5-year VaR and control = 9 for 1-year VaR
SwapCDF = function(forward_years, x, control, lambda, model, premium, pi, nsim=100){
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
    survival_df[,i] = survival_probability[control+i-1,1+i,]  # Change the 5 for a different VaR
  }
  
  if (premium == "Wang") {
    S_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - LCWanglambda) )
    K_t = rowSums( 1 - pnorm( qnorm( 1 - (survival_df) ) - 0) )
  }
  if (premium == "Proportional") {
    S_t = rowSums( (survival_df)^(1/lambda) )
    K_t =  rowSums( survival_df ) 
  }
  if (premium == "Dual") { # Check
    S_t = rowSums(1 -  (1 - (survival_df))^(lambda) )
    K_t = rowSums(1 -  (1 - (survival_df)) )
  }
  if (premium == "Gini") { # Check
    S_t = rowSums( ( (1 + lambda) * (survival_df) ) - lambda * ( (survival_df)^2 ) )
    K_t = rowSums( (1) * (survival_df) )
  }
  if (premium == "Exponential") { # I don't know if this K_t is correct
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
nsimCDF = 3000
x = 5 # Indicates Age 65 + x

LCWangForwardCDF = forwardCDF(forward_years,x, LCWanglambda, "LC", "Wang", pi=LC_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
RHWangForwardCDF = forwardCDF(forward_years,x, RHWanglambda, "RH", "Wang", pi=RH_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
CBDWangForwardCDF = forwardCDF(forward_years,x, CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
M6WangForwardCDF = forwardCDF(forward_years,x, M6Wanglambda, "M6", "Wang", pi=M6_Wang_Forward_Premium[forward_years], nsim=nsimCDF)

LCPropForwardCDF = forwardCDF(forward_years,x, LCProplambda, "LC", "Proportional", pi=LC_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
RHPropForwardCDF = forwardCDF(forward_years,x, RHProplambda, "RH", "Proportional", pi=RH_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
CBDPropForwardCDF = forwardCDF(forward_years,x, CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
M6PropForwardCDF = forwardCDF(forward_years,x, M6Proplambda, "M6", "Proportional", pi=M6_Prop_Forward_Premium[forward_years], nsim=nsimCDF)

LCDualForwardCDF = forwardCDF(forward_years,x, LCDuallambda, "LC", "Dual", pi=LC_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
RHDualForwardCDF = forwardCDF(forward_years,x, RHDuallambda, "RH", "Dual", pi=RH_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
CBDDualForwardCDF = forwardCDF(forward_years,x, CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
M6DualForwardCDF = forwardCDF(forward_years,x, M6Duallambda, "M6", "Dual", pi=M6_Dual_Forward_Premium[forward_years], nsim=nsimCDF)

LCGiniForwardCDF = forwardCDF(forward_years,x, LCGinilambda, "LC", "Gini", pi=LC_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
RHGiniForwardCDF = forwardCDF(forward_years,x, RHGinilambda, "RH", "Gini", pi=RH_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
CBDGiniForwardCDF = forwardCDF(forward_years,x, CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
M6GiniForwardCDF = forwardCDF(forward_years,x, M6Ginilambda, "M6", "Gini", pi=M6_Gini_Forward_Premium[forward_years], nsim=nsimCDF)

LCExponentialForwardCDF = forwardCDF(forward_years,x, LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
RHExponentialForwardCDF = forwardCDF(forward_years,x, RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
CBDExponentialForwardCDF = forwardCDF(forward_years,x, CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
M6ExponentialForwardCDF = forwardCDF(forward_years,x, M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)

LCStDevForwardCDF = forwardCDF(forward_years,x, LCStDevlambda, "LC", "StDev", pi=LC_Std_Forward_Premium[forward_years], nsim=nsimCDF)
RHStDevForwardCDF = forwardCDF(forward_years,x, RHStDevlambda, "RH", "StDev", pi=RH_Std_Forward_Premium[forward_years], nsim=nsimCDF)
CBDStDevForwardCDF = forwardCDF(forward_years,x, CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Forward_Premium[forward_years], nsim=nsimCDF)
M6StDevForwardCDF = forwardCDF(forward_years,x, M6StDevlambda, "M6", "StDev", pi=M6_Std_Forward_Premium[forward_years], nsim=nsimCDF)

LCVarForwardCDF = forwardCDF(forward_years,x, LCVarlambda, "LC", "Var", pi=LC_Var_Forward_Premium[forward_years], nsim=nsimCDF)
RHVarForwardCDF = forwardCDF(forward_years,x, RHVarlambda, "RH", "Var", pi=RH_Var_Forward_Premium[forward_years], nsim=nsimCDF)
CBDVarForwardCDF = forwardCDF(forward_years,x, CBDVarlambda, "CBD", "Var", pi=CBD_Var_Forward_Premium[forward_years], nsim=nsimCDF)
M6VarForwardCDF = forwardCDF(forward_years,x, M6Varlambda, "M6", "Var", pi=M6_Var_Forward_Premium[forward_years], nsim=nsimCDF)

LCMadForwardCDF = forwardCDF(forward_years,x, LCMadlambda, "LC", "Mad", pi=LC_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
RHMadForwardCDF = forwardCDF(forward_years,x, RHMadlambda, "RH", "Mad", pi=RH_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
CBDMadForwardCDF = forwardCDF(forward_years,x, CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
M6MadForwardCDF = forwardCDF(forward_years,x, M6Madlambda, "M6", "Mad", pi=M6_Mad_Forward_Premium[forward_years], nsim=nsimCDF)

############ SWAP CDF ###############
LCWangSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCWanglambda, "LC", "Wang", pi=LC_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
RHWangSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHWanglambda, "RH", "Wang", pi=RH_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
CBDWangSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDWanglambda, "CBD", "Wang", pi=CBD_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
M6WangSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Wanglambda, "M6", "Wang", pi=M6_Wang_Swap_Premium[forward_years], nsim=nsimCDF)

LCPropSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCProplambda, "LC", "Proportional", pi=LC_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
RHPropSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHProplambda, "RH", "Proportional", pi=RH_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
CBDPropSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDProplambda, "CBD", "Proportional", pi=CBD_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
M6PropSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Proplambda, "M6", "Proportional", pi=M6_Prop_Swap_Premium[forward_years], nsim=nsimCDF)

LCDualSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCDuallambda, "LC", "Dual", pi=LC_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
RHDualSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHDuallambda, "RH", "Dual", pi=RH_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
CBDDualSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDDuallambda, "CBD", "Dual", pi=CBD_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
M6DualSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Duallambda, "M6", "Dual", pi=M6_Dual_Swap_Premium[forward_years], nsim=nsimCDF)

LCGiniSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCGinilambda, "LC", "Gini", pi=LC_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
RHGiniSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHGinilambda, "RH", "Gini", pi=RH_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
CBDGiniSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDGinilambda, "CBD", "Gini", pi=CBD_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
M6GiniSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Ginilambda, "M6", "Gini", pi=M6_Gini_Swap_Premium[forward_years], nsim=nsimCDF)

LCExponentialSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCExponentiallambda, "LC", "Exponential", pi=LC_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
RHExponentialSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHExponentiallambda, "RH", "Exponential", pi=RH_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
CBDExponentialSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDExponentiallambda, "CBD", "Exponential", pi=CBD_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
M6ExponentialSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Exponentiallambda, "M6", "Exponential", pi=M6_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)

LCStDevSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCStDevlambda, "LC", "StDev", pi=LC_Std_Swap_Premium[forward_years], nsim=nsimCDF)
RHStDevSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHStDevlambda, "RH", "StDev", pi=RH_Std_Swap_Premium[forward_years], nsim=nsimCDF)
CBDStDevSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDStDevlambda, "CBD", "StDev", pi=CBD_Std_Swap_Premium[forward_years], nsim=nsimCDF)
M6StDevSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6StDevlambda, "M6", "StDev", pi=M6_Std_Swap_Premium[forward_years], nsim=nsimCDF)

LCVarSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCVarlambda, "LC", "Var", pi=LC_Var_Swap_Premium[forward_years], nsim=nsimCDF)
RHVarSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHVarlambda, "RH", "Var", pi=RH_Var_Swap_Premium[forward_years], nsim=nsimCDF)
CBDVarSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDVarlambda, "CBD", "Var", pi=CBD_Var_Swap_Premium[forward_years], nsim=nsimCDF)
M6VarSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Varlambda, "M6", "Var", pi=M6_Var_Swap_Premium[forward_years], nsim=nsimCDF)

LCMadSwapCDF = SwapCDF(forward_years,x, control=9, lambda= LCMadlambda, "LC", "Mad", pi=LC_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
RHMadSwapCDF = SwapCDF(forward_years,x, control=9, lambda= RHMadlambda, "RH", "Mad", pi=RH_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
CBDMadSwapCDF = SwapCDF(forward_years,x, control=9, lambda= CBDMadlambda, "CBD", "Mad", pi=CBD_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
M6MadSwapCDF = SwapCDF(forward_years,x, control=9, lambda= M6Madlambda, "M6", "Mad", pi=M6_Mad_Swap_Premium[forward_years], nsim=nsimCDF)

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

# format(abs(Swap_VaR_table), scientific=2, digits=4)

write.csv( format(abs(Forward_VaR_table), scientific=FALSE, digits=4)  )
# write.csv( format(abs(Swap_VaR_table), scientific=FALSE, digits=4) )

write.csv( format(abs(Forward_ES_table), scientific=FALSE, digits=4)  )
# write.csv( format(abs(Swap_ES_table), scientific=FALSE, digits=4) )

write.csv( format( abs(data.frame(Swap_VaR_table, Swap_ES_table)), scientific = TRUE, digits=4))

# Difference in range
# diff = as.numeric(sapply(abs(Forward_VaR_table), range)[2,] - sapply(abs(Forward_VaR_table), range)[1,])
# format(sapply(abs(Forward_VaR_table), range), scientific=TRUE, digits=4) 
# format(diff, scientific=TRUE, digits=4)
# 
# 
# diff = as.numeric(sapply(abs(Swap_VaR_table), range)[2,] - sapply(abs(Swap_VaR_table), range)[1,])
# format(sapply(abs(Swap_VaR_table), range), scientific=TRUE, digits=4) 
# format(diff, scientific=TRUE, digits=4)
# 
# diff = as.numeric(sapply(abs(Forward_ES_table), range)[2,] - sapply(abs(Forward_ES_table), range)[1,])
# format(sapply(abs(Forward_ES_table), range), scientific=TRUE, digits=4) 
# format(diff, scientific=TRUE, digits=4)
# 
# 
# diff = as.numeric(sapply(abs(Swap_ES_table), range)[2,] - sapply(abs(Swap_ES_table), range)[1,])
# format(sapply(abs(Swap_ES_table), range), scientific=TRUE, digits=4) 
# format(diff, scientific=TRUE, digits=4)


# Tables for paper
# 
# write.csv(format(data.frame(LC.premium.table,RH.premium.table), scientific=TRUE, digits=4) )
# 
# write.csv(format(data.frame(CBD.premium.table,M6.premium.table), scientific=TRUE, digits=4) )
# 
# write.csv(format(lambda_table, scientific=TRUE, digits=4) )

