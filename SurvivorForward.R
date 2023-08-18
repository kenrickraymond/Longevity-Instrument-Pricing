survivorForwardPremium = function(k, years_for, notional_principal, lambda, model, premium, nsim=100){
  model = toString(model)
  premium = toString(premium)
  # Model Selection
  if (model == "LC"){
    mod_for = forecast(LCfit, h=years_for)
    mod_fit = LCfit
    mod_sim = simulate(LCfit, nsim = nsim, h = years_for+1)
  }
  if (model == "RH"){
    mod_for = forecast(RHfit, h=years_for)
    mod_fit = RHfit
    mod_sim = simulate(RHfit, nsim = nsim, h = years_for+1)
  }
  if (model == "CBD"){
    mod_for = forecast(CBDfit, h=years_for)
    mod_fit = CBDfit
    mod_sim = simulate(CBDfit, nsim = nsim, h = years_for+1)
  }
  if (model == "M6"){
    mod_for = forecast(M6fit, h=years_for)
    mod_fit = M6fit
    mod_sim = simulate(M6fit, nsim = nsim, h = years_for+1)
  }
  
  # Forecasting
  ## First year
  if (years_for == 1){
    # Set as global variables
    forecasted_qxt <<- mod_for$rates[k]
    forecasted_pxt <<- 1 - mod_for$rates[k]
    
  }
  ## Other years
  else{
    forecasted_qxt <<- mod_for$rates[k,]
    forecasted_pxt <<- 1 - mod_for$rates[k,]
  }
  
  # Model implied fitted rate
  mod_mxtHat <- fitted(mod_fit, type = "rates")[k,]
  mod_pxtHat = 1 - mod_mxtHat
  mod_mxt_sim = mod_sim$rates[k, years_for, ] # Indexed as [k, year, sim no.]
  mod_pxt_sim = 1 - mod_mxt_sim
  
  survival_rates_mat = matrix(nrow=nsim, ncol=years_for)
  i=1
  while(i <= years_for){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    S_t = mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - LCWanglambda) )
    K_t = mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - 0) )
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt =  ( forecasted_pxt )^(1/lambda)
    
    S_t = mean( survival_rates_mat[,years_for]^(1/lambda) )
    K_t =  mean( survival_rates_mat[,years_for] )
  }
  if (premium == "Stdev") {
      S_t = mean( survival_rates_mat[,years_for] )
      K_t = mean( survival_rates_mat[,years_for] ) + lambda * sd( survival_rates_mat[,years_for] )
  }
  if (premium == "Var") {
    S_t = mean( survival_rates_mat[,years_for] )
    K_t = mean( survival_rates_mat[,years_for] ) + lambda * var( survival_rates_mat[,years_for] )
  }
  
  # risk_premium = ( log(K_t / S_t) /  years_for )
  risk_premium = (S_t / K_t) - 1
  return(risk_premium)
}