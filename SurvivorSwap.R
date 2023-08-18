survivorSwapPremium = function(k, years_for, notional_principal, lambda, model, premium, nsim=100){
  model = toString(model)
  premium = toString(premium)
  
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
  
  if (years_for == 1){
    # Set as global variables
    forecasted_qxt <<- mod_for$rates[k]
    forecasted_pxt <<- 1 - mod_for$rates[k]
    
  }
  else{
    forecasted_qxt <<- mod_for$rates[k,]
    forecasted_pxt <<- 1 - mod_for$rates[k,]
  }
  
  mod_mxtHat <- fitted(mod_fit, type = "rates")[k,]
  mod_pxtHat = 1 - mod_mxtHat
  mod_mxt_sim = diag(mod_sim$rates[k, , ]) # Indexed as [k, year, sim no.]
  mod_pxt_sim = 1 - mod_mxt_sim
  
  survival_rates_mat = matrix(nrow=nsim, ncol=years_for)
  i=1
  while(i <= years_for){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - lambda)
    
    S_t = risk_adjusted_pxt
    K_t = sum(discount_factor^(1:years_for) / sum( discount_factor^(1:years_for) ) * risk_adjusted_pxt[1:years_for])
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt = (forecasted_pxt)^(1/lambda)
    
    S_t =  risk_adjusted_pxt
    K_t = sum(discount_factor^(1:years_for) / sum( discount_factor^(1:years_for) ) * risk_adjusted_pxt[1:years_for])
  }
  if (premium == "Stdev") {
      S_t = forecasted_pxt
      K_t = ( mean( rowSums(survival_rates_mat) ) + lambda * sd( rowSums(survival_rates_mat) ) ) / years_for
  }
  if (premium == "Var") {
      S_t = forecasted_pxt
      K_t = ( mean( rowSums(survival_rates_mat) ) + lambda * notional_principal * (sd( rowSums(survival_rates_mat))^2 ) ) / years_for
  }
  
  risk_premium = log( sum(K_t * discount_factor^(-(1:years_for)) / discount_factor_simple^(1:years_for) ) / sum( S_t / discount_factor_simple^(-(1:years_for)) ) )
  return(risk_premium)
}