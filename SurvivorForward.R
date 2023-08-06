survivorForwardPremium = function(k, years_for, notional_principal, lambda, model, premium, nsim=100){
  model = toString(model)
  premium = toString(premium)
  # Model Selection
  if (model == "LC"){
    mod_for = forecast(LCfit, h=years_for)
    mod_fit = LCfit
    mod_sim = simulate(LCfit, nsim = nsim, h = years_for)
  }
  if (model == "RH"){
    mod_for = forecast(RHfit, h=years_for)
    mod_fit = RHfit
    mod_sim = simulate(RHfit, nsim = nsim, h = years_for)
  }
  if (model == "CBD"){
    mod_for = forecast(CBDfit, h=years_for)
    mod_fit = CBDfit
    mod_sim = simulate(CBDfit, nsim = nsim, h = years_for)
  }
  if (model == "M6"){
    mod_for = forecast(M6fit, h=years_for)
    mod_fit = M6fit
    mod_sim = simulate(M6fit, nsim = nsim, h = years_for)
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
  mod_mxt_sim = diag(mod_sim$rates[k, , ]) # Indexed as [k, year, sim no.]
  mod_pxt_sim = 1 - mod_mxt_sim
  
  # Include last fitted survival rate p_{x,0} to forecasted_pxt
  forecasted_pxt = c( tail(mod_pxtHat, n=1), forecasted_pxt )
  mod_pxt_sim = c( tail(mod_pxtHat, n=1), mod_pxt_sim )
  
  if (premium == "Wang") {
    risk_adjusted_pxt = pnorm(qnorm( forecasted_pxt ) - lambda)
    
    # Floating Leg for an s-forward, S(T)
    S_t =  tail(risk_adjusted_pxt, n=1)
    
    # Constant fixed-Leg 
    K_t = mean(risk_adjusted_pxt)
    
    price = notional_principal * discount_factor^(years_for) * (S_t - K_t)
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt =  ( forecasted_pxt )^(1/lambda)
    
    S_t =  tail(risk_adjusted_pxt, n=1)
    K_t = mean(risk_adjusted_pxt)
    
    price = notional_principal * discount_factor^(years_for) * (S_t - K_t)
  }
  if (premium == "Stdev") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = tail(forecasted_pxt, n=1)
      K_t = mean( mod_pxt_sim )
    }
    else{
      S_t = tail(forecasted_pxt, n=1)
      K_t = mean( mod_pxt_sim ) + lambda * sd( mod_pxt_sim )
    }
  }
  if (premium == "Var") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = tail(forecasted_pxt, n=1)
      K_t = mean( mod_pxt_sim )
    }
    else{
      S_t = tail(forecasted_pxt, n=1)
      K_t = mean( mod_pxt_sim ) + lambda * var( mod_pxt_sim )
    }
  }
  
  # Percentage basis
  risk_premium = ( log(K_t / S_t) /  years_for )
  return(risk_premium)
}