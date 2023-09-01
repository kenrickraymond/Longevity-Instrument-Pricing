survivorForwardPremium = function(years_for, lambda, model, premium, nsim=100){
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
  
  survival_rates_mat = matrix(nrow=nsim, ncol=years_for)
  i=1
  k=5
  while(i <= years_for){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    S_t = discount_factor^(years_for) * mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - LCWanglambda) )
    K_t = discount_factor^(years_for) * mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - 0) )
  }
  if (premium == "Proportional") {
    S_t = discount_factor^(years_for) * mean( survival_rates_mat[,years_for]^(1/lambda) )
    K_t =  discount_factor^(years_for) * mean( survival_rates_mat[,years_for] )
  }
  if (premium == "Dual") { # Check
    S_t = discount_factor^(years_for) * mean(1 -  (1 - survival_rates_mat[,years_for])^(lambda) )
    K_t =  discount_factor^(years_for) * mean(1 -  (1 - survival_rates_mat[,years_for]) )
  }
  if (premium == "Gini") { # Check
    S_t = discount_factor^(years_for) * ( mean( (1 + lambda) * survival_rates_mat[,years_for] ) - lambda * mean( survival_rates_mat[,years_for]^2 ) )
    K_t =  discount_factor^(years_for) * mean( (1) * survival_rates_mat[,years_for] )
  }
  if (premium == "Exponential") { # I don't know if this K_t is correct
    S_t = discount_factor^(years_for) * mean( 1 - exp( - lambda * survival_rates_mat[,years_for] ) )/(1-exp(- lambda) )
    K_t =  discount_factor^(years_for) * mean( 1 - exp( -1 * survival_rates_mat[,years_for] ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
      S_t = discount_factor^(years_for) * ( mean( survival_rates_mat[,years_for] ) + lambda * sd( survival_rates_mat[,years_for] ) )
      K_t = discount_factor^(years_for) * mean( survival_rates_mat[,years_for] )
  }
  if (premium == "Var") {
    S_t = discount_factor^(years_for) * ( mean( survival_rates_mat[,years_for] ) + lambda * var( survival_rates_mat[,years_for] ) )
    K_t = discount_factor^(years_for) * mean( survival_rates_mat[,years_for] )
  }
  if (premium == "Mad") {
    S_t = discount_factor^(years_for) * ( quantile(survival_rates_mat[,years_for], probs = 0.5, na.rm = FALSE) + lambda * mad( survival_rates_mat[,years_for] ) )
    K_t = discount_factor^(years_for) * quantile(survival_rates_mat[,years_for], probs = 0.5, na.rm = FALSE)
  }
  
  risk_premium = (S_t / K_t) - 1
  return(risk_premium)
}