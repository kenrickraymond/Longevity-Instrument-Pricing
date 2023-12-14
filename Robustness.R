forwardCDF = function(forward_years, maturity_length, lambda, model, premium, pi, nsim=100){
  model = toString(model)
  premium = toString(premium)
  
  horizon = forward_years+maturity_length
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = horizon)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = horizon)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = horizon)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = horizon)
  }
  
  survival_rates_mat = matrix(nrow=nsim, ncol=horizon)
  i=1 # Loop counter
  k=5 # Indicates that we start with age 65
  while(i <= horizon){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    S_t = discount_factor^(maturity_length) * ( 1 - pnorm( qnorm( 1 - survival_rates_mat[,horizon]) - LCWanglambda) )
    K_t = discount_factor^(maturity_length) * mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,horizon]) - 0) )
  }
  if (premium == "Proportional") {
    S_t = discount_factor^(maturity_length) * ( survival_rates_mat[,horizon]^(1/lambda) )
    K_t =  discount_factor^(maturity_length) * mean( survival_rates_mat[,horizon] )
  }
  if (premium == "Dual") { # Check
    S_t = discount_factor^(maturity_length) * (1 -  (1 - survival_rates_mat[,horizon])^(lambda) )
    K_t =  discount_factor^(maturity_length) * mean(1 -  (1 - survival_rates_mat[,horizon]) )
  }
  if (premium == "Gini") { # Check
    S_t = discount_factor^(maturity_length) * ( ( (1 + lambda) * survival_rates_mat[,horizon] ) - lambda * mean( survival_rates_mat[,horizon]^2 ) )
    K_t =  discount_factor^(maturity_length) * mean( (1) * survival_rates_mat[,horizon] )
  }
  if (premium == "Exponential") { # I don't know if this K_t is correct
    S_t = discount_factor^(maturity_length) * ( 1 - exp( - lambda * survival_rates_mat[,horizon] ) )/(1-exp(- lambda) )
    K_t =  discount_factor^(maturity_length) * mean( 1 - exp( -1 * survival_rates_mat[,horizon] ) )/(1-exp(-1) )
  }
  if (premium == "Stdev") {
    S_t = discount_factor^(maturity_length) * ( ( survival_rates_mat[,horizon] ) + lambda * sd( survival_rates_mat[,horizon] ) )
    K_t = discount_factor^(maturity_length) * mean( survival_rates_mat[,horizon] )
  }
  if (premium == "Var") {
    S_t = discount_factor^(maturity_length) * ( ( survival_rates_mat[,horizon] ) + lambda * var( survival_rates_mat[,horizon] ) )
    K_t = discount_factor^(maturity_length) * mean( survival_rates_mat[,horizon] )
  }
  if (premium == "Mad") {
    S_t = discount_factor^(maturity_length) * ( (survival_rates_mat[,horizon] ) + lambda * mad( survival_rates_mat[,horizon] ) ) # Recheck this
    K_t = discount_factor^(maturity_length) * quantile(survival_rates_mat[,horizon], probs = 0.5, na.rm = FALSE)
  }
  
  contract_value = S_t - (1+ pi) * K_t
  return(contract_value)
}