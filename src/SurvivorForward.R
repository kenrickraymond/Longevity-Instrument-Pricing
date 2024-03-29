# Variables that need to be defined in the global environment:
# payment
# total
# interest_rate -> discount_factor
# fitted mortality model i.e. LCfit, RHfit, CBDfit, M6fit

# User-defined inputs:
# years_for = forecasting horizon (years_for ahead swap)
# lambda: use getLambda.R to obtain lambda value for model and premium
# model: LC, RH, CBD, M6
# premium: Wang, Proportional, Dual, Gini, Exponential, Stdev, Var, Mad
# nsim: number of future mortality scenarios generated

# Outputs: list of risk-adjustment term for survival forward containing the risk-adjustment term for each of 1,2,...,years_for years

survivorForwardPremium = function(years_for, lambda, model, premium, nsim=100){
  model = toString(model)
  premium = toString(premium)
  # Model Selection
  if (model == "LC"){
    mod_sim = simulate(LCfit, nsim = nsim, h = years_for+1)
  }
  if (model == "RH"){
    mod_sim = simulate(RHfit, nsim = nsim, h = years_for+1)
  }
  if (model == "CBD"){
    mod_sim = simulate(CBDfit, nsim = nsim, h = years_for+1)
  }
  if (model == "M6"){
    mod_sim = simulate(M6fit, nsim = nsim, h = years_for+1)
  }
  
  survival_rates_mat = matrix(nrow=nsim, ncol=years_for)
  i=1 # Loop counter
  k=5 # Indicates that we start with age 65
  while(i <= years_for){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    S_t = discount_factor^(years_for) * mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - lambda) )
    K_t = discount_factor^(years_for) * mean( 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - 0) )
  }
  if (premium == "Proportional") {
    S_t = discount_factor^(years_for) * mean( survival_rates_mat[,years_for]^(1/lambda) )
    K_t =  discount_factor^(years_for) * mean( survival_rates_mat[,years_for] )
  }
  if (premium == "Dual") { 
    S_t = discount_factor^(years_for) * mean(1 -  (1 - survival_rates_mat[,years_for])^(lambda) )
    K_t =  discount_factor^(years_for) * mean(1 -  (1 - survival_rates_mat[,years_for]) )
  }
  if (premium == "Gini") { 
    S_t = discount_factor^(years_for) * ( mean( (1 + lambda) * survival_rates_mat[,years_for] ) - lambda * mean( survival_rates_mat[,years_for]^2 ) )
    K_t =  discount_factor^(years_for) * mean( (1) * survival_rates_mat[,years_for] )
  }
  if (premium == "Exponential") { 
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
