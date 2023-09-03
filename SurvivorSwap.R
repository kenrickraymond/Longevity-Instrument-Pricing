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

# Outputs: list of risk-adjustment term for survival swap containing the risk-adjustment term for each of 1,2,...,years_for years

survivorSwapPremium = function(years_for, lambda, model, premium, nsim=100){
  model = toString(model)
  premium = toString(premium)
  
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
  i=1
  k=5 # References the age 65 as the starting age
  while(i <= years_for){
    survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
    i=i+1
  }
  
  if (premium == "Wang") {
    S_t = sum( discount_factor^(1:years_for) * ( 1 - pnorm( qnorm( 1 - colMeans(survival_rates_mat)) - lambda) ) )
    K_t = sum( discount_factor^(1:years_for) * ( 1 - pnorm( qnorm( 1 - colMeans(survival_rates_mat)) - 0) ) )
  }
  if (premium == "Proportional") {
    S_t = sum( discount_factor^(1:years_for) * ( colMeans(survival_rates_mat)^(1/lambda) ) )
    K_t = sum( discount_factor^(1:years_for) * ( colMeans(survival_rates_mat) ) )
  }
  if (premium == "Dual") {
    S_t = sum( discount_factor^(1:years_for) * (1 -  (1 - colMeans(survival_rates_mat) )^(lambda) ) )
    K_t = sum( discount_factor^(1:years_for) * (1 -  (1 - colMeans( survival_rates_mat) ) ) )
  }
  if (premium == "Gini") {
    S_t = sum( discount_factor^(1:years_for) * ( ( (1 + lambda) * colMeans( survival_rates_mat ) ) - lambda * colMeans( survival_rates_mat )^2 ) )
    K_t =  sum( discount_factor^(1:years_for) * ( (1) * colMeans( survival_rates_mat ) ) )
  }
  if (premium == "Exponential") {
    S_t = sum( discount_factor^(1:years_for) * ( 1 - exp( - lambda * colMeans( survival_rates_mat ) ) )/(1-exp(- lambda) ) )
    K_t =  sum( discount_factor^(1:years_for) * ( 1 - exp( -1 * colMeans( survival_rates_mat ) ) )/(1-exp(-1) ) )
  }
  if (premium == "Stdev") {
    if (years_for == 1){ # The colMeans(.) function returns NA for single-value inputs.
      S_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
      K_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
    }
    else{ 
      S_t = sum(discount_factor^(1:years_for) * (colMeans(survival_rates_mat) + lambda * sd( colMeans(survival_rates_mat ) ) ) )
      K_t = sum(discount_factor^(1:years_for) * colMeans(survival_rates_mat) )
    }
  }
  if (premium == "Var") {
    if (years_for == 1){
      S_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
      K_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
    }
    else{
      S_t = sum(discount_factor^(1:years_for) * (colMeans(survival_rates_mat) + lambda * var( colMeans(survival_rates_mat ) ) ) )
      K_t = sum(discount_factor^(1:years_for) * colMeans(survival_rates_mat) )  
    }
  }
  if (premium == "Mad") {
    if (years_for == 1){
      S_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
      K_t = sum(discount_factor^(1:years_for) * (mean(survival_rates_mat)))
    }
    else{
      S_t = sum(discount_factor^(1:years_for) * ( quantile( colMeans(survival_rates_mat), probs = 0.5, na.rm = FALSE) + lambda * mad( colMeans(survival_rates_mat) ) ) )
      K_t = sum(discount_factor^(1:years_for) * quantile( colMeans(survival_rates_mat), probs = 0.5, na.rm = FALSE) )  
    }
  }
  
  risk_premium = (S_t / K_t) - 1
  return(risk_premium)
}