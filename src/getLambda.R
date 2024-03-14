# Variables that need to be defined in the global environment:
# ages.fit -> age_length
# interest_rate -> discount_factor
# fitted mortality model i.e. LCfit, RHfit, CBDfit, M6fit

# User-defined inputs:
# init_est: initial estimate for lambda, the user will have to obtain a lambda value close to the actual minima for the root-finding algorithm to converge
## Test out different values of lambda until the user obtains one that will result in a value close to total
### init_est value is not important for real world measures such as Stdev, Var, Mad. Value is merely a placeholder
# model: LC, RH, CBD, M6
# premium: Wang, Proportional, Dual, Gini, Exponential, Stdev, Var, Mad

# Procedure: Uses a root-finding algorithm involving nonlinear minimization to find local extrema (minima) value for lambda
# Outputs: pricing parameter lambda (Required input for survivor forward and survivor swaps)

getLambda = function(init_est, model, premium){
  model = toString(model)
  premium = toString(premium)
  
  k = 6 # Indicates that we start with age 65
  age_length = length(ages.fit)
  
  if (model == "LC"){
    # Model implied fitted rates
    qxt = fitted(LCfit, type = "rates")
    pxt = 1 - qxt[k:age_length,] # We assume that the annuity starts paying out at age 65 and that the entire reference population is dead by age 90.
  }
  if (model == "RH"){
    qxt = fitted(RHfit, type = "rates")
    pxt = 1 - qxt[k:age_length,]
  }
  if (model == "CBD"){
    qxt = fitted(CBDfit, type = "rates")
    pxt = 1 - qxt[k:age_length,]
  }
  if (model == "M6"){
    qxt = fitted(M6fit, type = "rates")
    pxt = 1 - qxt[k:age_length,]
  }
  
  wang_sse = function(lambda) {
    sum( payment * sum( discount_factor^(k:age_length) * (1 - pnorm(qnorm(1- diag(pxt) ) - lambda)) )  - total )^2
  }
  
  proportional_hazard_sse = function(lambda) {
    sum( payment * sum( discount_factor^(k:age_length) * diag(pxt)^(1/lambda) ) - total )^2
  }
  
  dual_power_sse = function(lambda) {
    sum( payment * sum( discount_factor^(k:age_length) * (1 -  (1 - diag(pxt))^(lambda) ) ) - total )^2
  }
  
  gini_sse = function(lambda) {
    sum( payment * sum( discount_factor^(k:age_length) * ( (1 + lambda) * diag(pxt) - lambda * diag(pxt)^2 ) ) - total )^2
  }
  
  exponential_sse = function(lambda) {
    sum( payment * sum( discount_factor^(k:age_length) * ( 1 - exp( - lambda * diag(pxt) ) )/(1-exp(- lambda) ) ) - total )^2
  }
  
  if (premium == "Wang") {
    lambda = nlm(wang_sse, init_est)$estimate
  }
  if (premium == "Proportional") {
    lambda = nlm(proportional_hazard_sse, init_est)$estimate
  }
  if (premium == "Dual") {
    lambda = nlm(dual_power_sse, init_est)$estimate
  }
  if (premium == "Gini") {
    lambda = nlm(gini_sse, init_est)$estimate
  }
  if (premium == "Exponential") {
    lambda = nlm(exponential_sse, init_est)$estimate
  }
  if (premium == "Stdev") {
    lambda =  ( total - sum(discount_factor^(k:age_length) * payment * diag(pxt)) ) / sqrt( sum( discount_factor^(k:age_length) * payment  * diag(pxt) * ( discount_factor^(k:age_length) * payment * (1 - diag(pxt)))) ) # (Expectation of the Portfolio - Expectation of the Risk) / Stdev Risk
  }
  if (premium == "Var") {
    lambda =  ( total - sum(discount_factor^(k:age_length) * payment * diag(pxt)) ) / sum( discount_factor^(k:age_length) * payment  * diag(pxt) * ( discount_factor^(k:age_length) * payment * (1 - diag(pxt))))  # (Expectation of the Portfolio - Expectation of the Risk) / Var Risk
  }
  if (premium == "Mad") {
    lambda =  ( total - sum(discount_factor^(k:age_length) * payment * quantile(diag(pxt), probs = 0.5, na.rm = FALSE) ) ) / sum( discount_factor^(k:age_length) * payment * mad(diag(pxt))) 
  }
  return(lambda)
}
