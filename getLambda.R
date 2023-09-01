getLambda = function(init_est, model, premium){
  model = toString(model)
  premium = toString(premium)
  
  if (model == "LC"){
    qxt = LCfit$Dxt / LCfit$Ext
    # We assume that annuities pay individuals beginning aged 65
    pxt = 1 - qxt[6:30,]
  }
  if (model == "RH"){
    qxt = RHfit$Dxt / RHfit$Ext
    pxt = 1 - qxt[6:30,]
  }
  if (model == "CBD"){
    qxt = CBDfit$Dxt / CBDfit$Ext
    pxt = 1 - qxt[6:30,]
  }
  if (model == "M6"){
    qxt = M6fit$Dxt / M6fit$Ext
    pxt = 1 - qxt[6:30,]
  }
  
  # We assume that the annuity starts paying out at age 65 and that the entire reference population is dead by age 90.
  age_length = length(pxt)
  
  wang_sse = function(lambda) {
    # Note that all other cohorts are implicitly linked to the first cohort year through LC_pxt[,1]
    sum( payment * sum( discount_factor^(6:30) * (1 - pnorm(qnorm(1- diag(pxt) ) - lambda)) )  - total )^2
  }
  
  proportional_hazard_sse = function(lambda) {
    sum( payment * sum( discount_factor^(6:30) * diag(pxt)^(1/lambda) ) - total )^2
  }
  
  dual_power_sse = function(lambda) {
    sum( payment * sum( discount_factor^(6:30) * (1 -  (1 - diag(pxt))^(lambda) ) ) - total )^2
  }
  
  gini_sse = function(lambda) {
    sum( payment * sum( discount_factor^(6:30) * ( (1 + lambda) * diag(pxt) - lambda * diag(pxt)^2 ) ) - total )^2
  }
  
  exponential_sse = function(lambda) {
    sum( payment * sum( discount_factor^(6:30) * ( 1 - exp( - lambda * diag(pxt) ) )/(1-exp(- lambda) ) ) - total )^2
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
    # (Expectation of the Portoflio - Expectation of the Risk) / Stdev Risk
    lambda =  ( total - sum(discount_factor^(6:30) * payment * diag(pxt)) ) / sqrt( sum( discount_factor^(6:30) * payment  * diag(pxt) * ( discount_factor^(6:30) * payment * (1 - diag(pxt)))) )
  }
  if (premium == "Var") {
    # (Expectation of the Portoflio - Expectation of the Risk) / Var Risk
    lambda =  ( total - sum(discount_factor^(6:30) * payment * diag(pxt)) ) / sum( discount_factor^(6:30) * payment  * diag(pxt) * ( discount_factor^(6:30) * payment * (1 - diag(pxt)))) 
  }
  if (premium == "Mad") {
    lambda =  ( total - sum(discount_factor^(6:30) * payment * quantile(diag(pxt), probs = 0.5, na.rm = FALSE)) ) / sum( discount_factor^(6:30) * payment * mad(diag(pxt))) 
  }
  
  return(lambda)
}