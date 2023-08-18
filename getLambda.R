getLambda = function(init_est, k, model, premium){
  model = toString(model)
  premium = toString(premium)
  
  if (model == "LC"){
    qxt = LCfit$Dxt / LCfit$Ext
    pxt = 1 - qxt
  }
  if (model == "RH"){
    qxt = RHfit$Dxt / RHfit$Ext
    pxt = 1 - qxt
  }
  if (model == "CBD"){
    qxt = CBDfit$Dxt / CBDfit$Ext
    pxt = 1 - qxt
  }
  if (model == "M6"){
    qxt = M6fit$Dxt / M6fit$Ext
    pxt = 1 - qxt
  }
  
  wang_sse = function(lambda) {
    # Note that all other cohorts are implicitly linked to the first cohort year through LC_pxt[,1]
    sum( payment * sum( discount_factor^(1:K+1) * pnorm(qnorm( diag(pxt) ) - lambda))  - total )^2
  }
  
  proportional_hazard_sse = function(lambda) {
    sum( payment * sum( discount_factor^(1:K+1) * (1 - (1 - diag(pxt))^(1/lambda) ) ) - total )^2
  }
  
  if (premium == "Wang") {
    lambda = nlm(wang_sse, init_est)$estimate
  }
  if (premium == "Proportional") {
    lambda = nlm(proportional_hazard_sse, init_est)$estimate
  }
  if (premium == "Stdev") {
    # (Expectation of the Portoflio - Expectation of the Risk) / Stdev Risk
    lambda =  ( total - sum(discount_factor^(1:K+1) * payment * diag(pxt)) ) / sqrt( sum( discount_factor^(1:K+1) * payment  * diag(pxt) * ( discount_factor^(1:K+1) * payment * (1 - diag(pxt)))) )
  }
  if (premium == "Var") {
    # (Expectation of the Portoflio - Expectation of the Risk) / Var Risk
    lambda =  ( total - sum(discount_factor^(1:K+1) * payment * diag(pxt)) ) / sum( discount_factor^(1:K+1) * payment  * diag(pxt) * ( discount_factor^(1:K+1) * payment * (1 - diag(pxt)))) 
  }
  
  return(lambda)
}