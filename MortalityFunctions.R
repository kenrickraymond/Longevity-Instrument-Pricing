hmd.mx <- function(country, username, password, label = country) {
  # Read raw MX and Exposure data
  mx <- HMDHFDplus::readHMDweb(country, item = "Mx_1x1",
                               username = username, password = password, fixup = TRUE)
  pop <- HMDHFDplus::readHMDweb(country, item = "Exposures_1x1",
                                username = username, password = password, fixup = TRUE)
  
  # Construct output
  obj <- list(type = "mortality", label = label, lambda = 0)
  obj$year <- sort(unique(mx[, "Year"]))
  n <- length(obj$year)
  m <- length(unique(mx[, "Age"]))
  obj$age <- mx[seq(m), "Age"]
  mnames <- names(mx)[-c(1:2, NCOL(mx))]
  n.mort <- length(mnames)
  obj$rate <- obj$pop <- list()
  for (i in seq(n.mort)) {
    obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
    obj$rate[[i]][obj$rate[[i]] < 0] <- NA
    obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
    obj$pop[[i]][obj$pop[[i]] < 0] <- NA
    dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
  }
  names(obj$pop) <- names(obj$rate) <- tolower(mnames)
  
  return(structure(obj, class = "demogdata"))
}

## Market Price of Risk
# Minimize SSE to fit lambda parameter
# getLambda = function(init_est, k, model, premium){
#   model = toString(model)
#   premium = toString(premium)
#   
#   if (model == "LC"){
#     qxt = LCfit$Dxt / LCfit$Ext
#     pxt = 1 - qxt
#   }
#   if (model == "RH"){
#     qxt = RHfit$Dxt / RHfit$Ext
#     pxt = 1 - qxt
#   }
#   if (model == "CBD"){
#     qxt = CBDfit$Dxt / CBDfit$Ext
#     pxt = 1 - qxt
#   }
#   if (model == "M6"){
#     qxt = M6fit$Dxt / M6fit$Ext
#     pxt = 1 - qxt
#   }
# 
#   wang_sse = function(lambda) {
#   # Note that all other cohorts are implicitly linked to the first cohort year through LC_pxt[,1]
#     sum( payment * sum( discount_factor^(0:K-1) * pnorm(qnorm(pxt[,k] ) - lambda))  - total )^2
#   }
#   
#   proportional_hazard_sse = function(lambda) {
#     sum( payment * sum( discount_factor^(0:K-1) * (1 - (1 - pxt[,k])^(1/lambda)) ) - total )^2
#   }
# 
#   # See MortalityFunctions.R for wang_sse()
#   if (premium == "Wang") {
#     lambda = nlm(wang_sse, init_est)$estimate  
#   }
#   if (premium == "Proportional") {
#     lambda = nlm(proportional_hazard_sse, init_est)$estimate  
#   }
#   if (premium == "Stdev") {
#     # (Expectation of the Portoflio - Expectation of the Risk) / Stdev Risk
#     lambda = ( mean(pxt) - mean(pxt[,k]) ) / sd(pxt[,k])
#   }
#   if (premium == "Var") {
#     # (Expectation of the Portoflio - Expectation of the Risk) / Var Risk
#     lambda = ( mean(pxt) - mean(pxt[,k]) ) / var(pxt[,k])
#   }
#   
#   return(lambda)
# }




getPrice = function(k, years_for, model, premium){
  model = toString(model)
  # Model Selection
  if (model == "LC"){
    mod_for = forecast(LCfit, h=years_for)
  }
  if (model == "RH"){
    mod_for = forecast(RHfit, h=years_for)
  }
  if (model == "CBD"){
    mod_for = forecast(CBDfit, h=years_for)
  }
  if (model == "M6"){
    mod_for = forecast(M6fit, h=years_for)
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
  
  if (premium == "Wang") {
    lambda = getLambda(0.5, k, model, "Wang")  
    risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - lambda)
    # Floating Leg
    S_t = sum( annuitants * risk_adjusted_pxt * discount_factor^(1:years_for) )
  }
  if (premium == "Proportional") {
    lambda = getLambda(0.5, k, model, "Proportional")  
    risk_adjusted_pxt =  1 - (1 - forecasted_pxt)^(1/lambda)
    # Floating Leg
    S_t = sum( annuitants * risk_adjusted_pxt * discount_factor^(1:years_for) )
  }
  if (premium == "Stdev") {
    # The init_est param is not used to obtain lambda. We set it equal to 0.5 for generalization.
    lambda = getLambda(0.5, k, model, "Stdev")
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      risk_adjusted_pxt = mean(forecasted_pxt)
    } 
    else{
      risk_adjusted_pxt = mean(forecasted_pxt) + lambda * sd(forecasted_pxt)
    }
    # Floating Leg
    S_t = sum( annuitants * risk_adjusted_pxt * discount_factor^(1:years_for) )
  }
  if (premium == "Var") {
    # The init_est param is not used to obtain lambda. We set it equal to 0.5 for generalization.
    lambda = getLambda(0.5, k, model, "Stdev")
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      risk_adjusted_pxt = mean(forecasted_pxt)
    } 
    else{
      risk_adjusted_pxt = mean(forecasted_pxt) + lambda * var(forecasted_pxt)
    }
    # Floating Leg
    S_t = sum( annuitants * risk_adjusted_pxt * discount_factor^(1:years_for) ) 
  }
  
  # Fixed-Leg 
  K_t = sum( X_k(k, years_for, model) * discount_factor^(1:years_for) ) # X_k(k, t, model)
  
  price = S_t - K_t
  riskprem = (S_t/K_t) - 1
  
  return(riskprem)
}


