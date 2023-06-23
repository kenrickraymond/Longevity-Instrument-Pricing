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
    sum( payment * sum( discount_factor^(0:K-1) * pnorm(qnorm(pxt[,k] ) - lambda))  - total )^2
  }
  
  proportional_hazard_sse = function(lambda) {
    sum( payment * sum( discount_factor^(0:K-1) * (1 - (1 - pxt[,k])^(1/lambda)) ) - total )^2
  }
  
  # See MortalityFunctions.R for wang_sse()
  if (premium == "Wang") {
    lambda = nlm(wang_sse, init_est)$estimate  
  }
  if (premium == "Proportional") {
    lambda = nlm(proportional_hazard_sse, init_est)$estimate  
  }
  if (premium == "Stdev") {
    # (Expectation of the Portoflio - Expectation of the Risk) / Stdev Risk
    lambda = ( mean(pxt) - mean(pxt[,k]) ) / sd(pxt[,k])
  }
  if (premium == "Var") {
    # (Expectation of the Portoflio - Expectation of the Risk) / Var Risk
    lambda = ( mean(pxt) - mean(pxt[,k]) ) / var(pxt[,k])
  }
  
  return(lambda)
}



survivorForwardPrice = function(k, years_for, annuitants, notional_principal, lambda, model, premium){
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
    risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - lambda)
    
    # Floating Leg for an s-forward, S(T)
    S_t = annuitants * tail(risk_adjusted_pxt, n=1)
    
    # Constant fixed-Leg K
    K_t = annuitants * mean(risk_adjusted_pxt)
    
    price = discount_factor^(years_for) * notional_principal * (S_t - K_t)
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt =  (forecasted_pxt)^(1/lambda)
    
    S_t = annuitants * tail(risk_adjusted_pxt, n=1)
    K_t = annuitants * mean(risk_adjusted_pxt)
    
    price = discount_factor^(years_for) * notional_principal * (S_t - K_t)
  }
  if (premium == "Stdev") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = annuitants * tail(forecasted_pxt, n=1)
      K_t = annuitants * mean(forecasted_pxt) 
      
      price = notional_principal * ( mean(S_t) - K_t )
    }
    else{
      S_t = annuitants * tail(forecasted_pxt, n=1)
      K_t = annuitants * ( mean(forecasted_pxt) + lambda * sd(forecasted_pxt) )
      
      price = notional_principal * ( S_t - K_t )
    }
  }
  if (premium == "Var") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = annuitants * tail(forecasted_pxt, n=1)
      K_t = annuitants * mean(forecasted_pxt) 
      
      price = notional_principal * ( S_t - K_t )
    }
    else{
      S_t = annuitants * tail(forecasted_pxt, n=1)
      K_t = annuitants * ( mean(forecasted_pxt) + lambda * notional_principal * var(forecasted_pxt) )
      
      price = notional_principal * ( S_t - K_t )
    }
  }
  
  # Percentage basis
  risk_premium = (  - log(K_t / S_t) /  years_for ) * 100
  return(price)
}





longevitySwapPrice = function(k, years_for, annuitants, notional_principal, lambda, model, premium){
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
    risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - lambda)
    
    # Floating Leg for an longevity swap, S(t), t=1,2,...,years_for
    S_t =  annuitants * risk_adjusted_pxt
    
    # Constant fixed-Leg K
    K_t = annuitants * sum( discount_factor^(1:years_for) * risk_adjusted_pxt ) / ( sum( discount_factor^(1:years_for) ) )
    
    price = notional_principal * discount_factor^(years_for) * sum(S_t - K_t)
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt = forecasted_pxt^(1/lambda)
    
    S_t = annuitants * risk_adjusted_pxt
    K_t = annuitants * sum( discount_factor^(1:years_for) * risk_adjusted_pxt ) / ( sum( discount_factor^(1:years_for) ) )
    
    price = notional_principal * discount_factor^(years_for) * sum(S_t - K_t)
  }
  if (premium == "Stdev") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = annuitants * forecasted_pxt
      K_t = annuitants * mean(forecasted_pxt)
      
      # Do not discount under real world measure
      price = notional_principal * ( mean(S_t) - K_t )
    }
    else{
      cum_avg <- cumsum(forecasted_pxt) / seq_along(forecasted_pxt)
      cum_sd = cumvar(forecasted_pxt, sd = TRUE)
      cum_sd[is.na(cum_sd)] <- 0
      
      S_t = annuitants * forecasted_pxt
      K_t = annuitants * ( cum_avg + lambda * cum_sd )
        
      
      price = notional_principal * sum( S_t - K_t ) 
    }
  }
  if (premium == "Var") {
    
    if (years_for == 1){
      # Pure premium for the first year, no risk loading
      S_t = annuitants * forecasted_pxt
      K_t = annuitants * mean(forecasted_pxt) 
      
      price = notional_principal * ( mean(S_t) - K_t )
    }
    else{
      cum_avg <- cumsum(forecasted_pxt) / seq_along(forecasted_pxt)
      cum_var = cumvar(forecasted_pxt, sd = FALSE)
      cum_var[is.na(cum_var)] <- 0
      
      S_t = annuitants * forecasted_pxt
      K_t = annuitants * ( cum_avg + lambda * notional_principal * cum_var )
      
      price = notional_principal * sum( S_t - K_t ) 
    }
  }
  
  # Percentage basis
  risk_premium = (  - log(K_t / S_t) /  years_for ) * 100
  return(price)
}

cumvar <- function (x, sd = FALSE) {
  x <- x - x[sample.int(length(x), 1)]
  n <- seq_along(x)
  v <- (cumsum(x ^ 2) - cumsum(x) ^ 2 / n) / (n - 1)
  if (sd) v <- sqrt(v)
  v
}
