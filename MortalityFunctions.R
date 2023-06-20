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
    risk_adjusted_pxt =  1 - (1 - forecasted_pxt)^(1/lambda)
    
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
      K_t = annuitants * mean(forecasted_pxt) + lambda * sd(forecasted_pxt)
      
      price = notional_principal * ( mean(S_t) - K_t + lambda * sd(S_t) )
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
      K_t = annuitants * mean(forecasted_pxt) + lambda * var(forecasted_pxt)
      
      price = notional_principal * ( mean(S_t) - K_t + lambda * notional_principal * var(S_t) )
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
    K_t = annuitants * mean( risk_adjusted_pxt * discount_factor^(1:years_for) ) / ( mean( discount_factor^(1:years_for) ) )
    
    price =  notional_principal * discount_factor^(years_for) * mean(S_t - K_t)
  }
  if (premium == "Proportional") {
    risk_adjusted_pxt =  1 - (1 - forecasted_pxt)^(1/lambda)
    
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
      K_t = annuitants * mean(forecasted_pxt) + lambda * sd(forecasted_pxt)
      
      price = notional_principal * ( mean(S_t) - K_t + lambda * sd(S_t) )
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
      K_t = annuitants * mean(forecasted_pxt) + lambda * var(forecasted_pxt)
      
      price = notional_principal * ( mean(S_t) - K_t + lambda * notional_principal * var(S_t) )
    }
  }
  
  # Percentage basis
  risk_premium = (  - log(K_t / S_t) /  years_for ) * 100
  return(price)
}
