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
wang_getLambda = function(init_est, model){
  model = toString(model)
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
  sum( payment * sum( discount_factor^(0:K-1) * pnorm(qnorm(pxt[,1] ) - lambda))  - total )^2
  }

  # See MortalityFunctions.R for wang_sse()
  lambda_wang = nlm(wang_sse, init_est)$estimate
  
  return(lambda_wang)
}

# Average of 30 year change in force of mortality
mortality_improvement = function(years_for, model){
  model = toString(model)
  if (model == "LC"){
    sixty_to_seventy_mortality_improvement = mean(LC_mxt[1:10,years_for] - LC_mxt[1:10,1])
    seventy_to_eighty_mortality_improvement = mean(LC_mxt[10:20,years_for] - LC_mxt[10:20,1])
    eighty_to_ninety_mortality_improvement = mean(LC_mxt[20:30,years_for] - LC_mxt[20:30,1])
  }
  if (model == "RH"){
    sixty_to_seventy_mortality_improvement = mean(RH_mxt[1:10,years_for] - RH_mxt[1:10,1])
    seventy_to_eighty_mortality_improvement = mean(RH_mxt[10:20,years_for] - RH_mxt[10:20,1])
    eighty_to_ninety_mortality_improvement = mean(RH_mxt[20:30,years_for] - RH_mxt[20:30,1])
  }
  if (model == "CBD"){
    sixty_to_seventy_mortality_improvement = mean(CBD_mxt[1:10,years_for] - CBD_mxt[1:10,1])
    seventy_to_eighty_mortality_improvement = mean(CBD_mxt[10:20,years_for] - CBD_mxt[10:20,1])
    eighty_to_ninety_mortality_improvement = mean(CBD_mxt[20:30,years_for] - CBD_mxt[20:30,1])
  }
  if (model == "M6"){
    sixty_to_seventy_mortality_improvement = mean(M6_mxt[1:10,years_for] - M6_mxt[1:10,1])
    seventy_to_eighty_mortality_improvement = mean(M6_mxt[10:20,years_for] - M6_mxt[10:20,1])
    eighty_to_ninety_mortality_improvement = mean(M6_mxt[20:30,years_for] - M6_mxt[20:30,1])
  }
  return( c( sixty_to_seventy_mortality_improvement, seventy_to_eighty_mortality_improvement, eighty_to_ninety_mortality_improvement) ) 
}

# Input the time in years for which the mortality improvements we will be using
X_k = function(k, t, model){
  model = toString(model)
  if(k <= 10){
    X_k = annuitants * forecasted_pxt * exp( - mortality_improvement(years_for, model)[1] * t )  
  }
  if(k <= 20 & k > 10) {
    X_k = annuitants * forecasted_pxt * exp( - mortality_improvement(years_for, model)[1] * 10 ) * exp( - mortality_improvement(years_for, model)[2] * 10 * (t-10) )
  }
  if(k <= 30 & k >20) {
    X_k = annuitants * forecasted_pxt * exp( - ( mortality_improvement(years_for, model)[1] * 10 + mortality_improvement(years_for, model)[2] * 10) ) * exp( - mortality_improvement(years_for, model)[3] * (t-20) )
  }
  return(X_k)
}



