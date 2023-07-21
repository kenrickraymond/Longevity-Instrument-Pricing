# From the StMoMo Documentation, visualize prediction intervals for mortality rates
  mxt <- LCfit$Dxt / LCfit$Ext
  mxtHat <- fitted(LCfit, type = "rates")
  mxtCentral <- LCfor$rates
  
  mxtPred2.5 <- apply(LCsimFit$rates, c(1, 2), quantile, probs = 0.025)
  mxtPred97.5 <- apply(LCsimFit$rates, c(1, 2), quantile, probs = 0.975)
  
  
  mxtHatPU2.5 <- apply(LCsimBoot$fitted, c(1, 2), quantile, probs = 0.025)
  mxtHatPU97.5 <- apply(LCsimBoot$fitted, c(1, 2), quantile, probs = 0.975)
  mxtPredPU2.5 <- apply(LCsimBoot$rates, c(1, 2), quantile, probs = 0.025)
  mxtPredPU97.5 <- apply(LCsimBoot$rates, c(1, 2), quantile, probs = 0.975)
  
  
  x <- c("60", "65", "80")
  matplot(LCfit$years, t(mxt[x, ]),
          xlim = range(LCfit$years,
                       LCfor$years),
          ylim = range(mxtHatPU97.5[x, ],
                       mxtPredPU2.5[x, ],
                       mxt[x, ]),
          type = "p",
          xlab = "years",
          ylab = "mortality rates (log scale)",
          log = "y",
          pch = 20,
          col = "black")
  
  matlines(LCfit$years, t(mxtHat[x, ]), lty = 1, col = "black")
  matlines(LCfit$years, t(mxtHatPU2.5[x, ]), lty = 5, col = "red")
  matlines(LCfit$years, t(mxtHatPU97.5[x, ]), lty = 5, col = "red")
  
  matlines(LCfor$years, t(mxtCentral[x, ]), lty = 4, col = "black")
  matlines(LCsimBoot$years, t(mxtPred2.5[x, ]), lty = 3, col = "black")
  matlines(LCsimBoot$years, t(mxtPred97.5[x, ]), lty = 3, col = "black")

  # Prediction Interval
  matlines(LCsimBoot$years, t(mxtPredPU2.5[x, ]), lty = 5, col = "red")
  matlines(LCsimBoot$years, t(mxtPredPU97.5[x, ]), lty = 5, col = "red")

# Counts the number of best-fitting mortality models based on Mean Absolute Percentage Error
  library(Metrics)
  
  mape_fun = function(i, model){
    model = toString(model)
    # Model Selection
    if (model == "LC"){
      mod_for = forecast(LCfit, h=years_for)
      modsimBoot = simulate(LCboot, h = years_for)
    }
    if (model == "RH"){
      mod_for = forecast(RHfit, h=years_for)
      modsimBoot = simulate(RHboot, h = years_for)
    }
    if (model == "CBD"){
      mod_for = forecast(CBDfit, h=years_for)
      modsimBoot = simulate(CBDboot, h = years_for)
    }
    if (model == "M6"){
      mod_for = forecast(M6fit, h=years_for)
      modsimBoot = simulate(M6boot, h = years_for)
    }
    
    mod_mape = mape(mod_for$rates, modsimBoot$rates[i])
  }
  
  
  LC_MAPE = as.numeric(lapply(1:samples, function(years_for) mape_fun(years_for, "LC")))
  RH_MAPE = as.numeric(lapply(1:samples, function(years_for) mape_fun(years_for, "RH")))
  # LC_MAPE
  # RH_MAPE
  
  # Number of mortality models
  MAPE_df = rbind(LC_MAPE, RH_MAPE)
  
  min_mat = as.numeric(lapply(1:samples, function(years_for) min(MAPE_df[, years_for])))
  # min_mat
  
  LC_best_count = length( which( sapply(LC_MAPE, function(string) sum(string==min_mat)) == 1 ) )
  RH_best_count = length( which( sapply(RH_MAPE, function(string) sum(string==min_mat)) == 1 ) )
  # LC_best_count
  # RH_best_count
  
# Unit testing for Mortality model uncertainty
  LCboot = bootstrap(LCfit, nBoot = samples, type = "semiparametric")
  # RHboot = bootstrap(RHfit, nBoot = samples, type = "semiparametric")
  
  LCsimFit = simulate(LCfit, nsim = samples, h = years_for)
  LCsimBoot <- simulate(LCboot, h = years_for)
  
  mxtPredPU97.5 = apply(LCsimBoot$rates, c(1, 2), quantile, probs = 0.975)[5,]
  mxtPredPU2.5 = apply(LCsimBoot$rates, c(1, 2), quantile, probs = 0.025)[5,]
  
  plot(LCfor$rates[5,], ylim=c(0, 0.1))
  lines(mxtPredPU97.5)
  lines(mxtPredPU2.5)
  
  LCfor$rates[5,]
  mxtPredPU97.5
  mxtPredPU2.5
  lambda=0.5
  
  risk_adjusted_pxt = pnorm(qnorm(1- LCfor$rates[5,]) - lambda)
  risk_adjusted_pxt_upper = pnorm(qnorm(1 - mxtPredPU97.5) - lambda)
  risk_adjusted_pxt_lower = pnorm(qnorm(1 - mxtPredPU2.5) - lambda)

  risk_adjusted_pxt
  risk_adjusted_pxt_upper
  risk_adjusted_pxt_lower
  
  
  # Floating Leg for an s-forward, S(T)
  S_t = annuitants * tail(risk_adjusted_pxt, n=1)
  S_t_upper = annuitants * tail(risk_adjusted_pxt_upper, n=1)
  S_t_lower = annuitants * tail(risk_adjusted_pxt_lower, n=1)

  # Constant fixed-Leg K
  K_t = annuitants * mean(risk_adjusted_pxt)
  K_t_upper = annuitants * mean(risk_adjusted_pxt_upper)
  K_t_lower = annuitants * mean(risk_adjusted_pxt_lower)

  price = as.numeric( discount_factor^(years_for) * notional_principal * (S_t - K_t) )
  price_upper = as.numeric( discount_factor^(years_for) * notional_principal * (S_t_upper - K_t_upper) )
  price_lower = as.numeric( discount_factor^(years_for) * notional_principal * (S_t_lower - K_t_lower) )

  price
  price_upper
  price_lower
  

