library("StMoMo")
library('matrixStats')

# Stressed-trend approach
LCsim = simulate(LCfit, nsim = 1000, h = 20)
stressed_proj = rowQuantiles(LCsim$rates["65",, ], probs=0.005)
central_proj = forecast(LCfit, h=20)$rates["65",]

h = 20
nsim = 500
iter = 100
updated_forecast = matrix(nrow=iter,ncol=h-1)
stressed_updated_forecast = matrix(nrow=iter,ncol=h-1)

for(i in 1:iter){
  LCsim = simulate(LCfit, nsim = nsim, h = h)
  ETR_one_year_ahead = EWMaleData$Ext[61:90,51] - EWMaleData$Dxt[61:90,51]/2 # Deterministic
  # one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(LCsim$rates[,1,]) ) ) # Stochastic
  one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(LCsim$rates[,1,]) ) ) # Stochastic
  
  updated_Dxt = cbind( EWMaleData$Dxt[61:90,], one_year_ahead_death_rates ) # Combined Dataset
  updated_Ext = cbind( EWMaleData$Ext[61:90,], ETR_one_year_ahead )
  colnames(updated_Dxt)[52] = "2012"
  colnames(updated_Ext)[52] = "2012"
  
  updated_fit = fit(LC, Dxt = updated_Dxt, Ext = updated_Ext) # Refit on combined dataset
  updated_fit$years = seq(1961,2012,1)
  updated_fit$ages = seq(60,89,1)
  
  updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.5)
  stressed_updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.005) 
}

plot(LCfit$years, fitted(LCfit,type='rate')['65',],
     xlim = range(LCfit$years, LCsim$years),
     ylim = c(0,0.05),
     type = "l", xlab = "Year", ylab = "Rate",
     main = "Mortality Rates at Age 65 under LC model")
matlines(LCsim$years, stressed_proj, type = "l", lty = 5, col=3) 
matlines(LCsim$years, central_proj, type = "l", lty = 5, col=2) 
# abline(v=2012, col="blue")
matlines(seq(2013,2013+h-2,1), colMeans(updated_forecast), col=1) # CENTRAL VAR, UNSTRESSED
matlines(seq(2013,2013+h-2,1), colMeans(stressed_updated_forecast), col=4)

legend("topright", legend=c("Central Projection", "99.5% Stressed-Trend Approach", "VaR (Central)", "VaR (Stressed)"),
       col=c(2, 3, 1,4), lty=c(5,5,1,1), cex=0.8)


# RH MODEL
RH_updated_forecast = matrix(nrow=iter,ncol=h-1)
RH_stressed_updated_forecast = matrix(nrow=iter,ncol=h-1)
# Stressed-trend approach
RHsim = simulate(RHfit, nsim = 100, h = 20)
stressed_proj = rowQuantiles(RHsim$rates["65",, ], probs=0.005)
central_proj = forecast(RHfit, h=20)$rates["65",]

h = 20
nsim = 10
iter = 5
updated_forecast = matrix(nrow=iter,ncol=h-1)
stressed_updated_forecast = matrix(nrow=iter,ncol=h-1)

for(i in 1:iter){
  RHsim = simulate(RHfit, nsim = nsim, h = h)
  ETR_one_year_ahead = EWMaleData$Ext[61:90,51] - EWMaleData$Dxt[61:90,51]/2 # Deterministic
  one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(RHsim$rates[,1,]) ) ) # Stochastic
  
  updated_Dxt = cbind( EWMaleData$Dxt[61:90,], one_year_ahead_death_rates ) # Combined Dataset
  updated_Ext = cbind( EWMaleData$Ext[61:90,], ETR_one_year_ahead )
  colnames(updated_Dxt)[52] = "2012"
  colnames(updated_Ext)[52] = "2012"
  
  updated_fit = fit(RH, Dxt = updated_Dxt, Ext = updated_Ext) # Refit on combined dataset
  updated_fit$years = seq(1961,2012,1)
  updated_fit$ages = seq(60,89,1)
  
  RH_updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.5)
  RH_stressed_updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.005)
  print(i)
}
# library(tidyr)
# RH_updated_forecast = na.omit(RH_updated_forecast)
# RH_stressed_updated_forecast = na.omit(RH_stressed_updated_forecast)

plot(colMeans(RH_stressed_updated_forecast))
colMeans(RH_stressed_updated_forecast)

plot(RHfit$years, fitted(RHfit,type='rate')['65',],
     xlim = range(RHfit$years, RHsim$years),
     ylim = c(0,0.05),
     type = "l", xlab = "Year", ylab = "Rate",
     main = "Mortality Rates at Age 65")
matlines(RHsim$years, stressed_proj, type = "l", lty = 5, col=3) 
matlines(RHsim$years, central_proj, type = "l", lty = 5, col=2) 

matlines(seq(2013,2013+h-2,1), colMeans(RH_updated_forecast), col=1) # CENTRAL VAR, UNSTRESSED
matlines(seq(2013,2013+h-2,1), colMeans(RH_stressed_updated_forecast), col=4)

legend("topright", legend=c("Central Projection", "99.5% Stressed-Trend Approach", "VaR (Central)", "VaR (Stressed)"),
       col=c(2, 3, 1,4), lty=c(5,5,1,1), cex=0.8)


# UNIT TEST

RHsim = simulate(RHfit, nsim = nsim, h = 5)
ETR_one_year_ahead = EWMaleData$Ext[61:90,51] - EWMaleData$Dxt[61:90,51]/2 # Deterministic
one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(RHsim$rates[,1,]) ) ) # Stochastic

updated_Dxt = cbind( EWMaleData$Dxt[61:90,], one_year_ahead_death_rates ) # Combined Dataset
updated_Ext = cbind( EWMaleData$Ext[61:90,], ETR_one_year_ahead )
colnames(updated_Dxt)[52] = "2012"
colnames(updated_Ext)[52] = "2012"

updated_fit = fit(RH, Dxt = updated_Dxt, Ext = updated_Ext) # Refit on combined dataset
updated_fit$years = seq(1961,2012,1)
updated_fit$ages = seq(60,89,1)

RH_updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.5)
RH_stressed_updated_forecast[i,] = rowQuantiles(simulate(updated_fit, h=h-1)$rates["65",,], probs=0.005)