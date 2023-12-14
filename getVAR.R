# getVaR = function(years_for, lambda, model, premium, nsim=1000, alpha=0.95, product){
#   model = toString(model)
#   premium = toString(premium)
#   product = toString(product)
#   
#   # Calculate one-year ahead simulations
#   if (model == "LC"){
#     mod_sim = simulate(LCfit, nsim = nsim, h = 1)
#   }
#   if (model == "RH"){
#     mod_sim = simulate(RHfit, nsim = nsim, h = 1)
#   }
#   if (model == "CBD"){
#     mod_sim = simulate(CBDfit, nsim = nsim, h = 1)
#   }
#   if (model == "M6"){
#     mod_sim = simulate(M6fit, nsim = nsim, h = 1)
#   }
#   
#   #Binomial Probability of death
#   # q_proj = 1 - exp(-)
#   
# }

library("StMoMo")
library('matrixStats')

# Stressed-trend approach
LCsim = simulate(LCfit, nsim = 1000, h = 20)
stressed_proj = rowQuantiles(LCsim$rates["65",, ], probs=0.005)
central_proj = forecast(LCfit, h=20)$rates["65",]
# central_proj = rowQuantiles(LCsim$rates["65",, ], probs=0.5) 

# plot(LCfit$years, fitted(LCfit,type='rate')['65',],
#      xlim = range(LCfit$years, LCsim$years),
#      ylim = range( max(fitted(LCfit,type='rate')['65',]), min(fitted(LCfit,type='rate')['65',]) - 0.01),
#     type = "l", xlab = "Year", ylab = "Rate",
#     main = "Mortality Rates at Age 65")
# 
# # matlines(LCsim$years, LCsim$rates["65", , 1:100], type = "l", lty = 1) #100 Paths  
# matlines(LCsim$years, stressed_proj, type = "l", lty = 5, col=3) 
# matlines(LCsim$years, central_proj, type = "l", lty = 5, col=2) 

# legend("topright", legend=c("Central Projection", "99.5% Stressed-Trend Approach"),
       # col=c(3, 2), lty=5, cex=0.8)

#### #### ####
# Log Graphs
# LCsim = simulate(LCfit, nsim = 1000, h = 20)
# stressed_proj = log( rowQuantiles(LCsim$rates["65",, ], probs=0.005) )
# central_proj = log( forecast(LCfit, h=20)$rates["65",] )
# # central_proj2 = log( rowQuantiles(LCsim$rates["65",, ], probs=0.5) )
# 
# plot(LCfit$years, log(fitted(LCfit,type='rate')['65',]),
#      xlim = range(LCfit$years, LCsim$years),
#      ylim = range( max(log(fitted(LCfit,type='rate')['65',])), min(log(fitted(LCfit,type='rate')['65',])) - 1 ),
#      type = "l", xlab = "Year", ylab = "ln(mxt)",
#      main = "Mortality Rates at Age 65")
# 
# # matlines(LCsim$years, LCsim$rates["65", , 1:100], type = "l", lty = 1) #100 Paths  
# matlines(LCsim$years, stressed_proj, type = "l", lty = 5, col=3) 
# matlines(LCsim$years, central_proj, type = "l", lty = 5, col=2) 
# 
# legend("topright", legend=c("Central Projection", "99.5% Stressed-Trend Approach"),
#        col=c(3, 2), lty=5, cex=0.8)
# # OK!
# DF = exp(-0.0204)
# central_survival = 1-central_proj
# stressed_survival = 1 -stressed_proj
# 
# central_ann_factor = 1/2 + sum(  central_survival[1:19] * DF^(1:19) ) + 1/2 * 20 * central_survival[20] * DF^(20)
# stressed_ann_factor =  1/2 + sum( stressed_survival[1:19] * DF^(1:19) ) + 1/2 * 20 * stressed_survival[20] * DF^(20)
# 
# ( (stressed_ann_factor / central_ann_factor) - 1 )* 100

# Might be ok?

#### Longevity VAR
h = 20
nsim = 500
iter = 1000
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
     ylim = range( max(fitted(LCfit,type='rate')['65',]), min(fitted(LCfit,type='rate')['65',]) - 0.01),
     type = "l", xlab = "Year", ylab = "Rate",
     main = "Mortality Rates at Age 65")
matlines(LCsim$years, stressed_proj, type = "l", lty = 5, col=3) 
matlines(LCsim$years, central_proj, type = "l", lty = 5, col=2) 
abline(v=2012, col="blue")
matlines(seq(2013,2013+h-2,1), colMeans(updated_forecast), col=1) # CENTRAL VAR, UNSTRESSED
matlines(seq(2013,2013+h-2,1), colMeans(stressed_updated_forecast), col=4)

legend("topright", legend=c("Central Projection", "99.5% Stressed-Trend Approach", "VaR (Central)", "VaR (Stressed)"),
       col=c(2, 3, 1,4), lty=c(5,5,1,1), cex=0.8)



# (quantile(as.numeric(updated_forecast), probs=0.995)/mean( as.numeric(updated_forecast)) - 1) * 100 # RATIO OF PROTECTION




  # Unit Test
LCsim = simulate(LCfit, nsim = 1000, h = 10)
ETR_one_year_ahead = EWMaleData$Ext[61:90,51] - EWMaleData$Dxt[61:90,51]/2 # Deterministic
# one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(LCsim$rates[,1,]) ) ) # Maybe Add trend risk by using quantile on the simulated rates
one_year_ahead_death_rates = ETR_one_year_ahead * (1 - exp( - rowMeans(LCsim$rates[,1,]) ) ) # Stochastic

updated_Dxt = cbind( EWMaleData$Dxt[61:90,], one_year_ahead_death_rates ) # Combined Dataset
updated_Ext = cbind( EWMaleData$Ext[61:90,], ETR_one_year_ahead )
colnames(updated_Dxt)[52] = "2012"
colnames(updated_Ext)[52] = "2012"

updated_fit = fit(LC, Dxt = updated_Dxt, Ext = updated_Ext)
updated_fit$years = seq(1961,2012,1)
updated_fit$ages = seq(60,89,1)

# plot(forecast(updated_fit, h=10)$rates["65",])
rowQuantiles(simulate(updated_fit, h=10)$rates["65",,], probs=0.5)
# Historical Fitted model
# plot(c(LCfit$years,'2012'), c(fitted(LCfit,type='rates')['65',], 0), ylim=c(0,0.05), main="Original Fitted Model vs One Year Ahead Model", xlab="Year", ylab='Rate', type='l', col='red') # Individual aged 65
# lines(updated_fit$years, fitted(updated_fit,type='rates')['65',]) # 5 corresponds to aged 65
# legend("topright", legend=c("Orig Model", "One Year Ahead Model"),
#        col=c('red','black'), lty=1, cex=0.8)

# VAR Calculations
library(PerformanceAnalytics)
mod_sim = simulate(LCfit, nsim = nsim, h = years_for+1)
nsim = 1000
survival_rates_mat = matrix(nrow=nsim, ncol=years_for)
i=1 # Loop counter
k=5 # Indicates that we start with age 65
while(i <= years_for){
  survival_rates_mat[,i] = 1 - mod_sim$rates[k+i,i,]
  i=i+1
}

CF_swap =  1 - pnorm( qnorm( 1 - colMeans(survival_rates_mat)) - LCWanglambda)
cash_flow = 1 - pnorm( qnorm( 1 - survival_rates_mat[,years_for]) - LCWanglambda)


log_returns_forward <- diff(log(cash_flow), lag=1)
log_returns_swap <- diff(log(CF_swap), lag=1)


abs( VaR(log_returns_forward, p=0.99, method="historical") )
abs( VaR(log_returns_swap, p=0.99, method="historical") )

