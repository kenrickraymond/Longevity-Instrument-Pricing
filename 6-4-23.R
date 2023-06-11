library(demography)
library(StMoMo)
library(lifecontingencies)

source("MortalityFunctions.R")
source("Credentials.R")

################################################################################################################################
########################################################## Preparation #########################################################
################################################################################################################################
# The Data
EWMaleData

# Converting cenrtal exposure to initial exposure for logit models
EWMaleIniData = central2initial(EWMaleData)

# Restrict the Data
ages.fit = 60:89
years.fit = EWMaleIniData$years

# Matrix of age weights. See StMoMo vignette page 16 (https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf)
wxt = genWeightMat(ages = ages.fit, years = EWMaleIniData$years)

# InitialiZe mortality models
LC = lc(link = "log")

# cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
RH = rh(link = "log", cohortAgeFun = "1")

# Logit is the q_{x,t}/{1 - q_{x,t}}
CBD = cbd(link="logit")
M6 = m6(link="logit")

# Fit Mortality Models
LCfit = fit(LC, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
RHfit = fit(RH, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
CBDfit = fit(CBD, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)
M6fit = fit(M6, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)

### Goodness of Fit
table = data.frame(matrix(nrow = 4, ncol =2, c(AIC(LCfit),
      AIC(RHfit),
      BIC(LCfit),
      BIC(RHfit),
      AIC(CBDfit),
      AIC(M6fit),
      BIC(CBDfit),
      BIC(M6fit))
  )
)
colnames(table) = c("AIC","BIC")
rownames(table) = c("LC","RH","CBD","M6")


# Plot fitted models
plot(LCfit)
plot(RHfit)
plot(CBDfit)
plot(M6fit)

# Generate h-year ahead forecasts
years_for = 30
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)

########################################### Generate Plots ##################################

# Plot of Forecasts
par(mfrow=c(1,2))
plot(LCfor, parametricbx = FALSE)
plot(RHfor, parametricbx = FALSE)

# Simulating the future distribution
nsim = 100
LCsim = simulate(LCfit, nsim=nsim, h=30)
RHsim = simulate(RHfit, nsim=nsim, h=30)


# Plot Simulation for kappa, qxt for age 65 in LC model
par(mfrow = c(1, 2))
plot(LCfit$years, LCfit$kt[1,], type="l", xlim=c(1968,2047),
     ylim=c(-40,12), xlab="year", ylab="kt",
     main="Period index (LC)")
matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1,,1:20], type="l",
         lty=1)

qxt = LCfit$Dxt / LCfit$Ext

plot(LCfit$years, qxt["65", ], xlim = range(LCfit$years, LCsim$years),
     ylim = range(qxt["65", ], LCsim$rates["65", , 1:20]), type = "l",
     xlab = "year", ylab = "rate", main = "Mortality rates at age 65")

matlines(LCsim$years, LCsim$rates["65", , 1:20], type = "l", lty = 1)


# Plot simulation for kappa, gamma (cohort parameter), and qxt for age 65 in RH model
par(mfrow = c(1, 3))
plot(RHfit$years, RHfit$kt[1,], type="l", xlim=c(1968,2047),
     ylim=c(-40,12), xlab="year", ylab="kt",
     main="Period index (RH)")
matlines(RHsim$kt.s$years, RHsim$kt.s$sim[1,,1:20], type="l",
         lty=1)


plot(RHfit$cohorts, RHfit$gc, xlim = range(RHfit$cohorts,
       RHsim$gc.s$cohorts), ylim = range(RHfit$gc, RHsim$gc.s$sim[, 1:20],                                
       na.rm = TRUE), type = "l", xlab = "year", ylab = "kt",  
       main = "Cohort index AR(1)")
matlines(RHsim$gc.s$cohorts, RHsim$gc.s$sim[, 1:20], type = "l", lty = 1)

qxt = RHfit$Dxt / RHfit$Ext

plot(RHfit$years, qxt["65", ], xlim = range(RHfit$years, RHsim$years),
     ylim = range(qxt["65", ], RHsim$rates["65", , 1:20]), type = "l",
     xlab = "year", ylab = "rate", main = "Mortality rates at age 65")

matlines(RHsim$years, RHsim$rates["65", , 1:20], type = "l", lty = 1)

###################################### Market Price of Risk ######################################
# Need to change if we are changing the year to 2022
# Based on the 2018 Gilt rate
interest_rate = 0.017
discount_factor = 1/(1+interest_rate)

LC_qxt = LCfit$Dxt / LCfit$Ext
RH_qxt = LCfit$Dxt / RHfit$Ext
CBD_qxt = CBDfit$Dxt / CBDfit$Ext
M6_qxt = M6fit$Dxt / M6fit$Ext

# 5,563 GBP per 100,000 annuity rate annuity rate for male age 65 in UK during 2017
# Mismatch between 2017 and 2018 is justified by small difference between the two (similar levels)
payment = 5563
total = 100000

#Define K as the total number of years
K = length(years.fit)

# Minimize SSE to fit lambda parameter
##### Torske (Wang Thesis) page 55 as reference
## Nonlinear minimization for aged 65
LC_lambda_wang = nlm(LC_wang_sse, 2)$estimate
RH_lambda_wang = nlm(RH_wang_sse, 2)$estimate
CBD_lambda_wang = nlm(CBD_wang_sse, 2)$estimate
M6_lambda_wang = nlm(M6_wang_sse, 2)$estimate

lambda = data.frame( c(LC_lambda_wang, RH_lambda_wang, CBD_lambda_wang, M6_lambda_wang) )
colnames(lambda) = "Lambda"
rownames(lambda) = c("LC","RH","CBD","M6")
lambda
############################################################ Pricing #####################################################
# Forecasted Wang transform risk-adjusted rates 
############## REPEAT FOR ALL
# An individual Aged 65
LC_pxt = 1- LC_qxt

forecasted_qxt = 1 - LCfor$rates
forecasted_pxt = LCfor$rates

forecasted_qxt_65 = forecasted_qxt[5,]
forecasted_pxt_65 = forecasted_pxt[5,]

#LC_wang_risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt_65) - LC_lambda_wang)
LC_wang_risk_adjusted_pxt = pnorm(qnorm(LC_pxt) - LC_lambda_wang)


# Forecasted Mortality Improvements
#sixty_to_seventy_mortality_improvement = mean(LCfor$rates[1:10,years_for] - LCfor$rates[1:10,1])
#seventy_to_eighty_mortality_improvement = mean(LCfor$rates[10:20,years_for] - LCfor$rates[10:20,1])
#eighty_to_ninety_mortality_improvement = mean(LCfor$rates[20:30,years_for] - LCfor$rates[20:30,1])
LC_mxt = LCfit$Dxt/LCfit$Ext

sixty_to_seventy_mortality_improvement = mean(LC_mxt[1:10,years_for] - LC_mxt[1:10,1])
seventy_to_eighty_mortality_improvement = mean(LC_mxt[10:20,years_for] - LC_mxt[10:20,1])
eighty_to_ninety_mortality_improvement = mean(LC_mxt[20:30,years_for] - LC_mxt[20:30,1])



# We are concerned with the strike level for a 60 year old perosn who will live to age 65, ten years from today
X_k_to_ten = function(t){
  X_k = LC_wang_risk_adjusted_pxt * exp( -sixty_to_seventy_mortality_improvement* t )
  return(X_k)
}

X_k = X_k_to_ten(years_for)


# LC_price = sum( LC_wang_risk_adjusted_pxt * discount_factor^(1:years_for) ) - sum( X_k_to_ten(years_for) * discount_factor^(1:years_for) )
S_t = mean( LC_wang_risk_adjusted_pxt[5, ] * discount_factor^(1:51) )
S_t
K_t = ( forecasted_pxt_65 * discount_factor^(1:years_for) )
K_t


LC_price = S_t - K_t
LC_price

riskprem = function(pi){
  sum( S_t - (1+pi) * K_t[1] )^2  
}

nlm(riskprem,1)$estimate


pi = S_t/K_t - 1
pi
