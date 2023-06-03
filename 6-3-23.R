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
EWMaleIniData <- central2initial(EWMaleData)

# Restrict the Data
ages.fit <- 60:89
years.fit = EWMaleIniData$years

# Matrix of age weights. See StMoMo vignette page 16 (https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf)
wxt <- genWeightMat(ages = ages.fit, years = EWMaleIniData$years)

# InitialiZe mortality models
LC <- lc()

# cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
RH = rh(cohortAgeFun = "1")

# Logit is the q_{x,t}/{1 - q_{x,t}}
CBD = cbd(link="logit")
M6 = m6(link="logit")

# Fit Mortality Models
LCfit <- fit(LC, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
RHfit <- fit(RH, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
CBDfit <- fit(CBD, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)
M6fit <- fit(M6, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)

### Goodness of Fit
AIC(LCfit)
AIC(RHfit)
BIC(LCfit)
BIC(RHfit)
AIC(CBDfit)
AIC(M6fit)
BIC(CBDfit)
BIC(M6fit)

# Plot fitted models
plot(LCfit)
plot(RHfit)
plot(CBDfit)
plot(M6fit)

# Generate h-year ahead forecasts
LCfor <- forecast(LCfit, h=30)
RHfor = forecast(RHfit, h=30)

# Plot of Forecasts
par(mfrow=c(1,2))
plot(LCfor, parametricbx = FALSE)
plot(RHfor, parametricbx = FALSE)

# Simulating the future distribution
nsim <- 100
LCsim <- simulate(LCfit, nsim=nsim, h=30)
RHsim <- simulate(RHfit, nsim=nsim, h=30)


# Plot Simulation for kappa, qxt for age 65 in LC model
par(mfrow = c(1, 2))
plot(LCfit$years, LCfit$kt[1,], type="l", xlim=c(1968,2047),
     ylim=c(-40,12), xlab="year", ylab="kt",
     main="Period index (LC)")
matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1,,1:20], type="l",
         lty=1)

qxt <- LCfit$Dxt / LCfit$Ext

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

qxt <- RHfit$Dxt / RHfit$Ext

plot(RHfit$years, qxt["65", ], xlim = range(RHfit$years, RHsim$years),
     ylim = range(qxt["65", ], RHsim$rates["65", , 1:20]), type = "l",
     xlab = "year", ylab = "rate", main = "Mortality rates at age 65")

matlines(RHsim$years, RHsim$rates["65", , 1:20], type = "l", lty = 1)

###################################### Market Price of Risk ######################################
# Need to change if we are changing the year to 2022
# Based on the 2018 Gilt rate
interest_rate = 0.017
discount_factor = 1/(1+interest_rate)
qxt_LC <- LCfit$Dxt / LCfit$Ext

# 5,563 GBP per 100,000 annuity rate annuity rate for male age 65 in UK during 2017
# Mismatch between 2017 and 2018 is justified by small difference between the two (similar levels)
payment = 5563
total = 100000

#Define K as the maximum age - minimum age
K = 90 - 60

# Minimize SSE to fit lambda parameter
##### Torske (Wang Thesis) page 55 as reference
LC_wang_sse = function(lambda) {
  sum( payment * sum( discount_factor^(0:K-1) * pnorm(qnorm(1- qxt_LC["65",] ) - lambda))  - total )^2
}

## Nonlinear minimization
LC_lambda_wang = nlm(LC_wang_sse, 2)$estimate


############################################################ Pricing #####################################################
# Wang transform risk-adjusted tPx
LC_wang_risk_adjusted_qxt = pnorm(qnorm(qxt_LC) - LC_lambda_wang)

LCfor_65_qxt = LCfor$rate[5,]

LC_poisson_mean = LC_wang_risk_adjusted_qxt
LC_poisson_s = (LC_wang_risk_adjusted_qxt * (1 - LC_wang_risk_adjusted_qxt))^(0.5)
Expected_death = 1 - pnorm( (LCfor_65_qxt - LC_poisson_mean)/LC_poisson_s )
Expected_death[5,]

V_first_term = sum( Expected_death[5,] * discount_factor^(1:51))
V_first_term
V_second_term = sum( LCfor_65_qxt * discount_factor^(1:30) )
V_second_term


Price = V_first_term - V_second_term 
Price
