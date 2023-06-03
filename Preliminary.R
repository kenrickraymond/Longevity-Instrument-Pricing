library(demography)
library(StMoMo)
library(lifecontingencies)

source("MortalityFunctions.R")
source("Credentials.R")

# Gather the data
UKdata <- hmd.mx(country = "GBR_NP", username=username,
                 password=password)
UKmale = UKdata$rate$male
# UKdata[65, 47:96]

# Plot UK Males
plot(UKdata, series = "male")

# Restrict the Data
ages.fit <- 60:89
years.fit <- 1968:2017

# Define StMoMo Object
UKmale <- StMoMoData(UKdata, series = "male")

# InitialiZe mortality models
LC <- lc()
# cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
RH = rh(cohortAgeFun = "NP")

# Fit Mortality Models
LCfit <- fit(LC, data=UKmale, ages.fit=ages.fit,
             years.fit=years.fit)
RHfit <- fit(RH, data=UKmale, ages.fit=ages.fit,
             years.fit=years.fit)

### Goodness of Fit
AIC(LCfit)
AIC(RHfit)
BIC(LCfit)
BIC(RHfit)

# Plot fitted models
plot(LCfit)
plot(RHfit)

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

### Market Price of Risk
# Based on the 2018 Gilt rate
discount_factor = 1/(1+0.017)
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
lambda = nlm(LC_wang_sse, 2)$estimate
lambda
