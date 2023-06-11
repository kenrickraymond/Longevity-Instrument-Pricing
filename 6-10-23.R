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
years_for = 10
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)


###################################### Market Price of Risk ######################################
interest_rate = 0.045
discount_factor = 1/(1+interest_rate)

LC_qxt = LCfit$Dxt / LCfit$Ext
LC_pxt = 1 - LC_qxt

payment = 6845
total = 100000

#Define K as the total number of years
K = 89-60

# Minimize SSE to fit lambda parameter
LC_wang_sse = function(lambda) {
  sum( payment * sum( discount_factor^(0:K-1) * pnorm(qnorm(LC_pxt[,1] ) - lambda))  - total )^2
}

LC_lambda_wang = nlm(LC_wang_sse, 0.5)$estimate

############################################################ Pricing #####################################################
# Forecasted Wang transform risk-adjusted rates 
## Lin and Cox Method
forecasted_qxt = LCfor$rates[5,]
forecasted_pxt = 1 - LCfor$rates[5,]

# Floating Leg
LC_wang_risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - LC_lambda_wang)
S_t = sum( LC_wang_risk_adjusted_pxt * discount_factor^(1:years_for) )

# Fixed-Leg 
LC_mxt = LCfit$Dxt/LCfit$Ext

# Average of 30 year    
seventy_to_eighty_mortality_improvement = mean(LC_mxt[10:20,years_for] - LC_mxt[10:20,1])
eighty_to_ninety_mortality_improvement = mean(LC_mxt[20:30,years_for] - LC_mxt[20:30,1])

# Input the time in years for which the mortality improvements we will be using
X_k = function(k, t){
  if(k <= 10){
    X_k = forecasted_pxt * exp( -seventy_to_eighty_mortality_improvement * t )  
  } else if (10 < k <= 20) {
    X_k = forecasted_pxt * exp( -seventy_to_eighty_mortality_improvement * 10 ) * exp( - eighty_to_ninety_mortality_improvement * (t-10) )
  }
  return(X_k)
}

K_t = sum( X_k_ten() * discount_factor^(1:years_for) )

S_t - K_t

# Dowd method
riskprem = (S_t/K_t) - 1
riskprem
