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
RH = rh(link = "log", 
        cohortAgeFun = "1") # cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
CBD = cbd(link="logit") # Logit link refers to q_{x,t}/{1 - q_{x,t}}
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
table

# Plot fitted models
plot(LCfit)
plot(RHfit)
plot(CBDfit)
plot(M6fit)

# Generate h-year ahead forecasts
years_for = 20
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)

################################################################################################################################
#################################################### Market Price of Risk ######################################################
################################################################################################################################
# Set the parameter values
lambda = 0.5
notional_principal = 1000
annuitants = 10000

interest_rate = 0.05
discount_factor = exp(- interest_rate)

# Replace with k as link cohort
forecasted_qxt <<- LCfor$rates[1,]
forecasted_pxt <<- 1 - forecasted_qxt

risk_adjusted_pxt = pnorm(qnorm(forecasted_pxt) - lambda)

# Last forecasted year for reference age, S(T)
K_t = annuitants * mean(risk_adjusted_pxt)
S_t = annuitants * tail(risk_adjusted_pxt, n=1) 

notional_principal * (S_t - K_t) 

IRP = log(K_t / S_t) /  years_for
           