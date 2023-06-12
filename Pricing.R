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
interest_rate = 0.045
discount_factor = 1/(1+interest_rate)
annuitants = 100

# https://www.sharingpensions.co.uk/
# Values here are as of 06/11/23
payment = 6845
total = 100000
K = length(ages.fit) - 1 # Define K as the total number of years

################################################################################################################################
#################################################### Pricing Mortality Swap ####################################################
################################################################################################################################
LC_mxt = LCfit$Dxt/LCfit$Ext
RH_mxt = RHfit$Dxt/RHfit$Ext
CBD_mxt = CBDfit$Dxt/CBDfit$Ext
M6_mxt = M6fit$Dxt/M6fit$Ext

# get the prices for the first "years_for" years
LC_Wang_prices = as.numeric( lapply(1:years_for, function(years_for) getPrice(5, years_for, "LC", "Wang")) )
RH_Prop_prices = as.numeric( lapply(1:years_for, function(years_for) getPrice(5, years_for, "RH", "Proportional")) )
CBD_Std_prices = as.numeric( lapply(1:years_for, function(years_for) getPrice(5, years_for, "CBD", "Stdev")) )