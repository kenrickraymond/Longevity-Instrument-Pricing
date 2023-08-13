source("Pricing.R")

# Survivor Forward
 # LC
par(mfrow=c(1,2))
mtext("Survivor Forward risk premium generated from LC model")
plot(LC_Wang_Forward_Premium,
     ylim = c( min(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium), max(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from LC model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(LC_Prop_Forward_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)


plot(LC_Std_Forward_Premium,
     ylim = c( min(LC_Std_Forward_Premium, LC_Var_Forward_Premium), max(LC_Std_Forward_Premium, LC_Var_Forward_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from LC model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(LC_Var_Forward_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)
 #

# Survivor Swap
par(mfrow=c(1,2))
plot(LC_Wang_Swap_Premium,
     ylim = c( min(LC_Wang_Swap_Premium, LC_Prop_Swap_Premium), max(LC_Wang_Swap_Premium, LC_Prop_Swap_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from LC model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(LC_Prop_Swap_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)

plot(LC_Std_Swap_Premium,
     ylim = c( min(LC_Std_Swap_Premium, LC_Var_Swap_Premium), max(LC_Std_Swap_Premium, LC_Var_Swap_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from LC model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(LC_Var_Swap_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)
