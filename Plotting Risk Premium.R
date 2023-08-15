source("Pricing.R")

# Survivor Forward
 # LC
par(mfrow=c(1,2))
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
 
mtext("Lee-Carter Model", side = 3, line = - 2, outer = TRUE)

# RH
plot(RH_Wang_Forward_Premium,
     ylim = c( min(RH_Wang_Forward_Premium, RH_Prop_Forward_Premium), max(RH_Wang_Forward_Premium, RH_Prop_Forward_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from RH model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(RH_Prop_Forward_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)


plot(RH_Std_Forward_Premium,
     ylim = c( min(RH_Std_Forward_Premium, RH_Var_Forward_Premium), max(RH_Std_Forward_Premium, RH_Var_Forward_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from RH model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(RH_Var_Forward_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("Renshaw-Haberman Model", side = 3, line = - 2, outer = TRUE)

# CBD

plot(CBD_Wang_Forward_Premium,
     ylim = c( min(CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium), max(CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from CBD model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(CBD_Prop_Forward_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)


plot(CBD_Std_Forward_Premium,
     ylim = c( min(CBD_Std_Forward_Premium, CBD_Var_Forward_Premium), max(CBD_Std_Forward_Premium, CBD_Var_Forward_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from CBD model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(CBD_Var_Forward_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("Cairns, Blake, Dowd Model", side = 3, line = - 2, outer = TRUE)

# M6

plot(M6_Wang_Forward_Premium,
     ylim = c( min(M6_Wang_Forward_Premium, M6_Prop_Forward_Premium), max(M6_Wang_Forward_Premium, M6_Prop_Forward_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from M6 model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(M6_Prop_Forward_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)


plot(M6_Std_Forward_Premium,
     ylim = c( min(M6_Std_Forward_Premium, M6_Var_Forward_Premium), max(M6_Std_Forward_Premium, M6_Var_Forward_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival forward risk premium generated from M6 model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survivor Forward")
lines(M6_Var_Forward_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("M6 Model", side = 3, line = - 2, outer = TRUE)
# Survivor Swap
# LC
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

mtext("Lee-Carter Model", side = 3, line = - 2, outer = TRUE)

# RH

plot(RH_Wang_Swap_Premium,
     ylim = c( min(RH_Wang_Swap_Premium, RH_Prop_Swap_Premium), max(RH_Wang_Swap_Premium, RH_Prop_Swap_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from RH model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(RH_Prop_Swap_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)

plot(RH_Std_Swap_Premium,
     ylim = c( min(RH_Std_Swap_Premium, RH_Var_Swap_Premium), max(RH_Std_Swap_Premium, RH_Var_Swap_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from RH model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(RH_Var_Swap_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("Renshaw Haberman Model", side = 3, line = - 2, outer = TRUE)
# CBD

plot(CBD_Wang_Swap_Premium,
     ylim = c( min(CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium), max(CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from CBD model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(CBD_Prop_Swap_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)

plot(CBD_Std_Swap_Premium,
     ylim = c( min(CBD_Std_Swap_Premium, CBD_Var_Swap_Premium), max(CBD_Std_Swap_Premium, CBD_Var_Swap_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from CBD model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(CBD_Var_Swap_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("Cairns, Blake, Dowd Model", side = 3, line = - 2, outer = TRUE)

# M6
plot(M6_Wang_Swap_Premium,
     ylim = c( min(M6_Wang_Swap_Premium, M6_Prop_Swap_Premium), max(M6_Wang_Swap_Premium, M6_Prop_Swap_Premium)),
     lwd=2,
     type="l",
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from M6 model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(M6_Prop_Swap_Premium, lwd = 2, col="red")
legend("bottomright", legend=c("Wang Principle", "Proportional Hazard Principle"),
       col=c("black", "red", "green", "blue"), lty=c(1,1), cex=0.8)

plot(M6_Std_Swap_Premium,
     ylim = c( min(M6_Std_Swap_Premium, M6_Var_Swap_Premium), max(M6_Std_Swap_Premium, M6_Var_Swap_Premium)),
     lwd=2,
     type="l",
     lty=2,
     xlim = c(1,years_for+1),
     # main= "Survival Swap risk premium generated from M6 model",
     ylab="Risk Premium",
     xlab = "Years to Maturity of Survival Swap")
lines(M6_Var_Swap_Premium, col="blue", lty=2)
legend("bottomright", legend=c("Standard Deviation Principle", "Variance Principle"),
       col=c("black", "red", "green", "blue"), lty=c(2,2), cex=0.8)

mtext("M6 Model", side = 3, line = - 2, outer = TRUE)
