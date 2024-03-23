# par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mai=rep(0.5, 4))
layout( matrix(c(1,2,3,4,5,5,5,5), ncol=4, byrow=TRUE), heights=c(4,0.5) ) 
plot(LC_Wang_Forward_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main = "LC model",
     ylab="",
     xlab = "")
lines(LC_Prop_Forward_Premium, lwd = 2, col="blue")
lines(LC_Dual_Forward_Premium, lwd = 2, col="red")
lines(LC_Gini_Forward_Premium, lwd = 2, col="gold")
lines(LC_Exponential_Forward_Premium, lwd = 2, col="green")
lines(LC_Std_Forward_Premium, lwd = 2, col="purple")
lines(LC_Var_Forward_Premium, lwd = 2, col="orange")
lines(LC_Mad_Forward_Premium, lwd = 2, col="gray")
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
       # col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)


plot(RH_Wang_Forward_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main = "RH model",
     ylab="",
     xlab = "")
lines(RH_Prop_Forward_Premium, lwd = 2, col="blue")
lines(RH_Dual_Forward_Premium, lwd = 2, col="red")
lines(RH_Gini_Forward_Premium, lwd = 2, col="gold")
lines(RH_Exponential_Forward_Premium, lwd = 2, col="green")
lines(RH_Std_Forward_Premium, lwd = 2, col="purple")
lines(RH_Var_Forward_Premium, lwd = 2, col="orange")
lines(RH_Mad_Forward_Premium, lwd = 2, col="gray")
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)


plot(CBD_Wang_Forward_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main = "CBD model",
     ylab="",
     xlab = "")
lines(CBD_Prop_Forward_Premium, lwd = 2, col="blue")
lines(CBD_Dual_Forward_Premium, lwd = 2, col="red")
lines(CBD_Gini_Forward_Premium, lwd = 2, col="gold")
lines(CBD_Exponential_Forward_Premium, lwd = 2, col="green")
lines(CBD_Std_Forward_Premium, lwd = 2, col="purple")
lines(CBD_Var_Forward_Premium, lwd = 2, col="orange")
lines(CBD_Mad_Forward_Premium, lwd = 2, col="gray")
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)


plot(M6_Wang_Forward_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main = "M6 model",
     ylab="",
     xlab = "")
lines(M6_Prop_Forward_Premium, lwd = 2, col="blue")
lines(M6_Dual_Forward_Premium, lwd = 2, col="red")
lines(M6_Gini_Forward_Premium, lwd = 2, col="gold")
lines(M6_Exponential_Forward_Premium, lwd = 2, col="green")
lines(M6_Std_Forward_Premium, lwd = 2, col="purple")
lines(M6_Var_Forward_Premium, lwd = 2, col="orange")
lines(M6_Mad_Forward_Premium, lwd = 2, col="gray")
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)
par(mai=c(0,0,0,0))
plot.new()
mtext("Years to Maturity of S-forward", cex=0.8)
mtext("Risk-Adjustment Term in Decimal Basis", side=2, line=-1.2, adj=-2.3, padj=0.5, cex=0.8)
legend(x='center',legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD"),
       col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), title="Premium Principle", lty=c(1,1,1,1,1,2,2,2), ncol=4, lwd=5, cex=1)




# Swap
par(mai=rep(0.5, 4))
layout( matrix(c(1,2,3,4,5,5,5,5), ncol=4, byrow=TRUE), heights=c(4,0.5) ) 
plot(LC_Wang_Swap_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main= "LC model",
     ylab="",
     xlab = "")
lines(LC_Prop_Swap_Premium, lwd = 2, col="blue")
lines(LC_Dual_Swap_Premium, lwd = 2, col="red")
lines(LC_Gini_Swap_Premium, lwd = 2, col="gold")
lines(LC_Exponential_Swap_Premium, lwd = 2, col="green")
lines(LC_Std_Swap_Premium, lwd = 2, col="purple", lty=2)
lines(LC_Var_Swap_Premium, lwd = 2, col="orange", lty=2)
lines(LC_Mad_Swap_Premium, lwd = 2, col="gray", lty=2)
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)


plot(RH_Wang_Swap_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main= "RH model",
     ylab="",
     xlab = "")
lines(RH_Prop_Swap_Premium, lwd = 2, col="blue")
lines(RH_Dual_Swap_Premium, lwd = 2, col="red")
lines(RH_Gini_Swap_Premium, lwd = 2, col="gold")
lines(RH_Exponential_Swap_Premium, lwd = 2, col="green")
lines(RH_Std_Swap_Premium, lwd = 2, col="purple", lty=2)
lines(RH_Var_Swap_Premium, lwd = 2, col="orange", lty=2)
lines(RH_Mad_Swap_Premium, lwd = 2, col="gray", lty=2)
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)

plot(CBD_Wang_Swap_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main= "CBD model",
     ylab="",
     xlab = "")
lines(CBD_Prop_Swap_Premium, lwd = 2, col="blue")
lines(CBD_Dual_Swap_Premium, lwd = 2, col="red")
lines(CBD_Gini_Swap_Premium, lwd = 2, col="gold")
lines(CBD_Exponential_Swap_Premium, lwd = 2, col="green")
lines(CBD_Std_Swap_Premium, lwd = 2, col="purple", lty=2)
lines(CBD_Var_Swap_Premium, lwd = 2, col="orange", lty=2)
lines(CBD_Mad_Swap_Premium, lwd = 2, col="gray", lty=2)
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)


plot(M6_Wang_Swap_Premium,
     ylim = c( min(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium)),
               max(c(LC_Wang_Forward_Premium, LC_Prop_Forward_Premium, LC_Dual_Forward_Premium, LC_Gini_Forward_Premium, LC_Exponential_Forward_Premium, LC_Std_Forward_Premium, LC_Var_Forward_Premium, LC_Mad_Forward_Premium,
                     RH_Wang_Forward_Premium, RH_Prop_Forward_Premium, RH_Dual_Forward_Premium, RH_Gini_Forward_Premium, RH_Exponential_Forward_Premium, RH_Std_Forward_Premium, RH_Var_Forward_Premium, RH_Mad_Forward_Premium,
                     CBD_Wang_Forward_Premium, CBD_Prop_Forward_Premium, CBD_Dual_Forward_Premium, CBD_Gini_Forward_Premium, CBD_Exponential_Forward_Premium, CBD_Std_Forward_Premium, CBD_Var_Forward_Premium, CBD_Mad_Forward_Premium,
                     M6_Wang_Forward_Premium, M6_Prop_Forward_Premium, M6_Dual_Forward_Premium, M6_Gini_Forward_Premium, M6_Exponential_Forward_Premium, M6_Std_Forward_Premium, M6_Var_Forward_Premium, M6_Mad_Forward_Premium,
                     LC_Wang_Swap_Premium, LC_Prop_Swap_Premium, LC_Dual_Swap_Premium, LC_Gini_Swap_Premium, LC_Exponential_Swap_Premium, LC_Std_Swap_Premium, LC_Var_Swap_Premium, LC_Mad_Swap_Premium,
                     RH_Wang_Swap_Premium, RH_Prop_Swap_Premium, RH_Dual_Swap_Premium, RH_Gini_Swap_Premium, RH_Exponential_Swap_Premium, RH_Std_Swap_Premium, RH_Var_Swap_Premium, RH_Mad_Swap_Premium,
                     CBD_Wang_Swap_Premium, CBD_Prop_Swap_Premium, CBD_Dual_Swap_Premium, CBD_Gini_Swap_Premium, CBD_Exponential_Swap_Premium, CBD_Std_Swap_Premium, CBD_Var_Swap_Premium, CBD_Mad_Swap_Premium,
                     M6_Wang_Swap_Premium, M6_Prop_Swap_Premium, M6_Dual_Swap_Premium, M6_Gini_Swap_Premium, M6_Exponential_Swap_Premium, M6_Std_Swap_Premium, M6_Var_Swap_Premium, M6_Mad_Swap_Premium))),
     lwd=2,
     type="l",
     xlim = c(1,years_for),
     main= "M6 model",
     ylab="",
     xlab = "")
lines(M6_Prop_Swap_Premium, lwd = 2, col="blue")
lines(M6_Dual_Swap_Premium, lwd = 2, col="red")
lines(M6_Gini_Swap_Premium, lwd = 2, col="gold")
lines(M6_Exponential_Swap_Premium, lwd = 2, col="green")
lines(M6_Std_Swap_Premium, lwd = 2, col="purple", lty=2)
lines(M6_Var_Swap_Premium, lwd = 2, col="orange", lty=2)
lines(M6_Mad_Swap_Premium, lwd = 2, col="gray", lty=2)
# legend("topleft", legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad"),
#        col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), lty=c(1,1,1,1,1,2,2,2), cex=0.8)

par(mai=c(0,0,0,0))
plot.new()
mtext("Years to Maturity of S-Swap", cex=0.8)
mtext("Risk-Adjustment Term in Decimal Basis", side=2, line=-1.2, adj=-2.3, padj=0.5, cex=0.8)
legend(x='center',legend=c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD"),
       col=c("black", "blue", "red", "gold", "green", "purple", "orange", "gray"), title="Premium Principle", lty=c(1,1,1,1,1,2,2,2), ncol=4, lwd=5, cex=1)

