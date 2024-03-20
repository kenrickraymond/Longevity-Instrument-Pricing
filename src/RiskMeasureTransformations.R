# Standardized Values
#  Get Values in CSV format to paste
ForwardMeasureTable = abs(data.frame(Forward_VaR_table, Forward_VaR_table_2Y))
# forward_standardized_table = sapply(ForwardMeasureTable, scale)
# rownames(forward_standardized_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
ForwardMeasureTable
(format(ForwardMeasureTable, scientific=TRUE, digits=4))

ForwardMeasureTable_ES = abs(data.frame(Forward_ES_table, Forward_ES_table_2Y))
# forward_standardized_table = sapply(ForwardMeasureTable, scale)
# rownames(forward_standardized_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
ForwardMeasureTable_ES
(format(ForwardMeasureTable_ES, scientific=TRUE, digits=4))


SwapMeasureTable = abs(data.frame(Swap_VaR_table,Swap_VaR_table_2Y ))
# Swap_standardized_table = sapply(SwapMeasureTable, scale)
# rownames(Swap_standardized_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
SwapMeasureTable
(format(SwapMeasureTable, scientific=TRUE, digits=4))

SwapMeasureTable_ES = abs(data.frame(Swap_ES_table,Swap_ES_table_2Y ))
# Swap_standardized_table = sapply(SwapMeasureTable, scale)
# rownames(Swap_standardized_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")
SwapMeasureTable_ES
(format(SwapMeasureTable_ES, scientific=TRUE, digits=4))


# Getting max values for axis
max(abs(data.frame(Forward_VaR_table, Forward_VaR_table_2Y, Swap_VaR_table, Swap_VaR_table_2Y,
                   Forward_ES_table, Forward_ES_table_2Y, Swap_ES_table, Swap_ES_table_2Y)))

#  Plotting Bar Charts
MortalityModels = c("LC", "RH", "CBD", "M6")
PremiumPrinciple = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "Std. Dev.", "Variance", "MAD")

library(RColorBrewer)
color1 =brewer.pal(8, name="Set2")  # c("green", "orange","pink","cyan","navy","purple","yellow","brown")
library(yarrr)
color2 = yarrr::transparent(color1, trans.val = .4)

# S-forward 1Y
par(mar = c(6, 4, 4, 2))

barplot(as.matrix(abs(Forward_VaR_table)),
        # main="99.5% VaR_1Y",
        ylab = "1Y VaR and ES for S-forward",
        ylim=c(0,0.040),
        beside=TRUE,
        col=color1,
        # legend = PremiumPrinciple,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix(abs(Forward_ES_table)),
        # main="99.5% VaR_1Y",
        beside=TRUE,
        col=color2,
        # legend = PremiumPrinciple,
        # horiz=TRUE
        add=TRUE
)

# S-swap 1Y
barplot(as.matrix(abs(Swap_VaR_table)),
        # main="99.5% VaR_1Y",
        ylab = "1Y VaR and ES for S-Swap",
        ylim=c(0,0.040),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix(abs(Swap_ES_table)),
        beside=TRUE,
        col=color2,
        add=TRUE
)

# S-forward 2Y
par(mar = c(6, 4, 4, 2))

barplot(as.matrix(abs(Forward_VaR_table_2Y)),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-forward",
        ylim=c(0,0.040),
        beside=TRUE,
        col=color1,
        # legend = PremiumPrinciple,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix(abs(Forward_ES_table_2Y)),
        # main="99.5% VaR_1Y",
        beside=TRUE,
        col=color2,
        # legend = PremiumPrinciple,
        # horiz=TRUE
        add=TRUE
)

# S-swap 2Y
barplot(as.matrix(abs(Swap_VaR_table_2Y)),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-Swap",
        ylim=c(0,0.040),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix(abs(Swap_ES_table_2Y)),
        beside=TRUE,
        col=color2,
        add=TRUE
)

#  Calculate Differences in range
diff = as.numeric(sapply(abs(Swap_ES_table), range)[2,] - sapply(abs(Swap_ES_table), range)[1,])
format(sapply(abs(Swap_ES_table), range), scientific=TRUE, digits=4)
format(diff, scientific=TRUE, digits=4)


# Difference between 1- and 2- Year Risk Measures
max(abs(data.frame(Forward_VaR_table - Forward_VaR_table_2Y, Swap_VaR_table - Swap_VaR_table_2Y,
                   Forward_ES_table - Forward_ES_table_2Y, Swap_ES_table - Swap_ES_table_2Y)))

min((data.frame(Forward_VaR_table - Forward_VaR_table_2Y, Swap_VaR_table - Swap_VaR_table_2Y,
                Forward_ES_table - Forward_ES_table_2Y, Swap_ES_table - Swap_ES_table_2Y)))

barplot(as.matrix( as.matrix(abs(Forward_VaR_table) - abs(Forward_VaR_table_2Y) ) ),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-Swap",
        ylim=c(-0.004,0.007),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix( as.matrix(abs(Swap_VaR_table) - abs(Swap_VaR_table_2Y) ) ),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-Swap",
        ylim=c(-0.004,0.007),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix( as.matrix(abs(Forward_ES_table) - abs(Forward_ES_table_2Y) ) ),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-Swap",
        ylim=c(-0.004,0.007),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

barplot(as.matrix( as.matrix(abs(Swap_ES_table) - abs(Swap_ES_table_2Y) ) ),
        # main="99.5% VaR_1Y",
        ylab = "2Y VaR and ES for S-Swap",
        ylim=c(-0.004,0.007),
        beside=TRUE,
        col=color1,
        legend = TRUE, args.legend = list(bty = "n", x = "top", ncol = 3)
        # horiz=TRUE
)

as.matrix(abs(Swap_ES_table) - abs(Swap_ES_table_2Y) )
as.matrix(abs(Swap_VaR_table) - abs(Swap_VaR_table_2Y) )          
