# Measures of Central Tendency
LC.Wang.range = max(LC_Wang_Forward_Premium) - min(LC_Wang_Forward_Premium)
LC.Wang.mean = mean(LC_Wang_Forward_Premium)
LC.Wang.variance = var(LC_Wang_Forward_Premium)

LC.Prop.range = max(LC_Prop_Forward_Premium) - min(LC_Prop_Forward_Premium)
LC.Prop.mean = mean(LC_Prop_Forward_Premium)
LC.Prop.variance = var(LC_Prop_Forward_Premium)

LC.Dual.range = max(LC_Dual_Forward_Premium) - min(LC_Dual_Forward_Premium)
LC.Dual.mean = mean(LC_Dual_Forward_Premium)
LC.Dual.variance = var(LC_Dual_Forward_Premium)

LC.Gini.range = max(LC_Gini_Forward_Premium) - min(LC_Gini_Forward_Premium)
LC.Gini.mean = mean(LC_Gini_Forward_Premium)
LC.Gini.variance = var(LC_Gini_Forward_Premium)

LC.Exponential.range = max(LC_Exponential_Forward_Premium) - min(LC_Exponential_Forward_Premium)
LC.Exponential.mean = mean(LC_Exponential_Forward_Premium)
LC.Exponential.variance = var(LC_Exponential_Forward_Premium)

LC.Std.range = max(LC_Std_Forward_Premium) - min(LC_Std_Forward_Premium)
LC.Std.mean = mean(LC_Std_Forward_Premium)
LC.Std.variance = var(LC_Std_Forward_Premium)

LC.Var.range = max(LC_Var_Forward_Premium) - min(LC_Var_Forward_Premium)
LC.Var.mean = mean(LC_Var_Forward_Premium)
LC.Var.variance = var(LC_Var_Forward_Premium)

LC.Mad.range = max(LC_Mad_Forward_Premium) - min(LC_Mad_Forward_Premium)
LC.Mad.mean = mean(LC_Mad_Forward_Premium)
LC.Mad.variance = var(LC_Mad_Forward_Premium)


LC.premium.table = data.frame(matrix(nrow = 8, ncol = 3, c(LC.Wang.range, LC.Prop.range, LC.Dual.range, LC.Gini.range, LC.Exponential.range, LC.Std.range, LC.Var.range, LC.Mad.range,
                                                           LC.Wang.mean, LC.Prop.mean, LC.Dual.mean, LC.Gini.mean, LC.Exponential.mean, LC.Std.mean, LC.Var.mean, LC.Mad.mean,
                                                           LC.Wang.variance, LC.Prop.variance, LC.Dual.variance, LC.Gini.variance, LC.Exponential.variance, LC.Std.variance, LC.Var.variance, LC.Mad.variance)
))
rownames(LC.premium.table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(LC.premium.table) = c("Range", "Mean", "Var")
round(LC.premium.table, digits=6)

RH.Wang.range = max(RH_Wang_Forward_Premium) - min(RH_Wang_Forward_Premium)
RH.Wang.mean = mean(RH_Wang_Forward_Premium)
RH.Wang.variance = var(RH_Wang_Forward_Premium)

RH.Prop.range = max(RH_Prop_Forward_Premium) - min(RH_Prop_Forward_Premium)
RH.Prop.mean = mean(RH_Prop_Forward_Premium)
RH.Prop.variance = var(RH_Prop_Forward_Premium)

RH.Dual.range = max(RH_Dual_Forward_Premium) - min(RH_Dual_Forward_Premium)
RH.Dual.mean = mean(RH_Dual_Forward_Premium)
RH.Dual.variance = var(RH_Dual_Forward_Premium)

RH.Gini.range = max(RH_Gini_Forward_Premium) - min(RH_Gini_Forward_Premium)
RH.Gini.mean = mean(RH_Gini_Forward_Premium)
RH.Gini.variance = var(RH_Gini_Forward_Premium)

RH.Exponential.range = max(RH_Exponential_Forward_Premium) - min(RH_Exponential_Forward_Premium)
RH.Exponential.mean = mean(RH_Exponential_Forward_Premium)
RH.Exponential.variance = var(RH_Exponential_Forward_Premium)

RH.Std.range = max(RH_Std_Forward_Premium) - min(RH_Std_Forward_Premium)
RH.Std.mean = mean(RH_Std_Forward_Premium)
RH.Std.variance = var(RH_Std_Forward_Premium)

RH.Var.range = max(RH_Var_Forward_Premium) - min(RH_Var_Forward_Premium)
RH.Var.mean = mean(RH_Var_Forward_Premium)
RH.Var.variance = var(RH_Var_Forward_Premium)

RH.Mad.range = max(RH_Mad_Forward_Premium) - min(RH_Mad_Forward_Premium)
RH.Mad.mean = mean(RH_Mad_Forward_Premium)
RH.Mad.variance = var(RH_Mad_Forward_Premium)


RH.premium.table = data.frame(matrix(nrow = 8, ncol = 3, c(RH.Wang.range, RH.Prop.range, RH.Dual.range, RH.Gini.range, RH.Exponential.range, RH.Std.range, RH.Var.range, RH.Mad.range,
                                                           RH.Wang.mean, RH.Prop.mean, RH.Dual.mean, RH.Gini.mean, RH.Exponential.mean, RH.Std.mean, RH.Var.mean, RH.Mad.mean,
                                                           RH.Wang.variance, RH.Prop.variance, RH.Dual.variance, RH.Gini.variance, RH.Exponential.variance, RH.Std.variance, RH.Var.variance, RH.Mad.variance)
))
rownames(RH.premium.table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(RH.premium.table) = c("Range", "Mean", "Var")
round(RH.premium.table, digits=6)

CBD.Wang.range = max(CBD_Wang_Forward_Premium) - min(CBD_Wang_Forward_Premium)
CBD.Wang.mean = mean(CBD_Wang_Forward_Premium)
CBD.Wang.variance = var(CBD_Wang_Forward_Premium)

CBD.Prop.range = max(CBD_Prop_Forward_Premium) - min(CBD_Prop_Forward_Premium)
CBD.Prop.mean = mean(CBD_Prop_Forward_Premium)
CBD.Prop.variance = var(CBD_Prop_Forward_Premium)

CBD.Dual.range = max(CBD_Dual_Forward_Premium) - min(CBD_Dual_Forward_Premium)
CBD.Dual.mean = mean(CBD_Dual_Forward_Premium)
CBD.Dual.variance = var(CBD_Dual_Forward_Premium)

CBD.Gini.range = max(CBD_Gini_Forward_Premium) - min(CBD_Gini_Forward_Premium)
CBD.Gini.mean = mean(CBD_Gini_Forward_Premium)
CBD.Gini.variance = var(CBD_Gini_Forward_Premium)

CBD.Exponential.range = max(CBD_Exponential_Forward_Premium) - min(CBD_Exponential_Forward_Premium)
CBD.Exponential.mean = mean(CBD_Exponential_Forward_Premium)
CBD.Exponential.variance = var(CBD_Exponential_Forward_Premium)

CBD.Std.range = max(CBD_Std_Forward_Premium) - min(CBD_Std_Forward_Premium)
CBD.Std.mean = mean(CBD_Std_Forward_Premium)
CBD.Std.variance = var(CBD_Std_Forward_Premium)

CBD.Var.range = max(CBD_Var_Forward_Premium) - min(CBD_Var_Forward_Premium)
CBD.Var.mean = mean(CBD_Var_Forward_Premium)
CBD.Var.variance = var(CBD_Var_Forward_Premium)

CBD.Mad.range = max(CBD_Mad_Forward_Premium) - min(CBD_Mad_Forward_Premium)
CBD.Mad.mean = mean(CBD_Mad_Forward_Premium)
CBD.Mad.variance = var(CBD_Mad_Forward_Premium)


CBD.premium.table = data.frame(matrix(nrow = 8, ncol = 3, c(CBD.Wang.range, CBD.Prop.range, CBD.Dual.range, CBD.Gini.range, CBD.Exponential.range, CBD.Std.range, CBD.Var.range, CBD.Mad.range,
                                                            CBD.Wang.mean, CBD.Prop.mean, CBD.Dual.mean, CBD.Gini.mean, CBD.Exponential.mean, CBD.Std.mean, CBD.Var.mean, CBD.Mad.mean,
                                                            CBD.Wang.variance, CBD.Prop.variance, CBD.Dual.variance, CBD.Gini.variance, CBD.Exponential.variance, CBD.Std.variance, CBD.Var.variance, CBD.Mad.variance)
))
rownames(CBD.premium.table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(CBD.premium.table) = c("Range", "Mean", "Var")
round(CBD.premium.table, digits=6)

M6.Wang.range = max(M6_Wang_Forward_Premium) - min(M6_Wang_Forward_Premium)
M6.Wang.mean = mean(M6_Wang_Forward_Premium)
M6.Wang.variance = var(M6_Wang_Forward_Premium)

M6.Prop.range = max(M6_Prop_Forward_Premium) - min(M6_Prop_Forward_Premium)
M6.Prop.mean = mean(M6_Prop_Forward_Premium)
M6.Prop.variance = var(M6_Prop_Forward_Premium)

M6.Dual.range = max(M6_Dual_Forward_Premium) - min(M6_Dual_Forward_Premium)
M6.Dual.mean = mean(M6_Dual_Forward_Premium)
M6.Dual.variance = var(M6_Dual_Forward_Premium)

M6.Gini.range = max(M6_Gini_Forward_Premium) - min(M6_Gini_Forward_Premium)
M6.Gini.mean = mean(M6_Gini_Forward_Premium)
M6.Gini.variance = var(M6_Gini_Forward_Premium)

M6.Exponential.range = max(M6_Exponential_Forward_Premium) - min(M6_Exponential_Forward_Premium)
M6.Exponential.mean = mean(M6_Exponential_Forward_Premium)
M6.Exponential.variance = var(M6_Exponential_Forward_Premium)

M6.Std.range = max(M6_Std_Forward_Premium) - min(M6_Std_Forward_Premium)
M6.Std.mean = mean(M6_Std_Forward_Premium)
M6.Std.variance = var(M6_Std_Forward_Premium)

M6.Var.range = max(M6_Var_Forward_Premium) - min(M6_Var_Forward_Premium)
M6.Var.mean = mean(M6_Var_Forward_Premium)
M6.Var.variance = var(M6_Var_Forward_Premium)

M6.Mad.range = max(M6_Mad_Forward_Premium) - min(M6_Mad_Forward_Premium)
M6.Mad.mean = mean(M6_Mad_Forward_Premium)
M6.Mad.variance = var(M6_Mad_Forward_Premium)


M6.premium.table = data.frame(matrix(nrow = 8, ncol = 3, c(M6.Wang.range, M6.Prop.range, M6.Dual.range, M6.Gini.range, M6.Exponential.range, M6.Std.range, M6.Var.range, M6.Mad.range,
                                                           M6.Wang.mean, M6.Prop.mean, M6.Dual.mean, M6.Gini.mean, M6.Exponential.mean, M6.Std.mean, M6.Var.mean, M6.Mad.mean,
                                                           M6.Wang.variance, M6.Prop.variance, M6.Dual.variance, M6.Gini.variance, M6.Exponential.variance, M6.Std.variance, M6.Var.variance, M6.Mad.variance)
))
rownames(M6.premium.table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(M6.premium.table) = c("Range", "Mean", "Var")
round(M6.premium.table, digits=6)