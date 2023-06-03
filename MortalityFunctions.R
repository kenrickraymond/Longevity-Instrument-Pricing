hmd.mx <- function(country, username, password, label = country) {
  # Read raw MX and Exposure data
  mx <- HMDHFDplus::readHMDweb(country, item = "Mx_1x1",
                               username = username, password = password, fixup = TRUE)
  pop <- HMDHFDplus::readHMDweb(country, item = "Exposures_1x1",
                                username = username, password = password, fixup = TRUE)
  
  # Construct output
  obj <- list(type = "mortality", label = label, lambda = 0)
  obj$year <- sort(unique(mx[, "Year"]))
  n <- length(obj$year)
  m <- length(unique(mx[, "Age"]))
  obj$age <- mx[seq(m), "Age"]
  mnames <- names(mx)[-c(1:2, NCOL(mx))]
  n.mort <- length(mnames)
  obj$rate <- obj$pop <- list()
  for (i in seq(n.mort)) {
    obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
    obj$rate[[i]][obj$rate[[i]] < 0] <- NA
    obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
    obj$pop[[i]][obj$pop[[i]] < 0] <- NA
    dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
  }
  names(obj$pop) <- names(obj$rate) <- tolower(mnames)
  
  return(structure(obj, class = "demogdata"))
}

### Dump:

# Central projections for age 65 for 30 years ahead
# LC_proj = LCfor$rates["65",]

# Wang Transform
# LC_wang_sse = function(lambda) {
#  sum( sum(pnorm(qnorm(1- LC_proj) - lambda) * 0.017) - standard_annuity )^2
# }

#LC_wang_sse = function(lambda) {
#  sum( 5563* sum(pnorm(qnorm(1- qxt_LC["65", ] ) - lambda) * discount_rate) - standard_annuity )^2
#}