# Internal values for use in package
# 
# This file contains a number of internal (global) values that
# are used throughout the functions in this package. It allows
# for easy redefinition of eg what is considered a special 
# function, which classes are accepted as valid for a pim-related
# object and so forth.
# 
.valids.pim <- c("character","factor","numeric","integer",
                 "logical", "ordered")

.specials.pim.lhs <- c("P","PO")
.specials.pim.rhs <- c("L","R")

.glmfamilies <- list(identity=gaussian(), 
                     logit=binomial(link = "logit"), 
                     probit=binomial(link = "probit"), 
                     inverse=Gamma(), 
                     `1/mu^2`=inverse.gaussian(), 
                     log=poisson())