# Masterthesis
## Introduction
This files serves as a supplementary material and aims to explain the coding done with regards to the master thesis titled "". This file is private and should not be distributed accordingly
### Implementation of tests
#### DM-test

DM.test =
function(y_hat1,y_hat2,y,tau)
{
    # calculate losses (any function that satisfies condition) --> coud extend to more functions
    l1=(y_hat1-y)^2
    l2=(y_hat2-y)^2

    if(tau < 1) stop("Predictive Horizon must to be greater than 1")     
    if(length(y_hat1) != length(y_hat2)) stop("size of prediction errors differ")
    
    # calculate loss differential
    ld=l1-l2
    
    # obtain autocovariance for prediction horizon tau
    cov_ld = acf(ld, na.action=na.omit,lag.max = tau-1, type = "covariance", plot=FALSE)$acf[,,1]
    
    # calculate weighted sums of autocovariance (look at screenshot for proof)
    var_ld = sum(c(cov_ld[1], 2*cov_ld[-1])) / length(ld)
    
    # calculate the statistic
    stat = mean(ld,na.rm=TRUE)/sqrt(var_ld)
    names(stat) = "DM"
    
    # two sided test 
    pval = 2 * pt(-abs(stat), df = length(ld)-1)
    cval = 2 * pt(-abs(stat), df = length(ld)-1)
    
    input = c(tau)
    names(input) = c("Forecast horizon")
    
    structure(list(statistic = stat, parameter = input, alternative = "two-sided test", 
                   p.value = pval, method = "Diebold-Mariano Test"))
} 



