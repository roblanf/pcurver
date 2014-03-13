library(MADAM)

#' Perform a test for right skew of p values between 0.00 and 0.05.
#'
#' This function uses Fisher's test (implemented in fisher.method
#' () function of the MADAM package) to ask whether the p values in the 
#' range 0.00 to 0.05 are more right skewed than you would expect (i.e. 
#' bunched up towards zero), compared to a uniform distribution.  
#' This function replicates an analysis described in Simonsohn et al 
#' (2013, see references). If you use it, please cite that paper 
#' and the MADAM package (see references). 

#' @references Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph 
#' P., P-Curve: A Key to the File Drawer (April 24, 2013). Journal of 
#' Experimental Psychology: General, Forthcoming. 
#' Available at SSRN: http://ssrn.com/abstract=2256237
#' @references Kugler KG, Mueller LA, Graber A: MADAM - An Open Source 
#' Toolbox for Meta-Analysis. Source Code for Biology and Medicine 2010, 5:3

#' @param p a vector of p values between 0.0 and 0.05 (inclusive)

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{S }{The test statistic from Fisher's test}
#'   \item{p }{The p value from Fisher's test}
#'   \item{num.p }{The number of p values used to compute the test statistic}
#'   \item{df }{The number of degrees of freedom}

#' @keywords Fisher's method, p curve
#' @export
#' @seealso \code{\link{fisher.lskew.test}}, \code{\link{fisher.bias.test}}

#' @examples
#' 
#' p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
#' fisher.rskew.test(p)

fisher.rskew.test <- function(p) {
	pp <- p.to.pp(p)
	r <- fisher.method(matrix(pp, nrow=1))
    return(process.madam(r))
}


#' Perform a test for left skew of p values between 0.00 and 0.05.
#'
#' This function uses Fisher's test (implemented in fisher.method
#' () function of the MADAM package) to ask whether the p values in the 
#' range 0.00 to 0.05 are more left skewed than you would expect (i.e. 
#' bunched up towards 0.05), compared to a uniform distribution.
#' This function replicates an analysis described in Simonsohn et al 
#' (2013, see references). If you use it, please cite that paper 
#' and the MADAM package (see references). 

#' @references Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph 
#' P., P-Curve: A Key to the File Drawer (April 24, 2013). Journal of 
#' Experimental Psychology: General, Forthcoming. 
#' Available at SSRN: http://ssrn.com/abstract=2256237
#' @references Kugler KG, Mueller LA, Graber A: MADAM - An Open Source 
#' Toolbox for Meta-Analysis. Source Code for Biology and Medicine 2010, 5:3

#' @param p a vector of p values between 0.0 and 0.05 (inclusive)

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{S }{The test statistic from Fisher's test}
#'   \item{p }{The p value from Fisher's test}
#'   \item{num.p }{The number of p values used to compute the test statistic}
#'   \item{df }{The number of degrees of freedom}

#' @keywords Fisher's method, p curve
#' @export
#' @seealso \code{\link{fisher.rskew.test}}, \code{\link{fisher.bias.test}}

#' @examples
#' 
#' p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
#' fisher.lskew.test(p)

fisher.lskew.test <- function(p) {
    p.reversed <- 0.05 - p 
    pp <- p.to.pp(p.reversed)
    r <- fisher.method(matrix(pp, nrow=1))
    return(process.madam(r))
}


#' Perform a test for left skew of p values close to 0.05.
#'
#' This function uses Fisher's test (implemented in fisher.method
#' () function of the MADAM package) to ask whether the p values in the 
#' range lower.limit to 0.05 are more left skewed than you would expect (i.e. 
#' bunched up towards 0.05), compared to a uniform distribution.
#' In general, we'll see a larger effect of p-hacking and publication bias
#' closer to 0.05, so the closer lower.limit is to 0.05, the bigger the
#' effect size. But we lose power as we throw out data by raising lower.limit.
#' Thus, the default lower.limit is 0.03, but if you have a lot of data
#' you might consider increasing it. This test is inspired by those in
#' in Simonsohn et al (2013).

#' @references Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph 
#' P., P-Curve: A Key to the File Drawer (April 24, 2013). Journal of 
#' Experimental Psychology: General, Forthcoming. 
#' Available at SSRN: http://ssrn.com/abstract=2256237
#' @references Kugler KG, Mueller LA, Graber A: MADAM - An Open Source 
#' Toolbox for Meta-Analysis. Source Code for Biology and Medicine 2010, 5:3

#' @param p a vector of p values between 0.0 and 0.05 (inclusive)
#' @param lower.limit a single number >=0.0 and <0.05

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{S }{The test statistic from Fisher's test}
#'   \item{p }{The p value from Fisher's test}
#'   \item{num.p }{The number of p values used to compute the test statistic}
#'   \item{df }{The number of degrees of freedom}

#' @keywords Fisher's method, p curve
#' @export
#' @seealso \code{\link{fisher.rskew.test}}, \code{\link{fisher.lskew.test}}

#' @examples
#' 
#' # here are some p values you might get from a strong effect size
#' p <- rexp(1000, 200)
#' p <- p[p<0.05]
#' 
#' # let's add some you might get from p-hacking and/or publication bias
#' h <- -1 * rexp(100, 200) + 0.05
#' h <- h[h>0.00]
#' p <- c(p, h)
#' 
#' # the fisher.rskew.test and fisher.lskew.test should show 
#' # significant right skew. That's expected - they uses all the data
#' # from 0.00 to 0.05
#' fisher.rskew.test(p)
#' fisher.lskew.test(p)
#' 
#' # the fisher.bias.test is more sensitive to p-hacking and/or
#' # publication bias - it uses just the data from 0.03 to 0.05
#' fisher.bias.test(p)
#' 
#' # Since we have a lot of data, we could increase the lower limit
#' # from the default (0.03) to 0.04
#' fisher.bias.test(p, lower.limit=0.04)

fisher.bias.test <- function(p, lower.limit=0.03) {
    limits <- c(lower.limit, 0.05)
    limits.check(limits)
    p <- p.check(p, limits) # removes p values outside limits    
    p.reversed <- 0.05 - p
    pp <- p.reversed * (1/(limits[2] - limits[1])) # biggest p value gets pp 1
    r <- fisher.method(matrix(pp, nrow=1))
    return(process.madam(r))
}

process.madam <- function(r){
    d <- r[1]
    d$p <- r$p.value
    d$num.p <- r$num.p
    d$df <- d$num.p * 2
    return(d)
}
