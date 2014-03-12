library(MADAM)

#' Perform a test for right skew p values between 0.00 and 0.05.
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
#' @param ... additional expressions passed to fisher.method() function
#'   from the MADAM package. 

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


#' Perform a test for left skew p values between 0.00 and 0.05.
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
#' @param ... additional expressions passed to fisher.method() function
#'   from the MADAM package. 

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

# NEEDS TESTING
# Basic idea
fisher.bias.test <- function(p, limits=c(0.03,0.05)) {
    limits.check(limits)
    p <- p.check(p, limits) # removes p values outside limits    
    p.reversed <- 0.05 - p
    pp <- p.reversed * (1/(limits[2] - limits[1])) # biggest p value gets pp 1
    r <- fisher.method(matrix(pp, nrow=1))
    return(process.madam(r))
}

process.madam <- function(r){
    d <- r[1:3]
    d$df <- d$num.p * 2
    return(d)
}
