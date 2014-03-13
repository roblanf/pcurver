
#' Perform a two-tailed sign test on p values between 0.00 and 0.05.
#'
#' This function tests the null hypothesis that there are an equal
#' number of p values in two bins: 0.0<=p<0.025, and 0.025<p<=0.05.
#' Note that p values of exactly 0.025 are excluded, because they 
#' do not fall in either bin. The test uses a two-tailed sign test.
#' A significant result indicates that you can reject the null 
#' hypothesis of an equal number of p values in each bin.
#' Significantly more p values in the smaller bin is expected for collections
#' of p values that contain evidential value. Significantly More p values in 
#' the larger bin is consistent with intensive p hacking or publication
#' bias.

#' @param p a vector of p values between 0.0 and 0.05 (inclusive)

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{lower }{The number of p values <0.025}
#'   \item{higher }{The number of p values >0.025}
#'   \item{p }{The p value from the two-talied sign test}

#' @keywords binomial, p curve
#' @export
#' @seealso \code{\link{binomial.bias.test}}, \code{\link{binomial.sns.test}}
#' @examples
#' 
#' p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
#' binomial.all.test(p)

binomial.all.test <- function(p) {
	# a two tailed binomial test on the whole dataset

	p <- p.check(p)
	midpoint <- 0.025

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r <- binom.test(c(higher,lower))
	d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
	
	return(d)
}

#' Perform a one-tailed sign test on p values between 0.03 and 0.05.
#'
#' This function tests the null hypothesis that there are at least as
#' many p values in the 0.03 - 0.04 bin as in the 0.04 - 0.05 bin.
#' Note that p values of exactly 0.04 are excluded, because they 
#' do not fall in either bin. The test uses a one-tailed sign test.
#' Significantly more p values in the smaller bin is consistent with
#' collections of p values with evidential value. Significantly more p  
#' values in the larger bin is consistent with p-hacking or publication 
#' bias. This is a more sensitive test of p-hacking or publication bias
#' than the related \code{\link{binomial.all.test}}
#' @param p a vector of p values between 0.0 and 0.05 (inclusive)

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{lower }{The number of p values in the 0.03-0.04 bin}
#'   \item{higher }{The number of p values in the 0.04-0.05 bin}
#'   \item{p }{The p value from the one-talied sign test}

#' @keywords binomial, p curve
#' @export
#' @seealso \code{\link{binomial.all.test}}, \code{\link{binomial.sns.test}}
#' @examples
#' 
#' # here are some p values you might get from a strong effect size
#' p <- rexp(1000, 200)
#' p <- p[p<0.05]
#' 
#' # let's add some you might get from p-hacking and/or publication bias
#' h <- -1 * rexp(100, 200) + 0.05
#' h <- h[h>0.00]
#' 
#' p <- c(p, h)
#' 
#' # the binomial.all.test should show significant right skew
#' # that's expected - it uses all the data from 0.00 to 0.05
#' binomial.all.test(p)
#' 
#' # the binomial.bias.test is more sensitive to p-hacking and/or
#' # publication bias - it uses just the data from 0.03 to 0.05
#' binomial.bias.test(p)


binomial.bias.test <- function(p) {

	limits = c(0.03, 0.05)
	midpoint <- 0.04
	limits.check(limits)
	p <- p.check(p, limits)

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r <- binom.test(c(higher,lower), alternative = "greater")
	d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
	
	return(d)
}

#' Perform two one-tailed sign tests on p values between 0.00 and 0.05.
#'
#' This function replicates the analysis described in Simonsohn et al 
#' (2013, see references), and is named for the three authors of that  
#' paper. If you use it, please cite that paper (see references). 
#' It performs two symmetrical one-tailed sign 
#' tests. We do not recommend its use, because doing two one-tailed tests
#' will double your false-positive rate compared to doing a single two-
#' tailed test. So rather than use this test, you should use 
#' \code{\link{binomial.all.test}}.
#' @references Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph 
#' P., P-Curve: A Key to the File Drawer (April 24, 2013). Journal of 
#' Experimental Psychology: General, Forthcoming. 
#' Available at SSRN: http://ssrn.com/abstract=2256237
#' @param p a vector of p values between 0.0 and 0.05 (inclusive)

#' @return This method returns a data frame containing the following columns:
#' 
#'   \item{lower }{The number of p values <0.025}
#'   \item{higher }{The number of p values >0.025}
#'   \item{p.rskew }{The p value from the one-talied sign test for right skew}
#'   \item{p.lskew }{The p value from the one-talied sign test for left skew}


#' @keywords binomial, p curve
#' @export
#' @seealso \code{\link{binomial.bias.test}}, \code{\link{binomial.all.test}}
#' @examples
#' 
#' p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
#' binomial.sns.test(p)

binomial.sns.test <- function(p) {

	p <- p.check(p)
	midpoint <- 0.025

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r.skew <- binom.test(c(higher,lower), alternative = "less")
	l.skew <- binom.test(c(higher,lower), alternative = "greater")
	d <- data.frame("lower"=lower, "higher"=higher, 
					"p.rskew"=r.skew$p.value,
					"p.lskew"=l.skew$p.value)
	
	return(d)
}