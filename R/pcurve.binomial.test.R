# the binomial test of p-values <0.05 described by
# this paper: http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237

# This function computes two one-tailed sign tests asking whether there's
# a tendency for the p values to be >0.025 or <0.025

pcurve.binomial.test <- function(p, limits = c(0.0, 0.05)) {

	p <- p.check(p, limits)
	limits.check(limits)
	midpoint <- sum(limits)/2.0

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r.skew <- binom.test(c(higher,lower), alternative = "less")
	l.skew <- binom.test(c(higher,lower), alternative = "greater")
	
	return(list("Number of p values >midpoint"=higher,
				"Number of p values <midpoint"=lower,
				"p value for right skew"=r.skew$p.value, 
				"p value for left skew"=l.skew$p.value,
				"midpoint"=midpoint))
}