# the binomial test of p-values <0.05 described by
# this paper: http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237

# This function computes two one-tailed sign tests asking whether there's
# a tendency for the p values to be >0.025 or <0.025

binomial_p_test <- function(p) {

	p <- p_check(p)

	higher <- sum(p>0.025)
	lower <- sum(p<0.025)

	r.skew <- binom.test(c(higher,lower), alternative = "less")
	l.skew <- binom.test(c(higher,lower), alternative = "greater")
	
	return(list("Number of p values >0.025"=higher,
				"Number of p values <0.025"=lower,
				"p value for right skew"=r.skew$p.value, 
				"p value for left skew"=l.skew$p.value))
}