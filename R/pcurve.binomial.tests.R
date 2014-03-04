
# A collection of binomial functions to check p curves

binomial.all.test <- function(p) {
	# a two tailed binomial test on the whole dataset

	p <- p.check(p)
	midpoint <- 0.025

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r <- binom.test(c(higher,lower))
	
	return(list("Number of p values >0.025"=higher,
				"Number of p values <0.025"=lower,
				"p value"=r$p.value))
}


binomial.bias.test <- function(p) {
	# a one-tailed binomial test to specifically
	# look for evidence of p-hacking or publication bias
	# in the 0.03-0.05 bins

	limits = c(0.03, 0.05)
	midpoint <- 0.04
	limits.check(limits)
	p <- p.check(p, limits)

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r <- binom.test(c(higher,lower), alternative = "greater")
	
	return(list("Number of p values 0.03<p<0.04"=lower,
				"Number of p values 0.04<p<0.05"=higher,
				"One-tailed p value for evidence of publication bias and/or p-hacking"=r$p.value))
}


binomial.sns.test <- function(p) {
	# the test outlined in this paper: http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237
	# it's two one-sided binomial tests, which seems a bit odd
	# I think it's better to use a single two-sided test

	p <- p.check(p)
	midpoint <- 0.025

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	r.skew <- binom.test(c(higher,lower), alternative = "less")
	l.skew <- binom.test(c(higher,lower), alternative = "greater")
	
	return(list("Number of p values >0.025"=higher,
				"Number of p values <0.025"=lower,
				"p value for right skew"=r.skew$p.value, 
				"p value for left skew"=l.skew$p.value))
}