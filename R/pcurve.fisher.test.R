# A function which takes a vector of p values
# turns them into pp values
# runs fisher's test
# returns the result

library(MADAM)

pcurve.fisher.test <- function(p) {
	p <- p.check(p)
	pp <- p.to.pp(p)
	r <- fisher.method(matrix(pp, nrow=1))
	return(r)
}