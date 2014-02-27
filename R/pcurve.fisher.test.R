# A function which takes a vector of p values
# turns them into pp values
# runs fisher's test
# returns the result

library(MADAM)

pcurve.fisher.test <- function(p) {
	p <- p_check(p)
	pp <- p_to_pp(p)
	r <- fisher.method(matrix(pp, nrow=1))
	return(r)
}