# check limits for p.binomial.test function

limits.check <- function(limits){
	if(!is.numeric(limits)) stop("Your limits for teh pcurve.binomial.test function must be numeric")
	if(length(limits)!=2) stop("Your limits for the pcurve.binomial.test function must contain 2 numbers")
	if(limits[1]<0.0) stop("Your lower limit for the pcurve.binomial.test must be >=0.0")
	if(limits[2]>0.05) stop("Your upper limit for the pcurve.binomial.test must be <=0.05")
	if(limits[1]>limits[2]) stop("Your lower limit for the pcurve.binomial.test function must be smaller than your upper limit")
	return(TRUE)
}