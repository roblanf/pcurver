# Function to check a list of p values
# And trim them according to some limits

p.check <- function(p, limits = c(0.0, 0.05)){
	
	error <- "Your vector of P values must only contain numbers  >=0.0 and <=0.05. Please check and try again"
	p <- as.vector(p) # in case of a single number(!)
	limits.check(limits)

	if(is.numeric(p)==FALSE) stop(error)
	if(min(p)<0.00) stop(error)
	if(max(p)>0.05) stop(error)

	# trim p values outside the limits
	if(limits[1]>0.0) p <- p[p>=limits[1]]
	if(limits[2]<0.05) p <- p[p<=limits[2]]

	return(p)
	
}