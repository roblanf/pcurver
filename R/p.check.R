# Function to check a list of p values


p.check <- function(p){
	
	error <- "Your vector of P values must only contain numbers  >=0.0 and <=0.05. Please check and try again"
	p <- as.vector(p) # in case of a single number(!)

	if(is.numeric(p)==FALSE) stop(error)
	if(min(p)<0.00) stop(error)
	if(max(p)>0.05) stop(error)

	return(p)
	
}