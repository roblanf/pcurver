# Function to check a list of p_values


p_check <- function(p_vector){
	
	error <- "Your vector of P values must only contain numbers  >=0.0 and <=0.05. Please check and try again"
	p_vector <- as.vector(p_vector) # in case of a single number(!)

	if(is.numeric(p_vector)==FALSE) stop(error)
	if(min(p_vector)<0.00) stop(error)
	if(max(p_vector)>0.05) stop(error)

	return(p_vector)
	
}