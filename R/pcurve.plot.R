library(ggplot2)

pcurve.plot <- function(p, binwidth = 0.01, jitter = TRUE) {

	p <- p.check(p)

	max = lower = upper = 0
	while(upper<0.05){
		upper = upper + binwidth
		size = length(which(p>lower & p<upper)) 
		if(size>max){ max = size }
		lower = lower + binwidth
	}

	offset = max * 0.02 * -1
	j = offset * -1 * 0.5

	p <- data.frame(p=p, y=offset)

	pl <- ggplot(p, aes(x=p))

	if(jitter==TRUE){
		pl <- 	pl + 
			geom_histogram(binwidth=binwidth, colour="dark grey") + 
			geom_jitter(aes(y=y), alpha = 0.25, position = position_jitter(height = j)) +
			xlim(c(0.0, 0.05))
	} else{
		pl <- 	pl + 
			geom_histogram(binwidth=binwidth, colour="dark grey") + 
			xlim(c(0.0, 0.05))
	}

	return(pl)

}