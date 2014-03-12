library(ggplot2)

pcurve.plot <- function(p, binwidth = 0.01) {
	
	pl <- ggplot(as.data.frame(p), aes(x=p))
	pl <- pl + 
		  geom_histogram(binwidth=binwidth, aes(fill=log(..count..))) + 
		  xlim(c(0.0, 0.05))
	pl

		
}