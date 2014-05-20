library(ggplot2)

#' Draw a histogram of p values between 0.00 and 0.05.

#' @param p a vector of p values between 0.0 and 0.05 (inclusive)
#' @param binwidth the width of the histogram bins (default is 0.01)
#' @param jitter logical TRUE/FALSE stating whether you want a plot of
#' the raw data just underneath the histogram.

#' @return This method returns a single ggplot2 plot object

#' @keywords plot, p curve
#' @export

#' @examples
#' 
#' p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
#' pcurve.plot(p)
#' 
#' # here are some p values you might get from a strong effect size
#' p <- rexp(1000, 200)
#' p <- p[p<0.05]
#' 
#' # let's add some you might get from p-hacking and/or publication bias
#' h <- -1 * rexp(100, 200) + 0.05
#' h <- h[h>0.00]
#' p <- c(p, h)
#' 
#' pcurve.plot(p)
#' pcurve.plot(p, binwidth = 0.005)
#' pcurve.plot(p, binwidth = 0.005, jitter = FALSE)


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


















