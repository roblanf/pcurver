p.mean <- function(p){
	p <- p.check(p)

	mean.p = median(p)

	return(mean.p)
}

mean.bias.test <- function(p, lower.limit = 0.03, reps = 1000){

    limits <- c(lower.limit, 0.05)
    limits.check(limits)
    p <- p.check(p, limits) # removes p values outside limits    
	n <- length(p)

  	observed.mean <- p.mean(p)
 
	# Now find distribution of the expected skew, giving sampling error with n samples
	rand <- matrix(runif(reps*n, min = lower.limit, max = 0.05), ncol = n, nrow = reps)
	boot.mean <- apply(rand, 1, p.mean)
	upper.CI <- quantile(boot.mean, probs = c(0.95))[[1]]
 
    # One-tailed p value
  	p.value <- (1 + length(boot.mean[boot.mean > observed.mean])) / reps

  	d <- data.frame(mean = observed.mean, p.num = length(p), p = p.value, upper.CI = upper.CI)
	pl <- ggplot(as.data.frame(boot.mean), aes(x=boot.mean)) + 
		geom_histogram(colour="dark grey") + 
		geom_vline(x=observed.mean, linetype = 'dashed', colour="dark grey") + 
		geom_vline(x = upper.CI, linetype='dashed')
  	plot(pl)
  	return(d)
}


skew.compare.test <- function(p1, p2, reps = 1000){

	p1 <- p.check(p1)
	p2 <- p.check(p2)

	n1 <- length(p1)
	n2 <- length(p2)
  
  
  	rand1 <- matrix(sample(p1,n1*reps,replace=T), ncol = n1, nrow = reps)
  	rand2 <- matrix(sample(p2,n2*reps,replace=T), ncol = n2, nrow = reps)
  
  	boot.skew1 <- apply(rand1, 1, p.skew)
  	boot.skew2 <- apply(rand2, 1, p.skew)
  
  	boot.diffs <- boot.skew1 - boot.skew2           # Find the difference. It should be zero on average if they have the same skew
  	quantiles <- quantile(boot.diffs, probs = c(0.025, 0.975))   # 95% CIs on the difference. Should not overlap zero if there is a difference
  
  	return(boot.diffs)
}
