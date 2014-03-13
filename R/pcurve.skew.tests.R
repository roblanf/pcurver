p.skew <- function(p){
	p <- p.check(p)

	if(sd(p)==0) skew = 0
	else skew = (mean(p) - median(p)) / sd(p)

	return(skew)
}

skew.bias.test <- function(p, lower.limit = 0.03, reps = 1000){

    limits <- c(lower.limit, 0.05)
    limits.check(limits)
    p <- p.check(p, limits) # removes p values outside limits    
	n <- length(p)

  	observed.skew <- p.skew(p)
 
	# Now find distribution of the expected skew, giving sampling error with n samples
	rand <- matrix(runif(reps*n, min = lower.limit, max = 0.05), ncol = n, nrow = reps)
	boot.skew <- apply(rand, 1, p.skew)
	CIs <- quantile(boot.skew, probs = c(0.025, 0.975))
 
    # One-tailed p value
  	p.value <- length(boot.skew[boot.skew < observed.skew]) / reps

  	d <- data.frame(skew = observed.skew, p.num = length(p), p = p.value, lower.CI = CIs[[1]], upper.CI = CIs[[2]])

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
