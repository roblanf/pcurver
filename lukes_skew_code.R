
luke.skew.test <- function(p.values, reps = 100000)
{
  p.values <- p.values[which(p.values > 0.03 & p.values < 0.05)]
  n <- length(p.values)

  observed.skew <- (mean(p.values) - median(p.values)) / sd(p.values)
 
 # Now find distribution of the expected skew, giving sampling error with n samples
  rand <- matrix(runif(reps*n, min = 0.03, max = 0.05), ncol = n, nrow = reps) # fully vectorised!
  means <- rowMeans(rand)
  medians <- apply(rand,1,median)
  sds <- apply(rand, 1, sd)
  boot.skew <- (means - medians) / sds
  quantiles <- quantile(boot.skew, probs = c(0.025, 0.975))
 
 # One-tailed p value
  p.null <- length(boot.skew[boot.skew < observed.skew]) / reps

  print(paste("Expected skew is 0.5, with 95% CIs of ", quantiles[1], " to ", quantiles[2], ", given that we have ", n, "p values in the range 0.03-0.05.", sep=""))
  print("Observed skew is ", observed.skew, ", which has a probability of ", p.null, "under the null hypothesis of no p-hacking, with an effect size of zero." sep="")
  
  # To do: add a histogram of the boot values with a little arrow on it showing the observed skew level, like this: http://stackoverflow.com/questions/11122002/how-do-i-draw-an-arrow-on-a-histogram-drawn-using-ggplot2
}



We can use a similar approach to compare the skew of two distributions. Basically resample the shit out of the two sets of p value, find the difference in skew each time, and then see if the 95% CIs of the distribution of bootstrapped differences includes zero:

luke.compare.skew <- function(p1, p2, reps = 100000)
{
  p1 <- p1[which(p1 > 0.03 & p1 < 0.05)]
  p2 <- p2[which(p2 > 0.03 & p2 < 0.05)]
  
  n1 <- length(p1)
  n2 <- length(p2)
  
  rand1 <- sample(n1,n1*reps,replace=T)
  rand2 <- sample(n2,n2*reps,replace=T)
  
  rand1 <- matrix(n1[rand1], ncol = n1, nrow = reps) # resample the shit out of p1 and p2
  rand2 <- matrix(n2[rand2], ncol = n2, nrow = reps)
  
  means1 <- rowMeans(rand1)                # Find the skew of each boot for both p1 and p2
  medians1 <- apply(rand1,1,median)
  sds1 <- apply(rand1, 1, sd)
  boot.skew1 <- (means1 - medians1) / sds1
  
  means2 <- rowMeans(rand2)
  medians2 <- apply(rand2,1,median)
  sds2 <- apply(rand2, 1, sd)
  boot.skew2 <- (means2 - medians2) / sds2
  
  difference.in.skew <- boot.skew1 - boot.skew2           # Find the difference. It should be zero on average if they have the same skew
  quantiles <- quantile(difference.in.skew, probs = c(0.025, 0.975))   # 95% CIs on the difference. Should not overlap zero if there is a difference
  
  print(paste("The bootstrapped difference in skew between p curves 1 and 2 is ", difference.in.skew, ", with 95% CIs of ", quantiles[1], "-", quantiles[2], ".", sep=""))
}
