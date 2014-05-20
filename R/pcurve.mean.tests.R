bootstrap.result <- function(observed.diff, boot.diffs){

    # calculate quantiles and p value for bootstrap replicates
    # p.value asks if observed diff differs from zero

    quantiles = quantile(boot.diffs, c(0.025, 0.975))    

    p.value = (length(which(boot.diffs<0)))/(reps+1)

    # if we're at the extremes we can't make exact statements
    if(p.value == 0){ p.value = paste("<", 1/(reps+1))}
    if(p.value == 1){ p.value = paste(">", reps/(reps+1))}

    return(list(observed.diff = observed.diff, 
                confidence.intervals = quantiles, 
                p.value = p.value))


}


bootstrap.skew.test <- function(p1, p2, reps=999){
    # bootstrap test to ask whether observed diff is bigger than expected by chance
    # i.e. is the skewness of the two distributions the same or different

    observed.diff = skewness(p1) - skewness(p2)

    p1.reps = replicate(reps, skewness(sample(p1, replace = TRUE))) 
    p2.reps = replicate(reps, skewness(sample(p2, replace = TRUE))) 
    boot.diffs = p1.reps - p2.reps
    boot.diffs = sort(boot.diffs)

    result = bootstrap.result(observed.diff, boot.diffs)
    return(result)
}


bootstrap.mean.test <- function(p1, p2, reps=999){
    # bootstrap test to ask whether observed diff is bigger than expected by chance
    # i.e. is the skewness of the two distributions the same or different

    observed.diff = mean(p1) - mean(p2)

    p1.reps = replicate(reps, mean(sample(p1, replace = TRUE))) 
    p2.reps = replicate(reps, mean(sample(p2, replace = TRUE))) 
    boot.diffs = p1.reps - p2.reps
    boot.diffs = sort(boot.diffs)

    result = bootstrap.result(observed.diff, boot.diffs)
    return(result)
}

