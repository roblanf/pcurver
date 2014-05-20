bootstrap.result <- function(observed.diff, boot.diffs){

    # calculate quantiles and p value for bootstrap replicates
    # p.value asks if observed diff differs from zero
    boot.diffs = sort(boot.diffs)

    quantiles = quantile(boot.diffs, c(0.025, 0.975))    

    # probability that diff >0
    p.greater = (length(which(boot.diffs<0)))/(reps+1)
    if(p.greater == 0){ p.value = paste("<", 1/(reps+1))}
    if(p.greater == 1){ p.value = paste(">", reps/(reps+1))}

    # probability that diff >0
    p.lower = (length(which(boot.diffs>0)))/(reps+1)
    if(p.lower == 0){ p.value = paste("<", 1/(reps+1))}
    if(p.lower == 1){ p.value = paste(">", reps/(reps+1))}


    return(list(observed.diff = observed.diff, 
                confidence.intervals = quantiles, 
                p.greater = p.greater,
                p.lower = p.lower))


}


bootstrap.skew.test <- function(p1, p2, reps=999){
    # bootstrap test to ask whether observed diff is bigger than expected by chance
    # i.e. is the skewness of the two distributions the same or different

    observed.diff = skewness(p1) - skewness(p2)

    p1.reps = replicate(reps, skewness(sample(p1, replace = TRUE))) 
    p2.reps = replicate(reps, skewness(sample(p2, replace = TRUE))) 
    boot.diffs = p1.reps - p2.reps

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

    result = bootstrap.result(observed.diff, boot.diffs)
    return(result)
}

