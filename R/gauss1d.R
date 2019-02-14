gauss1d = function(x, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2)))), lambda = 1){
    mixprop = rep(lambda,length(sd))[1:length(sd)] / sum(rep(lambda,length(sd))[1:length(sd)])
    out = sapply(X=x, FUN=dnorm, sd=sd) * mixprop
    return(colSums(rbind(out)))
}

