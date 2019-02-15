moffat1d = function(x, mean = 0, fwhm = 1, sd = fwhm / (2*(sqrt((2^(1/beta))-1))), scale = 1){
    scale = rep(scale, length(sd))[1:length(sd)]
    out = sapply(X=x, FUN=dnorm, mean=mean, sd=sd) * scale
    return(colSums(rbind(out)))
}


