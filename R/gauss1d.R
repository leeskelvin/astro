gauss1d = function(r, I0 = 1, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2))))){
    return(colSums(rbind(sapply(X=r, FUN=function(r, I0, sd){ return( I0 * exp( (-r^2) / (2*sd^2) ) ) }, I0=I0, sd=sd))))
}

