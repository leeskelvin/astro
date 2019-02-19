moffat1d = function(r, I0 = 1, beta = 1, fwhm = 1, rd = fwhm / (2*sqrt((2^(1/beta))-1))){
    return(colSums(rbind(sapply(X=r, FUN=function(r, I0, beta, rd){ return( I0 / ( ( 1 + (r / rd)^2 )^beta ) ) }, I0=I0, beta=beta, rd=rd))))
}

