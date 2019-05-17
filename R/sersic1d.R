#sersic1d = function(r, Ie = 1, n = 1, re = 1){
#    return(colSums(rbind(sapply(X=r, FUN=function(r, Ie, n, re){ return( Ie * exp( -qgamma(0.5,2*n) * ( ( (r/re)^(1/n) ) - 1 ) ) ) }, Ie=Ie, n=n, re=re))))
#}

sersic1d = function(r, Ie = 1, n = 1, re = 1){
    return( Ie * exp( -qgamma(0.5,2*n) * ( ( (r/re)^(1/n) ) - 1 ) ) )
}

