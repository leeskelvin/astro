fit = function(y, func, par1, par2, ..., sigma = 1){
    
    chisq = function(obs, exp, sig){
        return(sum(((obs-exp)/sig)^2))
    }
    
    out = nlm(f=chisq, p=as.numeric(par1), obs=do.call(what=func, args=c(par2,par1)), exp=y, sig=1)
    
    return(out)
    
}

###

x = 0:5

y = gauss1d(x=x, fwhm=2)

par1 = list(fwhm=1)
par2 = list(x=x)

func = gauss1d

gauss1d = function(x, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2)))), lambda = 1){
    mixprop = rep(lambda,length(sd))[1:length(sd)] / sum(rep(lambda,length(sd))[1:length(sd)])
    out = sapply(X=x, FUN=dnorm, sd=sd) * mixprop
    return(colSums(rbind(out)))
}

