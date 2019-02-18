fit = function(data, par, fn, arg = {}, ..., sigma = 1){
    
    # chi-squared function to be minimised
    minfunc = function(parvals, parnames, arg, func, exp, sig){
        
        # chi-squared statistic
        chisq = function(obs, exp, sig){
            return(sum(((obs - exp) / sig)^2, na.rm=TRUE))
        }
        
        # reconstitute par as list, generate obs data, and setup sigmas
        par = split(parvals, parnames)
        obs = do.call(what=func, args=c(par,arg))
        sig = rep(sig,length(obs))[1:length(obs)]
        
        # finish up
        return(chisq(obs=obs, exp=exp, sig=sig))
        
    }
    
    # split vars into numeric vector and name vector (faffing necessary due to optim requirement)
    parvals = as.numeric(unlist(par))
    parnames = rep(names(par), times=as.numeric(sapply(par, length)))
    
    # run optim, reconstitute output parameters
    out = suppressWarnings(optim(par=parvals, fn=minfunc, parnames=parnames, arg=arg, func=fn, exp=data, sig=sigma, ...))
    out$par = split(as.numeric(out$par), parnames)
    out$par = out$par[match(names(out$par), names(par))] # order into same order as input
    
    # output formatting
    names(out)[which(names(out)=="value")] = "chi2"
    df = length(data) - length(parvals)
    chi2nu = as.numeric(out$chi2) / df
    p = pchisq(q=as.numeric(out$chi2),df=df)
    out = c(out, df=df, chi2nu=chi2nu, p=p)
    out = out[c("par","chi2","df","chi2nu","p","counts","convergence","message")]
    return(out)
    
}

