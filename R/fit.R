fit = function(y, func, vars, pars = {}, ..., sigma = 1){
    
    # chi-squared function to be minimised
    minfunc = function(argvals, argnames, extras, func, exp, sig){
        
        # chi-squared statistic
        chisq = function(obs, exp, sig){
            return(sum(((obs - exp) / sig)^2))
        }
        
        # reconstitute args as list and run
        args = split(argvals, argnames)
        obs = do.call(what=func, args=c(args,extras))
        sig = rep(sig,length(obs))[1:length(obs)]
        
        # finish up
        return(chisq(obs=obs, exp=exp, sig=sig))
        
    }
    
    # split vars into numeric vector and name vector (necessary due to optim requirement)
    argvals = as.numeric(unlist(vars))
    argnames = rep(names(vars), times=as.numeric(sapply(vars, length)))
    
    # run optim, reconstitute output parameters as named list
    out = optim(par=argvals, fn=minfunc, argnames=argnames, extras=pars, func=func, exp=y, sig=sigma, ...)
    out$par = split(as.numeric(out$par), argnames)
    return(out)
    
}

