inverse = function(fn, interval = NULL, lower = min(interval), upper = max(interval), ...){
    Vectorize(function(y){
        uniroot(f=function(x){fn(x)-y}, lower=lower, upper=upper, ...)$root
    })
}

