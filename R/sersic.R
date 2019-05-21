# all lengths in pixels
# all intensities in ADUs
# a = half-light radius along the semi-major axis [pixels]

sersic2d = function(size = 5, Ie = 1, n = 1, a = 1, e = 0, pa = 0, norm = FALSE, discrete = FALSE){
    lo = ((size+1)/2) - size
    hi = size - ((size+1)/2)
    step = 1
    if(discrete){
        lo = lo - 0.45
        hi = hi + 0.45
        step = 0.1
        Ie = Ie / (10*10)
    }
    x = y = seq(lo, hi, by=step)
    xy = expand.grid(x,y)
    Irs = sersic.Ixy(x=xy[,1], y=xy[,2], Ie=Ie, n=n, a=a, e=e, pa=pa)
    mat = matrix(Irs, length(x), length(y))
    if(discrete){
        mat = rowsum(mat, group=ceiling(1:nrow(mat)/10))
        mat = t(rowsum(t(mat), group=ceiling(1:ncol(mat)/10)))
    }
    if(norm){mat = mat / sum(mat)}
    return(mat)
}

sersic.Ixy = function(x, y, Ie = 1, n = 1, a = 1, e = 0, pa = 0){
    bn = qgamma(0.5,2*n)
    pa.rad = pa * (pi/180)
    xmaj = (x * cos(-pa.rad)) - (y * sin(-pa.rad))
    xmin = (x * sin(-pa.rad)) + (y * cos(-pa.rad))
    r = sqrt((xmaj^2) + (xmin/(1-e))^2)
    Ixy = Ie * exp(-bn * ((r/a)^(1/n) - 1))
    return(Ixy)
}

sersic.Ir = function(r, Ie = 1, n = 1, a = 1){
    bn = qgamma(0.5,2*n)
    Ir = Ie * exp(-bn * ((r/a)^(1/n) - 1))
    return(Ir)
}

sersic.r = function(Ir, Ie = 1, n = 1, a = 1){
    bn = qgamma(0.5,2*n)
    r = a * (((log(Ir/Ie))/(-bn))+1)^n
    return(r)
}

sersic.Lr = function(r, Ie = 1, n = 1, a = 1, e = 0){
    bn = qgamma(0.5,2*n)
    x = bn * ((r/a)^(1/n))
    Lr = Ie * (a*sqrt(1-e))^2 * 2 * pi * n * ((exp(bn))/(bn^(2*n))) * pgamma(x,2*n)
    return(Lr)
}

sersic.Ltot = function(Ie = 1, n = 1, a = 1, e = 0){
    bn = qgamma(0.5,2*n)
    Ltot = Ie * (a*sqrt(1-e))^2 * 2 * pi * n * ((exp(bn))/(bn^(2*n))) * gamma(2*n)
    return(Ltot)
}

sersic.Ie = function(Ltot, n = 1, a = 1, e = 0){
    bn = qgamma(0.5,2*n)
    Ie = Ltot / ((a*sqrt(1-e))^2 * 2 * pi * n * ((exp(bn))/(bn^(2*n))) * gamma(2*n))
    return(Ie)
}

sersic.Ie2I0 = function(Ie, n = 1){
    bn = qgamma(0.5,2*n)
    I0 = Ie * exp(bn)
    return(I0)
}

sersic.I02Ie = function(I0, n = 1){
    bn = qgamma(0.5,2*n)
    Ie = I0 / exp(bn)
    return(Ie)
}

