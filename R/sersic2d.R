sersic2d = function(size = 5, Ie = 1, n = 1, re = 1, e = 0, pa = 0, norm = FALSE, discrete = FALSE){
    lo = ((size+1)/2) - size
    hi = size - ((size+1)/2)
    step = 1
    if(discrete){
        lo = lo - 0.45
        hi = hi + 0.45
        step = 0.1
        Ie = Ie/100
    }
    x = y = seq(lo, hi, by=step)
    xy = expand.grid(x,y)
    sxy = sersic2d.xy(x=xy[,1], y=xy[,2], Ie=Ie, n=n, re=re, e=e, pa=pa)
    mat = matrix(sxy, length(x), length(y))
    if(discrete){
        mat = rowsum(mat, group=ceiling(1:nrow(mat)/10))
        mat = t(rowsum(t(mat), group=ceiling(1:ncol(mat)/10)))
    }
    if(norm){mat = mat / sum(mat)}
    return(mat)
}

sersic2d.xy = function(x, y, Ie = 1, n = 1, re = 1, e = 0, pa = 0){
    bn = qgamma(0.5,2*n)
    pa.rad = pa * (pi/180)
    xmaj = (x * cos(pa.rad)) + (y * sin(pa.rad))
    xmin = -(x * sin(pa.rad)) + (y * cos(pa.rad))
    z = sqrt( ((xmaj / re)^2) + ((xmin / (re * (1-e)))^2) )
    zz = Ie * exp(-bn * (z^(1/n) - 1))
    return(zz)
}

