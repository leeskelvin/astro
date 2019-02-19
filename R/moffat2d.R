# moffat2d
moffat2d = function(size = 5, beta = 1, fwhm = 1, rd = fwhm / (2*sqrt((2^(1/beta))-1)), norm = TRUE, discrete = FALSE){
    lo = ((size+1)/2) - size
    hi = size - ((size+1)/2)
    step = 1
    if(discrete){
        lo = lo - 0.45
        hi = hi + 0.45
        step = 0.1
    }
    x = y = seq(lo, hi, by=step)
    xy = expand.grid(x,y)
    r = sqrt((xy[,1]^2) + (xy[,2]^2))
    m = 1 / ((1 + ((r/rd)^2))^beta)
    mat = matrix(m, length(x), length(y))
    if(discrete){
        mat = rowsum(mat, group=ceiling(1:nrow(mat)/10))
        mat = t(rowsum(t(mat), group=ceiling(1:ncol(mat)/10)))
    }
    if(norm){mat = mat / sum(mat)}
    return(mat)
}

