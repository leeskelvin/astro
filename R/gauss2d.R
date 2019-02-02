# gauss2d, creates an arbitrary 2D Gaussian
# http://dev.theomader.com/gaussian-kernel-calculator/
gauss2d = function(size = 5, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2)))), norm = TRUE, discrete = FALSE){
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
    g = exp(-(r^2) / (2*(sd^2)))
    mat = matrix(g, length(x), length(y))
    if(discrete){
        mat = rowsum(mat, group=ceiling(1:nrow(mat)/10))
        mat = t(rowsum(t(mat), group=ceiling(1:ncol(mat)/10)))
    }
    if(norm){mat = mat / sum(mat)}
    return(mat)
}

