approx2d = function(mat, xyout, nx = 5, ny = 5){
    
    # Adapted from package fields (Douglas Nychka, Reinhard Furrer, Stephan Sain)
    
    if(missing(xyout)){
        xyout = expand.grid(seq(1,nrow(mat),len=nx), seq(1,ncol(mat),len=ny))
    }
    
    xmid = approx(1:nrow(mat), 1:nrow(mat), xyout[,1])$y
    ymid = approx(1:ncol(mat), 1:ncol(mat), xyout[,2])$y
    
    xlo = floor(xmid)
    ylo = floor(ymid)
    
    xdiff = xmid - xlo
    ydiff = ymid - ylo
    xdiff[xlo == nrow(mat)] = 1
    ydiff[ylo == ncol(mat)] = 1
    
    xlo[xlo == nrow(mat)] = nrow(mat) - 1
    ylo[ylo == ncol(mat)] = ncol(mat) - 1
    
    out = mat[cbind(xlo,ylo)] * (1-xdiff) * (1-ydiff) + mat[cbind(xlo+1,ylo)] * xdiff * (1-ydiff) + mat[cbind(xlo,ylo+1)] * (1-xdiff) * ydiff + mat[cbind(xlo+1, ylo+1)] * xdiff * ydiff
    return(out)
    
}

