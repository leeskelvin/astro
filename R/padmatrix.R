padmatrix = function(mat, pad, value = 0){
    
    # mat = matrix(1:25,5); pad = -1; value = 0
    
    # setup
    pad = ceiling(rep(pad,4)[1:4])
    new = matrix(value, nrow=nrow(mat)+pad[2]+pad[4], ncol=ncol(mat)+pad[1]+pad[3])
    
    # translation indices
    xold = 1:nrow(mat)
    xnew = (1:nrow(mat)) + pad[2]
    xbad = which(!xnew %in% 1:nrow(new))
    if(length(xbad) > 0){
        xold = xold[-xbad]
        xnew = xnew[-xbad]
    }
    yold = 1:ncol(mat)
    ynew = (1:ncol(mat)) + pad[1]
    ybad = which(!ynew %in% 1:ncol(new))
    if(length(ybad) > 0){
        yold = yold[-ybad]
        ynew = ynew[-ybad]
    }
    
    # map onto new matrix
    new[xnew,ynew] = mat[xold,yold]
    
    # return
    return(new)
    
}

