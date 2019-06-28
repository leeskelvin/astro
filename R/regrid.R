regrid = function(mat, fact = 0.5){
    
    # setup
    ff = rep(fact,2)[1:2]
    if(any(ff<1)){ff[ff<1] = 1/round(1/ff[ff<1])}
    if(any(ff>1)){ff[ff>1] = round(ff[ff>1])}
    out = mat
    
    # x-expand/y-expand/x-contract/y-contract
    if(ff[1] > 1){out = out[rep(1:nrow(out),each=ff[1]),] / ff[1]}
    if(ff[2] > 1){out = out[,rep(1:ncol(out),each=ff[2])] / ff[2]}
    if(ff[1] < 1){out = rowsum(out, group=round(seq(0.5+1e-5,ceiling(nrow(out)*ff[1])+0.5-1e-5,len=nrow(mat))))}
    if(ff[2] < 1){out = t(rowsum(t(out), group=round(seq(0.5+1e-5,ceiling(ncol(out)*ff[2])+0.5-1e-5,len=ncol(mat)))))}
    
    # rownames/colnames
    if(is.null(rownames(mat))){xr = 1:nrow(mat)}else{xr = rownames(mat)}
    if(is.null(colnames(mat))){yr = 1:ncol(mat)}else{yr = colnames(mat)}
    xrstep = c(diff(xr)[1]/2, 0.5, NA); xrstep = xrstep[-which(is.na(xrstep))][1]
    yrstep = c(diff(yr)[1]/2, 0.5, NA); yrstep = yrstep[-which(is.na(yrstep))][1]
    xrnew = approx(x=c(xr[1]-xrstep,xr+xrstep), n=2*nrow(out)+1)$y[seq(2,2*nrow(out),by=2)]
    yrnew = approx(x=c(yr[1]-yrstep,yr+yrstep), n=2*ncol(out)+1)$y[seq(2,2*ncol(out),by=2)]
    rownames(out) = xrnew
    colnames(out) = yrnew
    
    # finish up
    return(out)
    
}

