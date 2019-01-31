aimage = function(input, xcen = NA, ycen = NA, xdim = NA, ydim = NA, scale.type = "linear", scale.mode = 100, scale.lo = NA, scale.hi = NA, cmap = "sls", ...){
    
    # force convert to list
    if(typeof(input)=="double" | typeof(input)=="integer"){
        input = list(input)
    }
    
    # trim image dimensions
    lgrid = as.list(rep(NA,length(input)))
    for(i in 1:length(input)){
        
        # setup
        ixcen=c(rep(xcen,length(input))[i],(dim(input[[i]])[1]+1)/2,NA);ixcen=ixcen[!is.na(ixcen)][1]
        iycen=c(rep(ycen,length(input))[i],(dim(input[[i]])[2]+1)/2,NA);iycen=iycen[!is.na(iycen)][1]
        ixdim=c(rep(xdim,length(input))[i],dim(input[[i]])[1],NA);ixdim=ceiling(ixdim[!is.na(ixdim)][1])
        iydim=c(rep(ydim,length(input))[i],dim(input[[i]])[2],NA);iydim=ceiling(iydim[!is.na(iydim)][1])
        igrid = matrix(NA, nrow=ixdim, ncol=iydim)
        
        # translate data to igrid
        xold = ceiling((ixcen - ((ixdim-1)/2)) : (ixcen + ((ixdim-1)/2)))
        xnew = (((dim(igrid)[1]+1)/2) - ((ixdim-1)/2)) : (((dim(igrid)[1]+1)/2) + ((ixdim-1)/2))
        xoldbad = which(!xold %in% 1:dim(input[[i]])[1])
        if(length(xoldbad) > 0){
            xold = xold[-xoldbad]
            xnew = xnew[-xoldbad]
        }
        yold = ceiling((iycen - ((iydim-1)/2)) : (iycen + ((iydim-1)/2)))
        ynew = (((dim(igrid)[2]+1)/2) - ((iydim-1)/2)) : (((dim(igrid)[2]+1)/2) + ((iydim-1)/2))
        yoldbad = which(!yold %in% 1:dim(input[[i]])[2])
        if(length(yoldbad) > 0){
            yold = yold[-yoldbad]
            ynew = ynew[-yoldbad]
        }
        igrid[xnew,ynew] = input[[i]][xold,yold]
        rownames(igrid) = xold
        colnames(igrid) = yold
        lgrid[[i]] = igrid
        
    }
    
    # apply transformations
    mgrid = as.list(rep(NA,length(input)))
    for(i in 1:length(input)){
        
        # rescale to linear range {0,1} (soft limits)
        slim = quantile(lgrid[[i]],probs=c((50-(scale.mode/2))/100,(50+(scale.mode/2))/100),na.rm=TRUE)
        ilo = c(rep(scale.lo, length(input))[i],slim[1],NA); ilo = ilo[!is.na(ilo)][1]
        ihi = c(rep(scale.hi, length(input))[i],slim[2],NA); ihi = ihi[!is.na(ihi)][1]
        igrid = (lgrid[[i]] - ilo) / (ihi - ilo)
        
        # apply scaling function
        if(scale.type == "lin" | scale.type == "linear"){
            fgrid = igrid
            mlo = 0
            mhi = 1
            ref = quantile(c(mlo,mhi), probs=seq(0.1,0.9,by=0.1)) / 1
        }else if(scale.type == "log"){
            fgrid = suppressWarnings(log10((igrid * 500) + 0.5))
            mlo = log10((0 * 500) + 0.5)
            mhi = log10((1 * 500) + 0.5)
            ref = (10^(quantile(c(mlo,mhi), probs=seq(0.1,0.9,by=0.1))) - 0.5) / 500
        }else{
            
        }
        
        print(ref)
        
        # rescale to modified range (0,1) (hard limits)
        mgrid[[i]] = (fgrid - mlo) / (mhi - mlo)
        if(any(mgrid[[i]] < 0)){mgrid[[i]][mgrid[[i]] < 0] = 0}
        if(any(mgrid[[i]] > 1)){mgrid[[i]][mgrid[[i]] > 1] = 1}
        
        
#        calib = switch(func, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3) / soft
#        
#        # scaling functions
#        if(func == "lin"){
#            fdat = imdat
#            flo = lo
#            fhi = hi
#            ref = quantile(c(flo,fhi), probs=probs) / calib # reference probabilities
#        }else if(func == "log"){
#            slide = 0.5
#            fdat = log10(imdat + slide)
#            flo = log10(lo + slide)
#            fhi = log10(hi + slide)
#            ref = (10^(quantile(c(flo,fhi), probs=probs)) - slide) / calib
#        }else if(func == "pow"){
#            fdat = imdat^pow
#            flo = lo^pow
#            fhi = hi^pow
#            ref = (quantile(c(flo,fhi), probs=probs)^(1/pow)) / calib
#        }else if(func == "atan"){
#            fdat = atan(imdat)
#            flo = atan(lo)
#            fhi = atan(hi)
#            ref = (tan(quantile(c(flo,fhi), probs=probs))) / calib
#        }else if(func == "asinh"){
#            fdat = asinh(imdat)
#            flo = asinh(lo)
#            fhi = asinh(hi)
#            ref = (sinh(quantile(c(flo,fhi), probs=probs))) / calib
#        }else if(func == "sinh"){
#            fdat = sinh(imdat)
#            flo = sinh(lo)
#            fhi = sinh(hi)
#            ref = (asinh(quantile(c(flo,fhi), probs=probs))) / calib
#        }else{
#            stop("unknown aimage function applied")
#        }
#        
#        
        
    }
    
    # image
    image(x=as.numeric(rownames(igrid)), y=as.numeric(colnames(igrid)), z=igrid, ...)
    
}

