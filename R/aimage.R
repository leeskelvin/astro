aimage = function(input, hdu = 1, xcen = NA, ycen = NA, xdim = NA, ydim = NA, scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, scale.probs = NA, col.map = "grey", col.alpha=1, col.invert = FALSE, asp = 1, ...){
    
    # input=c("calexp-HSC-G-8283-38.stamp.fits","calexp-HSC-R-8283-38.stamp.fits","calexp-HSC-I-8283-38.stamp.fits"); hdu = 1; xcen = NA; ycen = NA; xdim = NA; ydim = NA; scale.type = "log"; scale.mode = 99.5; scale.lo = NA; scale.hi = NA; scale.pow = 0.5; scale.probs = NA; col.map = "grey"; col.alpha=1; col.invert = FALSE; asp = 1; i = 1
    
    # force convert to list
    if(typeof(input)=="double" | typeof(input)=="integer"){
        input = list(input)
    }
    if(typeof(input)=="character"){
        input = as.list(input)
    }
    
    # read FITS files and assign row/column names
    for(i in 1:length(input)){
        if(typeof(input[[i]]) == "character"){
            input[[i]] = read.fits(input[[i]], hdu=rep(hdu,length(input))[i])$dat[[1]]
        }
        rownames(input[[i]]) = 1:nrow(input[[i]])
        colnames(input[[i]]) = 1:ncol(input[[i]])
    }
    
    # trim image dimensions (if required)
    input.trim = input
    if(any(!is.na(c(xcen,ycen,xdim,ydim)))){
        
        for(i in 1:length(input)){
            
            # setup
            ixcen=c(rep(xcen,length(input))[i],(dim(input[[i]])[1]+1)/2,NA);ixcen=ixcen[!is.na(ixcen)][1]
            iycen=c(rep(ycen,length(input))[i],(dim(input[[i]])[2]+1)/2,NA);iycen=iycen[!is.na(iycen)][1]
            ixdim=c(rep(xdim,length(input))[i],dim(input[[i]])[1],NA);ixdim=ceiling(ixdim[!is.na(ixdim)][1])
            iydim=c(rep(ydim,length(input))[i],dim(input[[i]])[2],NA);iydim=ceiling(iydim[!is.na(iydim)][1])
            igrid = matrix(NA, nrow=ixdim, ncol=iydim)
            
            # translate data to igrid and up to input.trim
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
            input.trim[[i]] = igrid
            
        }
        
    }
    
    # apply scale transformations
    ref = input.avg = NA
    input.scaled = as.list(rep(NA,length(input)))
    for(i in 1:length(input)){
        
        # rescale to linear range {0,1} (soft limits)
        slim = quantile(lgrid[[i]],probs=c((50-(scale.mode/2))/100,(50+(scale.mode/2))/100),na.rm=TRUE)
        ilo = c(rep(scale.lo,length(input))[i],slim[1],NA); ilo = ilo[!is.na(ilo)][1]
        ihi = c(rep(scale.hi,length(input))[i],slim[2],NA); ihi = ihi[!is.na(ihi)][1]
        igrid = (lgrid[[i]] - ilo) / (ihi - ilo)
        
        # setup calibration points
        calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
        imdat = igrid * calib
        lo = 0 * calib
        hi = 1 * calib
        
        # scaling functions
        if(scale.type == "lin"){
            fdat = imdat
            flo = lo
            fhi = hi
            ref = quantile(c(ilo,ihi), probs=(quantile(c(flo,fhi),probs=scale.probs)/calib))
        }else if(scale.type == "log"){
            slide = 0.5
            fdat = suppressWarnings(log10(imdat + slide))
            flo = log10(lo + slide)
            fhi = log10(hi + slide)
            ref = quantile(c(ilo,ihi), probs=((10^(quantile(c(flo,fhi),probs=scale.probs))-slide)/calib))
        }else if(scale.type == "pow"){
            fdat = suppressWarnings(imdat^scale.pow)
         d   flo = lo^scale.pow
            fhi = hi^scale.pow
            ref = quantile(c(ilo,ihi), probs=((quantile(c(flo,fhi),probs=scale.probs)^(1/scale.pow))/calib))
        }else if(scale.type == "atan"){
            fdat = suppressWarnings(atan(imdat))
            flo = atan(lo)
            fhi = atan(hi)
            ref = quantile(c(ilo,ihi), probs=((tan(quantile(c(flo,fhi),probs=scale.probs)))/calib))
        }else if(scale.type == "asinh"){
            fdat = suppressWarnings(asinh(imdat))
            flo = asinh(lo)
            fhi = asinh(hi)
            ref = quantile(c(ilo,ihi), probs=((sinh(quantile(c(flo,fhi),probs=scale.probs)))/calib))
        }else if(scale.type == "sinh"){
            fdat = suppressWarnings(sinh(imdat))
            flo = sinh(lo)
            fhi = sinh(hi)
            ref = quantile(c(ilo,ihi), probs=((asinh(quantile(c(flo,fhi),probs=scale.probs)))/calib))
        }else{
            stop("unknown aimage scale.type function applied")
        }
        
        # rescale to colour-appropriate range (0,255) (hard limits)
        input.scaled[[i]] = 255 * ((fdat - flo) / (fhi - flo))
        if(any(input.scaled[[i]] < 0)){input.scaled[[i]][input.scaled[[i]] < 0] = 0}
        if(any(input.scaled[[i]] > 255)){input.scaled[[i]][input.scaled[[i]] > 255] = 255}
        if(col.invert){input.scaled[[i]] = 255 - input.scaled[[i]]}
        if(i == 1){input.avg = input.scaled[[i]]}else{input.avg = input.avg + input.scaled[[i]]}
        
    }
    input.avg = input.avg / length(input.scaled)
    
    # generate colours
    if(col.map == "grey" | col.map == "gray"){
        red = green = blue = as.vector(input.avg)
        hsvmat = rgb2hsv(
            r=round(red), 
            g=round(green), 
            b=round(blue)
        )
    }else if(col.map == "sls"){
        hsvmat = rgb2hsv(col2rgb(sls(256)[input.avg + 1]))
    }else if(col.map == "rainbow"){
        hsvmat = rgb2hsv(col2rgb(rainbow(256)[input.avg + 1]))
    }else if(col.map == "heat"){
        hsvmat = rgb2hsv(col2rgb(heat.colors(256)[input.avg + 1]))
    }else if(col.map == "terrain"){
        hsvmat = rgb2hsv(col2rgb(terrain.colors(256)[input.avg + 1]))
    }else if(col.map == "topo"){
        hsvmat = rgb2hsv(col2rgb(topo.colors(256)[input.avg + 1]))
    }else if(col.map == "cm"){
        hsvmat = rgb2hsv(col2rgb(cm.colors(256)[input.avg + 1]))
    }
    col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
    
    # image
    x = as.numeric(rownames(lgrid[[1]]))
    y = as.numeric(colnames(lgrid[[1]]))
    image(x=x, y=y, z=matrix(1:length(col),nrow=nrow(input.scaled[[1]]),ncol=ncol(input.scaled[[1]])), col=col, asp=asp, ...)
    
    # probs
    if(!is.na(ref[1])){
        return(ref)
    }
    
}

