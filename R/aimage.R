aimage = function(input, hdu = 1, xcen = NA, ycen = NA, xdim = NA, ydim = NA, scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, scale.probs = NA, col.map = "rgb", col.alpha=1, col.invert = FALSE, asp = 1, ...){
    
    # library(astro); input=c("calexp-HSC-I-8283-38.stamp.fits","calexp-HSC-R-8283-38.stamp.fits","calexp-HSC-G-8283-38.stamp.fits"); hdu = 1; xcen = NA; ycen = NA; xdim = NA; ydim = NA; scale.type = "log"; scale.mode = 99.5; scale.lo = NA; scale.hi = NA; scale.pow = 0.5; scale.probs = seq(0,1,by=0.5); col.map = "rgb"; col.alpha=1; col.invert = FALSE; asp = 1; i = 1
    
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
            rownames(igrid) = (1:nrow(igrid)) + xold[1] - xnew[1]
            colnames(igrid) = (1:ncol(igrid)) + yold[1] - ynew[1]
            input.trim[[i]] = igrid
            
        }
        
    }
    
    # map to linear range {0,1} (soft limits)
    input.mapped = input.trim
    ilos = ihis = {}
    for(i in 1:length(input)){
        slim = quantile(input.trim[[i]],probs=c((50-(scale.mode/2))/100,(50+(scale.mode/2))/100),na.rm=TRUE)
        ilo = c(rep(scale.lo,length(input))[i],slim[1],NA); ilo = ilo[!is.na(ilo)][1]
        ihi = c(rep(scale.hi,length(input))[i],slim[2],NA); ihi = ihi[!is.na(ihi)][1]
        igrid = (input.trim[[i]] - ilo) / (ihi - ilo)
        input.mapped[[i]] = igrid
        ilos = c(ilos, ilo)
        ihis = c(ihis, ihi)
    }
    alo = mean(ilos, na.rm=TRUE)
    ahi = mean(ihis, na.rm=TRUE)
    
    # generate average image
    input.avg = input.mapped[[1]]
    input.ngood = !is.na(input.avg)
    if(any(is.na(input.avg))){input.avg[is.na(input.avg)] = 0}
    if(length(input) > 1){
        for(i in 2:length(input)){
            tgrid = input.mapped[[i]]
            input.ngood = input.ngood + !is.na(tgrid)
            if(any(is.na(tgrid))){tgrid[is.na(tgrid)] = 0}
            input.avg = input.avg + tgrid
        }
        input.avg = input.avg / input.ngood
    }
    
    # apply scaling function to averaged image
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    imdat = input.avg * calib
    lo = 0 * calib
    hi = 1 * calib
    if(scale.type == "lin"){
        flo = lo
        fhi = hi
        input.scaled = imdat
        ref = quantile(c(alo,ahi), probs=(quantile(c(flo,fhi),probs=scale.probs)/calib))
    }else if(scale.type == "log"){
        slide = 0.5
        flo = log10(lo + slide)
        fhi = log10(hi + slide)
        input.scaled = suppressWarnings(((log10(imdat + slide) - flo) / (fhi - flo)))
        ref = quantile(c(alo,ahi), probs=((10^(quantile(c(flo,fhi),probs=scale.probs))-slide)/calib))
    }else if(scale.type == "pow"){
        flo = lo^scale.pow
        fhi = hi^scale.pow
        input.scaled = suppressWarnings(((imdat^scale.pow - flo) / (fhi - flo)))
        ref = quantile(c(alo,ahi), probs=((quantile(c(flo,fhi),probs=scale.probs)^(1/scale.pow))/calib))
    }else if(scale.type == "atan"){
        flo = atan(lo)
        fhi = atan(hi)
        input.scaled = suppressWarnings(((atan(imdat) - flo) / (fhi - flo)))
        ref = quantile(c(alo,ahi), probs=((tan(quantile(c(flo,fhi),probs=scale.probs)))/calib))
    }else if(scale.type == "asinh"){
        flo = asinh(lo)
        fhi = asinh(hi)
        input.scaled = suppressWarnings(((asinh(imdat) - flo) / (fhi - flo)))
        ref = quantile(c(alo,ahi), probs=((sinh(quantile(c(flo,fhi),probs=scale.probs)))/calib))
    }else if(scale.type == "sinh"){
        flo = sinh(lo)
        fhi = sinh(hi)
        input.scaled = suppressWarnings(((sinh(imdat) - flo) / (fhi - flo)))
        ref = quantile(c(alo,ahi), probs=((asinh(quantile(c(flo,fhi),probs=scale.probs)))/calib))
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    # rescale scaled averaged image to colour-appropriate range (0,255) (hard limits) (mono only)
    cdat = 255 * input.scaled
    if(any(cdat < 0, na.rm=TRUE)){cdat[cdat < 0] = 0}
    if(any(cdat > 255, na.rm=TRUE)){cdat[cdat > 255] = 255}
    if(any(is.na(cdat))){cdat[is.na(cdat)] = 0}
    if(col.invert){cdat = 255 - cdat}
    input.rescaled = cdat
    
    # generate colours
    if(col.map == "rgb"){
        if(length(input) == 3){
            red = (input.mapped[[1]] / input.avg) * input.scaled
            green = (input.mapped[[2]] / input.avg) * input.scaled
            blue = (input.mapped[[3]] / input.avg) * input.scaled
        }else if(length(input) == 2){
            red = (input.mapped[[1]] / input.avg) * input.scaled
            green = input.scaled
            blue = (input.mapped[[2]] / input.avg) * input.scaled
        }else{
            red = green = blue = input.scaled
        }
        red = 255 * red
        green = 255 * green
        blue = 255 * blue
        if(any(red < 0, na.rm=TRUE)){red[red < 0] = 0}
        if(any(green < 0, na.rm=TRUE)){green[green < 0] = 0}
        if(any(blue < 0, na.rm=TRUE)){blue[blue < 0] = 0}
        if(any(red > 255, na.rm=TRUE)){red[red > 255] = 255}
        if(any(green > 255, na.rm=TRUE)){green[green > 255] = 255}
        if(any(blue > 255, na.rm=TRUE)){blue[blue > 255] = 255}
        if(any(is.na(red))){red[is.na(red)] = 0}
        if(any(is.na(green))){green[is.na(green)] = 0}
        if(any(is.na(blue))){blue[is.na(blue)] = 0}
        if(col.invert){
            red = 255 - red
            green = 255 - green
            blue = 255 - blue
        }
        hsvmat = rgb2hsv(
            r=round(as.vector(red))
            ,g=round(as.vector(green))
            ,b=round(as.vector(blue))
        )
    }else if(col.map == "grey" | col.map == "gray"){
        red = green = blue = input.rescaled
        hsvmat = rgb2hsv(
            r=round(red)
            ,g=round(green)
            ,b=round(blue)
        )
    }else if(col.map == "sls"){
        hsvmat = rgb2hsv(col2rgb(sls(256)[input.rescaled + 1]))
    }else if(col.map == "rainbow"){
        hsvmat = rgb2hsv(col2rgb(rainbow(256)[input.rescaled + 1]))
    }else if(col.map == "heat"){
        hsvmat = rgb2hsv(col2rgb(heat.colors(256)[input.rescaled + 1]))
    }else if(col.map == "terrain"){
        hsvmat = rgb2hsv(col2rgb(terrain.colors(256)[input.rescaled + 1]))
    }else if(col.map == "topo"){
        hsvmat = rgb2hsv(col2rgb(topo.colors(256)[input.rescaled + 1]))
    }else if(col.map == "cm"){
        hsvmat = rgb2hsv(col2rgb(cm.colors(256)[input.rescaled + 1]))
    }
    col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
    
    # image
    x = as.numeric(rownames(input.rescaled))
    y = as.numeric(colnames(input.rescaled))
    image(x=x, y=y, z=matrix(1:length(col),nrow=nrow(input.rescaled),ncol=ncol(input.rescaled)), col=col, asp=asp, ...)
    
    # return reference quantiles?
    if(!is.na(ref[1])){return(ref)}
    
}

