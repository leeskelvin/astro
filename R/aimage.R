aimage = function(input, hdu = 1, xcen = NA, ycen = NA, xdim = NA, ydim = NA, xlo = xcen-((xdim-1)/2), xhi = xcen+((xdim-1)/2), ylo = ycen-((ydim-1)/2), yhi = ycen+((ydim-1)/2), scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, scale.probs = NA, col.map = "rgb", col.alpha = 1, col.invert = FALSE, smooth.fwhm = 0, smooth.filter = "gauss", desat.limit = 0, desat.fwhm = 3, desat.filter = "gauss", padvalue = NA, asp = 1, ...){
    
    # library(astro); input=c("calexp-HSC-I-8283-38.stamp.fits","calexp-HSC-R-8283-38.stamp.fits","calexp-HSC-G-8283-38.stamp.fits"); hdu = 1; xcen = NA; ycen = NA; xdim = NA; ydim = 11; xlo = 3; xhi = 11; ylo = NA; yhi = NA; scale.type = "log"; scale.mode = 99.5; scale.lo = NA; scale.hi = NA; scale.pow = 0.5; scale.probs = seq(0,1,by=0.5); col.map = "rgb"; col.alpha=1; col.invert = FALSE; smooth.fwhm = 1; smooth.filter = "gauss"; desat.limit = 0.15; desat.fwhm = 3; desat.filter = "gauss"; padvalue = NA; asp = 1; i = 1
    
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
        if(is.null(rownames(input[[i]]))){rownames(input[[i]]) = 1:nrow(input[[i]])}
        if(is.null(colnames(input[[i]]))){colnames(input[[i]]) = 1:ncol(input[[i]])}
    }
    
    # trim image dimensions (if required)
    input.trim = input
    xlo = rep(xlo, length(input))[1:length(input)]
    xhi = rep(xhi, length(input))[1:length(input)]
    ylo = rep(ylo, length(input))[1:length(input)]
    yhi = rep(yhi, length(input))[1:length(input)]
    if(any(!is.na(c(xcen,ycen,xdim,ydim)))){
        for(i in 1:length(input)){
            ixcen=c(rep(xcen,length(input))[i],(dim(input[[i]])[1]+1)/2,NA);ixcen=ixcen[!is.na(ixcen)][1]
            iycen=c(rep(ycen,length(input))[i],(dim(input[[i]])[2]+1)/2,NA);iycen=iycen[!is.na(iycen)][1]
            ixdim=c(rep(xdim,length(input))[i],dim(input[[i]])[1],NA);ixdim=ceiling(ixdim[!is.na(ixdim)][1])
            iydim=c(rep(ydim,length(input))[i],dim(input[[i]])[2],NA);iydim=ceiling(iydim[!is.na(iydim)][1])
            if(is.na(xlo[i]) & is.na(xhi[i])){
                xlo[i] = ixcen - ((ixdim-1)/2)
                xhi[i] = ixcen + ((ixdim-1)/2)
            }else if(is.na(xlo[i])){
                xlo[i] = xhi[i] - ixdim + 1
            }else if(is.na(xhi[i])){
                xhi[i] = xlo[i] + ixdim -1
            }
            if(is.na(ylo[i]) & is.na(yhi[i])){
                ylo[i] = iycen - ((iydim-1)/2)
                yhi[i] = iycen + ((iydim-1)/2)
            }else if(is.na(ylo[i])){
                ylo[i] = yhi[i] - iydim + 1
            }else if(is.na(yhi[i])){
                yhi[i] = ylo[i] + iydim -1
            }
        }
    }
    if(any(!is.na(c(xlo,xhi,ylo,yhi)))){
        
        for(i in 1:length(input)){
            
            # setup
            ixlo = c(rep(xlo,length(input))[i],1,NA); ixlo = ixlo[!is.na(ixlo)][1]
            ixhi = c(rep(xhi,length(input))[i],dim(input[[i]])[1],NA); ixhi = ixhi[!is.na(ixhi)][1]
            iylo = c(rep(ylo,length(input))[i],1,NA); iylo = iylo[!is.na(iylo)][1]
            iyhi = c(rep(yhi,length(input))[i],dim(input[[i]])[2],NA); iyhi = iyhi[!is.na(iyhi)][1]
            ixdim = length(ceiling(ixlo : ixhi))
            iydim = length(ceiling(iylo : iyhi))
            igrid = matrix(padvalue, nrow=ixdim, ncol=iydim)
            
            # translate data to igrid and up to input.trim
            xold = ceiling(ixlo : ixhi)
            xnew = (((dim(igrid)[1]+1)/2) - ((ixdim-1)/2)) : (((dim(igrid)[1]+1)/2) + ((ixdim-1)/2))
            xoldbad = which(!xold %in% 1:dim(input[[i]])[1])
            if(length(xoldbad) > 0){
                xold = xold[-xoldbad]
                xnew = xnew[-xoldbad]
            }
            yold = ceiling(iylo : iyhi)
            ynew = (((dim(igrid)[2]+1)/2) - ((iydim-1)/2)) : (((dim(igrid)[2]+1)/2) + ((iydim-1)/2))
            yoldbad = which(!yold %in% 1:dim(input[[i]])[2])
            if(length(yoldbad) > 0){
                yold = yold[-yoldbad]
                ynew = ynew[-yoldbad]
            }
            igrid[xnew,ynew] = input[[i]][xold,yold]
            rownames(igrid) = (1:nrow(igrid)) + ixlo - 1
            colnames(igrid) = (1:ncol(igrid)) + iylo - 1
            input.trim[[i]] = igrid
            
        }
        
    }
    
    # smoothing? (can be time consuming)
    # NB: sigma_req^2 = sigma_final^2 + sigma_initial^2
    input.smoothed = input.trim
    if(smooth.fwhm > 0){
        for(i in 1:length(input)){
            xnames = rownames(input.smoothed[[i]])
            ynames = colnames(input.smoothed[[i]])
            input.smoothed[[i]] = smooth2d(input.smoothed[[i]], fwhm=smooth.fwhm, filter=smooth.filter)
            rownames(input.smoothed[[i]]) = xnames
            colnames(input.smoothed[[i]]) = ynames
        }
    }
    
    # map to linear range {0,1} (soft limits)
    input.mapped = input.smoothed
    ilos = ihis = {}
    for(i in 1:length(input)){
        slim = quantile(input.mapped[[i]],probs=c((50-(scale.mode/2))/100,(50+(scale.mode/2))/100),na.rm=TRUE)
        ilo = c(rep(scale.lo,length(input))[i],slim[1],NA); ilo = ilo[!is.na(ilo)][1]
        ihi = c(rep(scale.hi,length(input))[i],slim[2],NA); ihi = ihi[!is.na(ihi)][1]
        input.mapped[[i]] = (input.mapped[[i]] - ilo) / (ihi - ilo)
        ilos = c(ilos, ilo)
        ihis = c(ihis, ihi)
    }
    
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
    input.scaled.ref = .scale.func(input=input.avg, scale.type=scale.type, scale.pow=scale.pow, lo=ilos, hi=ihis, scale.probs=scale.probs)
    input.scaled = input.scaled.ref$input.scaled
    ref = input.scaled.ref$ref
    
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
        hsvmat = rgb2hsv(
            r=round(as.vector(input.rescaled))
            ,g=round(as.vector(input.rescaled))
            ,b=round(as.vector(input.rescaled))
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
    
    # desaturate faint pixels? (to intensify colour effect at bright end)
    if(desat.limit > 0){
        input.blur = smooth2d(input.avg, fwhm=desat.fwhm, filter=desat.filter) / desat.limit
        if(any(is.na(input.blur))){input.blur[which(is.na(input.blur))] = 0}
        if(any(input.blur < 0)){input.blur[input.blur < 0] = 0}
        if(any(input.blur > 1)){input.blur[input.blur > 1] = 1}
        hsvmat["s",] = hsvmat["s",] * as.vector(input.blur)
    }
    
    # image
    x = as.numeric(rownames(input.rescaled))
    y = as.numeric(colnames(input.rescaled))
    col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
    image(x=x, y=y, z=matrix(1:length(col),nrow=nrow(input.rescaled),ncol=ncol(input.rescaled)), col=col, asp=asp, ...)
    
    # return reference quantiles?
    if(!is.na(ref[1])){return(ref)}
    
}

tone.map = function(input, lo = min(input), hi = max(input), scale.type = "lin", scale.pow = 0.5){
    
    # rescale input to (soft) range (0,1)
    input.01 = (input - lo) / (hi - lo)
    
    # scaling functions
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    input.calib = input.01 * calib
    if(scale.type == "lin"){
        map.lo = 0
        map.hi = calib
    }else if(scale.type == "log"){
        map.lo = log10(scale.pow)
        map.hi = log10(calib + scale.pow)
        input.calib = suppressWarnings(log10(input.calib + scale.pow))
    }else if(scale.type == "pow"){
        map.lo = 0^scale.pow
        map.hi = calib^scale.pow
        input.calib = suppressWarnings(input.calib^scale.pow)
    }else if(scale.type == "atan"){
        map.lo = atan(0)
        map.hi = atan(calib)
        input.calib = suppressWarnings(atan(input.calib))
    }else if(scale.type == "asinh"){
        map.lo = asinh(0)
        map.hi = asinh(calib)
        input.calib = suppressWarnings(asinh(input.calib))
    }else if(scale.type == "sinh"){
        map.lo = sinh(0)
        map.hi = sinh(calib)
        input.calib = suppressWarnings(sinh(input.calib))
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    # return
    out = (input.calib - map.lo) / (map.hi - map.lo)
    return(out)
    
}

tone.unmap = function(probs, lo = 0, hi = 1, scale.type = "lin", scale.pow = 0.5){
    
    # scaling functions
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    if(scale.type == "lin"){
        map.lo = 0
        map.hi = calib
    }else if(scale.type == "log"){
        map.lo = log10(scale.pow)
        map.hi = log10(calib + scale.pow)
    }else if(scale.type == "pow"){
        map.lo = 0^scale.pow
        map.hi = calib^scale.pow
    }else if(scale.type == "atan"){
        map.lo = atan(0)
        map.hi = atan(calib)
    }else if(scale.type == "asinh"){
        map.lo = asinh(0)
        map.hi = asinh(calib)
    }else if(scale.type == "sinh"){
        map.lo = sinh(0)
        map.hi = sinh(calib)
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    # descale
    probs.scale = (probs * (map.hi - map.lo)) + map.lo
    if(scale.type == "lin"){
        probs.descale = probs.scale
    }else if(scale.type == "log"){
        probs.descale = suppressWarnings(10^(probs.scale) - scale.pow)
    }else if(scale.type == "pow"){
        probs.descale = suppressWarnings(probs.scale^(1/scale.pow))
    }else if(scale.type == "atan"){
        probs.descale = suppressWarnings(tan(probs.scale))
    }else if(scale.type == "asinh"){
        probs.descale = suppressWarnings(sinh(probs.scale))
    }else if(scale.type == "sinh"){
        probs.descale = suppressWarnings(asinh(probs.scale))
    }
    
    # return
    out = ((probs.descale / calib) * (hi - lo)) + lo
    return(out)
    
}

.scale.func = function(input, scale.type, scale.pow, los, his, scale.probs){
    
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    imdat = input * calib
    lo = 0 * calib
    hi = 1 * calib
    if(scale.type == "lin"){
        flo = lo
        fhi = hi
        input.scaled = imdat
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=(quantile(c(flo,fhi),probs=scale.probs)/calib)))
        }
    }else if(scale.type == "log"){
        slide = 0.5
        flo = log10(lo + slide)
        fhi = log10(hi + slide)
        input.scaled = suppressWarnings(((log10(imdat + slide) - flo) / (fhi - flo)))
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((10^(quantile(c(flo,fhi),probs=scale.probs))-slide)/calib)))
        }
    }else if(scale.type == "pow"){
        flo = lo^scale.pow
        fhi = hi^scale.pow
        input.scaled = suppressWarnings(((imdat^scale.pow - flo) / (fhi - flo)))
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((quantile(c(flo,fhi),probs=scale.probs)^(1/scale.pow))/calib)))
        }
    }else if(scale.type == "atan"){
        flo = atan(lo)
        fhi = atan(hi)
        input.scaled = suppressWarnings(((atan(imdat) - flo) / (fhi - flo)))
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((tan(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
        }
    }else if(scale.type == "asinh"){
        flo = asinh(lo)
        fhi = asinh(hi)
        input.scaled = suppressWarnings(((asinh(imdat) - flo) / (fhi - flo)))
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((sinh(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
        }
    }else if(scale.type == "sinh"){
        flo = sinh(lo)
        fhi = sinh(hi)
        input.scaled = suppressWarnings(((sinh(imdat) - flo) / (fhi - flo)))
        ref = {}
        for(i in 1:length(los)){
            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((asinh(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
        }
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    return(list(input.scaled=input.scaled, ref=ref))
    
}

