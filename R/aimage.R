aimage = function(input, hdu = 1, xcen = NA, ycen = NA, xdim = NA, ydim = NA, xlo = xcen-((xdim-1)/2), xhi = xcen+((xdim-1)/2), ylo = ycen-((ydim-1)/2), yhi = ycen+((ydim-1)/2), scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, scale.probs = NULL, col.map = "rgb", col.alpha = 1, col.invert = FALSE, cb = FALSE, smooth.fwhm = 0, smooth.filter = "gauss", desat.limit = 0, desat.fwhm = 3, desat.filter = "gauss", padvalue = NA, asp = 1, ...){
    
    # library(astro); input=c("calexp-HSC-I-8283-38.stamp.fits","calexp-HSC-R-8283-38.stamp.fits","calexp-HSC-G-8283-38.stamp.fits"); hdu = 1; xcen = NA; ycen = NA; xdim = NA; ydim = 11; xlo = 3; xhi = 11; ylo = NA; yhi = NA; scale.type = "log"; scale.mode = 99.5; scale.lo = NA; scale.hi = NA; scale.pow = 0.5; scale.probs = seq(0,1,by=0.5); col.map = "rgb"; col.alpha=1; col.invert = FALSE; smooth.fwhm = 1; smooth.filter = "gauss"; desat.limit = 0.15; desat.fwhm = 3; desat.filter = "gauss"; padvalue = NA; asp = 1; i = 1
    
    # force convert to list
    #if(typeof(input)=="double" | typeof(input)=="integer"){
    if(class(input)!="list"){
        input = list(input)
    }
    if(typeof(input)=="character"){
        input = as.list(input)
    }
    
    # read FITS files, convert data frames to matrices, and assign row/column names
    for(i in 1:length(input)){
        if(typeof(input[[i]]) == "character"){
            input[[i]] = read.fits(input[[i]], hdu=rep(hdu,length(input))[i])$dat[[1]]
        }
        if(class(input[[i]]) == "data.frame"){
            input[[i]] = as.matrix(input[[i]])
        }
        inputrownames = suppressWarnings(as.numeric(rownames(input[[i]])))
        inputcolnames = suppressWarnings(as.numeric(colnames(input[[i]])))
        if(any(is.na(inputrownames)) | length(inputrownames)==0){inputrownames = 1:nrow(input[[i]])}
        if(any(is.na(inputcolnames)) | length(inputcolnames)==0){inputcolnames = 1:ncol(input[[i]])}
        rownames(input[[i]]) = inputrownames
        colnames(input[[i]]) = inputcolnames
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
    
    # calculate los/his
    zlos = zhis = {}
    for(i in 1:length(input)){
        slim = quantile(input.trim[[i]],probs=c((50-(scale.mode/2))/100,(50+(scale.mode/2))/100),na.rm=TRUE)
        zlo = c(rep(scale.lo,length(input))[i],slim[1],NA)
        zhi = c(rep(scale.hi,length(input))[i],slim[2],NA)
        zlos = c(zlos, zlo[!is.na(zlo)][1])
        zhis = c(zhis, zhi[!is.na(zhi)][1])
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
    
    # generate average image (scaled to range lo=0, hi=1)
    input.avg = (input.smoothed[[1]] - zlos[1]) / (zhis[1] - zlos[1])
    input.ngood = !is.na(input.avg)
    if(any(is.na(input.avg))){input.avg[is.na(input.avg)] = 0}
    if(length(input) > 1){
        for(i in 2:length(input)){
            tgrid = (input.smoothed[[i]] - zlos[i]) / (zhis[i] - zlos[i])
            input.ngood = input.ngood + !is.na(tgrid)
            if(any(is.na(tgrid))){tgrid[is.na(tgrid)] = 0}
            input.avg = input.avg + tgrid
        }
        input.avg = input.avg / input.ngood
    }
    
    # apply scaling function
    input.scaled = tone.map(input=input.avg, lo=0, hi=1, scale.type=scale.type, scale.pow=scale.pow)
    
    # generate rescaled input image to colour appropriate range (0,255) (hard limits) (mono only)
    if(col.map == "rgb"){
        input.rescaled = input.smoothed
        for(i in 1:length(input)){
            input.rescaled[[i]]=pmin(pmax(255*(((input.rescaled[[i]]-zlos[i])/(zhis[i]-zlos[i]))/input.avg)*input.scaled,0),255)
        }
        if(length(input.rescaled) == 1){
            input.rescaled = list(input.rescaled[[1]], input.rescaled[[1]], input.rescaled[[1]])
        }
        if(length(input.rescaled) == 2){
            input.rescaled = list(input.rescaled[[1]], pmin(pmax(255*input.scaled,0),255), input.rescaled[[2]])
        }
    }else{
        input.rescaled = list(pmin(pmax(255*input.scaled, 0), 255))
    }
    for(i in 1:length(input.rescaled)){
        if(any(is.na(input.rescaled[[i]]))){input.rescaled[[i]][is.na(input.rescaled[[i]])] = 0}
        if(col.invert){input.rescaled[[i]] = 255 - input.rescaled[[i]]}
    }
    
    # generate colours
    if(col.map == "rgb"){
        hsvmat = rgb2hsv(
            r=round(as.vector(input.rescaled[[1]]))
            ,g=round(as.vector(input.rescaled[[2]]))
            ,b=round(as.vector(input.rescaled[[3]]))
        )
    }else if(col.map == "grey" | col.map == "gray"){
        hsvmat = rgb2hsv(
            r=round(as.vector(input.rescaled[[1]]))
            ,g=round(as.vector(input.rescaled[[1]]))
            ,b=round(as.vector(input.rescaled[[1]]))
        )
    }else if(col.map == "sls"){
        hsvmat = rgb2hsv(col2rgb(sls(256)[input.rescaled[[1]] + 1]))
    }else if(col.map == "rainbow"){
        hsvmat = rgb2hsv(col2rgb(rev(rainbow(256,start=0,end=5/6))[input.rescaled[[1]] + 1]))
    }else if(col.map == "heat"){
        hsvmat = rgb2hsv(col2rgb(heat.colors(256)[input.rescaled[[1]] + 1]))
    }else if(col.map == "terrain"){
        hsvmat = rgb2hsv(col2rgb(terrain.colors(256)[input.rescaled[[1]] + 1]))
    }else if(col.map == "topo"){
        hsvmat = rgb2hsv(col2rgb(topo.colors(256)[input.rescaled[[1]] + 1]))
    }else if(col.map == "cm"){
        hsvmat = rgb2hsv(col2rgb(cm.colors(256)[input.rescaled[[1]] + 1]))
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
    x = as.numeric(rownames(input.rescaled[[1]]))
    y = as.numeric(colnames(input.rescaled[[1]]))
    col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
    image(x=x, y=y, z=matrix(1:length(col),nrow=nrow(input.rescaled[[1]]),ncol=ncol(input.rescaled[[1]])), col=col, asp=asp, ...)
    
    # colour bar?
    zrange = tone.unmap(probs=c(0,1), lo=mean(zlos), hi=mean(zhis), scale.type=scale.type, scale.pow=scale.pow)
    if(is.logical(cb[1])){
        if(cb[1]){
            cb = formals(col.bar)
            cb$scale.lo = zrange[1]
            cb$scale.hi = zrange[2]
            cb$scale.type = scale.type
            cb$scale.pow = scale.pow
            cb$col.map = col.map
            cb$col.alpha = col.alpha
            cb$col.invert = col.invert
        }
    }
    if(is.list(cb)){
        if(!"scale.lo" %in% names(cb)){cb = c(cb,scale.lo=zrange[1])}
        if(!"scale.hi" %in% names(cb)){cb = c(cb,scale.hi=zrange[2])}
        if(!"scale.type" %in% names(cb)){cb = c(cb,scale.type=scale.type)}
        if(!"scale.pow" %in% names(cb)){cb = c(cb,scale.pow=scale.pow)}
        if(!"col.map" %in% names(cb)){cb = c(cb,col.map=col.map)}
        if(!"col.alpha" %in% names(cb)){cb = c(cb,col.alpha=col.alpha)}
        if(!"col.invert" %in% names(cb)){cb = c(cb,col.invert=col.invert)}
        cbd = formals(col.bar)
        cb = c(cb, cbd[which(!names(cbd) %in% names(cb))])
        do.call(col.bar, args=as.list(cb))
    }
    
    # return reference quantiles?
    if(!is.null(scale.probs)){
        ref = t(Vectorize(tone.unmap, vectorize.args=c("lo","hi"))(probs=scale.probs, lo=zlos, hi=zhis, scale.type=scale.type, scale.pow=scale.pow))
        rownames(ref) = NULL
        colnames(ref) = scale.probs
        return(ref)
    }
    
}

