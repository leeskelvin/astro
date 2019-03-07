apoints = function(x, y = NULL, z = NULL, type = "p", col = NULL, scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, col.map = "topo", col.alpha = 1, col.invert = FALSE, ...){
    
    ref = NULL
    if(is.null(z) & is.null(col)){col = 1}
    
    if(!is.null(col)){
        
        hsvmat = rgb2hsv(col2rgb(col))
        if(col.invert){hsvmat["h",] = (hsvmat["h",]+0.5)%%1}
        col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
        
    }else if(!is.null(z)){
        
        # rescale z
        slim = quantile(z, probs=c((50-(scale.mode/2))/100, (50+(scale.mode/2))/100), na.rm=TRUE)
        ilo = c(scale.lo,slim[1],NA); ilo = ilo[!is.na(ilo)][1]
        ihi = c(scale.hi,slim[2],NA); ihi = ihi[!is.na(ihi)][1]
        iz = (z - ilo) / (ihi - ilo)
        
        # apply scaling function
        input.scaled.ref = .scale.func(input=iz, scale.type=scale.type, scale.pow=scale.pow, lo=ilo, hi=ihi, scale.probs=c(0,1))
        #input.scaled = input.scaled.ref$input.scaled
        ref = input.scaled.ref$ref
        
        # rescale scaled averaged image to colour-appropriate range (0,255) (hard limits) (mono only)
        cdat = 255 * input.scaled.ref$input.scaled
        if(any(cdat < 0, na.rm=TRUE)){cdat[cdat < 0] = 0}
        if(any(cdat > 255, na.rm=TRUE)){cdat[cdat > 255] = 255}
        if(any(is.na(cdat))){cdat[is.na(cdat)] = 0}
        if(col.invert){cdat = 255 - cdat}
        input.rescaled = cdat
        
        # hsv colour matrix
        if(col.map == "grey" | col.map == "gray"){
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
        
        # final colours
        col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
        
    }
    
    # points & return
    points(x=x, y=y, type=type, col=col, ...)
    if(!is.null(ref)){return(ref)}
    
}

