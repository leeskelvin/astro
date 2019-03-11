apoints = function(x, y = NULL, z = NULL, type = "p", col = NULL, scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, col.map = "rainbow", col.alpha = 1, col.invert = FALSE, ...){
    
    ref = NULL
    if(is.null(z) & is.null(col)){col = 1}
    
    if(!is.null(col)){
        
        hsvmat = rgb2hsv(col2rgb(col))
        if(col.invert){hsvmat["h",] = (hsvmat["h",]+0.5)%%1}
        col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha)
        
    }else if(!is.null(z)){
        
        # rescale z
        slim = quantile(z, probs=c((50-(scale.mode/2))/100, (50+(scale.mode/2))/100), na.rm=TRUE)
        zlo = c(scale.lo,slim[1],NA)
        zlo = zlo[!is.na(zlo)][1]
        zhi = c(scale.hi,slim[2],NA)
        zhi = zhi[!is.na(zhi)][1]
        
        # apply scaling function
        input.scaled = tone.map(input = z, lo=zlo, hi=zhi, scale.type=scale.type, scale.pow=scale.pow)
        ref = tone.unmap(probs=c(0,1), lo=zlo, hi=zhi, scale.type=scale.type, scale.pow=scale.pow)
        
        # rescale scaled averaged image to colour-appropriate range (0,255) (hard limits) (mono only)
        input.rescaled = pmin(pmax(255*input.scaled,0),255)
        if(any(is.na(input.rescaled))){input.rescaled[is.na(input.rescaled)] = 0}
        if(col.invert){input.rescaled = 255 - input.rescaled}
        
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
            hsvmat = rgb2hsv(col2rgb(rainbow(256,start=0,end=2/3)[input.rescaled + 1]))
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

