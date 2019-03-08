col.bar = function(x = "right", y = NULL, n = 5, format = NA, digits = 2, flip = FALSE, inset = 0.5, horizontal = FALSE, bar.length = 0.9, seg.num = 9, seg.width = 1.5, seg.gap = 0.5, scale.type = "lin", scale.lo = 0, scale.hi = 1, scale.pow = 0.5, col.map = "rainbow", col.alpha = 1, col.invert = FALSE, cex = 1, bar.lwd = 1, bar.lty = 1, bar.col = "grey25", seg.lwd = 0, seg.lty = 2, seg.col = "grey25", bty = "n", bg = "white", box.lwd = 1, box.lty = 1, box.col = "grey25"){
    
    # unlog & xpd
    opar = par()
    xy = pos2xy(x=x, y=y, inset=inset)
    if(opar$xlog){
        xy$x = log10(xy$x)
        par("xlog"=FALSE)
    }
    if(opar$ylog){
        xy$y = log10(xy$y)
        par("ylog"=FALSE)
    }
    par("usr"=opar$usr)
    par("xpd"=NA) # allow plotting in outer regions
    
    # polygon lwds
    if(bar.lwd==0){bar.lwd=1; bar.col=NA}
    if(seg.lwd==0){seg.lwd=1; seg.col=NA}
    if(box.lwd==0){box.lwd=1; box.col=NA}
    
    # apply scaling function to generate reference labels
    ref = .scale.func(input=NA, scale.type=scale.type, scale.pow=scale.pow, lo=scale.lo, hi=scale.hi, scale.probs=seq(0, 1, len=seg.num))$ref
    if(!is.na(format)){
        if(format == "p"){
            tempref = {}
            for(i in 1:length(ref)){
                tempref = c(tempref, bquote(paste(10^.(log10(ref[i])))))
            }
            ref=tempref
        }else{
            ref = formatC(ref, format=format, digits=digits)
        }
    }
    #if(scale.lo > scale.hi){ref = rev(ref)}
    ref[!1:length(ref) %in% unique(round(seq(1, seg.num, len=n)))] = NA
    
    # colour-appropriate range (0,255) (hard limits) (mono only)
    input.rescaled = seq(0, 255, len=seg.num)
    if(col.invert){input.rescaled = 255 - input.rescaled}
    
    # hsv colour matrix and final colours
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
    col = hsv(h=hsvmat["h",], s=hsvmat["s",], v=hsvmat["v",], alpha=col.alpha) 
    
    # lengths
    cfr = par("pin") / (par("cin")[2])
    pxy = diff(par("usr"))[c(1,3)]
    ixy = (pxy / cfr) * cex
    xinset = ixy[1] / 1.5
    yinset = ixy[2] / 1.5
    if(horizontal){
        textmax = max(as.numeric(lapply(ref, strheight, cex=cex)))
        boxwidth = bar.length * pxy[1]
        boxheight = textmax + seg.width*ixy[2] + sign(n)*seg.gap*ixy[2] + 2*yinset
        bar.height = (bar.length * pxy[1]) - (2 * xinset)
        text.adj = ifelse(c(flip,flip),c(0.5,0),c(0.5,1))
    }else{
        textmax = max(as.numeric(lapply(ref, strwidth, cex=cex)))
        boxwidth = textmax + seg.width*ixy[1] + sign(n)*seg.gap*ixy[1] + 2*xinset
        boxheight = bar.length*pxy[2]
        bar.height = (bar.length * pxy[2]) - (2 * yinset)
        text.adj = ifelse(c(flip,flip),c(1,0.5),c(0,0.5))
        col = rev(col)
        ref = rev(ref)
    }
    
    # positional corrections
    if(is.character(x)){
        xcorr = switch(x, "bottomleft"=0, "left"=0, "topleft"=0, "top"=-boxwidth/2, "topright"=-boxwidth, "right"=-boxwidth, "bottomright"=-boxwidth, "bottom"=-boxwidth/2, -boxwidth/2)
        ycorr = switch(x, "bottomleft"=boxheight, "left"=boxheight/2, "topleft"=0, "top"=0, "topright"=0, "right"=boxheight/2, "bottomright"=boxheight, "bottom"=boxheight, boxheight/2)
        xy$x = xy$x + xcorr
        xy$y = xy$y + ycorr
    }
    
    # box
    if(bty != "n"){
        apolygon(x=c(rep(xy$x,2),rep(xy$x+boxwidth,2)), y=c(xy$y,rep(xy$y-boxheight,2),xy$y), col=bg, border=box.col, lty=box.lty, lwd=box.lwd)
    }
    
    # segments/text
    for(i in seg.num:1){
        
        # xy positions
        if(horizontal){
            xl = xy$x + xinset + (bar.height * ((i-1) / seg.num))
            xr = xy$x + xinset + (bar.height * (i / seg.num))
            xv = xy$x + xinset + (bar.height * ((2*i-1)/(2*seg.num)))
            if(!flip){
                yt = xy$y - yinset
                yb = xy$y - yinset - (seg.width * ixy[2])
                yv = xy$y - yinset - (seg.width * ixy[2]) - (seg.gap*ixy[2])
            }else{
                yt = xy$y - boxheight + yinset + (seg.width * ixy[2])
                yb = xy$y - boxheight + yinset
                yv = xy$y - boxheight + yinset + (seg.width * ixy[2]) + (seg.gap * ixy[2])
            }
        }else{
            if(!flip){
                xl = xy$x + xinset
                xr = xy$x + xinset + (seg.width * ixy[1])
                xv = xy$x + xinset + (seg.width * ixy[1]) + (seg.gap*ixy[1])
            }else{
                xl = xy$x + boxwidth - xinset - (seg.width * ixy[1])
                xr = xy$x + boxwidth - xinset
                xv = xy$x + boxwidth - xinset - (seg.width * ixy[1]) - (seg.gap*ixy[1])
            }
            yt = xy$y - yinset - (bar.height * ((i-1) / seg.num))
            yb = xy$y - yinset - (bar.height * (i / seg.num))
            yv = xy$y - yinset - (bar.height * ((2*i-1)/(2*seg.num)))
        }
        
        # subscript/superscript text offset correction
        yoffset = subadd = supadd = 0
        if(typeof(ref[[i]]) == "language"){
            hassub = length(grep("\\[",ref[[i]])) > 0
            hassup = length(grep("\\^",ref[[i]])) > 0
            if(hassub){subadd = strheight(bquote(x[3]),cex=cex)-strheight(bquote(x),cex=cex)}
            totheight = strheight(ref[[i]],cex=cex)
            trueheight = strheight(bquote(.(gsub("\\^","",paste(gsub("\n", "", ref[[i]]),collapse="")))),cex=cex)
            if(hassup){supadd = totheight - trueheight - subadd}
            if(hassub){yoffset = yoffset - subadd/2}
            if(hassup){yoffset = yoffset + supadd/2}
        }
        
        # polygon and text
        apolygon(x=c(rep(xl,2),rep(xr,2)), y=c(yb,rep(yt,2),yb), col=col[i], border=NA)
        if(!is.na(seg.col) & i<seg.num){
            if(horizontal){
                lines(x=c(xr,xr), y=c(yt,yb), col=seg.col, lty=seg.lty, lwd=seg.lwd, lend=1, ljoin=1)
            }else{
                lines(x=c(xl,xr), y=c(yb,yb), col=seg.col, lty=seg.lty, lwd=seg.lwd, lend=1, ljoin=1)
            }
        }
        if(!is.na(as.character(ref[[i]])[1])){
            text(x=xv, y=yv+yoffset, labels=bquote(.(ref[[i]])), adj=text.adj, cex=cex)
        }
        
    }
    
    # bar
    if(horizontal){
        xl = xy$x + xinset
        xr = xy$x + xinset + bar.height
    }else{
        yt = xy$y - yinset
        yb = xy$y - yinset - bar.height
    }
    apolygon(x=c(rep(xl,2),rep(xr,2)), y=c(yb,rep(yt,2),yb), col=NA, border=bar.col, lty=bar.lty, lwd=bar.lwd)
    
    # relog & xpd
    if(opar$xlog){par("xlog"=TRUE)}
    if(opar$ylog){par("ylog"=TRUE)}
    par("usr"=opar$usr)
    par("xaxp"=opar$xaxp)
    par("yaxp"=opar$yaxp)
    par("xpd"=opar$xpd)
    
}

