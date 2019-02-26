alegend = function(x, y = NULL, legend, type = list(), inset = 0.5, seg.len = 1.5, seg.gap = 0.5, cex = 1, bty = "n", bg = "white", box.lwd = 1, box.lty = 1, box.col = "grey75", ...){
    
    # setup
    if(length(legend)==0){stop('argument "legend" is missing, with no default')}
    xy = pos2xy(x=x, y=y, inset=inset)
    cfr = par("pin") / (par("cin")[2])
    pxy = diff(par("usr"))[c(1,3)]
    ixy = (pxy / cfr) * cex
    textmax = max(as.numeric(lapply(legend, strwidth, cex=cex)))
    
    
    # bottom fix
    if(length(grep("bottom",as.character(x))) > 0){
        xy$y = xy$y + length(legend)*ixy[2] + ixy[2]/7.5
    }
    
    # right fix
    if(length(grep("right",as.character(x))) > 0){
        xy$x = xy$x - textmax - seg.len*ixy[1] - seg.gap*ixy[1] - ixy[1]/1.75
    }
    
    # box
    if(bty != "n"){
        apolygon(x=c(rep(xy$x,2),rep(xy$x+textmax+seg.len*ixy[1]+seg.gap*ixy[1]+ixy[1]/1.75,2)), y=c(xy$y,rep(xy$y-length(legend)*ixy[2]-ixy[2]/7.5,2),xy$y), col=bg, border=box.col, lty=box.lty, lwd=box.lwd)
    }
    
    # points/text
    for(i in 1:length(legend)){
        
        # xy positions
        xl = xy$x + ixy[1]/2.75
        xm = xy$x + ixy[1]/2.75 + seg.len*ixy[1]/2
        xr = xy$x + ixy[1]/2.75 + seg.len*ixy[1]
        xt = xy$x + ixy[1]/2.75 + seg.len*ixy[1] + seg.gap*ixy[1]
        ym = xy$y - ixy[2]/1.75 - (i-1)*ixy[2]
        yt = xy$y - ixy[2]/1.75 - (i-1)*ixy[2] + ixy[2]/2.5
        yb = xy$y - ixy[2]/1.75 - (i-1)*ixy[2] - ixy[2]/2.5
        xlb = xm - ixy[1]/2.5
        xrb = xm + ixy[1]/2.5
        
        # line/point/fill
        if(length(type) >= i){
            if(names(type)[i] == "l" | names(type)[i] == "b"){
                do.call(what=lines, args=c(list(x=c(xl,xr), y=c(ym,ym)), type[[i]]))
            }
            if(names(type)[i] == "p" | names(type)[i] == "b"){
                do.call(what=points, args=c(list(x=xm, y=ym, cex=cex), type[[i]]))
            }
            if(names(type)[i] == "f"){
                do.call(what=apolygon, args=c(list(x=c(xlb,xlb,xrb,xrb), y=c(yb,yt,yt,yb)), type[[i]]))
            }
        }
        
        # text
        text(x=xt, y=ym, labels=bquote(.(legend[[i]])), adj=c(0,0.5), cex=cex)
        
    }
    
}

