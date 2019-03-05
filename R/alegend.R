alegend = function(x, y = NULL, legend, type = list(), inset = 0.5, seg.len = 1.5, seg.gap = 0.5, cex = 1, bty = "n", bg = "white", box.lwd = 1, box.lty = 1, box.col = "grey25", ...){
    
    # unlog
    xlog = par("xlog")
    ylog = par("ylog")
    usr = par("usr")
    xaxp = par("xaxp")
    yaxp = par("yaxp")
    xy = pos2xy(x=x, y=y, inset=inset)
    if(xlog){
        xy$x=log10(xy$x)
        par("xlog"=FALSE)
    }
    if(ylog){
        xy$y = log10(xy$y)
        par("ylog"=FALSE)
    }
    par("usr"=usr)
    
    # lengths
    cfr = par("pin") / (par("cin")[2])
    pxy = diff(par("usr"))[c(1,3)]
    ixy = (pxy / cfr) * cex
    textmax = max(as.numeric(lapply(legend, strwidth, cex=cex)))
    boxheight = length(legend)*ixy[2] + ixy[2]/7.5
    boxwidth = textmax + seg.len*ixy[1] + seg.gap*ixy[1] + ixy[1]/3.25
    
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
    
    # points/text
    for(i in 1:length(legend)){
        
        # xy positions
        xinset = ixy[1] / 5
        yinset = ixy[2] / 1.75
        fillhalfheight = ixy[2] / 2.5
        xl = xy$x + xinset
        xm = xy$x + xinset + seg.len*ixy[1]/2
        xr = xy$x + xinset + seg.len*ixy[1]
        xt = xy$x + xinset + seg.len*ixy[1] + seg.gap*ixy[1]
        ym = xy$y - yinset - (i-1)*ixy[2]
        yt = xy$y - yinset - (i-1)*ixy[2] + fillhalfheight
        yb = xy$y - yinset - (i-1)*ixy[2] - fillhalfheight
        xlb = xl # xm - fillhalfheight
        xrb = xr # xm + hillhalfheight
        
        # subscript/superscript text offset correction
        yoffset = subadd = supadd = 0
        if(typeof(legend[[i]]) == "language"){
            hassub = length(grep("\\[",legend[[i]])) > 0
            hassup = length(grep("\\^",legend[[i]])) > 0
            if(hassub){subadd = strheight(bquote(x[3]),cex=cex)-strheight(bquote(x),cex=cex)}
            totheight = strheight(legend[[i]],cex=cex)
            trueheight = strheight(bquote(.(gsub("\\^","",paste(gsub("\n", "", legend[[i]]),collapse="")))),cex=cex)
            if(hassup){supadd = totheight - trueheight - subadd}
            if(hassub){yoffset = yoffset - subadd/2}
            if(hassup){yoffset = yoffset + supadd/2}
        }
        
        # line/point/fill
        if(length(type) >= i){
            if(names(type)[i] == "l" | names(type)[i] == "b"){
                do.call(what=lines, args=c(list(x=c(xl,xr), y=c(ym,ym)), type[[i]]))
            }
            if(names(type)[i] == "p" | names(type)[i] == "b"){
                do.call(what=points, args=c(list(x=xm, y=ym), type[[i]]))
            }
            if(names(type)[i] == "f"){
                do.call(what=apolygon, args=c(list(x=c(xlb,xlb,xrb,xrb), y=c(yb,yt,yt,yb)), type[[i]]))
            }
        }
        
        # text
        text(x=xt, y=ym+yoffset, labels=bquote(.(legend[[i]])), adj=c(0,0.5), cex=cex)
        
    }
    
    # relog
    if(xlog){par("xlog"=TRUE)}
    if(ylog){par("ylog"=TRUE)}
    par("usr"=usr)
    par("xaxp"=xaxp)
    par("yaxp"=yaxp)
    
}

