alegend = function(x, y = NULL, legend, type = setNames(apply(cbind(lty=rep(1,length(legend))), 1, as.list),rep("l",length(legend))), inset = 0.5, outer = FALSE, seg.len = 1.5, seg.gap = 0.5, line.spacing = 1, cex = 1, bty = "n", bg = "white", box.lwd = 1, box.lty = 1, box.col = "grey25", box.pad = 0, ncol = 1, byrow = FALSE, ...){
    
    # unlog
    opar = par()
    xy = pos2xy(x=x, y=y, inset=inset, outer=outer)
    if(opar$xlog){
        xy$x = log10(xy$x)
        par("xlog"=FALSE)
    }
    if(opar$ylog){
        xy$y = log10(xy$y)
        par("ylog"=FALSE)
    }
    par("usr"=opar$usr)
    if(opar$xaxp[3] == 0){opar$xaxp[3] = 1}
    if(opar$yaxp[3] == 0){opar$yaxp[3] = 1}
    
    # legend matrix
    legncol = pmin(ncol, length(legend))
    legend.pad = rep(NA, len=legncol*ceiling(length(legend)/legncol))
    legend.pad[1:length(legend)] = legend
    legmat = matrix(legend.pad, ncol=legncol, byrow=byrow)
    badcols = which(as.logical(floor(colSums(apply(rbind(legmat,NA), 2, is.na)) / (nrow(legmat)+1))))
    if(length(badcols)>0){legmat = legmat[,-badcols]}
    legord = matrix(1:length(legmat), ncol=ncol(legmat), byrow=byrow)
    if(any(is.na(legmat))){legord[is.na(legmat)] = NA}
    
    # lengths
    cfr = par("pin") / (par("cin")[2])
    pxy = diff(par("usr"))[c(1,3)]
    ixy = (pxy / cfr) * cex # one character height in units of x/y axis units
    boxpad = rep(box.pad,2)[1:2]
    textmax = apply(apply(rbind(legmat,NA), 2, strwidth, cex=cex), 2, max)
    boxheight = (nrow(legmat)-1)*ixy[2]*line.spacing + 1*ixy[2] + ixy[2]/7.5 + 2*boxpad[2]*ixy[2]
    boxwidth = sum(ixy[1]/2 + seg.len*ixy[1] + seg.gap*ixy[1] + textmax) + 2*boxpad[1]*ixy[1]
    
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
    for(i in 1:ncol(legmat)){
        
        for(j in 1:nrow(legmat)){
            
            if(!is.na(legord[j,i])){
                
                # xy positions
                xinset = (ixy[1] / 3.5) + boxpad[1]*ixy[1]
                if(i > 1){
                    xinset = xinset + sum(textmax[1:(i-1)]) + ((i-1)*(ixy[1]/2 + seg.len*ixy[1] + seg.gap*ixy[1]))
                }
                yinset = (ixy[2] / 1.75) + boxpad[2]*ixy[2]
                fillhalfheight = (line.spacing*ixy[2]) / 2.5
                xl = xy$x + xinset
                xm = xy$x + xinset + seg.len*ixy[1]/2
                xr = xy$x + xinset + seg.len*ixy[1]
                xt = xy$x + xinset + seg.len*ixy[1] + seg.gap*ixy[1]
                ym = xy$y - yinset - (j-1)*ixy[2]*line.spacing
                yt = xy$y - yinset - (j-1)*ixy[2]*line.spacing + fillhalfheight
                yb = xy$y - yinset - (j-1)*ixy[2]*line.spacing - fillhalfheight
                
                # subscript/superscript text offset correction
                yoffset = subadd = supadd = 0
                if(typeof(legmat[j,i][[1]]) == "language"){
                    hassub = length(grep("\\[",legmat[j,i][[1]])) > 0
                    hassup = length(grep("\\^",legmat[j,i][[1]])) > 0
                    if(hassub){subadd = strheight(bquote(x[3]),cex=cex)-strheight(bquote(x),cex=cex)}
                    totheight = strheight(legmat[j,i][[1]],cex=cex)
                    trueheight = strheight(bquote(.(gsub("\\^","",paste(gsub("\n", "", legmat[j,i][[1]]),collapse="")))),cex=cex)
                    if(hassup){supadd = totheight - trueheight - subadd}
                    if(hassub){yoffset = yoffset - subadd/2}
                    if(hassup){yoffset = yoffset + supadd/2}
                }
                
                # line/point/fill
                if(length(type) >= legord[j,i]){
                    if(names(type)[legord[j,i]] == "l" | names(type)[legord[j,i]] == "b"){
                        do.call(what=lines, args=c(list(x=c(xl,xr), y=c(ym,ym)), type[[legord[j,i]]]))
                    }
                    if(names(type)[legord[j,i]] == "p" | names(type)[legord[j,i]] == "b"){
                        do.call(what=points, args=c(list(x=xm, y=ym), type[[legord[j,i]]]))
                    }
                    if(names(type)[legord[j,i]] == "f"){
                        do.call(what=apolygon, args=c(list(x=c(xl,xl,xr,xr), y=c(yb,yt,yt,yb)), type[[legord[j,i]]]))
                    }
                }
                
                # text
                text(x=xt, y=ym+yoffset, labels=bquote(.(legmat[j,i][[1]])), adj=c(0,0.5), cex=cex)
                
            }
            
        }
        
    }
    
    # relog
    if(opar$xlog){par("xlog"=TRUE)}
    if(opar$ylog){par("ylog"=TRUE)}
    par("usr"=opar$usr)
    par("xaxp"=opar$xaxp)
    par("yaxp"=opar$yaxp)
    
}

