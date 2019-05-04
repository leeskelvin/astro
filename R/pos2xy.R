pos2xy = function(x, y = NULL, inset = 0, outer = FALSE, adj = NA){
    
    # function: use LSK lines to get coordinate positions - fractions of line heights
    pos2xyfunc = function(x, y, inset, outer, adj){
        adj = rep(adj,2)[1:2]
        if(is.character(x)){
            cfr = par("pin") / (par("cin")[2]) # number of fractional character heights
            pxy = diff(par("usr"))[c(1,3)] # plot region in xy coordinates
            if(length(inset)==1){
                inset = switch(x, "bottomleft"=c(inset,inset), "left"=c(inset,0), "topleft"=c(inset,inset), "top"=c(0,inset), "topright"=c(inset,inset), "right"=c(inset,0), "bottomright"=c(inset,inset), "bottom"=c(0,inset), c(inset,inset))
            }
            ixy = (pxy / cfr) * inset # inset in xy coordinates
            xyusr = par("usr")
            if(outer){
                xyusr = c(grconvertX(c(0,1),from='ndc'), grconvertY(c(0,1),from='ndc'))
                if(par("xlog")){xyusr[1:2] = log10(xyusr[1:2])}
                if(par("ylog")){xyusr[3:4] = log10(xyusr[3:4])}
                pxy = diff(xyusr)[c(1,3)]
            }
            xx = rep(xyusr[1] + (pxy[1]/2) + ixy[1], length(x))[1:length(x)] # central default x
            yy = rep(xyusr[3] + (pxy[2]/2) - ixy[2], length(y))[1:length(y)] # central default y
            if(length(grep("left",x)) > 0){
                xx[grep("left",x)] = xyusr[1] + ixy[1]
                if(is.na(adj[1])){adj[1] = 0}
            }
            if(length(grep("right",x)) > 0){
                xx[grep("right",x)] = xyusr[2] - ixy[1]
                if(is.na(adj[1])){adj[1] = 1}
            }
            if(length(grep("left",x))==0 & length(grep("right",x))==0){
                #xx = xyusr[1] + (pxy[1]/2)
                if(is.na(adj[1])){adj[1] = 0.5}
            }
            if(length(grep("top",x)) > 0){
                yy[grep("top",x)] = xyusr[4] - ixy[2]
                if(is.na(adj[2])){adj[2] = 1}
            }
            if(length(grep("bottom",x)) > 0){
                yy[grep("bottom",x)] = xyusr[3] + ixy[2]
                if(is.na(adj[2])){adj[2] = 0}
            }
            if(length(grep("top",x))==0 & length(grep("bottom",x))==0){
                #yy = xyusr[3] + (pxy[2]/2)
                if(is.na(adj[2])){adj[2] = 0.5}
            }
            if(par("xlog")){xx = 10^xx}
            if(par("ylog")){yy = 10^yy}
        }else{
            xx = x
            yy = y
            if(is.null(yy)){yy = xx}
            if(is.na(adj[1])){adj[1] = 0.5}
            if(is.na(adj[2])){adj[2] = 0.5}
        }
        return(list(x=xx, y=yy, xadj=adj[1], yadj=adj[2]))
    }
    
    # run
    maxlen = max(c(length(x),length(y)))
    x = rep(x,maxlen)[1:maxlen]
    y = rep(y,maxlen)[1:maxlen]
    if(is.null(y)){y = x}
    #out = pos2xyfunc(x=x, y=y, inset=inset, outer=outer, adj=adj)
    vout = Vectorize(pos2xyfunc, vectorize.args=c("x","y"))(x=x, y=y, inset=inset, outer=outer, adj=adj)
    out = list(x=as.numeric(vout["x",]), y=as.numeric(vout["y",]), xadj=as.numeric(vout["xadj",]), yadj=as.numeric(vout["yadj",]))
    
    # return
    return(out)
    
}

