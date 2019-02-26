pos2xy = function(x, y = NULL, inset = 0.5, adj = NA){
    
    # function: use LSK lines to get coordinate positions - fractions of line heights
    pos2xyfunc = function(x, y, inset, adj){
        adj = rep(adj,2)[1:2]
        if(is.character(x)){
            cfr = par("pin") / (par("cin")[2]) # number of fractional character heights
            pxy = diff(par("usr"))[c(1,3)] # plot region in xy coordinates
            if(length(inset)==1){
                inset = switch(x, "bottomleft"=c(inset,inset), "left"=c(inset,0), "topleft"=c(inset,inset), "top"=c(0,inset), "topright"=c(inset,inset), "right"=c(inset,0), "bottomright"=c(inset,inset), "bottom"=c(0,inset), c(inset,inset))
            }
            ixy = (pxy / cfr) * inset # inset in xy coordinates
            xx = rep(par("usr")[1] + (pxy[1]/2) + ixy[1], length(x))[1:length(x)]
            yy = rep(par("usr")[3] + (pxy[2]/2) + ixy[2], length(y))[1:length(y)]
            if(length(grep("left",x)) > 0){
                xx[grep("left",x)] = par("usr")[1] + ixy[1]
                if(is.na(adj[1])){adj[1] = 0}
            }
            if(length(grep("right",x)) > 0){
                xx[grep("right",x)] = par("usr")[2] - ixy[1]
                if(is.na(adj[1])){adj[1] = 1}
            }
            if(length(grep("left",x))==0 & length(grep("right",x))==0){
                #xx = par("usr")[1] + (pxy[1]/2)
                if(is.na(adj[1])){adj[1] = 0.5}
            }
            if(length(grep("top",x)) > 0){
                yy[grep("top",x)] = par("usr")[4] - ixy[2]
                if(is.na(adj[2])){adj[2] = 1}
            }
            if(length(grep("bottom",x)) > 0){
                yy[grep("bottom",x)] = par("usr")[3] + ixy[2]
                if(is.na(adj[2])){adj[2] = 0}
            }
            if(length(grep("top",x))==0 & length(grep("bottom",x))==0){
                #yy = par("usr")[3] + (pxy[2]/2)
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
        return(list(x=xx, y=yy, adj=adj))
    }
    
    # run
    maxlen = max(c(length(x),length(y)))
    x = rep(x,maxlen)[1:maxlen]
    y = rep(y,maxlen)[1:maxlen]
    if(is.null(y)){y = x}
    out = pos2xyfunc(x=x, y=y, inset=inset, adj=adj)
    
    # return
    return(out)
    
}

