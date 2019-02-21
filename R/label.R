label = function(x, y = NULL, lab, inset = 0.5, adj = c(0.75,0.75), col = "grey50", ...){
    
#    # use legend to get coordinate positions
#    if(is.character(x)){
#        adj = c(NA,NA)
#        legpos = legend(x=x, y=y, legend=lab, inset=inset, plot=FALSE)
#        if(length(grep("left",x)) > 0){
#            xx = legpos$rect$left
#            adj[1] = 0
#        }else if(length(grep("right",x)) > 0){
#            xx = legpos$rect$left + legpos$rect$w
#            adj[1] = 1
#        }else{
#            xx = legpos$rect$left + (legpos$rect$w / 2)
#            adj[1] = 0.5
#        }
#        if(par("xlog")){xx = 10^xx}
#        if(length(grep("top",x)) > 0){
#            yy = legpos$rect$top
#            adj[2] = 1
#        }else if(length(grep("bottom",x)) > 0){
#            yy = legpos$rect$top - legpos$rect$h
#            adj[2] = 0
#        }else{
#            yy = legpos$rect$top - (legpos$rect$h/2)
#            adj[2] = 0.5
#        }
#        if(par("ylog")){yy = 10^yy}
#    }else{
#        xx = x
#        yy = y
#    }
    
    # use LSK lines to get coordinate positions - fractions of line heights
    if(is.character(x)){
        adj = c(NA,NA)
        cfr = par("pin") / (par("cin")[2]) # number of fractional character heights
        pxy = diff(par("usr"))[c(1,3)] # plot region in xy coordinates
        ixy = (pxy / cfr) * rep(inset,2)[1:2] # inset in xy coordinates
        if(length(grep("left",x)) > 0){
            xx = par("usr")[1] + ixy[1]
            adj[1] = 0
        }else if(length(grep("right",x)) > 0){
            xx = par("usr")[2] - ixy[1]
            adj[1] = 1
        }else{
            xx = par("usr")[1] + (pxy[1]/2)
            adj[1] = 0.5
        }
        if(length(grep("top",x)) > 0){
            yy = par("usr")[4] - ixy[2]
            adj[2] = 1
        }else if(length(grep("bottom",x)) > 0){
            yy = par("usr")[3] + ixy[2]
            adj[2] = 0
        }else{
            yy = par("usr")[3] + (pxy[2]/2)
            adj[2] = 0.5
        }
    }else{
        xx = x
        yy = y
        if(is.null(yy)){yy = xx}
    }
    
    # plot with text
    text(x=xx, y=yy, labels=lab, adj=adj, col=col, ...)
    
}

