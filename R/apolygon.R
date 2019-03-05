apolygon = function(x, y, ...){
    
    xx = x
    yy = y
    xlog = par("xlog")
    ylog = par("ylog")
    usr = par("usr")
    xaxp = par("xaxp")
    yaxp = par("yaxp")
    
    if(xlog){
        xx=log10(x)
        par("xlog"=FALSE)
    }
    if(ylog){
        yy = log10(y)
        par("ylog"=FALSE)
    }
    par("usr"=usr)
    
    if(any(xx < usr[1]/10)){xx[xx < usr[1]/10] = usr[1]/10}
    if(any(xx > usr[2]*10)){xx[xx > usr[2]*10] = usr[2]*10}
    if(any(yy < usr[3]/10)){yy[yy < usr[3]/10] = usr[3]/10}
    if(any(yy > usr[4]*10)){yy[yy > usr[4]*10] = usr[4]*10}
    
    polygon(x=xx, y=yy, ...)
    
    if(xlog){par("xlog"=TRUE)}
    if(ylog){par("ylog"=TRUE)}
    par("usr"=usr)
    par("xaxp"=xaxp)
    par("yaxp"=yaxp)
    
}

