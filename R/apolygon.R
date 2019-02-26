apolygon = function(x, y, ...){
    
    xx = x
    yy = y
    xlog = par("xlog")
    ylog = par("ylog")
    usr = par("usr")
    
    if(xlog){
        xx=log10(x)
        if(any(x<10^(par("usr")[1]))){xx[x<10^(par("usr")[1])] = log10((10^(par("usr")[1]))/10)}
        par("xlog"=FALSE)
    }
    if(ylog){
        yy = log10(y)
        if(any(y<10^(par("usr")[3]))){yy[y<10^(par("usr")[3])] = log10((10^(par("usr")[3]))/10)}
        par("ylog"=FALSE)
    }
    par("usr"=usr)
    
    polygon(x=xx, y=yy, ...)
    
    if(xlog){par("xlog"=TRUE)}
    if(ylog){par("ylog"=TRUE)}
    par("usr"=usr)
    
}

