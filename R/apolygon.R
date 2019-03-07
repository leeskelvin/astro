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
    xusr = c(extendrange(usr[1:2],f=0.1), extendrange(usr[3:4],f=0.1))
    
    if(any(xx < xusr[1])){xx[xx < xusr[1]] = xusr[1]}
    if(any(xx > xusr[2])){xx[xx > xusr[2]] = xusr[2]}
    if(any(yy < xusr[3])){yy[yy < xusr[3]] = xusr[3]}
    if(any(yy > xusr[4])){yy[yy > xusr[4]] = xusr[4]}
    
    polygon(x=xx, y=yy, ...)
    
    if(xlog){par("xlog"=TRUE)}
    if(ylog){par("ylog"=TRUE)}
    par("usr"=usr)
    par("xaxp"=xaxp)
    par("yaxp"=yaxp)
    
}

