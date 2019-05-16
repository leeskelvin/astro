apolygon = function(x, y = NULL, ...){
    
    # unlog
    opar = par()
    xx = x
    yy = y
    if(opar$xlog){
        xx = log10(x)
        par("xlog"=FALSE)
    }
    if(opar$ylog){
        if(!is.null(y)){yy = log10(y)}
        par("ylog"=FALSE)
    }
    par("usr"=opar$usr)
    
    # outer buffer limits
    #xusr = c(extendrange(opar$usr[1:2],f=0.1), extendrange(opar$usr[3:4],f=0.1))
    xyusr = c(grconvertX(c(0,1),from='ndc'), grconvertY(c(0,1),from='ndc'))
    xusr = c(extendrange(xyusr[1:2],f=0.1), extendrange(xyusr[3:4],f=0.1))
    
    # trim to outer buffer limits
    if(any(xx < xusr[1])){xx[xx < xusr[1]] = xusr[1]}
    if(any(xx > xusr[2])){xx[xx > xusr[2]] = xusr[2]}
    if(any(yy < xusr[3])){yy[yy < xusr[3]] = xusr[3]}
    if(any(yy > xusr[4])){yy[yy > xusr[4]] = xusr[4]}
    
    # draw polygon
    polygon(x=xx, y=yy, ...)
    
    # relog
    if(opar$xlog){par("xlog"=TRUE)}
    if(opar$ylog){par("ylog"=TRUE)}
    par("usr"=opar$usr)
    par("xaxp"=opar$xaxp)
    par("yaxp"=opar$yaxp)
    
}

