bars = function(x, y, width, anchor = 1, joined = FALSE, col = "grey75", border = NA, ljoin = 1, ...){
    
    # unlog
    xy = pos2xy(x=x, y=y)
    opar = par()
    if(opar$xlog){
        xy$x = log10(xy$x)
        par("xlog"=FALSE)
    }
    if(opar$ylog){
        xy$y = log10(xy$y)
        par("ylog"=FALSE)
    }
    par("usr"=opar$usr)
    
    # coordinates & dimensions
    if(missing(width) & anchor %in% c(1,3)){width = c(diff(xy$x)[1],diff(xy$x))}
    if(missing(width) & anchor %in% c(2,4)){width = c(diff(xy$y)[1],diff(xy$y))}
    if(any(is.na(width))){width[is.na(width)] = 1}
    bhw = rep(width, length(xy$x))[1:length(xy$x)] / 2
    base = switch(anchor, par("usr")[3], par("usr")[1], par("usr")[4], par("usr")[2])
    obase = switch(anchor, base/10, base/10, base*10, base*10)
    
    # define xy coords
    if(anchor %in% c(1,3)){
        if(joined){
            xx = list(c(xy$x[1]-bhw[1], xy$x[1]-bhw[1], rep(xy$x[-length(xy$x)]+diff(xy$x)/2,each=2), xy$x[length(xy$x)]+bhw[(length(xy$x))], xy$x[length(xy$x)]+bhw[length(xy$x)]))
            yy = list(c(obase, rep(xy$y,each=2), obase))
        }else{
            xx = yy = {}
            for(i in 1:length(xy$x)){
                xx = c(xx, list(c(xy$x[i]-bhw[i], xy$x[i]-bhw[i], xy$x[i]+bhw[i], xy$x[i]+bhw[i])))
                yy = c(yy, list(c(obase, xy$y[i], xy$y[i], obase)))
            }
        }
    }else{
        if(joined){
            xx = list(c(obase, rep(xy$x,each=2), obase))
            yy = list(c(xy$y[1]-bhw[1], xy$y[1]-bhw[1], rep(xy$y[-length(xy$y)]+diff(xy$y)/2,each=2), xy$y[length(xy$y)]+bhw[(length(bhw))], xy$y[length(xy$y)]+bhw[(length(bhw))]))
        }else{
            xx = yy = {}
            for(i in 1:length(xy$y)){
                xx = c(xx, list(c(obase, xy$x[i], xy$x[i], obase)))
                yy = c(yy, list(c(xy$y[i]-bhw[i], xy$y[i]-bhw[i], xy$y[i]+bhw[i], xy$y[i]+bhw[i])))
            }
        }
    }
    
    # polygon
    for(i in 1:length(xx)){
        apolygon(x=xx[[i]], y=yy[[i]], col=col, border=border, ljoin=ljoin, ...)
    }
    
    # relog
    if(opar$xlog){par("xlog"=TRUE)}
    if(opar$ylog){par("ylog"=TRUE)}
    par("usr"=opar$usr)
    par("xaxp"=opar$xaxp)
    par("yaxp"=opar$yaxp)
    
}

