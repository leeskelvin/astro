bars = function(x, y, width, anchor = 1, joined = FALSE, col = "grey75", border = NA, ljoin = 1, ...){
    
    # setup
    if(missing(width) & anchor %in% c(1,3)){width = c(diff(x)[1],diff(x))}
    if(missing(width) & anchor %in% c(2,4)){width = c(diff(y)[1],diff(y))}
    bhw = rep(width, length(x))[1:length(x)] / 2
    base = switch(anchor, par("usr")[3], par("usr")[1], par("usr")[4], par("usr")[2])
    if(par("ylog") & anchor %in% c(1,3)){base = 10^base}
    if(par("xlog") & anchor %in% c(2,4)){base = 10^base}
    
    # define xy coords
    if(anchor %in% c(1,3)){
        if(joined){
            xx = list(c(x[1]-bhw[1], x[1]-bhw[1], rep(x[-length(x)]+diff(x)/2,each=2), x[length(x)]+bhw[(length(x))], x[length(x)]+bhw[length(x)]))
            yy = list(c(base, rep(y,each=2), base))
        }else{
            xx = yy = {}
            for(i in 1:length(x)){
                xx = c(xx, list(c(x[i]-bhw[i], x[i]-bhw[i], x[i]+bhw[i], x[i]+bhw[i])))
                yy = c(yy, list(c(base, y[i], y[i], base)))
            }
        }
    }else{
        if(joined){
            xx = list(c(base, rep(x,each=2), base))
            yy = list(c(y[1]-bhw[1], y[1]-bhw[1], rep(y[-length(y)]+diff(y)/2,each=2), y[length(y)]+bhw[(length(bhw))], y[length(y)]+bhw[(length(bhw))]))
        }else{
            xx = yy = {}
            for(i in 1:length(y)){
                xx = c(xx, list(c(base, x[i], x[i], base)))
                yy = c(yy, list(c(x[i]-bhw[i], y[i]-bhw[i], y[i]+bhw[i], y[i]+bhw[i])))
            }
        }
    }
    
    # polygon
    for(i in 1:length(xx)){
        polygon(x=xx[[i]], y=yy[[i]], col=col, border=border, ljoin=ljoin, ...)
    }
    
}

