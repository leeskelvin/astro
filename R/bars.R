bars = function(x, y, width = 1, anchor = 1, joined = TRUE, ...){
    
    bw = rep(width, length(x))[1:length(x)]
    
    if(anchor %in% c(1,3)){
        
        if(joined){
            xx = c(x[1]-bw[1]/2, x[1]-bw[1]/2, rep(x+bw/2,each=2))
        }else{
            xx = c(rep(x-bw/2, each=2), x[length(x)]+bw/2)
        }
        
    }else{
        
        
        
    }
    
    
    
    if(width < 0){ # contiguous region
        
        bw = -width/2
        xx = c( x[1]-bw, rep(x[-length(x)]+diff(x)/2,each=2), x[length(x)]+bw )
        xx = c( xx, rev(xx) )
        yy = c( rep(y,each=2), rep(par("usr")[3],len=2*length(y)) )
        polygon(x=xx, y=yy, ...)
        
    }else{ # stand alone bars
        
        cfr = par("pin") / (par("cin")[2]) # number of fractional character heights
        pxy = diff(par("usr"))[c(1,3)] # plot region in xy coordinates
        sxy = (pxy / cfr) # character step size in xy coordinates
        bw = sxy[1] * width/2
        
        for(i in 1:length(x)){
            polygon(x=c(x[i]-bw,x[i]-bw,x[i]+bw,x[i]+bw), y=c(par("usr")[3],y[i],y[i],par("usr")[3]), ...)
        }
        
    }
    
}

