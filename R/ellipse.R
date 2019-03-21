ellipse = function(a = 1, e = 0, b = a*(1-e), pa = 0, x0 = 0, y0 = 0, t = seq(0,360,len=101), internal = FALSE){
    
    pa.rad = pa * (pi/180)
    t.rad = t * (pi/180)
    
    if(!internal){
        
        x = a * cos(t.rad) * cos(pa.rad) - b * sin(t.rad) * sin(pa.rad) + x0
        y = a * cos(t.rad) * sin(pa.rad) + b * sin(t.rad) * cos(pa.rad) + y0
        
        return(cbind(x,y))
        
    }else{
        
        aa = ceiling(a)
        xy = expand.grid((-aa-1):(aa+1)+round(x0),(-aa-1):(aa+1)+round(y0))
        colnames(xy) = c("x","y")
        
        x = xy[,"x"] - x0
        y = xy[,"y"] - y0
        
        xp = x * cos(-pa.rad) - y * sin(-pa.rad)
        yp = x * sin(-pa.rad) + y * cos(-pa.rad)
        
        ev = (((xp)^2)/((a)^2)) + (((yp)^2)/((b)^2))
        
        return(xy[ev<=1,])
        
    }
    
}

