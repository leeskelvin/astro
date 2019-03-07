aplot = function(x, y = NULL, z = NULL, type = "p", log = "", ..., axes = TRUE, side = 1:4, xat = NULL, yat = NULL, labels = 1:2, tick = TRUE, lwd = 0, lwd.ticks = 1, xfn = function(x){return(x)}, yfn = function(y){return(y)}, xformat = NA, yformat = NA, xdigits = 2, ydigits = 2, xnmin = 0, ynmin = 0, xunlog = FALSE, yunlog = FALSE, las = 0, lend = 1, mgp = c(2,0.25,0), tcl = 0.4, tcl.min = 0.2, bty = "o", col.box = "grey75", lty.box = "solid", lwd.box = 2, col = NULL, scale.type = "lin", scale.mode = 100, scale.lo = NA, scale.hi = NA, scale.pow = 0.5, col.map = "topo", col.alpha = 1, col.invert = FALSE){
    
    plot(x=x, y=y, type="n", log=log, axes=FALSE, ..., las=las, lend=lend, mgp=mgp)
    
    apoints(x=x, y=y, z=z, type=type, col=col, scale.type=scale.type, scale.mode=scale.mode, scale.lo=scale.lo, scale.hi=scale.hi, scale.pow=scale.pow, col.map=col.map, col.alpha=col.alpha, col.invert=col.invert, ...)
    
    if(axes){
        
        box(bty=bty, col=col.box, lty=lty.box, lwd=lwd.box)
        
        aaxes(side=side, xat=xat, yat=yat, labels=labels, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, xformat=xformat, yformat=yformat, xdigits=xdigits, ydigits=ydigits, xnmin=xnmin, ynmin=ynmin, xunlog=xunlog, yunlog=yunlog, las=las, lend=lend, mgp=mgp, tcl=tcl, tcl.min=tcl.min)
        
    }
    
}

