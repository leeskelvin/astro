aplot = function(..., axes = TRUE, side = 1:4, xat = NULL, yat = NULL, labels = 1:2, tick = TRUE, lwd = 0, lwd.ticks = 1, xformat = NA, yformat = NA, xdigits = 2, ydigits = 2, xnmin = 0, ynmin = 0, xunlog = FALSE, yunlog = FALSE, las = 0, lend = 3, mgp = c(2.5,0.5,0), tcl = 0.5, tcl.min = 0.25, col.box = "grey75", lty.box = "solid", lwd.box = 2){
    
    plot(..., axes=FALSE, las=las, lend=lend, mgp=mgp)
    
    if(axes){
        
        box(col=col.box, lty=lty.box, lwd=lwd.box)
        
        aaxes(side=side, xat=xat, yat=yat, labels=labels, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, xformat=xformat, yformat=yformat, xdigits=xdigits, ydigits=ydigits, xnmin=xnmin, ynmin=ynmin, xunlog=xunlog, yunlog=yunlog, las=las, lend=lend, mgp=mgp, tcl=tcl, tcl.min=tcl.min)
        
    }
    
}

