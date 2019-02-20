aaxes = function(side = 1:4, xat = NULL, yat = NULL, labels = 1:2, tick = TRUE, lwd = 0, lwd.ticks = 1, xformat = NA, yformat = NA, xdigits = 2, ydigits = 2, xnmin = 0, ynmin = 0, xunlog = FALSE, yunlog = FALSE, las = 0, lend = 3, mgp = c(2.5,0.5,0), tcl = 0.5, tcl.min = 0.25, col.box = "grey75", lty.box = "solid", lwd.box = 2, ...){
    
    # loop
    for(i in 1:4){
        
        if(i %in% side){
            
            # labels
            if(i %in% labels){
                ilab = TRUE
            }else{
                ilab = FALSE
            }
            
            # x/y splitting
            if(i %in% c(1,3)){
                iat = xat
                iformat = xformat
                idigits = xdigits
                inmin = xnmin
                iunlog = xunlog
            }else{
                iat = yat
                iformat = yformat
                idigits = ydigits
                inmin = ynmin
                iunlog = yunlog
            }
            
            # axis
            aaxis(side=i, at=iat, labels=ilab, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, format=iformat, digits=idigits, nmin=inmin, unlog=iunlog, las=las, lend=lend, mgp=mgp, tcl=tcl, tcl.min=tcl.min, ...)
            
        }
        
    }
    
    box(col=col.box, lty=lty.box, lwd=lwd.box)
    
}

