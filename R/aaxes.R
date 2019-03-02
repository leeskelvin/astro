aaxes = function(side = 1:4, xat = NULL, yat = NULL, labels = 1:2, tick = TRUE, lwd = 0, lwd.ticks = 1, xfn = function(x){return(x)}, yfn = function(y){return(y)}, xformat = NA, yformat = NA, xdigits = 2, ydigits = 2, xnmin = 0, ynmin = 0, xunlog = FALSE, yunlog = FALSE, las = 0, lend = 3, mgp = c(2,0.25,0), tcl = 0.5, tcl.min = 0.25, ...){
    
    # loop
    for(i in 1:4){
        
        if(i %in% side){
            
            # labels
            ilab = ifelse(i %in% ilab, TRUE, FALSE)
            
            # x/y splitting
            if(i %in% c(1,3)){
                iat = xat
                ifn = xfn
                iformat = xformat
                idigits = xdigits
                inmin = xnmin
                iunlog = xunlog
            }else{
                iat = yat
                ifn = yfn
                iformat = yformat
                idigits = ydigits
                inmin = ynmin
                iunlog = yunlog
            }
            
            # axis
            aaxis(side=i, at=iat, labels=ilab, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, fn=ifn, format=iformat, digits=idigits, nmin=inmin, unlog=iunlog, las=las, lend=lend, mgp=mgp, tcl=tcl, tcl.min=tcl.min, ...)
            
        }
        
    }
    
}

