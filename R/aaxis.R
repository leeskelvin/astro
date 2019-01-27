aaxis = function(side, at = NULL, labels = TRUE, lwd = 0, lwd.ticks = 1, nmin = 0, tcl = 0.5, tcl.min = 0.25, lend = 3, ...){
    
    # generate tick locations
    if(is.null(at)){at = axTicks(side)}
    
    # major tick marks
    axis(side, at=at, labels=labels, lwd=lwd, lwd.ticks=lwd.ticks, tcl=tcl, lend=lend, ...)
    
    # minor tick marks
    if(nmin > 0){
        
        gap = diff(at)
        xat = c(at[1]-gap[1], at, at[length(at)]+gap[length(gap)])
        ntick = length(xat) + ((nmin*length(xat))-nmin)
        at.all = approx(xat, n=ntick)$y
        at.min = at.all[-which(at.all %in% xat)]
        axis(side, at=at.min, labels=FALSE, lwd=lwd, lwd.ticks=lwd.ticks, tcl=tcl.min, lend=lend, ...)
        
    }
    
}

