aaxis = function(side, at = NULL, labels = TRUE, tick = TRUE, lwd = 0, lwd.ticks = 1, format = NA, digits = 2, nmin = 0, unlog = FALSE, las = 0, lend = 3, mgp = c(2.5,0.5,0), tcl = 0.5, tcl.min = 0.25, ...){
    
    # generate tick locations
    if(is.null(at)){at = axTicks(side)}
    stepby = sign(at[2]-at[1])
    
    # logged axes tick locations
    islogged = FALSE
    if(side %in% c(1,3)){
        if(par("xlog")){
            at = 10^((floor(par("usr"))[1]-stepby):(ceiling(par("usr"))[2]+stepby))
            islogged = TRUE
        }else if(unlog){
            at = (floor(par("usr"))[1]-stepby):(ceiling(par("usr"))[2]+stepby)
        }
    }
    if(side %in% c(2,4)){
        if(par("ylog")){
            at = 10^((floor(par("usr"))[3]-stepby):(ceiling(par("usr"))[4]+stepby))
            islogged = TRUE
        }else if(unlog){
            at = (floor(par("usr"))[3]-stepby):(ceiling(par("usr"))[4]+stepby)
        }
    }
    
    # major tick marks
    axis(side, at=at, labels=FALSE, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl, ...)
    
    # tick labels
    if(labels){
        for(i in 1:length(at)){
            if(unlog){
                ilab = 10^at[i]
            }else{
                ilab = at[i]
            }
            if(!is.na(format)){
                if(format == "p"){
                    ilab = bquote(paste(10^.(log10(ilab))))
                }else{
                    ilab = formatC(ilab, format=format, digits=digits)
                }
            }
            axis(side, at=at[i], labels=ilab, tick=FALSE, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl, ...)
        }
    }
    
    # minor tick marks
    if(nmin > 0 | islogged | unlog){
        
        if(islogged){
            xat = c(log10(at[1])-stepby, log10(at), log10(at[length(at)])+stepby)
            at.all = expand.grid(log10(2:9), xat)
            at.min = 10^(at.all[,1] + at.all[,2])
        }else if(unlog){
            xat = c(at[1]-stepby, at, at[length(at)]+stepby)
            at.all = expand.grid(log10(2:9), xat)
            at.min = at.all[,1] + at.all[,2]
        }else{
            gap = diff(at)
            xat = c(at[1]-gap[1], at, at[length(at)]+gap[length(gap)])
            ntick = length(xat) + ((nmin*length(xat))-nmin)
            at.all = approx(xat, n=ntick)$y
            at.min = at.all[-which(at.all %in% xat)]
        }
        axis(side, at=at.min, labels=FALSE, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl.min, ...)
        
    }
    
}

