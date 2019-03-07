aaxis = function(side, at = NULL, labels = TRUE, tick = TRUE, lwd = 0, lwd.ticks = 1, fn = function(x){return(x)}, format = NA, digits = 2, nmin = 0, unlog = FALSE, las = 0, lend = 1, mgp = c(2,0.25,0), tcl = 0.4, tcl.min = 0.2, ...){
    
    # generate tick locations
    xusr = ifelse(rep(par("xlog"),2),10^par("usr")[1:2],par("usr")[1:2])
    yusr = ifelse(rep(par("ylog"),2),10^par("usr")[3:4],par("usr")[3:4])
    usr.real = switch(side, xusr, yusr, xusr, yusr)
    usr.reals = approx(usr.real, n=1001)$y
    usr.funcs = suppressWarnings(fn(usr.reals))
    bad = which(is.na(usr.funcs))
    if(length(bad) > 0){usr.reals = usr.reals[-bad]; usr.funcs = usr.funcs[-bad]}
    usr.func = xusr.func = range(usr.funcs, na.rm=T)
    if(is.null(at)){at = pretty(usr.func)}
    stepsign = sign(at[2]-at[1])
    
    # logged axes tick locations
    islogged = ifelse((side%in%c(1,3)&par("xlog"))|(side%in%c(2,4)&par("ylog")), TRUE, FALSE)
    if(islogged){
        at = 10^((floor(log10(usr.func[1]))-stepsign) : (ceiling(log10(usr.func[2]))+stepsign))
    }else if(unlog){
        at = (floor(usr.func[1])-stepsign) : (ceiling(usr.func[2])+stepsign)
    }
    if(islogged | unlog){
        usr.reals = approx(x=c(usr.real[1]/10,usr.real[2]*10), n=1001)$y
        xusr.funcs = suppressWarnings(fn(usr.reals))
        bad = which(is.na(xusr.funcs))
        if(length(bad) > 0){usr.reals = usr.reals[-bad]; xusr.funcs = xusr.funcs[-bad]}
        xusr.func = range(xusr.funcs, na.rm=T)
    }
    
    # major tick marks
    if(any(at < min(xusr.func))){at = at[-which(at < min(xusr.func))]} # req. for inv. interval below
    if(any(at > max(xusr.func))){at = at[-which(at > max(xusr.func))]} # req. for inv. interval below
    fn.inv = ifelse(identical(usr.real,usr.func), fn, inverse(fn=fn, interval=usr.reals))
    at.real = fn.inv(at)
    axis(side, at=at.real, labels=FALSE, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl, ...)
    
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
            axis(side, at=at.real[i], labels=ilab, tick=FALSE, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl, ...)
        }
    }
    
    # minor tick marks
    if(nmin > 0 | islogged | unlog){
        
        if(islogged){
            xat = c(log10(at[1])-stepsign, log10(at), log10(at[length(at)])+stepsign)
            at.all = expand.grid(log10(2:9), xat)
            at.min = 10^(at.all[,1] + at.all[,2])
        }else if(unlog){
            xat = c(at[1]-stepsign, at, at[length(at)]+stepsign)
            at.all = expand.grid(log10(2:9), xat)
            at.min = at.all[,1] + at.all[,2]
        }else{
            gap = diff(at)
            xat = c(at[1]-gap[1], at, at[length(at)]+gap[length(gap)])
            ntick = length(xat) + ((nmin*length(xat))-nmin)
            at.all = approx(xat, n=ntick)$y
            at.min = at.all[-which(at.all %in% xat)]
        }
        if(any(at.min < min(usr.func))){at.min = at.min[-which(at.min < min(usr.func))]}
        if(any(at.min > max(usr.func))){at.min = at.min[-which(at.min > max(usr.func))]}
        at.min.real = fn.inv(at.min)
        axis(side, at=at.min.real, labels=FALSE, tick=tick, lwd=lwd, lwd.ticks=lwd.ticks, las=las, lend=lend, mgp=mgp, tcl=tcl.min, ...)
        
    }
    
}

