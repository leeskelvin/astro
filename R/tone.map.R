tone.map = function(input, lo = min(input), hi = max(input), scale.type = "lin", scale.pow = 0.5){
    
    # rescale input to (soft) range (0,1)
    input.01 = (input - lo) / (hi - lo)
    
    # scaling functions
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    input.calib = input.01 * calib
    if(scale.type == "lin"){
        map.lo = 0
        map.hi = calib
    }else if(scale.type == "log"){
        map.lo = log10(scale.pow)
        map.hi = log10(calib + scale.pow)
        input.calib = suppressWarnings(log10(input.calib + scale.pow))
    }else if(scale.type == "pow"){
        map.lo = 0^scale.pow
        map.hi = calib^scale.pow
        input.calib = suppressWarnings(input.calib^scale.pow)
    }else if(scale.type == "atan"){
        map.lo = atan(0)
        map.hi = atan(calib)
        input.calib = suppressWarnings(atan(input.calib))
    }else if(scale.type == "asinh"){
        map.lo = asinh(0)
        map.hi = asinh(calib)
        input.calib = suppressWarnings(asinh(input.calib))
    }else if(scale.type == "sinh"){
        map.lo = sinh(0)
        map.hi = sinh(calib)
        input.calib = suppressWarnings(sinh(input.calib))
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    # return
    out = (input.calib - map.lo) / (map.hi - map.lo)
    return(out)
    
}

tone.unmap = function(probs, lo = 0, hi = 1, scale.type = "lin", scale.pow = 0.5){
    
    # scaling functions
    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
    if(scale.type == "lin"){
        map.lo = 0
        map.hi = calib
    }else if(scale.type == "log"){
        map.lo = log10(scale.pow)
        map.hi = log10(calib + scale.pow)
    }else if(scale.type == "pow"){
        map.lo = 0^scale.pow
        map.hi = calib^scale.pow
    }else if(scale.type == "atan"){
        map.lo = atan(0)
        map.hi = atan(calib)
    }else if(scale.type == "asinh"){
        map.lo = asinh(0)
        map.hi = asinh(calib)
    }else if(scale.type == "sinh"){
        map.lo = sinh(0)
        map.hi = sinh(calib)
    }else{
        stop("unknown aimage scale.type function applied")
    }
    
    # descale
    probs.scale = (probs * (map.hi - map.lo)) + map.lo
    if(scale.type == "lin"){
        probs.descale = probs.scale
    }else if(scale.type == "log"){
        probs.descale = suppressWarnings(10^(probs.scale) - scale.pow)
    }else if(scale.type == "pow"){
        probs.descale = suppressWarnings(probs.scale^(1/scale.pow))
    }else if(scale.type == "atan"){
        probs.descale = suppressWarnings(tan(probs.scale))
    }else if(scale.type == "asinh"){
        probs.descale = suppressWarnings(sinh(probs.scale))
    }else if(scale.type == "sinh"){
        probs.descale = suppressWarnings(asinh(probs.scale))
    }
    
    # return
    out = ((probs.descale / calib) * (hi - lo)) + lo
    return(out)
    
}

#.scale.func = function(input, scale.type, scale.pow, los, his, scale.probs){
#    
#    calib = switch(scale.type, lin=1, log=500, pow=1, atan=5, asinh=10, sinh=3)
#    imdat = input * calib
#    lo = 0 * calib
#    hi = 1 * calib
#    if(scale.type == "lin"){
#        flo = lo
#        fhi = hi
#        input.scaled = imdat
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=(quantile(c(flo,fhi),probs=scale.probs)/calib)))
#        }
#    }else if(scale.type == "log"){
#        slide = 0.5
#        flo = log10(lo + slide)
#        fhi = log10(hi + slide)
#        input.scaled = suppressWarnings(((log10(imdat + slide) - flo) / (fhi - flo)))
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((10^(quantile(c(flo,fhi),probs=scale.probs))-slide)/calib)))
#        }
#    }else if(scale.type == "pow"){
#        flo = lo^scale.pow
#        fhi = hi^scale.pow
#        input.scaled = suppressWarnings(((imdat^scale.pow - flo) / (fhi - flo)))
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((quantile(c(flo,fhi),probs=scale.probs)^(1/scale.pow))/calib)))
#        }
#    }else if(scale.type == "atan"){
#        flo = atan(lo)
#        fhi = atan(hi)
#        input.scaled = suppressWarnings(((atan(imdat) - flo) / (fhi - flo)))
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((tan(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
#        }
#    }else if(scale.type == "asinh"){
#        flo = asinh(lo)
#        fhi = asinh(hi)
#        input.scaled = suppressWarnings(((asinh(imdat) - flo) / (fhi - flo)))
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((sinh(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
#        }
#    }else if(scale.type == "sinh"){
#        flo = sinh(lo)
#        fhi = sinh(hi)
#        input.scaled = suppressWarnings(((sinh(imdat) - flo) / (fhi - flo)))
#        ref = {}
#        for(i in 1:length(los)){
#            ref = rbind(ref,quantile(c(los[i],his[i]), probs=((asinh(quantile(c(flo,fhi),probs=scale.probs)))/calib)))
#        }
#    }else{
#        stop("unknown aimage scale.type function applied")
#    }
#    
#    return(list(input.scaled=input.scaled, ref=ref))
#    
#}

