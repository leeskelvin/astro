corner = function(x, nrow = 6, ncol = 6, corner = "hh"){
    
    ops = strsplit(corner, "")[[1]]
    
    if(any(!ops %in% c("h","t"))){
        stop("input corner not in [hh, ht, th, tt]")
    }
    
    if(ops[1] == "h"){
        out = head(x, n=nrow)
    }else if(ops[1] == "t"){
        out = tail(x, n=nrow)
    }
    
    if(ops[2] == "h"){
        out = head(t(out), n=ncol)
    }else if(ops[2] == "t"){
        out = tail(t(out), n=ncol)
    }
    
    return(t(out))
    
}

