get.fitskey = function(key, hdr){
    
    # setup results vector
    vals = rep(NA, length(key))
    
    # get existing keys
    if(any(key %in% hdr[,"key"])){
        
        # determine order of vectors
        inhdr = which(hdr[,"key"] %in% key)
        inkey = which(key %in% hdr[,"key"])
        matched = inhdr[match(key[inkey],hdr[inhdr,"key"])]
        
        # get key values
        vals[inkey] = hdr[matched,"value"]
        
    }
    
    # return results
    return(as.vector(vals))
    
}

