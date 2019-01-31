strip = function(x, strip=" "){
    
    # workhorse function
    sfunc = function(x, strip){
        x = strsplit(x,"")[[1]]
        for(i in 1:length(strip)){
            s = strip[i]
            good = which(x!=s)
            if(length(good)>0){
                x = x[min(good):max(good)]
            }else{
                x = ""
            }
        }
        x = paste(x,collapse="")
        return(x)
    }
    
    # vectorize
    stripped = as.character(Vectorize(sfunc, vectorize.args=c("x"))(x=x, strip=strip))
    
    # return results
    return(stripped)
    
}

