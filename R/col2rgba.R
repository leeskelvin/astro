col2rgba = function(col, alpha = 1){
    return(rgb(t(rbind(col2rgb(col)/255)),alpha=alpha))
}

