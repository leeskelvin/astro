col2hex = function(col, alpha = NA){
    rgbamat = t(rbind(col2rgb(col,alpha=TRUE)/255))
    if(!is.na(alpha)){rgbamat[,"alpha"]=alpha}
    return(rgb(red=rgbamat[,"red"],green=rgbamat[,"green"],blue=rgbamat[,"blue"],alpha=rgbamat[,"alpha"]))
}

