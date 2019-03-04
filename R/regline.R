regline = function(y, ...){
    
    usr = par("usr")[1:2]
    if(par("xlog")){usr = 10^usr}
    x = seq(usr[1], usr[2], len=1001)
    
    if(class(y) %in% c("lm")){
        newdata = data.frame(x)
        colnames(newdata) = names(y$coefficients)[2]
        fx = predict(y, newdata=newdata)
    }else if(class(y) == "function"){
        
    }else{
        stop("unrecognised input")
    }
    
    
    
}

