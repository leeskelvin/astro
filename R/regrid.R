# function to subdivide a matrix into multiple sub pixels
regrid = function(data, fact = 0.5, norm = FALSE){
    
    # factor splitting
    if(length(fact) == 1){
        fact = c(fact,fact)
    }
    
    # function to manipulate (expand/contract) a single dimension of a matrix
    matmanip = function(data, dim = 1, fact = 2){
        
        # factor integerisation (factor = out/in)
        if(fact < 1){
            fact = 1/round(1/fact)
        }else{
            fact = round(fact)
        }
        
        # dimension fix?
        inputdata = data
        if(dim!=1){
            inputdata = t(inputdata)
        }
        
        # dimensions and data arrays
        oldxdim = dim(inputdata)[1]
        oldydim = dim(inputdata)[2]
        olddim = oldxdim
        inputwt = matrix(1, nrow=oldxdim, ncol=oldydim)
        newdim = ceiling(olddim*fact)
        tempdata = matrix(0, nrow=newdim, ncol=oldydim)
        tempwt = matrix(1, nrow=newdim, ncol=oldydim)
        fact = newdim / olddim
        
        # expand?
        if(fact > 1){
            
            oldindex = rep(1:olddim, each=fact)
            newindex = 1:newdim
            tempdata[newindex,] = inputdata[oldindex,]
            tempwt = tempwt * fact
            if(dim!=1){
                tempdata = t(tempdata)
                tempwt = t(tempwt)
            }
            
            # normalise
            tempdata = tempdata / tempwt
            
        # contract
        }else if(fact < 1){
            
            mult = olddim / newdim
            mdiff = (mult - round(mult))
            tempdata = inputdata
            #tempwt = inputwt
            # add extra rows if needed
            if(mdiff != 0){
                tempdim = newdim * ceiling(mult)
                newlines = tempdim - olddim
                tempdata = padmatrix(inputdata, xlo=0, ylo=0, xhi=newlines, yhi=0)
                #tempwt = padmatrix(inputwt, xlo=0, ylo=0, xhi=newlines, yhi=0)
                oldxdim = dim(tempdata)[1]
                oldydim = dim(tempdata)[2]
                olddim = oldxdim
                fact = newdim / olddim
            }
            cellsize = 1/fact
            nx = cellsize
            ny = newdim
            nz = dim(tempdata)[2]
            tempdata = colSums(array(tempdata, dim=c(nx, ny, nz)))
            #tempwt = colSums(array(tempwt, dim=c(nx, ny, nz))) / cellsize
            if(dim!=1){
                tempdata = t(tempdata)
                #tempwt = t(tempwt)
            }
            
        # no change?
        }else{
            
            tempdata = data
            #tempwt = inputwt
            
        }
        
        # return data
        return(tempdata)
        
    }
    
    # manipulate x first
    data1 = matmanip(data, dim=1, fact=fact[1])
    
    # manipulate y second
    data2 = matmanip(data1, dim=2, fact=fact[2])
    
    # normalise?
    if(norm){
        data2 = data2 * (fact[1] * fact[2])
    }
    
    # return data
    return(data2)
    
}

