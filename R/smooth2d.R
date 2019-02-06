smooth2d = function(mat, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2)))), filter = "tophat", size = NA, kern = NA, reflect = TRUE){
    
    # mat = matrix(1:25,5); mat[3,2] = NA; fwhm = 1; sd = fwhm / (2*(sqrt(2*log(2)))); filter = "gauss"; size = NA; kern = NA; reflect = TRUE
    
    # setup
    out = mat.orig = mat
    
    # check if any convolution needs to be done
    if(fwhm > 0){
        
        # Smoothing kernels
        if(is.na(kern[1])){
            if(filter == "gauss"){
                if(is.na(size)){size = 2 * ceiling(qnorm(0.99) * sd) + 1}   # size encompasses 99% of Gaussian
                if(size >= dim(mat)[1] | size >= dim(mat)[2]){size = size - 2}
                if(size < 1){stop("Dimensions of 'mat' must be larger than filter.")}
                kern = gauss2d(size=size, fwhm=fwhm, norm=FALSE, discrete=TRUE)
            }else if(filter == "boxcar"){
                fwhm = sd * (2 * (sqrt(2 * log(2))))
                if(is.na(size)){size = ceiling(fwhm / 2) * 2 + 1}
                if(size >= dim(mat)[1] | size >= dim(mat)[2]){size = size - 2}
                if(size < 1){stop("Dimensions of 'mat' must be larger than filter.")}
                kern = matrix(1, nrow=size, ncol=size)
            }else if(filter == "tophat"){
                fwhm = sd * (2 * (sqrt(2 * log(2))))
                if(is.na(size)){size = ceiling(fwhm / 2) * 2 + 1}
                if(size >= dim(mat)[1] | size >= dim(mat)[2]){size = size - 2}
                if(size < 1){stop("Dimensions of 'mat' must be larger than filter.")}
                kern = matrix(0, nrow=size, ncol=size)
                cens = (size + 1) / 2
                seqs = seq(-cens+1, cens-1, by=1)
                xyseq = expand.grid(seqs, seqs)
                rad = matrix(sqrt(xyseq[,1]^2 + xyseq[,2]^2), nrow=size, ncol=size)
                kern[rad <= fwhm/2] = 1
            }
        }
        
        # final size check and normalization (image size > kernel size, kernel must be square, and sum to 1)
        mindim = min(dim(kern))
        if(dim(mat)[1] <= mindim | mindim < dim(kern)[1]){
            kmid = (dim(kern)[1] + 1) / 2
            kseq = (kmid-min((dim(mat)[1]%/%2)-1,(mindim%/%2))) : (kmid+min((dim(mat)[1]%/%2)-1,(mindim%/%2)))
            kern = kern[kseq,]
        }
        if(dim(mat)[2] <= mindim | mindim < dim(kern)[2]){
            kmid = (dim(kern)[2] + 1) / 2
            kseq = (kmid-min((dim(mat)[2]%/%2)-1,(mindim%/%2))) : (kmid+min((dim(mat)[2]%/%2)-1,(mindim%/%2)))
            kern = kern[,kseq]
        }
        kern = kern / sum(kern)
        
        # NA values
        napos = {}
        if(any(is.na(mat))){
            napos = which(is.na(mat))
            mat[napos] = 0
        }
        
        # reflective boundary?
        if(reflect){
            xyadd = ceiling(mindim / 2)
            mat = padmatrix(mat, pad=xyadd, value=0)
        }
        
        # transformation array
        xhalf = dim(mat)[1] %/% 2
        yhalf = dim(mat)[2] %/% 2
        khalf = dim(kern)[1] %/% 2
        tran = matrix(0, nrow = dim(mat)[1], ncol = dim(mat)[2])
        tran[((xhalf - khalf):(xhalf + khalf)), ((yhalf - khalf):(yhalf + khalf))] = kern
        tran = fft(tran)
        
        # convert mat into 3D array
        dim(mat) = c(dim(mat), 1)
        area = prod(dim(mat)[1:2])
        
        # cyclical wrapping/mapping indices
        index1 = c(xhalf:dim(mat)[1], 1:(xhalf - 1))
        index2 = c(yhalf:dim(mat)[2], 1:(yhalf - 1))
        
        # perform FFT, returning real parts only
        out = apply(mat, 3, function(z){
            dim(z) = dim(mat)[1:2]
            Re(fft(fft(z) * tran, inverse = TRUE) / area)[index1,index2]
        })
        dim(out) = dim(mat)[1:2]
        
        # unreflect (if required)
        if(reflect){
            xout = c( (1:xyadd), (dim(mat.orig)[1]+xyadd+(1:xyadd)) )
            xin = c( ((xyadd:1)+xyadd), (dim(mat.orig)[1]+(xyadd:1)) )
            out[xin,] = out[xin,] + out[xout,]
            yout = c( (1:xyadd), (dim(mat.orig)[2]+xyadd+(1:xyadd)) )
            yin = c( ((xyadd:1)+xyadd), (dim(mat.orig)[2]+(xyadd:1)) )
            out[,yin] = out[,yin] + out[,yout]
            out = padmatrix(out, pad=-xyadd)
        }
        
        # NA values?
        if(length(napos) > 0){
            out[napos] = NA
        }
        
    }
    
    # return image
    return(out)
    
}

