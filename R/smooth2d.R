smooth2d = function(mat, fwhm = 1, sd = fwhm / (2*(sqrt(2*log(2)))), filter = "tophat", size = NA, kern = NA){
    
    # check if any convolution needs to be done
    if(fwhm > 0){
        
        # Smoothing kernels
        if(is.na(kern[1])){
            if(filter == "tophat"){
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
                kern = kern / sum(kern)
            }
            if(filter == "boxcar"){
                fwhm = sd * (2 * (sqrt(2 * log(2))))
                if(is.na(size)){size = ceiling(fwhm)}
                if(size %% 2 == 0){size = size + 1}
                if(size >= dim(mat)[1] | size >= dim(mat)[2]){size = size - 2}
                if(size < 1){stop("Dimensions of 'mat' must be larger than filter.")}
                kern = matrix(1/(size^2), nrow=size, ncol=size)
            }
            if(filter == "gauss"){
                if(is.na(size)){size = 2 * ceiling(qnorm(0.99) * sd) + 1}   # size encompasses 99% of Gaussian
                if(size >= dim(mat)[1] | size >= dim(mat)[2]){size = size - 2}
                if(size < 1){stop("Dimensions of 'mat' must be larger than filter.")}
                kern = gauss2d(size=size, fwhm=fwhm, norm=TRUE, discrete=TRUE)
            }
        }
        
        # 
        
    }else{
        
        out = mat # no convolution
        
    }
    
    # return image
    return(out)
    
}

