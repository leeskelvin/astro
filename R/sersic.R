sersic = function(r, fluxfrac, mag = 0, n = 1, re = 1, e = 0){
    if(missing(r) & missing(fluxfrac)){
        stop("either r or fluxfrac must be specified!")
    }else if(missing(r)){
        bn1 = qgamma(0.5,2*n)
        bn2 = qgamma(fluxfrac,2*n)
        r = re*((bn2/bn1)^n)
    }
    bn = qgamma(0.5,2*n)
    lumtot = 1*(re^2)*2*pi*n*((exp(bn))/(bn^(2*n)))*gamma(2*n)*(1-e)
    magtot = -2.5*log10(lumtot)
    Ie = 1/(10^(0.4*(mag-magtot)))
    x = bn*(r/re)^(1/n)
    lumr = Ie*lumtot*pgamma(x,2*n)
    intenr = Ie*exp(-bn*(((r/re)^(1/n))-1))
    lumtot = Ie*lumtot
    magr = -2.5*log10(lumr)
    mur = -2.5*log10(intenr)
    muavgr = -2.5*log10(lumr/(pi*r*r*(1-e)))
    return(cbind(R=r, FLUXFRAC=lumr/lumtot, MAG=magr, MU=mur, MUAVG=muavgr))
}

sersic.re2h = function(n, re = 1){
    bn = qgamma(0.5,2*n)
    h = re/(bn^n)
    return(h)
}

sersic.h2re = function(n, h = 1){
    bn = qgamma(0.5,2*n)
    re = h*(bn^n)
    return(re)
}

sersic.r2fluxfrac = function(r, n = 1, r.ref = 1, fluxfrac.ref = 0.5){
    bn = qgamma(fluxfrac.ref,2*n)
    x = bn*(r/r.ref)^(1/n)
    fluxfrac = pgamma(x,2*n)
    return(fluxfrac)
}

sersic.fluxfrac2r = function(fluxfrac, n = 1, r.ref = 1, fluxfrac.ref = 0.5){
    bn1 = qgamma(fluxfrac.ref,2*n)
    bn2 = qgamma(fluxfrac,2*n)
    r = r.ref*((bn2/bn1)^n)
    return(r)
}

sersic.r2mu = function(r, mag = 0, n = 1, re = 1, e = 0){
    bn = qgamma(0.5,2*n)
    lumtot = 1*(re^2)*2*pi*n*((exp(bn))/(bn^(2*n)))*gamma(2*n)*(1-e)
    magtot = -2.5*log10(lumtot)
    Ie = 1/(10^(0.4*(mag-magtot)))
    intenr = Ie*exp(-bn*(((r/re)^(1/n))-1))
    mur = -2.5*log10(intenr)
    return(mur)
}

sersic.mu2r = function(mu, mag = 0, n = 1, re = 1, e = 0){
    bn = qgamma(0.5,2*n)
    lumtot = 1*(re^2)*2*pi*n*((exp(bn))/(bn^(2*n)))*gamma(2*n)*(1-e)
    magtot = -2.5*log10(lumtot)
    Ie = 1/(10^(0.4*(mag-magtot)))
    intenr = 10^(-0.4*mu)
    rmu = re*((((log(intenr/Ie))/(-bn))+1)^n)
    return(rmu)
}

sersic.r2mu2 = function(r, n = 1, re = 1, mu.ref = 0, r.ref = re){
    bn = qgamma(0.5,2*n)
    mu = mu.ref + ((2.5*bn)/log(10))*(((r/re)^(1/n))-((r.ref/re)^(1/n)))
    return(mu)
}

sersic.mu2r2 = function(mu, n = 1, re = 1, mu.ref = 0, r.ref = re){
    bn = qgamma(0.5,2*n)
    r = re * (((((log(10))*(mu-mu.ref))/(2.5*bn)) + ((r.ref/re)^(1/n)))^n)
    return(r)
}

sersic.Ie2Lr = function(r, Ie = 1, n = 1, re = 1){
    innerfunc = function(r, Ie, n, re){
        bn = qgamma(0.5,2*n)
        x = bn * ((r/re)^(1/n))
        incgam = pgamma(x,2*n)
        lum = Ie * re^2 * 2 * pi * n * ((exp(bn)) / ((bn)^(2*n))) * incgam
        return(lum)
    }
    return(sapply(X=r, FUN=innerfunc, Ie=Ie, n=n, re=re))
}

sersic.Lr2Ie = function(r, Lr = 1, n = 1, re = 1){
    innerfunc = function(r, Lr, n, re){
        bn = qgamma(0.5,2*n)
        x = bn * ((r/re)^(1/n))
        incgam = pgamma(x,2*n)
        Ie = Lr / ( re^2 * 2 * pi * n * ((exp(bn)) / ((bn)^(2*n))) * incgam )
        return(Ie)
    }
    return(sapply(X=r, FUN=innerfunc, Lr=Lr, n=n, re=re))
}

sersic.Ie2L = function(Ie, n = 1, re = 1){
    bn = qgamma(0.5,2*n)
    comgam = gamma(2*n)
    lum = Ie * re^2 * 2 * pi * n * ((exp(bn)) / ((bn)^(2*n))) * comgam
    return(lum)
}

sersic.L2Ie = function(L, n = 1, re = 1){
    bn = qgamma(0.5,2*n)
    comgam = gamma(2*n)
    Ie = L / ( re^2 * 2 * pi * n * ((exp(bn)) / ((bn)^(2*n))) * comgam )
    return(Ie)
}

