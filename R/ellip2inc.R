ellip2inc = function(x, emax = 1){
    xx = pmin(pmax(x, 0), emax)
    if(any(x < 0)){xx[x < 0] = NA}
    if(any(x > 1)){xx[x > 1] = NA}
    return(acos(sqrt((((1-xx)^2) - ((1-emax)^2)) / (1-((1-emax)^2)))) * (180/pi))
}

inc2ellip = function(x, emax = 1){
    xx = pmin(pmax(x, 0), 90)
    if(any(x < 0)){xx[x < 0] = NA}
    if(any(x > 90)){xx[x > 90] = NA}
    return(1 - sqrt(((1-((1-emax)^2))*((cos(xx*(pi/180))^2))) + ((1-emax)^2)))
}

