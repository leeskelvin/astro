shade = function(x, ylo, yhi, col = hsv(alpha=0.5), border = NA, ...){
    x = c(x, rev(x))
    y = c(ylo, rev(yhi))
    polygon(x, y, col=col, border=border, ...)
}

