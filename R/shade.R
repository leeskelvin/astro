shade = function(x, ylo, yhi, col = "#00000040", border = NA, lend = 1, ...){
    x = c(x, rev(x))
    y = c(ylo, rev(yhi))
    polygon(x, y, col=col, border=border, lend=lend, ...)
}

