label = function(x, y = NULL, labels = seq_along(x), inset = 0.5, adj = NA, col = "grey50", outline = NA, outline.lwd = 0.25, outline.num = 32, ...){
    
    # calculate xy position
    xy = pos2xy(x=x, y=y, inset=inset, adj=adj)
    
    # outline
    if(!is.na(outline)){
        for(i in seq(-pi/4,-pi/4+2*pi,length=outline.num+1)[-1]){
            text(x=xy$x+cos(i)*outline.lwd*strwidth("M"), y=xy$y+sin(i)*outline.lwd*strheight("M"), labels=labels, adj=xy$adj, col=outline, ...)
        }
    }
    
    # plot with text
    text(x=xy$x, y=xy$y, labels=labels, adj=xy$adj, col=col, ...)
    
}

