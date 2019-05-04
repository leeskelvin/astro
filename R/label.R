label = function(x, y = NULL, labels = seq_along(x), inset = 0, outer = FALSE, adj = NA, col = "black", outline = NA, outline.lwd = 0.25, outline.num = 32, ...){
    
    # calculate xy position
    xy = pos2xy(x=x, y=y, inset=inset, outer=outer, adj=adj)
    
    # text.xyadj
    text.xyadj = function(x, y, labels, xadj, yadj, col, ...){
        text(x=x, y=y, labels=labels, adj=c(xadj,yadj), col=col, ...)
    }
    
    # outline
    if(!is.na(outline)){
        for(i in seq(-pi/4,-pi/4+2*pi,length=outline.num+1)[-1]){
            text.xyadj(x=xy$x+cos(i)*outline.lwd*strwidth("M"), y=xy$y+sin(i)*outline.lwd*strheight("M"), labels=labels, xadj=xy$xadj, yadj=xy$yadj, col=outline, ...)
        }
    }
    
    # plot with text
    text.xyadj(x=xy$x, y=xy$y, labels=labels, xadj=xy$xadj, yadj=xy$yadj, col=col, ...)
    
}

