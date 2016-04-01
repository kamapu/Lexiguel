# TODO:   Plotting function for sloss outputs
# 
# Author: Miguel Alvarez
################################################################################

plot.SLOSS <- function(x, y=NULL, sl.lty=2, sl.lwd=1, sl.col="black", ls.lty=1,
        ls.lwd=1, ls.col="black", show.index=TRUE, digits.index=2, cex.index=1,
        pos.index=c(0.05,0.95), show.legend=FALSE, pos.legend="bottomright",
        bty.legend="o", main="SLOSS curves",...) {
    with(x$SL, plot(area, species, type="l", lty=sl.lty, lwd=sl.lwd, col=sl.col,
                    main=main, ...))
    with(x$LS, lines(area, species, lty=ls.lty, lwd=ls.lwd, col=ls.col))
    if(show.legend) {
        legend(pos.legend, lty=c(sl.lty,ls.lty), lwd=c(sl.lwd,ls.lwd),
                legend=c("small to large","large to small"), bty=bty.legend)
    }
    if(show.index) {
        with(x$SL, text(max(area)*pos.index[1], max(species)*pos.index[2],
                        labels=paste("SLOSS-index =",
                                round(x$Index, digits.index)),
                        cex=cex.index, pos=4))
    }
}
