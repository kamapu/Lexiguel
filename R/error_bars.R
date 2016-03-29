# TODO:   Low level function adding points with error bars in a plot
# 
# Author: Miguel Alvarez
################################################################################

error_bars <- function(tendency, upper, lower, at, show.tendency=TRUE,
        bar.col="black", lwd=1, lty=1, ...) {
    if(missing(at)) at <- 1:length(tendency)
    if(missing(lower)) lower <- upper
    segments(at, tendency + upper, at, tendency - lower, col=bar.col, lwd=lwd,
            lty=lty)
    if(show.tendency) points(at, tendency, ...)
}
