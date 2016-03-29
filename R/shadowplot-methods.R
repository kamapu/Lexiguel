# TODO:   Shadows added to stringplots
# 
# Author: Miguel Alvarez
################################################################################

# low level function applied to integer
shadows <- function(x, start, end, at, maxwidth=1, col="grey",
        border=NA, ...) {
    if(!is.integer(x))
        stop("'x' have to be of class integer", call.=FALSE)
    x <- summary(as.factor(x))
    x <- x/sum(x)
    if(missing(start)) start <- min(as.numeric(names(x)))
    if(missing(end)) end <- max(as.numeric(names(x)))
    shadows <- data.frame(class=c(start:end),
            freq=x[match(start:end, as.numeric(names(x)))],
            row.names=c(start:end))
    shadows$group <- cumsum(as.numeric(is.na(shadows[,2])))
    shadows <- shadows[!is.na(shadows$freq),]
    shadows <- split(shadows[,c("class","freq")], shadows$group)
    ## shadows <- split(shadows, cumsum(as.numeric(is.na(shadows[,2]))))
    for(i in 1:length(shadows)) {
        shadows[[i]] <- shadows[[i]][!is.na(shadows[[i]]$freq),]
        if(min(shadows[[i]]$class) != start) {
            shadows[[i]] <- do.call(rbind,
                    list(data.frame(class=min(shadows[[i]]$class) - 0.5,
                                    freq=0), shadows[[i]]))
        }
        if(max(shadows[[i]]$class) != end) {
            shadows[[i]] <- do.call(rbind, list(shadows[[i]],
                            data.frame(class=max(shadows[[i]]$class) + 0.5,
                                    freq=0)))
        }
    }
    # Now one polygon per element in the list
    for(i in 1:length(shadows)) {
        polygon(c(shadows[[i]]$freq*maxwidth/2 + at,
                        at - rev(shadows[[i]]$freq)*maxwidth/2),
                c(shadows[[i]]$class,rev(shadows[[i]]$class)), col=col,
                border=border, ...)
    }
}

# generic function and default function
shadowplot <- function(x, ...) UseMethod("shadowplot", x)

shadowplot.default <- function(x, ...)
    cat("The class of 'x' does not match any method for 'shadowplot'", "\n")

# the high level function for a single stringplot (low level with at argument)
shadowplot.integer <- function(x, start, end, at, maxwidth=1, col="grey",
        border=NA, bty="n", xaxt="n", ylim, xlab="", ylab="classes", ...) {
    if(missing(start)) start <- min(x)
    if(missing(end)) end <- max(x)
    if(!missing(at)) {
        shadows(x, start, end, at, maxwidth, col, border)
    } else {
        if(missing(ylim)) ylim <- c(start,end)
        plot(NA, bty=bty, xaxt=xaxt, ylim=ylim, xlab=xlab, ylab=ylab, type="n",
                ...)
        shadows(x, start, end, at=1, maxwidth, col, border)
    }
}

# The same for factor
shadowplot.factor <- function(x, ...) {
    x <- as.integer(x)
    shadowplot(x, ...)
}

# method for data frame
shadowplot.data.frame <- function(x=data.frame(), what, by, start, end, at,
        maxwidth=1, col="grey", border=NA, xlab="", ylab="classes", xlim, ylim,
        xaxt="s", ...) {
    what <- substitute(what)
    what <- eval(what, x, parent.frame())
    by <- substitute(by)
    by <- eval(by, x, parent.frame())
    if(!is.factor(by)) by <- as.factor(by)
    what <- split(what, by)
    col <- rep_len(col, length(what))
    border <- rep_len(border, length(what))
    # Format start and end
    if(missing(start)) start <- unlist(lapply(what, min))
    if(missing(end)) end <- unlist(lapply(what, max))
    start <- rep_len(start, length(what))
    end <- rep_len(end, length(what))
    if(missing(at)) {
        at <- 1:length(what)
        if(xaxt == "s") xaxt1 <- "n"
        if(missing(xlim)) xlim <- c(0.5,length(what) + 0.5)
        if(missing(ylim)) ylim <- c(min(start), max(end))
        plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, xaxt=xaxt1,
                type="n")
        if(xaxt == "s") axis(1, at=at, labels=names(what))
    } else at <- at:(at + length(what) - 1)
    for(i in at) {
        shadowplot(x=what[[i]], start=start[i], end=end[i], at=at[i],
                maxwidth=maxwidth, col=col[i], border=border[i])
    }
}

# method for formula
shadowplot.formula <- function(formula, data=data.frame(), ...) {
    if(missing(formula) || !inherits(formula, "formula")) 
        stop("'formula' missing or incorrect")
    if(length(formula) != 3L) 
        stop("'formula' must have both left and right hand sides")
    what <- as.character(formula)[2]
    what <- eval(substitute(get(what)), data, parent.frame())
    by <- attr(terms(formula), "term.labels")
    y <- list()
    for(i in by) {
        y[[i]] <- eval(substitute(get(i)), data, parent.frame())
        if(!is.factor(y[[i]])) y[[i]] <- as.factor(y[[i]])
    }
    y <- interaction(y)
    shadowplot(data, what, y, ...)
}
