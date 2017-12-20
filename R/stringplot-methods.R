# TODO:   Function stringplot and associated S3 methods
# 
# Author: Miguel Alvarez
################################################################################

# basic function as low level
strp <- function(x, lwd=1, maxwidth=1, col="black", at) {
	if(!(is.integer(x)))
		stop("'x' have to be of class integer", call.=FALSE)
	x <- summary(as.factor(x))
	y <- as.integer(names(x))
	x <- x/sum(x)
	x0 <- at - x*maxwidth/2
	x1 <- at + x*maxwidth/2
	# plotting
	segments(x0=x0, y0=y, x1=x1, y1=y, col=col, lwd=lwd)
}

# set generic function and default function
stringplot <- function(x, ...) UseMethod("stringplot", x)

stringplot.default <- function(x, ...)
	cat("The class of 'x' does not match any method for 'stringplot'", "\n")

# the high level function for a single stringplot (low level with at argument)
stringplot.numeric <- function(x, lwd=1, maxwidth=1, col="black", at, bty="n",
		xaxt="n", ylim, xlab="", ylab="classes", ...) {
	if(!is.integer(x)) x <- as.integer(x)
	if(!missing(at)) {
		strp(x, lwd, maxwidth, col, at)
	} else {
		at <- 1
		if(missing(ylim)) ylim <- range(as.integer(x))
		plot(NA, bty=bty, xaxt=xaxt, ylim=ylim, xlab=xlab, ylab=ylab, type="n",
				...)
		strp(x, lwd, maxwidth, col, at)
	}
}

## # The same for factor
## stringplot.factor <- function(x, ...) {
##     x <- as.integer(paste(x))
##     stringplot(x, ...)
## }

# method for data frame
stringplot.data.frame <- function(x=data.frame(), what, by, at, col, lwd=2,
		maxwidth=1, xlab="", ylab="classes", xlim, ylim, xaxt="s", ...) {
	what <- substitute(what)
	what <- eval(what, x, parent.frame())
	if(missing(ylim)) ylim <- range(as.integer(what))
	by <- substitute(by)
	by <- eval(by, x, parent.frame())
	if(!is.factor(by)) by <- as.factor(by)
	what <- split(what, by)
	if(missing(col)) col <- "black"
	col <- rep_len(col, length(what))
	if(missing(at)) {
		at <- 1:length(what)
		if(xaxt == "s") xaxt1 <- "n"
		if(missing(xlim)) xlim <- c(0.5,length(what) + 0.5)
		plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, xaxt=xaxt1,
				type="n", ...)
		if(xaxt == "s") axis(1, at=at, labels=names(what))
	} else at <- at:(at + length(what) - 1)
	for(i in at) stringplot(x=what[[i]], at=at[i], col=col[i], lwd=lwd,
				maxwidth=maxwidth)
}

# method for formula
stringplot.formula <- function(formula, data=data.frame(), ...) {
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
	stringplot(data, what, y, ...)
}
