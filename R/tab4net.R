#' Prepare inputs for sankeyNetwork()
#' 
#' This function generates lists from column-oriented tables (data frames) for
#' Sankey Networks diagrams produced by
#' \code{\link[networkD3:sankeyNetwork]{networkD3::sankeyNetwork()}}.
#' 
#' 
#' @param formula A formula passed to
#' \code{\link[stats:aggregate]{stats::aggregate()}}.
#' @param data A data frame with the variables included in 'formula'.
#' @param FUN A function passed to
#' \code{\link[stats:aggregate]{stats::aggregate()}}.
#' @param ... Further arguments passed to
#' \code{\link[stats:aggregate]{stats::aggregate()}}.
#' @return A list with two data frames called 'nodes' and 'links'.
#' @examples
#' 
#' require(networkD3)
#' 
#' ## Different values of WET-Health scores for vegetation per land use
#' veg_wh <- tab4net(UseunitID ~ Landuse + WET_veg, WHscores, length)
#' 
#' sankeyNetwork(Links=veg_wh$links, Nodes=veg_wh$nodes, Source = 'source',
#'     Target = 'target', Value = 'value', NodeID = 'name',
#'     units = 'TWh', fontSize = 12, nodeWidth = 30)
#' 
#' @export tab4net
#' 
tab4net <- function(formula, data, FUN, ...) {
	# Some checks
	if(attr(terms(formula), "response") != 1)
		stop("Only one response is allowed in 'formula'.")
	Vars <- rownames(attr(terms(formula), "factors"))
	if(any(!Vars %in% colnames(data)))
		stop(paste("Some of the terms in 'formula' are not included as",
						"variable in 'data'."))
	# Factors to character
	for(i in Vars[-1])
		if(is.factor(data[,i]))
			data[,i] <- paste(data[,i])
	# Prepare lists
	Nodes <- list()
	Links <- list()
	for(i in 1:(length(Vars) - 2)) {
		Links[[i]] <- with(data, aggregate(as.formula(paste(Vars[1], "~",
										Vars[i + 1], "+", Vars[i + 2])),
						FUN=FUN, ...))
		colnames(Links[[i]]) <- c("source", "target", "value")
		Nodes[[i]] <- data.frame(name=c(Links[[i]]$source, Links[[i]]$target),
				stringsAsFactors=FALSE)
	}
	Links <- list(nodes=unique(do.call(rbind, Nodes)),
			links=do.call(rbind, Links))
	for(i in c("source", "target"))
		Links$links[,i] <- with(Links, match(links[,i], nodes$name) - 1)
	return(Links)
}
