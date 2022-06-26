#' @name fill_nas
#'
#' @title Replacing missing values from a reference table.
#'
#' @description
#' Partial filling of tables, when only part of the data is missing.
#'
#' Description to be completed.
#'
#' @param x Data frame with the variable to be completed.
#' @param y Data frame providing values for those missing in 'x'.
#' @param x_id,y_id Character values indicating the variable to be used as
#'     identifier.
#' @param x_var,y_var Character values indicating the name of the variable to
#'     be completed.
#'
#' @return Data frame \code{'x'} with completed values.
#'
#' @author Miguel Alvarez (\email{kamapu78@@gmail.com}).
#'
#' @examples
#'
#' ## No example at the moment.
#'
fill_nas <- function(x, y, x_id, x_var, y_id, y_var) {
  if (missing(y_id)) y_id <- x_id
  if (missing(y_var)) y_var <- x_var
  new_var <- y[match(x[, x_id], y[, y_id]), y_var]
  x[is.na(x[, x_var]), x_var] <- new_var[is.na(x[, x_var])]
  return(x)
}
