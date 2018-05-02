# TODO:   Fill missing values from one to another table
# 
# Author: Miguel Alvarez
################################################################################

fill_nas <- function(x, y, x_id, x_var, y_id, y_var) {
	if(missing(y_id)) y_id <- x_id
	if(missing(y_var)) y_var <- x_var
	new_var <- y[match(x[,x_id], y[,y_id]),y_var]
	x[is.na(x[,x_var]),x_var] <- new_var[is.na(x[,x_var])]
	return(x)
}
