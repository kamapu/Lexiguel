#' @name get_s_groups
#'
#' @title Retrieve significant groups after pairwise post-hoc tests
#'
#' @description
#' Retrieve significant groups after a pairwise post-hoc test.
#'
#' @param g A character vector with the names of the groups.
#' @param m A numeric vector with the average values for the tested variable in
#'   each group. This vector is used to sort the groups (from bigger to smaller
#'   values) and assign the respective letters.
#' @param t_tab A data frame with the results of pairwise contrast. Two columns
#'   are mandatory in this table, namely 'group1' and 'group2' containing the
#'   contrasted groups. Note that the values in these columns need to match 'g'.
#' @param criterion An expression (symbol) used to define groups belonging to
#'   the same significant group, for instance a cut level for a p-value.
#'
#' @return
#' A data frame with the names of the groups and their respective significant
#' groups.
#'
#' @export
get_s_groups <- function(g, m, t_tab, criterion) {
  if (length(g) != length(m)) {
    stop("Argument 'g' have to be of the same length as argument 'm'")
  }
  g <- g[order(m, decreasing = TRUE)]
  idx <- 1:length(g)
  if (!all(c("group1", "group2") %in% names(t_tab))) {
    stop("Columns 'group1' and 'group2' are mandatory in 't_tab'")
  }
  t_tab <- list(t_tab, t_tab)
  t_tab[[2]]$group1 <- t_tab[[1]]$group2
  t_tab[[2]]$group2 <- t_tab[[1]]$group1
  t_tab <- do.call(rbind, t_tab)
  t_tab$idx1 <- idx[match(t_tab$group1, g)]
  t_tab$idx2 <- idx[match(t_tab$group2, g)]
  criterion <- substitute(criterion)
  sig_gr <- list()
  i <- 0
  repeat {
    i <- i + 1
    aux_gr <- with(t_tab, unique(idx1[min(idx1)]))
    aux_tab <- t_tab[t_tab$idx1 == aux_gr, ]
    sig_gr[[i]] <- c(aux_gr, aux_tab$idx2[eval(
      criterion,
      aux_tab, parent.frame()
    )])
    t_tab <- t_tab[!t_tab$idx1 %in% sig_gr[[i]], ]
    if (nrow(t_tab) == 0) break
  }
  Gr <- list()
  for (i in 1:length(sig_gr)) {
    Gr[[i]] <- c(letters[i], "")[match(
      idx %in% sig_gr[[i]],
      c(TRUE, FALSE)
    )]
  }
  return(data.frame(group = g, sig_gr = do.call(paste0, Gr)))
}
