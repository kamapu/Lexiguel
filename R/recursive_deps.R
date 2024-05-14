#' @title List Package Dependencies Recursively
#'
#' @description
#' In order to recognize all requirements that needs to be installed in a fresh
#' system, you need to collect information about dependencies recursively.
#' This package is a wrapper for [package_dependencies()].
#'
#' The packages do not necessarily need to be installed locally, they only need
#' to be available from CRAN.
#'
#'
#' @param packages A character vector with names of packages.
#' @param ... Further arguments passed to [package_dependencies()]
#'
#' @return
#' A character vector with the name of all dependencies required recursively.
#'
#' @export
recursive_deps <- function(packages, ...) {
  all_deps <- deps <- unique(do.call(c, package_dependencies(packages, ...)))
  repeat {
    deps <- deps[!deps %in% packages]
    if (!length(deps)) {
      break
    } else {
      packages <- c(packages, deps)
      deps <- unique(do.call(c, package_dependencies(packages, ...)))
      all_deps <- unique(c(all_deps, deps))
    }
  }
  return(all_deps)
}
