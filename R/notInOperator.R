#' Not In Operator
#'
#' Provides the compliment to the base R \code{\%in\%} operator. Included here instead of via import
#' due to stability issues with the source package, \href{\code{Hmsic}}{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s},
#' during original package development in October, 2017. Used under terms of \href{\code{Hmisc}}{https://cran.r-project.org/web/packages/Hmisc/index.html}'s
#' \href{GPL-3 License}{https://cran.r-project.org/web/licenses/GPL-3}.
#'
#' @param x vector or \code{NULL}: the values to be matched
#' @param table vector or \code{NULL}: the values to be matched against
#'
#' @source \href{\code{Hmsic}}{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s}
#'
#' @examples
#' x <- 2
#' y <- 2
#' z <- 3
#'
#' x %in% y
#' x %nin% y
#'
#' x %in% z
#' x %nin% z
#'
#' @export
"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0
