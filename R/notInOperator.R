#' Not In Operator
#' 
#' Provides the compliment to the base R \code{%in%} operator. Included here instead of via import
#' due to stability issues with the source package, \href{\code{Hmsic}}{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s},
#' during original package development in October, 2017.
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
"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0