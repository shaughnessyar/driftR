#' driftR: Drift Correcting Water Quality Data
#'
#' @description There are many sources of water quality data including instruments
#'     (ex: YSI instruments) and open source data sets (ex: USGS and NDBC), all of
#'     which are susceptible to errors/inaccuracies due to drift. \code{driftR} provides
#'     a grammar for cleaning and correcting these data in a "tidy", reproducible
#'     manner.
#'
#' @details The \code{driftR} package implements a series of equations used in
#'     \href{https://www.slu.edu/arts-and-sciences/earth-atmospheric-sciences/faculty/hasenmueller-elizabeth.php}{Dr. Elizabeth Hasenmueller's}
#'     hydrology and geochemistry research. These equations correct continuous water quality
#'     monitoring data for incremental drift that occurs over time. There are two
#'     forms of corrections included in the package - a one-point calibration and
#'     a two-point calibration. One-point and two-point calibration values are suited
#'     for different types of measurements. The package is currently written for the
#'     easiest use with YSI Sonde products.
#'
#'     There are four key verbs that are introduced in \code{driftR}:
#'     \itemize{
#'       \item{\emph{read}: The \code{\link{dr_readSonde}} function imports and properly
#'         formats output from YSI Sonde instrument}
#'       \item{\emph{factor}: The \code{\link{dr_factor}} function calculates factors
#'         based on the time of the observation and the total amount of time that the
#'         instrument had been deployed. They are used in the equations for both the
#'         one-point and two-point drift corrections.}
#'       \item{\emph{correct}: The \code{\link{dr_correctOne}} and \code{\link{dr_correctTwo}}
#'         functions take both the factors and standard values as parameters for calculating
#'         drift corrected versions of specific measurements.}
#'       \item{\emph{drop}: The \code{\link{dr_drop}} function allows for removing erroneous
#'         observations from both the head and the tail of the data.}
#'     }
#'
#' @section Tidy Evaluation:
#'     \code{driftR} makes use of tidy evaluation and the pronoun \code{.data}, meaning
#'     that variable references may be either quoted or unquoted (i.e. bare). This also
#'     means that \code{driftR} works seamlessly with \code{magrittr} pipe operators.
#'
#' @author
#'     \strong{Maintainer:} Andrew Shaughnessy \href{andrew.shaughnessy@slu.edu}{andrew.shaughnessy@slu.edu}
#'
#'     Authors:
#'     \itemize{
#'       \item{Christopher Prener, Ph.D. \href{chris.prener@slu.edu}{chris.prener@slu.edu}}
#'       \item{Elizabeth Hasenmueller, Ph.D. \href{elizabeth.hasenmueller@slu.edu}{elizabeth.hasenmueller@slu.edu}}
#'     }
#'
#' @seealso Useful links:
#' \itemize{
#'   \item{\href{https://shaughnessyar.github.io/driftR/}{Package Website and Documentation}}
#'   \item{\href{https://github.com/shaughnessyar/driftR}{Source Code on GitHub}}
#'   \item{\href{https://github.com/shaughnessyar/driftR/issues}{Bug Reports and Feature Requests}}
#' }
#'
#' @name driftR
#' @docType package
NULL

