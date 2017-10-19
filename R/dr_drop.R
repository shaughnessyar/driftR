#' Dropping observations for equilibration period
#'
#' @param dataFrame The working data frame
#' @param head A number
#' @param tail A number
#' @return A data frame with fewer observations
#' @examples
#' \dontrun{
#' dr_drop(df, 3, 9)
#' dr_drop(df, , 4)
#' dr_drop(df, 7,)
#' }
#'
#' @export
"dr_drop" <- function(dataFrame, head = NULL, tail = NULL){
  Output <- dataFrame
  if (base::is.null(head)){
    Output <- Output [-c((base::nrow(Output)-(`tail`-1)):base::nrow(Output)),]
    return(Output)
  }
  else if (base::is.null(tail)) {
    Output <- Output [-c(1:`head`), ]
    return(Output)
  }
  else
  Output <- Output [-c((base::nrow(Output)-(`tail`-1)):base::nrow(Output)),]
  Output <- Output [-c(1:`head`), ]
  return(Output)
}
