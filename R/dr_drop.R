#' Dropping observations for equilibration period
#' @description When taking the instrument out of the water, there is often several observations that pass before the run can be downloaded. Additionally, once the instrument is in the water, it often takes about 30 minutes for the sensors to equilibrate. This function allows you to drop observations from the bottom and top of the dataset for each of those issues respectively.
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
