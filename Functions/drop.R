dr_drop <- function(dataframe,head=NULL,tail=NULL){
  Output <- dataframe
  if (is.null(head)){
    Output <- Output [-c((nrow(Output)-(`tail`-1)):nrow(Output)),]
    return(Output)
  }
  else if (is.null(tail)) {
    Output <- Output [-c(1:`a`), ]
    return(Output)
  }
  else
  Output <- Output [-c((nrow(Output)-(`tail`-1)):nrow(Output)),]
  Output <- Output [-c(1:`head`), ]
  return(Output)
}
