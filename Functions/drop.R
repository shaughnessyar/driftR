dr_drop <- function(dataframe,a=NULL,b=NULL){
  Output <- dataframe
  if (is.null(a)){
    Output <- Output [-c((nrow(Output)-(`b`-1)):nrow(Output)),]
    return(Output)
  }
  else if (is.null(b)) {
    Output <- Output [-c(1:`a`), ]
    return(Output)
  }
  else
  Output <- Output [-c((nrow(Output)-(`b`-1)):nrow(Output)),]
  Output <- Output [-c(1:`a`), ]
  return(Output)

}
