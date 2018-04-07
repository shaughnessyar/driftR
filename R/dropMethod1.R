dropMethod1 <- function(.data, head = NULL, tail = NULL){
  # Check for input errors
  if (!is.null(head)) {
    if (!(typeof(head) %in% c('integer', 'double'))) {
      return(stop(glue::glue('Head value {head} not acceptable - value should be NULL or integer >= 1')))
    }

    if ((head %% 1 != 0) | (head <= 0)) {
      return(stop(glue::glue('Head value {head} not acceptable - value should be NULL or integer >= 1')))
    }
  }
  if (!is.null(tail)) {
    if (!(typeof(tail) %in% c('integer', 'double'))) {
      return(stop(glue::glue('Tail value {tail} not acceptable - value should be NULL or integer >= 1')))
    }
    if ((tail %% 1 != 0) | (tail <= 0)) {
      return(stop(glue::glue('Tail value {tail} not acceptable - value should be NULL or integer >= 1')))
    }
  }
  # calculate slice positions
  headPos <- head+1

  rows <- nrow(.data)
  tailPos <- rows-tail

  # evaluate head and tail
  if (base::is.null(head)){
    dplyr::slice(.data, 1:tailPos)
  }
  else if (base::is.null(tail)) {
    dplyr::slice(.data, headPos:n())
  }
  else {
    dplyr::slice(.data, headPos:tailPos)
  }
}
