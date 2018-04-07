dropMethod3 <- function(.data, expression = NULL){
  .data <- .data %>% dplyr::filter_(paste0("!", expression))
  return(.data)
}
