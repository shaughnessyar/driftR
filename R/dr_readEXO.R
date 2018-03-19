dr_readEXO <- function (file, defineVar = TRUE) {
  if (!file.exists(file)) {
    stop("File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
  }
  if (!(typeof(defineVar) %in% c("logical"))) {
    stop(glue::glue("defineVar value {defineVar} not acceptable - value should TRUE or FALSE"))
  }
  if (defineVar == FALSE) {
    df <- readxl::read_excel(file)
    df <- tibble::as_tibble(df)
    return(df)
  }
  else if (defineVar == TRUE) {
    df <- readxl::read_excel(file)
    a <- which(df[,1] == "1")[1]
    df <- readxl::read_excel(file, skip = (a+1))
    df$`Date (MM/DD/YYYY)` <- base::as.character(df$`Date (MM/DD/YYYY)`)
    df$`Time (HH:MM:SS)` <- base::as.character(df$`Time (HH:MM:SS)`)
    df$`Time (HH:MM:SS)` <-  base::gsub("^.*? ", "", df$`Time (HH:MM:SS)`)
    df <- tibble::as_tibble(df)
    return(df)
  }
}
