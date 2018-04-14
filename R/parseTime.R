# Parse Time Values
#
# These are internal functions that are used by both dr_drop and dr_replace to manage user input.
# They allow users to provide a variety of valid date constructions for both functions.
#
# from input
parseFrom <- function(from){

  fromVal <- as.character(
    lubridate::parse_date_time(from, orders = c("ymd", "dmy", "mdy",
                                                "ymd HMS", "dmy HMS", "mdy HMS",
                                                "ymd HM", "dmy HM", "mdy HM"))
    )

  return(fromVal)

}

# to input
parseTo <- function(to, addDay = FALSE){

  toVal <- as.character(
    lubridate::parse_date_time(to, orders = c("ymd", "dmy", "mdy",
                                              "ymd HMS", "dmy HMS", "mdy HMS",
                                              "ymd HM", "dmy HM", "mdy HM"))
    )

  return(toVal)
}
