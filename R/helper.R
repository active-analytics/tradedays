#' Following Friday
#'
#' If input date is not a Friday, returns the following Friday
#'
#' @param dt date
#'
#' @return date
following_friday <- function(dt){
    dt <- lubridate::ymd(dt)
    weekday <- as.character(lubridate::wday(dt, label = TRUE))
    adjust_days <-
        dplyr::case_when(
            weekday == "Fri" ~ 0
            , weekday == "Sat" ~ 6
            , weekday == "Sun" ~ 5
            , weekday == "Mon" ~ 4
            , weekday == "Tue" ~ 3
            , weekday == "Wed" ~ 2
            , weekday == "Thu" ~ 1
        )
    following_friday <- dt + lubridate::days(adjust_days)
    following_friday
}


#' Previous Friday
#'
#' If input date is not a Friday, returns the previous Friday
#'
#' @param dt date
#'
#' @return date
previous_friday <- function(dt){
    dt <- lubridate::ymd(dt)
    weekday <- as.character(lubridate::wday(dt, label = TRUE))
    adjust_days <-
        dplyr::case_when(
            weekday == "Fri" ~ 0
            , weekday == "Sat" ~ 1
            , weekday == "Sun" ~ 2
            , weekday == "Mon" ~ 3
            , weekday == "Tue" ~ 4
            , weekday == "Wed" ~ 5
            , weekday == "Thu" ~ 6
        )
    following_friday <- dt - lubridate::days(adjust_days)
    following_friday
}
