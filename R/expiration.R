#################################
## Option Expiration Functions ##
#################################

#' Check if third Friday of month
#'
#' This function checks if the given Friday is the
#' third Friday of the month
#'
#' @param dt_friday date
#'
#' @return boolean
#' @export
is_third_friday <- function(dt_friday){

    # exception-handling to add
    # check if it's a valid date
    # check if it's a Friday

    dt_first_of_month <- lubridate::floor_date(dt_friday, "month")

    chr_first_day <-
        as.character(lubridate::wday(dt_first_of_month, label = TRUE))

    if (chr_first_day == "Sun") {
        int_first_friday <- 6
    } else if (chr_first_day == "Mon"){
        int_first_friday <- 5
    } else if (chr_first_day == "Tue"){
        int_first_friday <- 4
    } else if (chr_first_day == "Wed"){
        int_first_friday <- 3
    } else if (chr_first_day == "Thu"){
        int_first_friday <- 2
    } else if (chr_first_day == "Fri"){
        int_first_friday <- 1
    } else if (chr_first_day == "Sat"){
        int_first_friday <- 7
    }

    int_third_friday <- int_first_friday + 14

    bln_third_friday <- FALSE
    if (int_third_friday == lubridate::day(dt_friday)){
        bln_third_friday <- TRUE
    }

    bln_third_friday
}


#' Third Friday of month
#'
#' Use this function to get the date of the third Friday
#' of the given year and month.  This is an internal function.
#'
#' @param year integer
#' @param month integer
#'
#' @return date
third_friday <- function(year, month){

    ## exception handling
    # check if it's a valid year
    # check if it's a valid month


    chr_first_of_month <-
        paste0(
            as.character(year), "-"
            , as.character(month), "-"
            , "1"
        )

    dt_first_of_month <- lubridate::ymd(chr_first_of_month)

    chr_first_day <-
        as.character(lubridate::wday(dt_first_of_month, label = TRUE))

    if (chr_first_day == "Sun") {
        int_first_friday <- 6
    } else if (chr_first_day == "Mon"){
        int_first_friday <- 5
    } else if (chr_first_day == "Tue"){
        int_first_friday <- 4
    } else if (chr_first_day == "Wed"){
        int_first_friday <- 3
    } else if (chr_first_day == "Thu"){
        int_first_friday <- 2
    } else if (chr_first_day == "Fri"){
        int_first_friday <- 1
    } else if (chr_first_day == "Sat"){
        int_first_friday <- 7
    }

    int_third_friday <- int_first_friday + 14

    dt_third_friday <-
        lubridate::ymd(
            paste0(
                as.character(year), "-"
                , as.character(month), "-"
                , as.character(int_third_friday)
            )
        )

    dt_third_friday

}


#' Regular monthly expiration
#'
#' Use this function to get the regular expiration for a given year month.
#'
#' @param year integer
#' @param month integer
#'
#' @return date
#'
#' @export
monthly_expiration <- function(year, month){

    # find third friday of month
    dt_third_friday <- third_friday(year, month)

    # prior to 2/20/2015, expirations were listed as a Saturday
    if(dt_third_friday < lubridate::ymd(20150220)) {
        dt_monthly_exp <- dt_third_friday + 1
    } else {
        dt_monthly_exp <- dt_third_friday
        # if third friday of month falls on a holiday,
        # then the expiration is the previous business day
        if (!bizdays::is.bizday(dt_third_friday)){
            dt_monthly_exp <-
                bizdays::add.bizdays(dt_third_friday, -1)
        }
    }
    dt_monthly_exp
}

#' Last trade-date for monthly expiration
#'
#' Use this function to get the last trade date for the regular-expiration
#' option for this year month.
#'
#' @param year integer
#' @param month integer
#'
#' @export
monthly_last_tradeday <- function(year, month){

    dt_monthly_exp <- monthly_expiration(year, month)
    dt_last_td <- dt_monthly_exp
    # prior to 2/20/2015, expiration date was a Saturday
    if(dt_monthly_exp < lubridate::ymd(20150220)){
        dt_last_td <- bizdays::add.bizdays(dt_monthly_exp, -1)
    }
    dt_last_td
}


#' Find the next monthly expiration
#'
#' For the given input date, will find the next expiration that's after the
#' end-of-day of the input date
#'
#' @param dt date
#'
#' @return date
#' @export
next_monthly_expiration <- function(dt) {

    ## OPEN QUESTION: what should the behavior be when expirations
    ## are defined on Saturdays and we input a Friday.  Currently, just
    ## give the following Saturday.  But not sure if that is what we want.
    dt <- lubridate::ymd(dt)

    # extracting the year and month from the test date
    year <- lubridate::year(dt)
    month <- lubridate::month(dt)

    # calculating the
    curr_month_exp <- monthly_expiration(year, month)

    if (dt < curr_month_exp) {

        # if the test-date is before the current month's option expiration
        # then the next expiration is just the current month's expiration
        dt_next_exp <- curr_month_exp

    } else {

        # if the test-date is on or after the current month's expiration,
        # then the next expiration is the next month's expiration

        # creating the first of the month
        dt_first_of_month <- lubridate::floor_date(dt)

        # calculating the first of the next month
        dt_first_of_next_month <-
            dt_first_of_month + lubridate::period(1, "month")

        # extracting the month and year from the first of the next month
        next_year = lubridate::year(dt_first_of_next_month)
        next_month = lubridate::month(dt_first_of_next_month)

        dt_next_exp <- monthly_expiration(next_year, next_month)
    }

    dt_next_exp
}
