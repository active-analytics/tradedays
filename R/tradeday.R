#######################
# Trade Day Functions #
#######################


#' Nearest trade date
#'
#' If the current date is a trade date, returns the current date.
#' Otherwise return the next business day.
#'
#' @param dt date
#'
#' @return date
#' @export
nearest_tradeday <- function(dt) {
    if (bizdays::is.bizday(dt)) {
        trade_date <- dt
    } else {
        trade_date <- bizdays::add.bizdays(dt, 1)
    }
    trade_date
}
