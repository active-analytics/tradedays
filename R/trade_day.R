########################
# Trade Date Functions #
########################


#' Nearest business day
#'
#' If the current date is a business day, returns the current date.
#' Otherwise return the next business day.
#'
#' @param current_date date
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
