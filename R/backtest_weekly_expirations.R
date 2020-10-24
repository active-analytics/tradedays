#' Backtest weekly expirations
#'
#' Returns a dataframe that consists of all weekly expriations
#' in date range, along with execution dates.  This is used to organize
#' all the expirations in a backtest.
#'
#' @param start_date date
#' @param end_date date
#'
#' @return dataframe
#' @export
backtest_weekly_expirations <- function(start_date=20020101,
                                        end_date=20181231){

    # to avoid "no visible binding" warning from devtools::check()
    df_expiration <- NULL

    start_date <- lubridate::ymd(start_date)
    # if start_date is not a Friday, start on the following Friday
    start_date <- following_friday(start_date)


    end_date <- lubridate::ymd(end_date)
    # if end_date is not a Friday, send on the previous Friday
    end_date <- previous_friday(end_date)

    #browser()
    ## move this logic into the deltaneutral package
    ## this is about data availability, and this package should
    ## be data agnostic
    # start_date <-
    #     dplyr::case_when(
    #         underlying == "SPY" ~ lubridate::ymd(20100604)
    #         , underlying == "IWM" ~ lubridate::ymd(20100604)
    #         , underlying == "QQQ" ~ lubridate::ymd(20100604)
    #         , underlying == "GLD" ~ lubridate::ymd(20100625)
    #         , underlying == "SLV" ~ lubridate::ymd(20101231)
    #     )
    # end_date <- lubridate::ymd(20181228)

    df_expiration <-
        tibble::tibble(
            friday = seq(start_date, end_date, "weeks")
            #, underlying = underlying
            , monthly = NA
            , expiration = as.Date(NA)
            , last_trade_date = as.Date(NA)
            , d2x = NA_integer_
            # , num_opts = NA_integer_
            # , exec_day_volume = NA_integer_
            # , realized_vol = NA_real_
            # , ret = NA_real_
        )

    for (ix in 1:nrow(df_expiration)){

        dt_friday <- df_expiration$friday[ix]

        # is it third Friday of the month
        bln_is_third <- tradedays::is_third_friday(dt_friday)

        # expiration
        if (bln_is_third == TRUE){
            dt_expiration <-
                tradedays::monthly_expiration(
                    lubridate::year(dt_friday)
                    , lubridate::month(dt_friday)
                )
        } else {
            dt_expiration <- dt_friday
            if(!bizdays::is.bizday(dt_friday)){
                dt_expiration <-
                    bizdays::add.bizdays(dt_friday, -1)
            }
        }

        # last trade-date
        if (bln_is_third == TRUE){
            dt_last_td <-
                tradedays::monthly_last_tradeday(
                    lubridate::year(dt_friday)
                    , lubridate::month(dt_friday)
                )
        } else {
            dt_last_td <- dt_expiration
        }

        # this doesn't catch anything because there is logic
        # above to account for this, and there are no market holidays
        # on a regular expiration.  I'll leave it in for now
        if(!bizdays::is.bizday(dt_last_td)){
            dt_last_td <- bizdays::add.bizdays(dt_last_td, -1)
        }

        # updating df_expiration
        df_expiration$monthly[ix] <- bln_is_third
        df_expiration$expiration[ix] <- dt_expiration
        df_expiration$last_trade_date[ix] <- dt_last_td
    }

    df_expiration <-
        df_expiration %>%
        dplyr::mutate(execution = dplyr::lag(.data$last_trade_date))

    df_expiration <-
        df_expiration[-1, ] %>%
        dplyr::select(
            #underlying,
            .data$monthly, .data$expiration, .data$last_trade_date
            , .data$execution, .data$d2x
            #, num_opts, exec_day_volume
            #, realized_vol, ret
        )

    df_expiration <-
        df_expiration %>%
            dplyr::mutate(d2x = as.integer(bizdays::bizdays(.data$execution, .data$expiration)))

    # move this logic into deltaneutral package
    # this is a data quirk, and this package should be data agnostic
    # if(underlying == "SPY"){
    #     #this is a brute force hack to correct the 12/18/2015 expiration
    #     for(ix in 1:nrow(df_expiration)){
    #         dt_expiration <- df_expiration$expiration[ix]
    #         if (dt_expiration == as.Date("2015-12-18")){
    #             df_expiration$expiration[ix] <- as.Date("2015-12-19")
    #         }
    #     }
    # }

    df_expiration
}
