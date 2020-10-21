.onAttach <- function(libname, pkgname) {
    start_year = 2002 #first year in delta-neutral data-set
    end_year = as.integer(format(Sys.Date(), "%Y"))
    bizdays::load_rmetrics_calendars(start_year:end_year)
    bizdays::bizdays.options$set(default.calendar="Rmetrics/NYSE")
}
