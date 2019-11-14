
format_time_int <- function (st0)
{
    st <- as.integer (difftime (Sys.time (), st0, units = "sec"))
    hh <- floor (st / 3600)
    mm <- floor ((st - hh * 3600) / 60)
    ss <- st - hh * 3600 - mm * 60
    mm <- sprintf ("%02d", mm)
    ss <- sprintf ("%02d", ss)
    paste0 (hh, ":", mm, ":", ss)
}
