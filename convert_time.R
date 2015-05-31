convert_time <- function(date_string, from_tz, to_tz) {
  # date_string, e.g. "2015-05-23 19:45"
  # from_tz indicates timezone of the given date_string, e.g. "America/New_York"
  # to_tz indicates to which timezone the date should be converted, e.g. "Europe/Berlin"
  date_fmt <- as.POSIXct(date_string, tz=from_tz)
  format(date_fmt, tz=to_tz, usetz=TRUE)
}
