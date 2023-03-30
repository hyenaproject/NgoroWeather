#' Log of when weather stations were active.
#'
#' \itemize{
#'   \item station_name. Name of station equipment.
#'   \item site_name. Name of location where station is installed. In theory, a station may move over time, although
#'   currently all stations are at one site.
#'   \item start_date. Date at which station was *installed*. Stations are not installed at midnight, so
#'   this date will have incomplete data.
#'   \item end_date. Date at which station was *removed*. This date will have incomplete data. Notice, if station
#'   is still active will be Inf.
#'   \item observation_period. Period during which station was active. Useful for grouping data when plotting.
#'   \item comment. Information about the observation period. Will usually describe why station was removed or
#'   stopped collecting data.
#'   \item geometry. Coordinates of site.
#' }
#' @name weather_station_activity
NULL

utils::globalVariables(".database")
utils::globalVariables("weather_station_activity")
utils::globalVariables(".data")
