#' Create a table of the meta-data attribute of a dataframe.
#'
#' @param input.tbl Weather data table to extract meta-data.
#'
#' @return Tibble of meta-data information.
#' @export
#'
#' @examples
#' #Example weather data collected from IZW
#' test_data <- load_data_weatherstation.file(
#'   system.file("extdata/working_weather/weather_Mlima",
#'   "weather_data_test1.xlsx",
#'   package = "NgoroWeather"))
#'
#' #Extract metadata
#' create_tbl_metadata(test_data)
create_tbl_metadata <- function(input.tbl){

  attributes(input.tbl)$metadata

}

#' Create a table of all weather data at 30 min intervals
#'
#' @inheritParams arguments
#'
#' @return Tibble of meta-data information.
#' @export
#'
#' @examples
#' #Example weather data collected from IZW
#' test_data <- create_weather_raw.table(system.file("extdata/working_weather",
#'                                           package = "NgoroWeather"))
create_weather_raw.table <- function(input.folder){

  weather_folders <- list.dirs(input.folder, recursive = FALSE)

  weather_data <- purrr::map(.x = weather_folders, .f = function(dir){

    load_data_weatherstation.all(dir)

  })

  weather_metadata <- purrr::map_df(.x = weather_data, .f = function(station){

    create_tbl_metadata(station) |>
      dplyr::mutate(station_name = unique(station$station_name)) |>
      #If we want to add this into our sqlite database we can't have nested lists
      tidyr::unnest(cols = c("data", "messages"))

  })

  ## Load weather activity data
  weather_station_activity <- create_station_activity.log(input.folder)

  ## Create two new columns logical columns with 'temperature' and 'rainfall'
  ## When these are FALSE, the data should be used
  weather_data_active <- purrr::map(.x = weather_data, .f = function(station){

    station |>
      dplyr::mutate(date = lubridate::as_date(.data$date_time)) |>
      ## Join in activity periods for
      fuzzyjoin::fuzzy_left_join(weather_station_activity$activity_log |>
                                   sf::st_drop_geometry(),
                                 by = c("station_name" = "station_name",
                                        "site_name" = "site_name",
                                        "date" = "start_date",
                                        "date" = "end_date"),
                                 match_fun = list(`==`,
                                                  `==`,
                                                  `>`,
                                                  `<`)) |>
      ## Remove the other join cols (they are kept by default in fuzzy join)
      dplyr::rename(station_name = station_name.x,
                    site_name = site_name.x) |>
      ## Then deal with cleaning days (this is only relevant for rainfall)
      ## We expect many-to-many because there are multiple cleanings per site (and multiple records in a day)
      dplyr::left_join(weather_station_activity$cleaning_log,
                       by = c("site_name",
                              "date" = "cleaning_date"), relationship = "many-to-many") |>
      ## Define whether rainfall and temperature are true
      dplyr::mutate(temperature = dplyr::case_when(is.na(temperature) ~ FALSE,
                                            TRUE ~ temperature),
                    rainfall = dplyr::case_when(is.na(rainfall) ~ FALSE,
                                                action == "cleaned" ~ FALSE,
                                         TRUE ~ rainfall)) |>
      dplyr::select(station_name:file_number, temperature, rainfall, observation_period)

  }) |>
    dplyr::bind_rows() |>
    #Split date and time columns and treat as characters. This way they can be easily stored in db
    #And easily re-compiled when loading db in R
    tidyr::separate(col = .data$date_time, into = c("date", "time"), sep = " ", convert = TRUE) |>
    ## When separating midnight, 00:00 is actually coded as NA and so the time is lost!
    dplyr::mutate(time = tidyr::replace_na(time, "00:00:00"))

  ## Midnight is still included here...

  return(dplyr::tibble(name = c("weather", "weather_metadata"), data = list(weather_data_active,
                                                                            weather_metadata), datecheck = TRUE)) ## no date check necessary

}

#' Create summary statistics of weather
#'
#' @inheritParams arguments
#' @export
create_crater_weather.summary.table <- function(resolution = "day",
                                                from = NULL, to = NULL, at = NULL,
                                                variable = c("air_temp", "precip"),
                                                station = NULL,
                                                location = NULL){

  thirtymin_data <- create_crater_weather.table(from = from, to = to, at = at,
                                                ## We read in all variables and calculate summary stats then filter later...
                                                ## Otherwise we need lots of if/else to check which summary fn we can do
                                                variable = NULL,
                                                station = station, location = location)

  variable <- check_function_arg.variable.weather(variable)

  ## Take the weather data only...
  summary_data <- thirtymin_data |>
    ## Work out which group each record will fall into (i.e. which day, hour, week etc)
    dplyr::mutate(datetime = lubridate::floor_date(.data$date_time, resolution)) |>
    ## Now, for each station/time step, create summary stats
    dplyr::group_by(.data$site_name, .data$datetime) |>
    dplyr::summarise(station_name = dplyr::first(.data$station_name),
                     latitude = dplyr::first(.data$latitude),
                     longitude = dplyr::first(.data$longitude),
                     period = dplyr::first(.data$observation_period),
                     dplyr::across(.cols = c("air_temp", "relative_humidity", "atmospheric_pressure", "battery_percent"),
                                   .fns = list(mean = mean,
                                               max = max,
                                               min = min)),
                     dplyr::across(.cols= "precip", .fns = list(mean = mean,
                                                                max = max,
                                                                total = sum)),
                     dplyr::across(.cols = "precip_max_hourly", .fns = list(mean = mean,
                                                                            max = max)),
                     .groups = "drop") |>
    ## Convert back to date and time cols for simplicity
    dplyr::mutate(date = format(.data$datetime, "%Y-%m-%d"),
                  time = format(.data$datetime, "%H:%M")) |>
    dplyr::select("site_name", "station_name", "date", "time",
                  "latitude":"period",
                  ## Need perl = TRUE to accept lookahead regex
                  dplyr::matches(variable, perl = TRUE))

  ## If there's only one time (i.e. we grouped by date or higher)
  ## Then just remove this col.
  if (length(unique(summary_data$time)) == 1) {

    summary_data <- summary_data |>
      dplyr::select(-"time")

  }

  summary_data

}

#' Create a table of weather data in a given date range
#'
#' @inheritParams arguments
#'
#' @return Tibble of weather data
#' @export
#'
#' @examples
#' load_package_database.weather(system.file("extdata/working_weather",
#' package = "NgoroWeather"), overwrite.db = "yes")
#'
#' #Get temp data from jua station
#' create_crater_weather.table(variable = "air_temp", station = "jua")
create_crater_weather.table <- function(from = NULL, to = NULL, at = NULL,
                                        variable = c("air_temp", "precip"),
                                        station = NULL,
                                        location = NULL){

  if (!exists(".database") | (exists(".database") & !"weather" %in% .database$database$table_name)) {

    stop("Use `load_package_database.weather()` to load weather data")

  }

  #User must provide one of either station or location, not both
  if ((!is.null(station) & !is.null(location))) {

    stop("Please provide only one of `station` or `location`.")

  }

  #Check station, location, and variable names
  station  <- check_function_arg.station(station)
  location <- check_function_arg.location.weather(location)
  variable <- check_function_arg.variable.weather(variable)

  ## Create summary table
  ## Midnight disappears here somehow?!
  weather_data <- hyenaR::extract_database_table("weather") |>
    ## Combine to a date time file (we separate them for saving as .csv for output)
    dplyr::mutate(date_time = lubridate::ymd_hms(paste(.data$date, .data$time, sep = " "),
                                                 tz = "Africa/Dar_es_Salaam"))

  ## We can determine this from when temp and weather are available
  focal_station_activity <- weather_data |>
    dplyr::filter(temperature | rainfall) |>
    dplyr::mutate(date = lubridate::as_date(date))

    # weather_station_activity |>
    # dplyr::filter(.data$station_name %in% !!station & .data$site_name %in% !!location)

  ## Data are already subset to only include the active period of each station (e.g. exclude periods of repair)
  ## Therefore, if from/to/at not provided just use Inf!
  date_range <- hyenaR::check_function_arg.date.fromtoat(from, to, at,
                                                         .fill = TRUE,
                                                         min.date = min(focal_station_activity$date),
                                                         max.date = max(focal_station_activity$date),
                                                         arg.max.length = 1L, data.type = "weather")

  ## Convert to POSIXct so we can compare to date-time data
  from       <- as.POSIXct(date_range$from)
  to         <- as.POSIXct(date_range$to)

  output <- weather_data |>
    dplyr::filter(.data$site_name %in% !!location & .data$station_name %in% !!station & .data$date_time >= from & .data$date_time <= to) |>
    dplyr::select("site_name", "station_name", "date_time", "latitude", "longitude",
                  ## Need perl = TRUE to accept lookahead regex
                  dplyr::matches(variable, perl = TRUE),
                  "temperature", "rainfall",
                  "observation_period")

  output

}

#' Create a table of weather station activity
#'
#' @inheritParams arguments
#'
#' @return List containing two items: activity_log (summary of active
#' periods for each weather station) and cleaning_log (table of days
#' when station was cleaned)
#' @export
create_station_activity.log <- function(input.folder, ...){

  ## Expect two new files in the raw data
  ## - station_cleaning_log.csv: contains info on discrete activity periods
  ## This replaces the existing weather_station_activity.rda
  ## It can be used to record extended breaks in data collection, for one
  ## or both gauges. It can also be used to record change of position for the
  ## station.
  ## - station_cleaning_log.csv: contains information on individual checks and cleaning
  ## routine. This will lead to single day gaps in the data.

  activity_log <- suppressWarnings({read.csv(file.path(input.folder, "station_activity.csv")) |>
      ## Inf is replaced by max and min dates outside of possible range
      dplyr::mutate(start_date = tidyr::replace_na(lubridate::dmy(start_date, quiet = TRUE), lubridate::ymd("1990-01-01")),
                    end_date = tidyr::replace_na(lubridate::dmy(end_date, quiet = TRUE), lubridate::ymd(Sys.Date()) + 1)) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")
  })

  cleaning_log <- suppressWarnings({
    read.csv(file.path(input.folder, "station_cleaning_log.csv")) |>
      dplyr::mutate(cleaning_date = lubridate::dmy(date)) |>
      dplyr::filter(action == "cleaned") |>
      dplyr::select(site_name, cleaning_date, action)
  })

  output <- list(activity_log = activity_log,
                 cleaning_log = cleaning_log)

  return(output)


}
