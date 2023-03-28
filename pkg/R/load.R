#' Load all sheets from an excel file (.xls or .xslx).
#'
#' @inheritParams arguments
#' @param ... Arguments passed to readxl::read_excel.
#'
#' @return A list of tibbles. Each list item represents a sheet in the excel
#' spreadsheet
#' @export
#'
#' @examples
#' #Example data collected from IZW is stored with the package for example
#' load_data_excel(system.file("extdata/weather_Mlima",
#'                                           "weather_data_test1.xlsx",
#'                                           package = "hyenaR"))
#'
#' #Can pass arguments like ignoring columns names using ...
#' load_data_excel(system.file("extdata/weather_Mlima",
#'                                           "weather_data_test1.xlsx",
#'                                           package = "hyenaR"), col_names = FALSE)
load_data_excel <- function(excel.path, ...) {

  force(excel.path)

  hyenaR::check_function_arg.path(excel.path)

  all_sheets <- readxl::excel_sheets(excel.path)

  purrr::map(.x = all_sheets,
             .f = function(sheet_name, ...){

               readxl::read_excel(excel.path, sheet = sheet_name, ...)

             }, ...) -> output

  #Create named list
  names(output) <- all_sheets

  output

}

#' Load single weather station file into R.
#'
#' 'tz' argument is used when converting excel date times (default = "Africa/Dar_es_Salaam").
#'
#' @inheritParams arguments
#'
#' @return A list with two items: weather data & station meta data
#' @export
#'
#' @examples
#' #Example data collected from IZW is stored with the package for example
#' load_data_weatherstation.file(system.file("extdata/weather_Mlima",
#'                                           "weather_data_test1.xlsx",
#'                                           package = "NgoroWeather"))
load_data_weatherstation.file <- function(excel.path, tz = "Africa/Dar_es_Salaam", verbose = TRUE) {

  #Check if readxl is installed
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("You must installed the R package readxl to work with weather data.")
  }

  #Path checked inside load function
  #Suppress messages required because there will be name repair that produces a message
  #This will not occur in every excel spreadsheet, so apply here rather than internal
  excel_data <- suppressMessages(load_data_excel(excel.path, col_names = FALSE, col_type = "text"))

  #Extract meta-data info
  excel_data$Metadata |>
    stats::setNames(nm = c("metadata_category", "info", "value")) |>
    tidyr::drop_na(.data$info) |>
    tidyr::fill(.data$metadata_category, .direction = "down") |>
    #Convert everything to lower snake case
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = hyenaR::recode_chr_snake.case)) |>
    dplyr::group_by(.data$metadata_category) -> meta_data

  #Clean weather measurements
  #Use the first and second sheet. If there are multiple configurations
  #in the data, it will take the most recent one.
  excel_data[[1]] <- stats::setNames(excel_data[[1]], make.names(as.character(excel_data[[1]][3, ]), unique = TRUE))

  excel_data[[1]] |>
    dplyr::slice(4:dplyr::n()) |>
    dplyr::mutate(date_time = hyenaR::recode_numeric_excel.to.date(as.numeric(.data$Timestamp), tz = tz)) |>
    dplyr::mutate(station_name = meta_data$value[which(meta_data$info == "device_name")],
                  site_name = meta_data$value[which(meta_data$info == "site_name")],
                  latitude = meta_data$value[which(meta_data$info == "latitude")],
                  longitude = meta_data$value[which(meta_data$info == "longitude")]) -> weather_data

  weather_data |>
    dplyr::select("station_name":"site_name", "date_time",
                  "latitude", "longitude",
                  ##TODO: We 'recode' to these col names in `check_function_arg.variable.weather`
                  ##So we don't really need to rename each one here
                  ##Could just do standard coercion to e.g. snake case
                  ##That way it will also work with dummy data that doesn't have all the cols.
                  air_temp = "X.C.Air.Temperature",
                  atmospheric_pressure = "kPa.Atmospheric.Pressure",
                  relative_humidity = "RH.Relative.Humidity",
                  precip = "mm.Precipitation",
                  precip_max_hourly = "mm.h.Max.Precip.Rate",
                  battery_percent = "X..Battery.Percent") |>
    dplyr::mutate(across(.data$latitude:.data$battery_percent, .fns = as.numeric)) -> output

  #Run checks on important meta-data attributes
  meta_data <- check_weatherstation_metadata.file(metadata = meta_data, verbose = verbose)

  #Give it meta-data as an attribute
  #Nest into categories so it's easier to see when returned
  meta_data |>
    tidyr::nest() |>
    dplyr::mutate(any_error = purrr::map_lgl(.x = .data$data,
                                             .f = function(data){

                                               any(data$error)

                                             }),
                  messages = purrr::map(.x = .data$data,
                                        .f = function(data){

                                          stats::na.omit(unique(data$message))[1]

                                        })) |>
    dplyr::ungroup() -> attr(output, "metadata")

  output

}

#' Load and compile all files from a single weather station into R
#'
#' 'tz' argument is used when converting excel date times.
#'
#' @inheritParams arguments
#'
#' @return A list with two items: weather data & station meta data
#' @export
#'
#' @examples
#' #Example data collected from IZW is stored with the package for example
#' load_data_weatherstation.all(system.file("extdata/weather_Mlima", package = "hyenaR"))

load_data_weatherstation.all <- function(input.folder, verbose = TRUE) {

  all_files <- list.files(input.folder,
                          pattern = ".xlsx", full.names = TRUE)

  df_list <- purrr::imap(.x = all_files,
                         .f = function(file, file_number, verbose){

                           load_data_weatherstation.file(excel.path = file, verbose = verbose) |>
                             dplyr::mutate(file_number = file_number)

                         }, verbose = verbose)

  metadata_list <- purrr::imap(.x = df_list,
                               .f = function(data, file_number){

                                 create_tbl_metadata(data) |>
                                   dplyr::mutate(file_number = file_number)

                               })

  #Combine data together
  output <- dplyr::bind_rows(df_list)

  #Carry out additional set of checks when combining weather station data
  #If pass, assign as meta-data for output
  check_weatherstation_metadata.all(metadata_list) -> attr(output, "metadata")

  #Return output file with meta-data
  output

}
