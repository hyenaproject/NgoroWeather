#' Load weather data.
#'
#' If no hyenaR database is present, will create a standalone .database object
#' for weather. This will function in a similar way to hyenaR processes.
#'
#' If a hyenaR database is present, will append weather data to the database
#' allowing weather and hyena information to be used simultaneously.
#'
#' @inheritParams arguments
#' @export
load_package_database.weather <- function(input.folder,
                                          overwrite.db = c("prompt", "yes", "no"),
                                          verbose = TRUE){

  overwrite.db <- match.arg(overwrite.db) ##  'prompt' is default as it is the first argument

  ### If there is already a .database object...
  if (exists(".database")) {

    ### If it's a stand-alone weather database...
    if (attr(.database, "datatype") == "weather") {

      # Check if the session is being run interactively
      # or the user hasn't provided a default behaviour for dealing with clashes...
      if (interactive() & overwrite.db == "prompt") {

        # Allow the user to choose whether they:
        # a) overwrite the existing data
        # b) exit (i.e. use the existing data)
        overwrite.db <- utils::menu(
          choices = c("Overwrite existing data", "Exit"),
          title = "Some weather data has already been loaded. What do you want to do?"
        )

        ## Convert menu outcome to match 'yes' and 'no' option from the 'overwrite.db' argument
        overwrite.db <- dplyr::case_when(
          overwrite.db == 1 ~ "yes",
          overwrite.db == 2 ~ "no"
        )
      } else if (!interactive() & overwrite.db == "prompt") {
        stop("User prompt is not possible in non-interactive mode.
           Please provide a default behaviour to deal with clashes. Use the argument 'overwrite.db'.")
      }

      # If they choose to overwrite the file, delete the original and continue.
      # (Not ideal because the delete takes time, actual overwrite would be better but SQL problems)
      if (overwrite.db == "yes") {

        if (verbose) {
          message("Data already exists. Loading new data...")
        }

        weather_data <- create_weather_raw.table(input.folder = input.folder) |>
        ## Coerce this to be in hyenaR format
          dplyr::select(table_name = "name",
                        "data")

        ### Update the .database object to have a new data and folder path
        assign(x = "database", value = weather_data, envir = .database)
        assign(x = "db.path", value = input.folder, envir = .database)

        # Otherwise, exit without doing anything...
      } else {
        if (verbose) {
          message("Exit without loading data...")
        }
        return(invisible(NULL))
      }

    } else if (attr(.database, "datatype") %in% c("full", "dummy", "sim")) {

      ## Check to see if it already has a weather table
      if ("weather" %in% .database$database$table_name) {

        ## In which case, we do the same process as above (i.e. overwrite or exit)
        # Check if the session is being run interactively
        # or the user hasn't provided a default behaviour for dealing with clashes...
        if (interactive() & overwrite.db == "prompt") {

          # Allow the user to choose whether they:
          # a) overwrite the existing data
          # b) exit (i.e. use the existing data)
          overwrite.db <- utils::menu(
            choices = c("Overwrite existing data", "Exit"),
            title = "Weather data already loaded. What do you want to do?"
          )

          ## Convert menu outcome to match 'yes' and 'no' option from the 'overwrite.db' argument
          overwrite.db <- dplyr::case_when(
            overwrite.db == 1 ~ "yes",
            overwrite.db == 2 ~ "no"
          )
        } else if (!interactive() & overwrite.db == "prompt") {
          stop("User prompt is not possible in non-interactive mode.
           Please provide a default behaviour to deal with clashes. Use the argument 'overwrite.db'.")
        }

        # If they choose to overwrite the file, delete the original and continue.
        # (Not ideal because the delete takes time, actual overwrite would be better but SQL problems)
        if (overwrite.db == "yes") {

          if (verbose) {
            message("Data already exists. Loading new data...")
          }

          weather_data <- create_weather_raw.table(input.folder = input.folder) |>
            ## Coerce this to be in hyenaR format
            dplyr::select(table_name = "name",
                          "data")

          ### Update the weather and weather_metadata parts of the db
          .database$database$data[[which(.database$database$table_name == "weather")]] <- weather_data$data[[1]]
          .database$database$data[[which(.database$database$table_name == "weather_metadata")]] <- weather_data$data[[2]]

          # Otherwise, exit without doing anything...
        } else {
          if (verbose) {
            message("Exit without loading data...")
          }
          return(invisible(NULL))
        }

      } else {

        weather_data <- create_weather_raw.table(input.folder = input.folder) |>
          ## Coerce this to be in hyenaR format
          dplyr::select(table_name = "name",
                        "data")

        ### Append weather data to this .database
        .database$database <- dplyr::bind_rows(.database$database, weather_data)

      }

    }

  } else {

    .database <<- new.env()
    attr(.database, "name")     <- "environment storing the database and its path"
    attr(.database, "datatype") <- "weather"

    weather_data <- create_weather_raw.table(input.folder = input.folder) |>
      ## Coerce this to be in hyenaR format
      dplyr::select(table_name = "name",
                    "data")

    assign(x = "database", value = weather_data, envir = .database)
    assign(x = "db.path", value = input.folder, envir = .database)

  }

}

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
#' load_data_weatherstation.file(system.file("extdata/working_weather/weather_Mlima",
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
  meta_data <- stats::setNames(excel_data$Metadata, nm = c("metadata_category", "info", "value"))

  meta_data |>
    tidyr::drop_na(.data$info) |>
    tidyr::fill(.data$metadata_category, .direction = "down") |>
    #Convert everything to lower snake case
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = hyenaR::recode_chr_snake.case)) |>
    dplyr::group_by(.data$metadata_category) -> meta_data

  #Clean weather measurements
  #Use the first and second sheet.
  #If there are multiple configurations in the data,
  #it will combine them.
  processed_data_configs <- excel_data[grepl(pattern = "Process", names(excel_data))]
  purrr::map_df(.x = processed_data_configs, .f = ~{

    output <- stats::setNames(..1, make.names(as.character(.[3, ]), unique = TRUE))

    output |>
      dplyr::slice(4:dplyr::n()) |>
      dplyr::mutate(date_time = hyenaR::recode_numeric_excel.to.date(as.numeric(.data$Timestamp), tz = tz)) |>
      dplyr::mutate(station_name = meta_data$value[which(meta_data$info == "device_name")],
                    site_name = meta_data$value[which(meta_data$info == "site_name")],
                    latitude = meta_data$value[which(meta_data$info == "latitude")],
                    longitude = meta_data$value[which(meta_data$info == "longitude")])

  }) -> weather_data

  weather_data |>
    dplyr::select(.data$station_name:.data$site_name, .data$date_time,
                  .data$latitude, .data$longitude,
                  ##TODO: We 'recode' to these col names in `check_function_arg.variable.weather`
                  ##So we don't really need to rename each one here
                  ##Could just do standard coercion to e.g. snake case
                  ##That way it will also work with dummy data that doesn't have all the cols.
                  air_temp = .data$X.C.Air.Temperature,
                  atmospheric_pressure = .data$kPa.Atmospheric.Pressure,
                  relative_humidity = .data$RH.Relative.Humidity,
                  precip = .data$mm.Precipitation,
                  precip_max_hourly = .data$mm.h.Max.Precip.Rate,
                  battery_percent = .data$X..Battery.Percent) |>
    dplyr::mutate(across(.data$latitude:.data$battery_percent, .fns = as.numeric)) |>
    #Remove any cases where date_time is NA. These cannot be used.
    dplyr::filter(!is.na(.data$date_time)) |>
    #Arrange by date time
    dplyr::arrange(.data$date_time) -> output

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

  #Arrange by date time and remove any duplicates
  output |>
    dplyr::arrange(.data$date_time) |>
    #Remove rows that have same date, temp and precip values.
    dplyr::distinct(.data$date_time, .data$air_temp, .data$precip, .keep_all = TRUE) -> output

  duplicates <- output |>
    dplyr::group_by(date_time) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {

    stop("There is contradictory data for a given date-time")

  }

  #Carry out additional set of checks when combining weather station data
  #If pass, assign as meta-data for output
  check_weatherstation_metadata.all(metadata_list) -> attr(output, "metadata")

  #Return output file with meta-data
  output

}
