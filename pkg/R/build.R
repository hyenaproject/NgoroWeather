#' Build weather report
#'
#' @inheritParams arguments
#' @param output Character. Should report be pdf (default) or html.
build_package_weather.report <- function(output = "pdf", args.vignette = list(), overwrite = FALSE, verbose = FALSE,
                                         devtools.ok = TRUE, require.data = TRUE) {

  verbose <- hyenaR::check_function_arg.verbose(verbose)

  file.name = paste0("NCAA_weather_report_", output, ".Rmd")

  ## The vignette source files are stored in different places depending on whether one
  ## uses the installed version of the package or the one loaded with devtools during
  ## the development of the package. We thus try both:
  path1 <- paste0(find.package("NgoroWeather"), "/inst/extdata/reports")
  path2 <- paste0(find.package("NgoroWeather"), "/extdata/reports")
  if (dir.exists(path1)) {
    path <- path1
    if (!devtools.ok) stop("Reports cannot be created using the package loading from devtools, please restart your R session and use instead 'library(hyenaR)' to load the package before building the vignette.")
    message("Note: creation of reports using the development version of the package (not the installed one)")
  }
  if (dir.exists(path2)) {
    path <- path2
    message("Note: creation of reports using the installed version of the package (not the developemental one)")
  }
  path_complete <- paste(path, file.name, sep = "/")
  path_complete <- hyenaR::check_function_arg.path(path_complete, mustWork = TRUE)

  ## We check if {rmarkdown} is there because we don't have it  in IMPORTS:
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("You need to install the package 'rmarkdown' to use this function.")
  }

  ## Create a clean environment with just what is needed: ## FIXME: currently export global envir and should not
  vignette_env <- new.env() ## before new.env(parent = as.environment("package:hyenaR")) to mask the global env, but that creates scoping issues
  assign("args.vignette", value = args.vignette, envir = vignette_env)

  ## The actual job:
  message("The report is being created (be patient)...")
  vignette_path <- rmarkdown::render(path_complete, quiet = !verbose,
                                       envir = vignette_env,
                                       params = list(args.vignette = args.vignette))

  message("The report has be created and is stored at the following location:")
  message(vignette_path)

  ## We open the vignette:
  utils::browseURL(vignette_path)

  ## We return the path:
  invisible(vignette_path)
}
