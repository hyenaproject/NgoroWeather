# library(rmarkdown)
#
# # Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
# find_pandoc()
#
# suppressMessages(render(input = "./pkg/inst/extdaata/reports/NCAA_weather_report_html.Rmd",
#                         output_file = "./report/NCAA_weather_report.html"))
# suppressMessages(render(input = "./pkg/inst/extdaata/reports/NCAA_weather_report_pdf.Rmd",
#                         output_file = "./report/NCAA_weather_report.pdf"))
#
# print(paste0("Most recent report generated: ", Sys.Date()))
