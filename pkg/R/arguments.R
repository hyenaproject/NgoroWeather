#' Definition of the function arguments
#'
#' Here is the list of most function arguments used in the package.
#' The arguments not contained in this list are those for which the exact meaning depends on the context.
#'
#'
#' @name arguments
#'
###########################################################################################################################
# Reminder for developers: sort by alphabetical order, single line, use points as delimiter, start with cap, final point. #
###########################################################################################################################
#'
#' @param excel.path File path to .xlsx or .xls file.
#' @param input.folder File path. Folder containing all input files.
#' @param metadata Data frame. Metadata of a file to be checked.
#' @param tz Time zone used of date-time object. See ?timezones for more details on time zones in R.
#' @param verbose Logical. Should the function generate messages (default: TRUE)
#'
#' @param .cache Logical for internal use (do not change). It indicates if the function should create and use cached data. When TRUE (default) function will use cached data if it exists and create cached data if one does not#exist. When FALSE function will always build table from scratch, even if cached data already exists. The cached output differs from the non cached one since it contains 2 alternative implementation for the ending date corresponding to the possible settings for the argument `censored.to.last.sighting`.
#' @param .clean.old.csv A logical indicating whether existing `*.csv` files should be deleted from the `csv.output.folder` before new database files are downloaded. If TRUE, *all* existing `*.csv` files in `csv.output.folder` will be deleted, not just old versions of the database files. New database files will then be downloaded. If FALSE, no existing `*.csv` files in `csv.output.folder` will be deleted. New database files will then be downloaded and will *overwrite* old versions of the database files. This argument can be safely ignored if you are using R inside RStudio (interactive), you will be shown a menu where you can decide how to deal with existing '*.csv' files instead.
#' @param .fill Whether to automatically fill missing argument(s).
#' @param .git.ref The name of the commit/branch/tag on the GitHub repository from which to download the `.*csv` files. Keep the default (i.e. 'master') to download the latest `*.csv`. See the [GitHub API website](https://developer.github.com/v3/repos/contents/) for more details.
#' @param .internal Logical. Is function being used internally (TRUE) or being called directly from the global environment (FALSE). Many functions that are called internally can skip check functions (e.g. `check_function_arg.date.fromtoat()`) because inputs will have already been checked in parent function.
#' @param .parallel.min The minimum number of cases for parallel computation to be triggered (triggering parallel computing on small jobs costs more time!)
#' @param age The age(s) of the hyena(s).
#' @param age.mature The age at which an individual is considered mature and will be counted towards reproductive success.
#' Typically, this is considered to be 2yo.
#' @param age.dependent The age below which an individual is considered a dependent offspring and will be counted towards number of
#' dependent cubs. Typically, this is considered to be 1yo.
#' @param ancestorID An ID of a known ancestors at which to stop the search.
#' @param arg.max.length The maximal length of the argument (Inf: default).
#' @param args.vignette A named list defining arguments to be passed to the vignette.
#' @param argument.name Name of argument being checked. Allows for more informative errors & warnings.
#' @param at A single date using the format "YYYY-MM-DD" or "YYYY/MM/DD". Cannot be used with arguments 'from' and 'to'.
#' @param base.family Font family.
#' @param base.line.size Line thickness. Relative to font size by default.
#' @param base.rect.size Rectangle line thickness. Relative to font size by default.
#' @param base.size Font size.
#' @param by Increment for building a date sequence. Accepts character string values such as "month", "6 month", or "2 week".
#' @param censored.left A boolean: TRUE (default) to consider as censored individuals those whose beginning of life is unknown (or occurred before `from`), or FALSE to ignore this.
#' @param censored.right A boolean: TRUE (default) to consider as censored individuals those whose end of life is unknown (or occurred after `to`), or FALSE to ignore this.
#' @param censored.to.last.clan A logical indicating whether the last known location should be used to define the clan for right censored individuals (default = TRUE).
#' @param censored.to.last.sighting A logical indicating whether to replace the last life history date by the last sighting date for the given individual, FALSE (default) otherwise.
#' @param character An argument of class character.
#' @param chr A vector of character strings.
#' @param clan A vector of clan letter(s) (quoted).
#' @param clan.birth A vector of clan letter(s) (quoted). Only individuals born in these clans are returned. This is independent from clan, clan.overlap, and from/to/at arguments.
#' @param clan.overlap Overlap requirement between the clan and the focal period. Can be:
#'   - 'start' (individual started their membership in the clan during the focal period) `[ |---]`
#'   - 'end' (individual ended their membership in the clan during the focal period) `[---| ]`
#'   - 'within' (individual started and ended their membership in the clan during the focal period) `[|---|]`
#'   - 'always' (individual was a member of the clan during the whole focal period) `[-----]`
#'   - 'any' (individual was a member of the clan at some point within the focal period. The default) `[ --  ]`
#' @param coerce A logical indicating whether to coerce the output into the particular class expected for the input or not. Default is `TRUE`.
#' @param column The column name (quoted).
#' @param column.point The column name of a list-column containing point or multipoints objects (quoted).
#' @param column.polygon The column name of a list-column containing polygons(quoted).
#' @param compact A boolean indicating whether (TRUE, default) or not (FALSE) to trim the output to the most useful columns.
#' @param CPUcores The number of CPUcores to use for the computation.
#' @param crs Coordinate reference system used to create spatial objects. If missing, will use crs of rim polygon in sf_hyenaR (WGS84; EPSG:4326).
#' You may consider EPSG:21036 (Arc 1960 / UTM zone 36S) or EPSG:32736 (WGS 84 / UTM zone 36S)
#' as alternative projected coordinate systems. See [hyenaR_projection] for more details.
#' @param csv.output.folder Path to the folder where the `*.csv` files will should be read or stored.
#' @param cubID ID of the cub.
#' @param cubID.tbl A dataframe with 1 col (ID).
#' @param data.plot Primary data used as the basis of the plot. Must contain a column called 'clan'.
#' @param data.R6 R6 simulation object generated by the HyenaPopmod repository.
#' @param data.type Character string. The type of data being queried. May be 'hyena' (default) or 'weather' for weather station data.
#' @param date An argument corresponding to a date.
#' @param date.excel A numeric, corresponding to a date stored in excel as a numeric.
#' @param daughters.nb An integer vector with the number of daughters for each litter.
#' @param db.name Name of new sqlite database to create.
#' @param db.output.folder Location where new database will be saved. By default, this is the same as the location of csv files.
#' @param db.path File path. If an .sqlite file path is provided, this database file will be used. If a file path is provided to a folder, an .sqlite file inside this folder will be used. If multiple .sqlite files are found in the path, the user will be prompted to choose one. If unspecified, the function will use the dummy database file.
#' @param deaths.tbl deaths table from the database.
#' @param debug Whether the functions should run into debugging mode (`TRUE`) or not (`FALSE`, default).
#' @param devtools.ok A boolean indicating whether to build the vignette when the package is loading using devtools (`TRUE`, the default) or not (`FALSE`).
#' @param df A data frame. May be base R data frame class or tibble.
#' @param digits The number of digits to be displayed in the levels.
#' @param download.method Method passed to `download.file()`. 'auto' (default) will choose an OS appropriate method. 'wininet' is the Windows only default. 'libcurl' is the standard method of Unix systems. 'curl' provides an alternative download method, which requires the `curl` package. Using method 'curl' may be necessary to avoid corruption of .xslx weather files on download for Windows.
#' @param drat.folder Location of drat folder.
#' @param drop Whether (`TRUE`, default) or not (`FALSE`) to collapse a list column into a vector whenever possible.
#' @param duplicates A character string indicating whether the input (`input`, default) or the output (`output`) can be duplicated. Use (`none`) if neither input nor output should be replicated.
#' @param duration A number providing the maximal duration of follow up.
#' @param error.margin.alive the window period of estimated death.
#' @param error.margin.selection the window period of estimated selection.
#' @param femaleID The ID of a female.
#' @param file.name The file name of the vignette to be build, ending with `*.Rmd`.
#' @param filiation The type of parent-offspring relationship: "mother_genetic", "mother_social", "mother_social_genetic", "father", or a combination. All options are mutually exclusive: adopted cub will have mothers whose filiation are "mother_genetic" for the genetic mother and "mother_social" for the female who adopted him. Cubs never adopted will have mothers whose filiation is "mother_social_genetic", because the mother is both at the same time. Relatedness functions accept only 'mother_genetic', 'mother_social', 'father' and not 'mother_social_genetic': indeed, in this context the distinction between 'mother_social_genetic' and 'mother_genetic' is not informative, since both are real genetic mothers for the cub.
#' @param fill.value If fill is TRUE, what value should be used to replace NULL?
#' @param first.event A character vector with three options: 'observation', 'birthdate', 'conception'. When 'from' is NULL, this argument specifies the time window within which to extract information.
#' For 'observation', 'from' is the first observation in the population.
#' For 'birthdate', 'from' is the first birth date recorded in the population.
#' For 'conception', 'from' is the first conception in the population (first birthdate - 110 days).
#' @param fn A vector of functions or function names.
#' @param from The start date of a date range using the format "YYYY-MM-DD" or "YYYY/MM/DD". Combined with argument 'to' to create date range. Will default to earliest database observation date unless otherwise specified. Cannot be used with argument 'at'.
#' @param from.conception Whether the data of the first sighting is estimated from dates inferred from conception (TRUE) or not (FALSE: default).
#' @param full.clan.names A logical indicating whether clanIDS (FALSE, default) or full clan names (TRUE) should be returned.
#' @param hr.args A list containing optional arguments for the `amt::hr_xxx` function used to compute the home range.
#' @param hr.method A character string indicating which method to use to compute the home range: 'mcp' (default), or 'akde', 'kde', 'locoh'. Note that non-default method may require additional packages to be installed and are sometimes not reliable.
#' @param hr.raw Whether to output the raw output from the `amt::hr_xxx` function used to compute the home range (default = FALSE).
#' @param hyenas.tbl Hyenas data frame.
#' @param ID The ID code of hyena(s).
#' @param ID.1 One or several IDs (depending on the function).
#' @param ID.2 One or several IDs (depending on the function).
#' @param include.conception When working with sightings data. Should 'conception sightings' (those estimated from cub birthdate) be included? These do not represent true sightings of an individual, so default = FALSE.
#' @param include.na When working with sightings data. Should sightings with no coordinates be included? These may be legitimate sightings, so default = TRUE.
#' @param input.tbl Input tibble or data frame object.
#' @param input.unique Logical. Are clan values expected to be unique?
#' @param interaction.type The type of interaction between ID.1 and ID.2 either `native_related`, `native_unrelated`, `migrant_migrant`, `native_migrant`, or `migrant_native`.
#' @param join.by The explicit joining variables e.g. c("ID" = "parentID").
#' @param keep.class A boolean to choose to keep the class or not (default = TRUE).
#' @param label.data Additional data used to generate labels at the end of lines. This is not required when plotting at the population level. Must contain a column called 'label'.
#' @param length.out An integer defining the desired length of the sequence. If used, it can be in combination to 2 other arguments among `from`, `to` and `by` (optional).
#' @param libname Argument needed but automatically defined.
#' @param libpath Argument needed but automatically defined.
#' @param lifestage The specified lifestage of the individuals. Those can either be raw lifestages or meta-lifestages which correspond to combinations of raw lifestages.
#'
#'## RAW LIFESTAGES
#'   - "dead": individual that was found dead or that was not observed for >= 1 year from [`fetch_id_date.observation.last`].
#'   - "cub": individual < 1 year old.
#'   - "subadult": individual >= 1 year old and either < 2 years old or before the age of first conception or before the age of first clan selection (whichever comes first)
#'   - "natal": male born in a main clan who is >= 2 years old (sexually mature) but who has not selected a clan (not sexually active).
#'   - "unknown": male born outside a main clan who is >= 2 years old (sexually mature) but who has not yet selected a main clan. Because this individual is outside the main clans at this point we do not know their exact lifestage. Once they select a main clan they will be given the lifestage 'foreigner_X'.
#'   - "philopatric": individual born in a main clan and whose first clan selection was their birth clan. Females are 'philopatric' at 2 years old or at the age of first conception (whichever comes first).
#'   - "disperser": individual born in a main clan whose first clan selection was a clan other than their birth clan.
#'   - "selector_X": individual previously classified as 'philopatric' or 'disperser' that has made X observed selections. The number of selection events is known exactly. For example, selector_2 is an individual that we know has made 2 previous selection events.
#'   - "founder_male": sexually active adult male present in the crater at the start of the study (i.e. not cub, subadult, or natal). In these individuals the birth clan, dispersal status (philopatric or disperser), and number of previous dispersal events are all unknown.
#'   - "foreigner_X": individual whose birth clan is unknown (X) or was born outside of the main clans (rim clans e.g. U, C) and that has made X observed selections. The number of previous selection events that occurred before the first selection is unknown, therefore X is a minimum number of previous selections. foreigner_1 is an individual from X or rim clans that we have first observed making a selection. foreigner_2 is an individual from X or rim clans where we have observed 2 selection events.
#'   - "transient": any individual that left a clan but did not join another clan.
#'
#'## META-LIFESTAGES
#'   - "all": All possible lifestages including dead.
#'   - "alive": All possible lifestages excluding dead.
#'   - "preselector": Individuals that are not yet sexually active (= "cub" + "subadult" + "natal").
#'   - "sexually_active": Individuals that are sexually active (= "philopatric" + "disperser" + "selector_X" + 'foreigner_X' + "founder_male").
#'   - "selector": Individuals born in the main clans that are sexually active (= "philopatric" + "disperser" + "selector_X").
#'   - "foreigner": Individuals born outside the main clans that are sexually active (= 'foreigner_X').
#'   - "adult": All sexually mature individuals (= "natal" + "philopatric" + "foreigner" + "founder_male" + "disperser" + "transient" + "selector_X" + "foreigner_X" + "unknown").
#'   - "native": Individuals that have not left their birth clan (= "cub" + "subadult" + "natal" + "philopatric").
#'
#' When using the argument lifestage, you can also exclude lifestages using ! (e.g. "!dead").
#'
#' Note: `lifestage = NULL` (default) and `lifestage = 'all'` will return different results because `lifestage = 'all'` includes dead individuals.
#' @param lifestage.overlap Overlap requirement for the lifestage argument during the focal period. Can be:
#'   - 'start' (individual started the specified life stage during the focal period) `[ |---]`
#'   - 'end' (individual ended the specified life stage during the focal period) `[---| ]`
#'   - 'within' (individual started and ended the specified life stage during the focal period) `[|---|]`
#'   - 'always' (individual was in the specified life stage during the whole focal period) `[-----]`
#'   - 'any' (individual was in the specified life stage at some point within the focal period. The default) `[ --  ]`
#' @param lineage A vector of type of ascendancy ("mothersocial", "mothergenetic", "father").
#' @param litterID Litter code of given litters.
#' @param location Character vector. Location of weather station where data should be used. Can be 'ngoitokitok' or 'acacia'.
#' @param logical Logical argument `TRUE` or `FALSE`.
#' @param loserID The ID of the individual not supported by the MRCA.
#' @param main.clans Whether to include only crater clans (TRUE) or include rim clans (FALSE).
#' @param main.clans.birth When clan.birth is NULL, which clans should be used? FALSE (default), include individuals born in any clan. TRUE, include only individuals born in main clans.
#' @param max.commits The maximum number of commits with benchmark results to return (default = 10).
#' @param max.date If fill is TRUE, the date used to replace `to` when NULL.
#' @param migrantID The ID of a migrant individual.
#' @param migrant.mother.as.native A logical indicating if the migratory status should be changed after first conception for female migrant. Default is `FALSE`.
#' @param min.date If fill is TRUE, the date used to replace `from` when NULL.
#' @param mrcaID The ID of the Most Recent Common Ancestor.
#' @param mustWork logical: if `TRUE` then an error is given if the path does not exist; if `NA` then a warning.
#' @param nativeID The ID of a native individual.
#' @param null.ok Is NULL an accepted input? (FALSE: Default).
#' @param offspringID The ID code of hyenas(s).
#' @param output.IDcolumn The name of the variable containing the ID in the output.tbl.
#' @param output.nested Whether the new columns should be merged within a single columns (default is FALSE).
#' @param output.tbl The output table.
#' @param overlap Requirement of the overlap between a state (e.g. clan, lifestage) and a given focal period. Can be:
#'   - 'start' (state must start during the focal period) `[ |---]`
#'   - 'end' (state must end during the focal period) `[---| ]`
#'   - 'within' (state must start and end during the focal period) `[|---|]`
#'   - 'always' (state of individual does not change during the focal period) `[-----]`
#'   - 'all' (state must overlap at some point within the focal period. The default) `[ --  ]`.
#' @param overwrite A boolean indicating whether to overwrite existing compiled vignettes (`TRUE`) or not (`FALSE`, the default). If `FALSE`, the user will be prompted in case the vignette already exists.
#' @param overwrite.db Default behaviour if db.name already exists. yes: Overwrite old database file. no: Generate new db.name with todays date and version number. prompt: User is prompted to choose a behaviour. If function is run non-interactively (e.g. on the cluster) a default behaviour (TRUE or FALSE) \strong{must} be supplied.
#' @param parentID The ID code of hyenas(s).
#' @param path An argument corresponding to a path.
#' @param period A vector of period(s) (as class `character` or `Period`).
#' @param pkgname Argument needed but automatically defined.
#' @param prob.ranks The probabilities used to split the ranks.
#' @param py.input.tbl Can take a dataframe/tibble or named list. Input should have two columns/list items called `ID` (name of hyenas) and `date` ("YYYY-MM-DD").
#' @param ranks A vector of ranks.
#' @param require.data A boolean indicating whether to build the vignette data must have been loaded (`TRUE`, the default) or not (`FALSE`).
#' @param selections.tbl Selections data frame.
#' @param sex A vector of character strings. Sex can be 'male', 'female' or NA (unknown).
#' @param sightings.tbl Sightings data frame.
#' @param social.daughters.nb An integer vector with the number of social daughters for each litter.
#' @param social.sons.nb An integer vector with the number of social males for each litter.
#' @param social.unknown.nb An integer vector with the number of social cubs with unknown sex for each litter.
#' @param sons.nb An integer vector with the number of sons for each litter.
#' @param sorting.fn The function used to sort the IDs. Default is [`find_dyad_interaction.winner.from.tbl`].
#' @param station A character vector. Vector of weather station names.
#' @param std Whether the data are standardised (TRUE) or absolute (FALSE: default).
#' @param strict Whether the test should be strict (TRUE: default) and fail if not satisfied, or less strict (FALSE) and drop unknown argument values.
#' @param subtitle Character string. The subtitle of the plot.
#' @param suffix Character string. Added to the end of column name to differentiate different summary statistics columns.
#' @param tbl A data.frame or tibble.
#' @param tbl.name Character string. The name of the table to save to the database.
#' @param tbl.names Which tables you wish to load.
#' @param tick.margins Distance of tick values from the ticks. Larger value mean larger distance.
#' @param title Character string. The title of the plot.
#' @param to The end date of a date range using the format "YYYY-MM-DD" or "YYYY/MM/DD". Combined with argument 'from' to create date range. Will default to latest database observation date unless otherwise specified. Cannot be used with argument 'at'.
#' @param txt.output.folder Path to the folder where the `*.txt` files will be stored.
#' @param unit The unit for the age(s) of the hyena(s): "day", "week", "month" or "year" (default).
#' @param unknown.nb An integer vector with the number of cubs with unknown sex for each litter.
#' @param use.quarto Logical. Should the quarto package be used to render optional vignettes? By default (NULL) quarto is used to render .qmd file, while rmarkdown is used for .Rmd files.
#' @param variable A character vector. Name of columns to select from the dataframe.
#' @param vec A vector.
#' @param weather.data Logical. Should weather data be included? NOTE: Downloading and building with weather data can cause problems on Windows. See argument `download.method` in [download_package_csv] to help with trouble-shooting.
#' @param x.label Character string. The label on the x axis of the plot.
#' @param x.var Unquoted argument. Column name from primary data to plot on the x axis. Will be evaluated in the context of the primary data inside the plot.
#' @param y.label Character string. The label on the y axis of the plot.
#' @param y.var Unquoted argument. Column name from primary data to plot on the y axis. Will be evaluated in the context of the primary data inside the plot.
NULL
