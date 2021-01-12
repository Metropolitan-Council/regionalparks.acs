#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

#' @import leaflet
#' @import tibble
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @import tidyr
#' @import stringr
#' @import cowplot
#' @import councilR
#' @import ggforestplot

e_col <- "#78A22F" # CD green
p_col <- "#AA5FEC" # purple
s_col <- "#DDCC77" # yellow

renamekey <- tibble::tribble(
  ~goodname, ~"ACS", ~pal,
  "Total population", "adj_2019pop", "Blues",
  "Age, % under 15", "adj_ageunder15_per", "BuPu",
  "Age, % 15-24", "adj_age15_24_per", "BuPu",
  "Age, % 25-64", "adj_age25_64_per", "BuPu",
  "Age, % 65 and up", "adj_age65up_per", "BuPu",
  "Race, % White", "adj_whitenh_per", "RdPu",
  "Race, % Black", "adj_blacknh_per", "RdPu",
  "Race, % Asian", "adj_asiannh_per", "RdPu",
  "Race, % American Indian", "adj_amindnh_per", "RdPu",
  "Race, % Other + Multiracial", "adj_othermultinh_per", "RdPu",
  "Ethnicity, % Hispanic", "adj_hisppop_per", "YlGnBu",
  "Ethnicity, % not-Hispanic", "adj_nothisppop_per", "YlGnBu",
  "Mean household income", "adj_meanhhi", "YlGnBu",
  "% Housholds without a vehicle", "adj_novehicle_per", "Blues",
  "% speaking English less than very well", "adj_lep_per", "Blues",
  "% Spanish speakers", "adj_span_per", "Blues",
  "Ability, % any disability", "adj_anydis_per", "OrRd",
  "Origin, % US-born", "adj_usborn_per", "YlGn",
  "Origin, % foreign-born", "adj_forborn_per", "YlGn"
)

recodeadjtable <- tibble::tribble(
  ~ACS,
  ~nicename,
  "adj_poptotal",
  "Population",
  "adj_ageunder15_per",
  "% under age 15",
  "adj_age15_24_per",
  "% age 15-24",
  "adj_age25_64_per",
  "% age 25-64",
  "adj_age65up_per",
  "% age 65+",
  "adj_whitenh_per",
  "% White",
  "adj_blacknh_per",
  "% Black",
  "adj_asiannh_per",
  "% Asian",
  "adj_amindnh_per",
  "% Am. Indian",
  "adj_othermultinh_per",
  "% Other + Multi",
  "adj_hisppop_per",
  "% Hispanic",
  "adj_nothisppop_per",
  "% not-Hispanic",
  "adj_meanhhi",
  "Mean household income",
  "adj_novehicle_per",
  "% Housholds without a vehicle",
  "adj_lep_per",
  "% speaking English less than very well",
  "adj_span_per",
  "% Spanish speakers",
  "adj_anydis_per",
  "Ability, % any disability",
  "adj_usborn_per",
  "Origin, % US-born",
  "adj_forborn_per",
  "Origin, % foreign-born"
)

popkey <- tibble::tribble( #------
  ~goodname, ~"popvar", ~"short",
  "2019 pop.", "PopEst_2019", "2019 pop.\n(persons)",
  "2019 pop. density", "PopDens_2019", "2019 density\n(by percentile)",
  "2040 pop.", "POP2040", "2040 pop.\n(persons)",
  "2040 pop. density", "popdens_2040_mi", "2040 density\n(by percentile)",
  "Growth, relative", "growth_rel_10_40", "Relative growth\n(by percentile)",
  "Growth, absolute", "growth_abs_10_40", "Absolute growth\n(persons)"
)


iconwater <- leaflet::awesomeIcons(
  icon = "tint",
  iconColor = "black",
  library = "glyphicon",
  markerColor = "blue"
)

iconentry <- leaflet::awesomeIcons(
  icon = "map-marker",
  iconColor = "black",
  library = "glyphicon",
  markerColor = "orange"
)
