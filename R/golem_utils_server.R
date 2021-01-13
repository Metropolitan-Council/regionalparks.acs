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
  "% population under age 15", "adj_ageunder15_per", "BuPu",
  "% population age 15-24", "adj_age15_24_per", "BuPu",
  "% population age 25-64", "adj_age25_64_per", "BuPu",
  "% population age 65 and up", "adj_age65up_per", "BuPu",
  "% White population", "adj_whitenh_per", "RdPu",
  "% Black population", "adj_blacknh_per", "RdPu",
  "% Asian population", "adj_asiannh_per", "RdPu",
  "% American Indian population", "adj_amindnh_per", "RdPu",
  "% Other + Multiracial population", "adj_othermultinh_per", "RdPu",
  "% Hispanic population", "adj_hisppop_per", "YlGnBu",
  "% not-Hispanic population", "adj_nothisppop_per", "YlGnBu",
  
  "Mean household income", "adj_meanhhi", "YlGnBu",
  "% households below 185% poverty line", "adj_pov185_per", "YlGnBu",

  "% housholds without a vehicle", "adj_novehicle_per", "Blues",
  "% population with limited English proficiency", "adj_lep_per", "Blues",
  "% population primarily speaking Spanish", "adj_span_per", "Blues",
  
  "% population with any disability", "adj_anydis_per", "OrRd",
  "% population with ambulatory disability", "adj_ambdis_per", "OrRd", 
  "% population with any other disability","adj_nonambdis_per", "OrRd",
  
  "% US-born population", "adj_usborn_per", "YlGn",
  "% foreign-born population", "adj_forborn_per", "YlGn"
)

recodeadjtable <- tibble::tribble(
  ~ACS, ~nicename,
  "adj_poptotal", "Population",
  "adj_ageunder15_per",  "Population under age 15",
  "adj_age15_24_per",  "Population age 15-24",
  "adj_age25_64_per", "Population age 25-64",
  "adj_age65up_per",  "Population age 65+",
  "adj_whitenh_per",  "White population",
  "adj_blacknh_per",  "Black population",
  "adj_asiannh_per",  "Asian population",
  "adj_amindnh_per",  "Am. Indian population",
  "adj_othermultinh_per",  "Other + Multiracial population",
  "adj_hisppop_per",  "Hispanic population",
  "adj_nothisppop_per",  "Not-Hispanic population",
  
  "adj_meanhhi",  "Mean household income",
  "adj_pov185_per",  "Households below 185% poverty line",
  
  "adj_novehicle_per",  "Housholds without a vehicle",
  "adj_lep_per",  "Population with limited English proficiency",
  "adj_span_per", "Population primarily speaking Spanish",
  
  "adj_anydis_per",  "Population with any disability",
  "adj_ambdis_per", "Population with ambulatory disability",
  "adj_nonambdis_per", "Population with any other disability",
  
  "adj_usborn_per",  "US-born population",
  "adj_forborn_per",  "Foreign-born population"
)

popkey <- tibble::tribble( #------
  ~goodname, ~"popvar", ~"short",
  "2019 population", "PopEst_2019", "2019 population\n(persons)",
  "2019 population density", "PopDens_2019", "2019 density\n(by percentile)",
  "2040 population", "POP2040", "2040 pop.\n(persons)",
  "2040 population density", "popdens_2040_mi", "2040 density\n(by percentile)",
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
