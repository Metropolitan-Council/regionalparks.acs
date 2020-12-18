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


e_col <- "#009e61"
p_col <- "#E69F00"
s_col <- "#FDE725FF"



# renamekey <- tibble::tribble(
#   ~goodname,
#   ~"ACS",
#   "Total population",
#   "adj_poptotal",
#   "Age, % under 15",
#   "adj_ageunder15_per",
#   "Age, % 15-24",
#   "adj_age15_24_per",
#   "Age, % 25-64",
#   "adj_age25_64_per",
#   "Age, % 65 and up",
#   "adj_age65up_per",
#   "Race, % White",
#   "adj_whitenh_per",
#   "Race, % Black",
#   "adj_blacknh_per",
#   "Race, % Asian",
#   "adj_asiannh_per",
#   "Race, % American Indian",
#   "adj_amindnh_per",
#   "Race, % Other + Multi",
#   "adj_othermultinh_per",
#   "Ethnicity, % Hispanic",
#   "adj_hisppop_per",
#   "Ethnicity, % not-Hispanic",
#   "adj_nothisppop_per",
#   "Mean household income",
#   "adj_meanhhi",
#   "% Housholds without a vehicle",
#   "adj_novehicle_per",
#   "% speaking English less than very well",
#   "adj_lep_per",
#   "% Spanish speakers",
#   "adj_span_per"
# )


renamekey <- tibble::tribble(
  ~goodname,
  ~"ACS variable",
  "Total population",
  "adj_poptotal",
  "Age, % under 15",
  "adj_ageunder15_per",
  "Age, % 15-24",
  "adj_age15_24_per",
  "Age, % 25-64",
  "adj_age25_64_per",
  "Age, % 65 and up",
  "adj_age65up_per",
  "Race, % White",
  "adj_whitenh_per",
  "Race, % Black",
  "adj_blacknh_per",
  "Race, % Asian",
  "adj_asiannh_per",
  "Race, % American Indian",
  "adj_amindnh_per",
  "Race, % Other + Multiracial",
  "adj_othermultinh_per",
  "Ethnicity, % Hispanic",
  "adj_hisppop_per",
  "Ethnicity, % not-Hispanic",
  "adj_nothisppop_per",
  "Mean household income",
  "adj_meanhhi",
  "% Housholds without a vehicle",
  "adj_novehicle_per",
  "% speaking English less than very well",
  "adj_lep_per",
  "% Spanish speakers",
  "adj_span_per"
)
