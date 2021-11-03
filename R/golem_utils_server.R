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
#' @import shinyhelper
#' @import leaflet.extras
#' @import waiter

e_col <- "#78A22F" # CD green
p_col <- "#AA5FEC" # purple
s_col <- "#DDCC77" # yellow

require(magrittr)
load("./data/name_helper.rda")
ACSMenu <- list(
  `Age` = with(dplyr::filter(name_helper, category == "Age"), split(acscode, dropdownname)),
  `Disability` = with(dplyr::filter(name_helper, category == "Disability"), split(acscode, dropdownname)),
  `Ethnicity & Race` = with(dplyr::filter(name_helper, category == "EthRace"), split(acscode, dropdownname)),
  `Language` = with(dplyr::filter(name_helper, category == "Language"), split(acscode, dropdownname)),
  `National origin` = with(dplyr::filter(name_helper, category == "Origin"), split(acscode, dropdownname)),
  `Socioeconomic` = with(dplyr::filter(name_helper, category == "Socioeconomic"), split(acscode, dropdownname)),
  # "Housing, % cost burdened" = "adj_costburd_per"),
  `Transportation` = with(dplyr::filter(name_helper, category == "Transportation"), split(acscode, dropdownname))
)



popkey <- tibble::tribble(
  #------
  ~goodname,
  ~"popvar",
  ~"short",
  ~s2,
  "2019 population",
  "PopEst_2019",
  "2019 population\n(persons)",
  "2019 population<br>(persons)",
  "2019 population density",
  "PopDens_2019",
  "2019 population density\n(by percentile)",
  "2019 population density<br>(by percentile)",
  "2040 population",
  "POP2040",
  "2040 pop.\n(persons)",
  "2040 population<br>(persons)",
  "2040 population density",
  "popdens_2040_mi",
  "2040 population density\n(by percentile)",
  "2040 population density<br>(by percentile)",
  "Growth, relative",
  "growth_rel_10_40",
  "Relative growth\n(by percentile)",
  "Relative growth<br>(by percentile)",
  "Growth, absolute",
  "growth_abs_10_40",
  "Absolute growth\n(persons)",
  "Absolute growth<br>(persons)"
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

tract_vars <- tibble::tibble(ACS = c("adj_anydis_per", "adj_ambdis_per", "adj_costburd_per", "adj_forborn_per", "adj_usborn_per"))

quantile_vars <- tibble::tibble(mainpop = c("PopDens_2019", "popdens_2040_mi", "growth_rel_10_40"))

bin_vars <- tibble::tibble(mainacs = c("adj_ageunder15_per", "adj_age15_24_per", "adj_age25_64_per", "adj_age65up_per"))

labelFormat2 <- function(prefix = "",
                         suffix = "",
                         between = " &ndash; ",
                         digits = 0,
                         big.mark = ",",
                         transform = identity) {
  formatNum2 <- function(x) {
    format(
      round(transform(x), digits),
      trim = TRUE,
      scientific = FALSE,
      big.mark = big.mark
    )
  }

  function(type, ...) {
    switch(type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum2(cuts), suffix)
      })(...),
      # nolint
      bin = (function(cuts) {
        n <- length(cuts)
        paste0(prefix, formatNum2(cuts[-n]), between, formatNum2(cuts[-1]), suffix)
      })(...),
      # nolint
      quantile = (function(cuts, p) {
        n <- length(cuts)
        p <- paste0(round(p * 100), "%")
        cuts <- paste0(formatNum2(cuts[-n]), between, formatNum2(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          "<span title=\"",
          cuts,
          "\">",
          prefix,
          p[-n],
          between,
          p[-1],
          suffix,
          "</span>"
        )
      })(...),
      # nolint
      factor = (function(cuts) {
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...) # nolint
    )
  }
}

# leaflet global options -------------------------------------------------------

leaflet_highlight_options <- quote(
  highlightOptions(
    stroke = TRUE,
    color = "black",
    weight = 6,
    bringToFront = TRUE,
    sendToBack = TRUE,
    opacity = 1
  )
)

leaflet_popup_options <- quote(
  popupOptions(
    closeButton = FALSE,
    style = list(
      "font-size" = "18px",
      "font-family" = "Arial"
    )
  )
)
