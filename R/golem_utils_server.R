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

renamekey <- tibble::tribble(
  ~goodname,
  ~"ACS",
  ~pal,
  ~gn2,
  "Total population",
  "adj_2019pop",
  "Blues",
  "Total population",
  "% population under age 15",
  "adj_ageunder15_per",
  "BuPu",
  "% population<br>under age 15",
  "% population age 15-24",
  "adj_age15_24_per",
  "BuPu",
  "% population<br>age 15-24",
  "% population age 25-64",
  "adj_age25_64_per",
  "BuPu",
  "% population<br>age 25-64",
  "% population age 65 and up",
  "adj_age65up_per",
  "BuPu",
  "% population<br>age 65 and up",
  "% White population",
  "adj_whitenh_per",
  "RdPu",
  "% White<br>population",
  "% Black population",
  "adj_blacknh_per",
  "RdPu",
  "% Black<br>population",
  "% Asian population",
  "adj_asiannh_per",
  "RdPu",
  "% Asian<br>population",
  "% American Indian population",
  "adj_amindnh_per",
  "RdPu",
  "% American Indian<br>population",
  "% Other + Multiracial population",
  "adj_othermultinh_per",
  "RdPu",
  "% Other + Multi-<br>racial population",
  "% Hispanic population",
  "adj_hisppop_per",
  "YlGnBu",
  "% Hispanic<br>population",
  "% not-Hispanic population",
  "adj_nothisppop_per",
  "YlGnBu",
  "% not-Hispanic<br>population",
  "Mean household income",
  "adj_meanhhi",
  "YlGnBu",
  "Mean household<br>income ($)",
  "% households below 185% poverty line",
  "adj_185pov_per",
  "YlGnBu",
  "% households below<br>185% poverty line",
  "% cost burdened households (housing >30% income)",
  "adj_costburd_per",
  "YlGnBu",
  "% cost burdened<br>households",
  "% households without a vehicle",
  "adj_novehicle_per",
  "Blues",
  "% households<br>without a vehicle",
  "% population with limited English proficiency",
  "adj_lep_per",
  "Blues",
  "% population with<br>limited English proficiency",
  "% population primarily speaking Spanish",
  "adj_span_per",
  "Blues",
  "% population primarily<br>speaking Spanish",
  "% population with any other disability",
  "adj_anydis_per",
  "OrRd",
  "% population with<br>any other disability",
  "% population with ambulatory disability",
  "adj_ambdis_per",
  "OrRd",
  "% population with<br>ambulatory disability",
  "% US-born population",
  "adj_usborn_per",
  "YlGn",
  "% US-born<br>population",
  "% foreign-born population",
  "adj_forborn_per",
  "YlGn",
  "% foreign-<br>born population"
)

recodeadjtable <- tibble::tribble(
  ~ACS,
  ~nicename,
  "adj_2019pop",
  "Weighted population (#)",
  "adj_2019hh",
  "Weighted households (#)",
  "adj_ageunder15_per",
  "Population under age 15 (%)",
  "adj_age15_24_per",
  "Population age 15-24 (%)",
  "adj_age25_64_per",
  "Population age 25-64 (%)",
  "adj_age65up_per",
  "Population age 65+ (%)",
  "adj_amindnh_per",
  "American Indian population (%)",
  "adj_asiannh_per",
  "Asian population (%)",
  "adj_blacknh_per",
  "Black population (%)",
  "adj_othermultinh_per",
  "Other + Multiracial population (%)",
  "adj_whitenh_per",
  "White population (%)",
  "adj_hisppop_per",
  "Hispanic population (%)",
  "adj_nothisppop_per",
  "Not-Hispanic population (%)",
  "adj_meanhhi",
  "Mean household income ($)",
  "adj_185pov_per",
  "Households below 185% poverty line (%)",
  "adj_costburd_per",
  "Cost burdened (housing > 30% income) households (%)",
  "adj_novehicle_per",
  "Households without a vehicle (%)",
  "adj_lep_per",
  "Population with limited English proficiency (%)",
  "adj_span_per",
  "Population primarily speaking Spanish (%)",
  "adj_ambdis_per",
  "Population with ambulatory disability (%)",
  "adj_anydis_per",
  "Population with any other disability (%)",
  "adj_forborn_per",
  "Foreign-born population (%)",
  "adj_usborn_per",
  "US-born population (%)"
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

labelFormat2 <- function(
  prefix = "",
  suffix = "",
  between = " &ndash; ",
  digits = 0,
  big.mark = ",",
  transform = identity
) {
  formatNum2 <- function(x) {
    format(
      round(transform(x), digits),
      trim = TRUE,
      scientific = FALSE,
      big.mark = big.mark
    )
  }

  function(type, ...) {
    switch(
      type,
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


# geom_stripes code rather than pulling in entire new package: -----------

#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes. The codes
#' would preferably be in the 8-hex ARGB format to allow for transparency if
#' the geom is meant to be used as visual background.
#'

geom_stripes <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomStripes <- ggplot2::ggproto(
  "GeomStripes",
  ggplot2::Geom,
  required_aes = c("y"),
  default_aes = ggplot2::aes(
    xmin = -Inf,
    xmax = Inf,
    odd = "#22222222",
    even = "#00000000",
    # Change 'size' below from 0 to NA.
    # When not NA then when *printing in pdf device* borders are there despite
    # requested 0th size. Seems to be some ggplot2 bug caused by grid overriding
    # an lwd parameter somewhere, unless the size is set to NA. Found solution here
    # https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA,
    colour = "black",
    linetype = "solid",
    size = NA
  ),
  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,
  draw_panel = function(data, panel_params, coord) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(.data$y),
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5
        ) %>%
        dplyr::select(
          .data$xmin,
          .data$xmax,
          .data$ymin,
          .data$ymax,
          .data$odd,
          .data$even,
          .data$alpha,
          .data$colour,
          .data$linetype,
          .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)

load("./data/name_helper.rda")
ACSMenu <- list(
  `Age` = with(filter(name_helper, category == "Age"), split(acscode, dropdownname)),
  `Disability` = with(filter(name_helper, category == "Disability"), split(acscode, dropdownname)),
  `Ethnicity & Race` = with(filter(name_helper, category == "EthRace"), split(acscode, dropdownname)),
  `Language` = with(filter(name_helper, category == "Language"), split(acscode, dropdownname)),
  `National origin` = with(filter(name_helper, category == "Origin"), split(acscode, dropdownname)),
  `Socioeconomic` = with(filter(name_helper, category == "Socioeconomic"), split(acscode, dropdownname)),
  # "Housing, % cost burdened" = "adj_costburd_per"),
  `Transportation` = with(filter(name_helper, category == "Transportation"), split(acscode, dropdownname))
)

