#' A country finder.
#'
#' This function allows you to find out what countries are part of the given continent.
#' @param continent Write down any continent you would like!
#' @return The countries that are part of your continent.
#' @export
#' @examples
#' cou.finder("x")
cou_finder <- function(continent_wanted){
  dataset_countries <- read.csv("raw_data/package.csv")
  dataset_filtered <- select(dataset_countries, Country, continent)
  countries_found <- filter(dataset_filtered, continent == continent_wanted) %>%
  group_by(Country) %>%
    distinct(continent) %>%
    arrange(Country)
  paste0(countries_found$Country)
}
