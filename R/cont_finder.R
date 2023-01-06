#' A continent finder.
#'
#' This function allows you to find out what continent your country of choice resides in.
#' @param country Write down any country you would like! (as long as it's in the database that is)
#' @return The continent your country resides in.
#' @export
#' @examples
#' cont.finder("x")
cont_finder <- function(country_wanted){
  dataset_countries <- read.csv("raw_data/package.csv")
  dataset_filtered <- select(dataset_countries, Country, continent)
  continent_found <- filter(dataset_filtered, Country == country_wanted)
  continent_found <- dataset_filtered %>% filter(Country == country_wanted) %>%
    group_by(continent) %>%
    distinct(Country) %>%
    arrange(continent)
  paste0(continent_found$continent)
}







