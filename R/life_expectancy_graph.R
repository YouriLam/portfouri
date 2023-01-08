#' A graph that shows the life expectancy in a specific year, in a specific continent.
#'
#' This function allows you to find out what the difference is between the life expectancy, taken from countries all residing in the same continent, in the same year.
#' @param continent,year Make sure to write down both the year AND continent wanted.The year can only be from 2002-2015, since the database doesn't go any further than those years.
#' @return A bar graph showing the difference in life expectancy between the countries.
#' @export
#' @examples
#' life_expectancy_graph("x","y")
life_expectancy_graph <- function(continent_wanted, year_wanted){
  dataset_countries <- read.csv("raw_data/package.csv")
  dataset_filtered <- select(dataset_countries, continent, Year, Country, life_expectancy)
  countries_Wanted <- dataset_filtered %>% filter(continent == continent_wanted, Year == year_wanted) %>%
    group_by(continent, life_expectancy) %>%
    arrange(Country)
  plotting <- countries_Wanted %>% ggplot(aes(x = Country, y = life_expectancy, fill = Country))+
    geom_col() +
    labs(title = "Life expectancy in different countries from a continent in a specific year",
         x = "Countries in requested continent",
         y = "Life expectancy") +
    theme_minimal()+
    theme(axis.ticks.x=element_blank())
  plotting
}

