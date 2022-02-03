#' generate_world_map
#'
#' @description A function to generate the dataset used to create the chloropleth world map
#'
#' @return A dataset to be used to create a chloropleth world map
#' @export
#'
generate_world_map <- function() {
  multi <- rv$datasets %>%
    select(Country.of.data.collection) %>%
    filter(str_detect(Country.of.data.collection, ",")) %>%
    summarise(
      Country.of.data.collection = strsplit(Country.of.data.collection, ","),
      multi_country = "Yes"
    ) %>%
    unnest(Country.of.data.collection) %>%
    mutate(
      Country.of.data.collection = str_trim(Country.of.data.collection)
    )

  participant_country <- rv$datasets %>%
    select(Country.of.data.collection, Number.recruited) %>%
    mutate(
      Country.of.data.collection = if_else(Country.of.data.collection == "United States", "United States of America", Country.of.data.collection)
    ) %>%
    group_by(Country.of.data.collection) %>%
    summarise(
      participants = sum(Number.recruited, na.rm = TRUE),
      n = n()
    )

  map <- map_data("world") %>%
    mutate(
      region = if_else(region == "UK", "United Kingdom", region),
      region = if_else(region == "USA", "United States of America", region)
    )

  map <- full_join(map, participant_country(), by = c("region" = "Country.of.data.collection")) %>%
    full_join(multi(), by = c("region" = "Country.of.data.collection")) %>%
    filter(region %in% map_filter()) %>%
    distinct()

  if (input$continent == "Oceania") {
    map <- map %>% filter(long >= 0)
  } else {
    map <- map
  }

  if (input$continent == "North America") {
    map <- map %>% filter(long <= 0)
  } else {
    map <- map
  }

  return(map)
}