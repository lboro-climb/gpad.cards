#' multi_country_prep
#' 
#' @description A function to split the datasets that have collected data in multiple countries into separate vectors
#'
#' @param data A dataframe - datasets
#'
#' @return A list with datasets that collect data in multiple countries split into individual countries
#' @export
#'
multi_country_prep <- function(data = rv$datasets){
  data %>%
    dplyr::select(Country.of.data.collection) %>%
    dplyr::filter(stringr::str_detect(Country.of.data.collection, ",")) %>%
    dplyr::summarise(
      Country.of.data.collection = strsplit(Country.of.data.collection, ","),
      multi_country = "Yes"
    ) %>%
    tidyr::unnest(Country.of.data.collection) %>%
    dplyr::mutate(
      Country.of.data.collection = stringr::str_trim(Country.of.data.collection)
    )
}