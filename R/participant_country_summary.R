#' participant_country_summary
#' 
#' @description A function to summarise the number of participants in each dataset
#'
#' @param data  A dataframe to be supplied to the function
#'
#' @return A dataframe summarising the number of participants collected in each country 
#' @export
#'
#' @examples
participant_country_summary <- function(data = rv$datasets){
  data %>%
    dplyr::select(Country.of.data.collection, Number.recruited) %>%
    dplyr::mutate(
      Country.of.data.collection = dplyr::if_else(Country.of.data.collection == "United States", "United States of America", Country.of.data.collection)
    ) %>%
    dplyr::group_by(Country.of.data.collection) %>%
    dplyr::summarise(
      participants = sum(Number.recruited, na.rm = TRUE),
      n = dplyr::n()
    )
}