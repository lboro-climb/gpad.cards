#' dataframe_container 
#'
#' @description A function to generate a container to display the datasets dataframe
#'
#' @return An HTMl container for a dataframe
#'
#' @noRd


dataframe_container <- function(){
  htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(colspan = 16, "Dataset Information", style = "text-align:center"),
        th(colspan = 13, "Sample Information", style = "text-align:center"),
        th(colspan = 10, "Device Information", style = "text-align:center"),
        th(colspan = 19, "Health Outcomes", style = "text-align:center"),
        th(colspan = 9, "Covariates", style = "text-align:center")
      ),
      tr(
        lapply(c(
          "Dataset ID", "Source", "Paper URL", "Dataset name", "Dataset cycle", "Study type", "Design", "Year data collection started", "Year data collection ended", "Sampling method",
          "Multi-centre?", "Continent of data collection", "Country of data collection", "Season of data collection", "Data access status", "Link (if available)", "Mean age",
          "Lower age range", "Upper age range", "Retrieved male", "Retrieved female", "Male percentage", "Female percentage", "Number recruited", "Male derived", "Female derived",
          "Derived sample", "Ethnicity group", "Ethnicity percentage", "Device brand", "Device model", "Device placement", "Number of axes measured", "Other sensors?",
          "Number of days wear", "Epoch (seconds)", "Sampling frequency (Hz)", "Removal?", "File type available", "Height", "Weight", "Waist circumference", "Hip circumference",
          "Fat mass", "Visceral fat", "SBP", "DBP", "Resting heart rate", "HDL cholesterol", "LDL cholesterol", "Total cholesterol", "Triglyceride", "HbA1c", "VLDL",
          "Blood glucose", "Insulin", "Oral glucose tolerance test", "Sex", "Age", "Ethnicity", "Education level", "Household income",
          "Socio-economic status", "Smoking status", "Clinical diagnosis medical history", "Fruit and vegetable intake"
        ), th)
      )
    )
  ))
}