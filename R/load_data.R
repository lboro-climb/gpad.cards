load_data <- function(data){ 
  data %>%
  mutate(
    dataset_name = str_trunc(dataset_name, width = 30, side = "right"),
    Placement = tolower(Placement),
    Placement = if_else(str_detect(Placement, ",") | str_detect(Placement, "/") | str_detect(Placement, "and"), Placement, 
                        if_else(str_detect(Placement, "hip"), "waist", 
                                if_else(str_detect(Placement, "wrist"), "wrist", 
                                        if_else(str_detect(Placement, "waist"), "waist", Placement)))),
    anthropometry = if_else(Height == "Yes" & Weight == "Yes" & Waist.circumference == "Yes" | Hip.circumference == "Yes" | Fat.mass == "Yes" | Visceral.fat == "Yes", "Yes", "No"),
    blood_pressure = if_else(SBP == "Yes" | DBP == "Yes", "Yes", "No"),
    lipids = if_else(HDL.cholesterol == "Yes" | LDL.cholesterol == "Yes" | Triglyceride == "Yes" | VLDL == "Yes", "Yes", "No"),
    glucose = if_else(Glucose == "Yes" | Insulin == "Yes" | HbA1c == "Yes" | Oral.glucose.tolerance.test == "Yes", "Yes", "No")
  )
  return(data)
}