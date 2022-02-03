datasets <- readxl::read_xlsx("./data-raw/Data extraction form_FINAL.xlsx", skip = 1, sheet = 1, 
                              .name_repair = "universal") %>%
  dplyr::mutate(
    dataset_name = stringr::str_trunc(dataset_name, width = 30, side = "right"),
    Placement = tolower(Placement),
    Placement = dplyr::if_else(stringr::str_detect(Placement, ",") | stringr::str_detect(Placement, "/") | 
                                 stringr::str_detect(Placement, "and"), Placement, 
                        dplyr::if_else(stringr::str_detect(Placement, "hip"), "waist", 
                                       dplyr::if_else(stringr::str_detect(Placement, "wrist"), "wrist", 
                                                      dplyr::if_else(stringr::str_detect(Placement, "waist"), "waist", Placement)))),
    anthropometry = dplyr::if_else(Height == "Yes" & Weight == "Yes" & Waist.circumference == "Yes" | Hip.circumference == "Yes" | 
                                     Fat.mass == "Yes" | Visceral.fat == "Yes", "Yes", "No"),
    blood_pressure = dplyr::if_else(SBP == "Yes" | DBP == "Yes", "Yes", "No"),
    lipids = dplyr::if_else(HDL.cholesterol == "Yes" | LDL.cholesterol == "Yes" | Triglyceride == "Yes" | VLDL == "Yes", "Yes", "No"),
    glucose = dplyr::if_else(Glucose == "Yes" | Insulin == "Yes" | HbA1c == "Yes" | Oral.glucose.tolerance.test == "Yes", "Yes", "No")
  )

usethis::use_data(datasets, overwrite = TRUE)
