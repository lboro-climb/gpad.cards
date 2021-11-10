#' Access files in the current app
#' 
#' NOTE: If you manually change your package name in the DESCRIPTION, 
#' don't forget to change it here too, and in the config file. 
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#' 
#' @param ... character vectors, specifying subdirectory and file(s) 
#' within your package. The default, none, returns the root of the app. 
#' 
#' @noRd
app_sys <- function(...){
  system.file(..., package = "gpad.cards")
}


#' Read App Config
#' 
#' @param value Value to retrieve from the config file. 
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE. 
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' 
#' @noRd
get_golem_config <- function(
  value, 
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE", 
    Sys.getenv(
      "R_CONFIG_ACTIVE", 
      "default"
    )
  ), 
  use_parent = TRUE
){
  config::get(
    value = value, 
    config = config, 
    # Modify this if your config file is somewhere else:
    file = app_sys("golem-config.yml"), 
    use_parent = use_parent
  )
}

custom_colors_theme <- fresh::create_theme(
  fresh::bs4dash_status(
    primary = "#101C31"
  )
)

ggplot2::theme_set(ggthemes::theme_economist())
