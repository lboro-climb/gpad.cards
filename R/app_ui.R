#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "GPAD Cards",
        color = "primary",
        href = "",
        image = ""
      ),
      fixed = TRUE,
      tags$style("#home_page {cursor: pointer;}"),
      tags$style("#issue_report {cursor: pointer; padding-left: 20px;"),
      div(id = "home_page", icon("home")),
      div(id = "issue_report", "Something we can improve?")
    ),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        id = "sidebar",
        bs4Dash::menuItem("GPAD Cards", tabName = "gpadcards", icon = icon("address-card")),
        bs4Dash::menuItem("Search health outcomes", tabName = "health_outcomes", icon = icon("heartbeat")),
        bs4Dash::menuItem("Full dataset", tabName = "dataset", icon = icon("database")),
        bs4Dash::menuItem("Information", tabName = "info", icon = icon("info"))
      )
    ),
    body = bs4Dash::dashboardBody(
      shinyjs::useShinyjs(),
      tags$style(
        type = "text/css",
        ".modal-lg {width: 800px;}"
      ),
      tags$style(
        type = "text/css",
        ".modal-sm {width: 600px}"
      ),
      tags$head(tags$style("#start_modal .modal-header {background-color: #101C31; text-align: center; color: #FFFFFF;}")), 
      tags$head(tags$style(HTML(".info-box {min-height: 500px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}"))),
      tags$head(tags$style(HTML("a {color: #17A2B8; text-decoration: underline;}"))),
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "gpadcards",
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              shiny::selectizeInput(inputId = "continent", "Please select a continent:", 
                                    choices = c("All", "Europe", "North America", "South America", "Africa", "Asia", "Oceania"))
            ),
            shiny::column(
              width = 6, align = "center",
              shiny::selectizeInput(inputId = "country", "Please select a country:", 
                                    choices = NULL)
            )
          ),
          bs4Dash::box(
            shiny::fluidRow(
              shiny::column(
                width = 2,
                shiny::selectInput("sex",
                            label = "Please select a sex to filter by:",
                            choices = c("All", "Male", "Female")
                )
              ),
              shiny::column(
                width = 2,
                shiny::sliderInput("age",
                            label = "Please select an age range to filter by:",
                            min = 18, max = 100, value = c(18, 100), dragRange = TRUE, ticks = FALSE
                )
              ),
              shiny::column(
                width = 2,
                shiny::selectizeInput("ethnicity",
                               label = "Please select an ethnicity to filter by:",
                               choices = NULL,
                               multiple = TRUE,
                               options = list(placeholder = "All")
                )
              ),
              shiny::column(
                width = 2,
                shiny::selectizeInput("device_brand",
                               label = "Please select a device brand to filter by:",
                               choices = NULL,
                               multiple = TRUE,
                               options = list(placeholder = "All")
                )
              ),
              shiny::column(
                width = 2,
                shiny::selectizeInput("device_model",
                               label = "Please select a device model to filter by:",
                               choices = NULL,
                               multiple = TRUE,
                               options = list(placeholder = "All")
                )
              ),
              shiny::column(
                width = 2,
                shiny::fluidRow(
                  shiny::selectizeInput("placement",
                                 label = paste("Please select a device wear location to filter by:", sep = "\n"),
                                 choices = NULL,
                                 multiple = TRUE,
                                 options = list(placeholder = "All")
                  )
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                bs4Dash::actionButton("filter", label = "Filter"),
                bs4Dash::actionButton("reset", label = "Reset"),
                width = 12, align = "right"
              )
            ),
            width = 12, collapsed = TRUE, title = "Filter...", status = "primary"
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "left",
              bs4Dash::popover(
                shiny::h4("Summary of number of datasets per country"),
                title = "Map of datasets",
                placement = "right",
                content = "A map showing the country in which datasets have been collected and how many participants data was collected from each country.
                   The map can be filtered using the drop down menu's at the top of this page. "
              )
            ),
            shiny::column(
              width = 6, align = "left",
              shiny::h4("Key statistics about available datasets")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              plotly::plotlyOutput("map")
            ),
            shiny::column(
              width = 3, align = "center",
              tags$style("#clickdiv {cursor: pointer;}"),
              shiny::div(
                id = "clickdiv",
                bs4Dash::bs4ValueBoxOutput("n_dataset", width = 12)
              ),
              bs4Dash::bs4ValueBoxOutput("n_participants", width = 12),
              bs4Dash::bs4ValueBoxOutput("largest_dataset", width = 12)
            ),
            shiny::column(
              width = 3, align = "center",
              bs4Dash::bs4ValueBoxOutput("modal_brand", width = 12),
              bs4Dash::bs4ValueBoxOutput("modal_placement", width = 12),
              bs4Dash::bs4ValueBoxOutput("open_access", width = 12)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              bs4Dash::box(
                title = "Datasets available by participant size",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                shiny::plotOutput("largest_datasets")
              )
            ),
            shiny::column(
              width = 6, align = "center",
              bs4Dash::box(
                title = "Study type",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                shiny::fluidRow(
                  bs4Dash::bs4ValueBoxOutput("observational", width = 6),
                  bs4Dash::bs4ValueBoxOutput("longitudinal", width = 6)
                ),
                shiny::fluidRow(
                  bs4Dash::bs4ValueBoxOutput("interventional", width = 6),
                  bs4Dash::bs4ValueBoxOutput("abstract", width = 6)
                ),
                footer = shinyWidgets::radioGroupButtons(
                  inputId = "study_type_toggle",
                  choices = c("Datasets", "Participants"),
                  status = "primary"
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              bs4Dash::tabBox(
                id = "health_outcomes",
                title = "Health outcomes",
                selected = "Grouped",
                status = "primary",
                type = "tabs",
                shiny::tabPanel(
                  title = "Grouped",
                  shiny::plotOutput("grouped_health_outcomes")
                ),
                shiny::tabPanel(
                  title = "Individual",
                  shiny::plotOutput("individual_health_outcomes")
                ), solidHeader = TRUE, side = "right", width = 12,
                footer = shinyWidgets::radioGroupButtons(
                  inputId = "health_outcomes_toggle",
                  choices = c("Datasets", "Participants"),
                  status = "primary"
                )
              )
            ),
            shiny::column(
              width = 6, align = "center",
              bs4Dash::tabBox(
                id = "devices",
                side = "right",
                title = "Device Information",
                selected = "Brand",
                status = "primary",
                type = "tabs",
                shiny::tabPanel(
                  title = "Brand",
                  shiny::plotOutput("device_brand")
                ),
                shiny::tabPanel(
                  title = "Placement",
                  shiny::plotOutput("device_placement")
                ), solidHeader = TRUE, width = 12,
                footer = shinyWidgets::radioGroupButtons(
                  inputId = "device_toggle",
                  choices = c("Datasets", "Participants"),
                  status = "primary"
                )
              )
            )
          )
        ),
        bs4Dash::tabItem(
          tabName = "health_outcomes",
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              shiny::selectInput("grouped_health_outcome_select",
                          label = "If you would like to select a group of health outcomes, please do so below:",
                          choices = c("All",
                                      "Anthropometry" = "anthropometry",
                                      "Blood pressure" = "blood_pressure",
                                      "Lipids" = "lipids",
                                      "Glucose" = "glucose"
                          )
              )
            ),
            shiny::column(
              width = 6, align = "center",
              shiny::selectizeInput(
                inputId = "health_outcome_select", label = "If you would like to select an individual health outcome, please do so below:",
                choices = c("Height",
                            "Weight",
                            "Waist circumference" = "Waist.circumference",
                            "Hip circumference" = "Hip.circumference",
                            "Fat mass" = "Fat.mass",
                            "Visceral fat" = "Visceral.fat",
                            "SBP",
                            "DBP",
                            "Resting heart rate" = "Resting.heart.rate",
                            "HDL cholesterol" = "HDL.cholesterol",
                            "LDL cholesterol" = "LDL.cholesterol",
                            "Total cholesterol" = "Total.cholesterol",
                            "Triglyceride",
                            "VLDL",
                            "HbA1c",
                            "Glucose",
                            "Insulin",
                            "Oral glucose tolerance test" = "Oral.glucose.tolerance.test",
                            "Metabolic syndrome" = "Metabolic.syndrome"
                ), multiple = FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              bs4Dash::box(
                title = "Datasets",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                shiny::plotOutput("health_datasets")
              )
            ),
            shiny::column(
              width = 6, align = "center",
              bs4Dash::tabBox(
                title = "Device",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                side = "right",
                type = "tab",
                shiny::tabPanel(
                  title = "Brand",
                  shiny::plotOutput("health_device_brand")
                ),
                shiny::tabPanel(
                  title = "Placement",
                  shiny::plotOutput("health_device_placement")
                ),
                footer = shinyWidgets::radioGroupButtons(
                  inputId = "health_device_toggle",
                  choices = c("Datasets", "Participants"),
                  status = "primary"
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12, align = "center",
              DT::DTOutput("health_search")
            )
          )
        ),
        bs4Dash::tabItem(
          tabName = "dataset",
          shiny::fluidRow(
            shiny::column(
              width = 12, align = "center",
              bs4Dash::actionButton("dataset_summary", "Visualise selected dataset?"),
              bs4Dash::actionButton("dataset_compare", "Compare selected datasets?")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12, align = "center",
              DT::dataTableOutput("table")
            )
          )
        ),
        bs4Dash::tabItem(
          tabName = "info",
          shiny::fluidRow(
            shiny::column(
              width = 6, align = "center",
              bs4Dash::box(
                title = "Rationale for this resource",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                "The Global Physical Activity dataset (GPAD) cards have been designed to showcase the results of a recent scoping review. The
              review aimed to identify datasets that have collected physical activity using an accelerometric device alongside
              collecting cardiometabolic health outcomes. It was decided to obtain the most utility from this review, the creation of an
              online visualisation dashboard would best showcase our results. Therefore, we created the GPAD cards so our findings could be
              explored by others.",
                shiny::br(),
                "The full paper can be found at: "
              ),
              bs4Dash::box(
                title = "Future development",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                "It is hoped this is the first step in creating a resource that will be useful to a range of researchers looking to perform secondary data analysis, pooled data analysis or
              data harmonisation. To increase the utility of the resource, it is planned the datasets included within it will be increased over time. As a first step, we aim to add
              datasets that include mental health outcomes and in the future hope to add datasets that have measured mortality and disease outcomes.",
                shiny::br(),
                "We welcome collaborators on this project. The easiest way to contribute is to look at Research Ideas Catalogue = Knowledge Insights (RIC=KI) website. RIC=KI is an ideas
              catalogue created by Professor Amanda Daley. Ideas for future development of this resource will be posted on RIC=KI and interested researchers can express their interest.
              RIC=KI can be found at: !! Link when available !!. Alternatively, please feel free to contact Jonah Thomas at:", a(HTML("<u>", "j.j.c.thomas@lboro.ac.uk", "</u>"),
                                                                                                                                 href = "mailto:j.j.c.thomas@lboro.ac.uk"
              ),
              "and he will get back to you as soon as possible.",
              shiny::br(),
              "Source code for this application can be found in the following Github repository:", "!!Link when available!!",
              "If you find any issues or bugs with this application, please report them in the Github Issues page. If there are any enhancements you would like to see added to the
              application, please also add these to the Github Issues page."
              )
            ),
            shiny::column(
              width = 6, align = "center",
              bs4Dash::userBox(
                title = bs4Dash::userDescription(
                  title = "Jonah Thomas",
                  subtitle = "PhD Candidate",
                  type = 2,
                  image = "https://jonahthomas.netlify.app/images/jonah.png"
                ),
                status = "primary",
                width = 12,
                collapsible = FALSE,
                "Jonah is the main author of the scoping review and primary developer of this resource. Jonah is a PhD candidate in the school of Sport Exercise and Health Science at
              Loughborough University. Jonah's PhD is part of the Snacktivity project and focuses on the cardiometabolic benefits of short bouts of physical activity. For more information
              regarding Jonah's PhD, as well as some examples of his data science work can be found on his blog at:", a(HTML("<u>", "https://jonahthomas.netlify.app/", "</u>"), href = "https://jonahthomas.netlify.app/")
              ),
              bs4Dash::userBox(
                title = bs4Dash::userDescription(
                  title = "CLiMB",
                  subtitle = "Research Group",
                  type = 2,
                  image = "https://www.lboro.ac.uk/media/wwwlboroacuk/external/content/schoolsanddepartments/ssehs/photos/670x300/78677%20CLiMB%20Logo%20RGB%20A5.png"
                ),
                status = "primary",
                width = 12,
                collapsible = FALSE,
                "CLiMB is a research group based in Loughborough University. The aim of the Centre for Lifestyle Medicine and Behaviour
              (CLiMB) is to identify and evaluate innovative health behaviour interventions and policies to prevent and treat chronic
              diseases.",
                shiny::br(),
                shiny::br(),
                "The Centre works with researchers nationally and internationally, members of the public, public health organisations, health charities, the voluntary sector,
              commercial partners and the NHS. With our partners we aim to provide policymakers and organisations with robust research evidence to improve population health
              and well-being."
              )
            )
          )
        )
      )
    ),
    footer = bs4Dash::bs4DashFooter(left = shiny::HTML("We welcome all contributions and feedback on <a href=https://github.com/jonahthomas/gpad.cards>Github</a>"), 
                                    right = shiny::HTML("Version 0.0.9 &copy Jonah Thomas")),
    freshTheme = custom_colors_theme,
    preloader = list(html = shiny::tagList(shiny::h4("Welcome to the Global Physical Activity Dataset (GPAD) visualisation tool"), shiny::br(), waiter::spin_1(), "Loading..."), 
                     color = "#0092BD"),
    dark = NULL,
    help = TRUE
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'gpad.cards'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

