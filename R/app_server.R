#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  rv <- shiny::reactiveValues(
    datasets = NULL,
    datasets_table = NULL,
    health = NULL,
    sel = NULL,
    map_filter = NULL, 
    multi = NULL,
    participant_country = NULL, 
    count = 0
  )
  
  health_outcomes <- c("Height", "Weight", "Waist.circumference", "Hip.circumference", "Fat.mass", "Visceral.fat", "SBP", "DBP", "Resting.heart.rate", "HDL.cholesterol", 
                       "LDL.cholesterol", "Total.cholesterol", "Triglyceride", "VLDL", "HbA1c", "Glucose", "Insulin", "Oral.glucose.tolerance.test", "Metabolic.syndrome")
  
  # control modal dialog at start
  
  shiny::observeEvent(input$sidebar, {
    if(input$sidebar == "gpadcards" & rv$count < 1){
      start_up_modal()
      rv$count <- rv$count + 1
    }
  })
  
  shinyjs::onclick("home_page",
                   start_up_modal())
  
  shinyjs::onclick("issue_report",
                   showModal(modalDialog(
                     size = "l", 
                     renderUI({tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSfOz1vyYNMidS0xUe09XTLgCkEhmbhMg8rIzIAhF7CdmpYKlg/viewform?embedded=true", width="640", height="810")})
                   ))
                   )
  
  shinyjs::onclick("clickdiv", shiny::showModal(shiny::modalDialog(
    title = "Papers from these datasets",
    DT::dataTableOutput("paper_links"), size = "l"
  )))
  
  # add additional filters as above for 6 new inputs
  
  shiny::observeEvent(c(input$sidebar, input$country), {
    if (input$sidebar == "gpadcards") {
      if (input$country == "All") {
        if (input$continent == "All") {
          rv$datasets <- datasets
        }
      } else {
        rv$datasets <- datasets %>%
          dplyr::filter(Country.of.data.collection %in% input$country)
      }
    }
  })
  
  # need to work out how best to filter by sex - recommend convert all to percentage, then percentage of dataset, then use this to count participant numbers
  # have age filter filter by age range as well as central tendency measure
  # Have male/female filter work off datasets that contain rather than specific number
  
  shiny::observeEvent(input$filter, {
    if (input$sex == "Male") {
      rv$datasets <- rv$datasets %>% dplyr::filter(derived_male != 0)
    }
    if (input$sex == "Female") {
      rv$datasets <- rv$datasets %>% dplyr::filter(derived_female != 0)
    }
    if (input$age[1] != 18 | input$age[2] != 100) {
      rv$datasets <- rv$datasets %>% dplyr::filter(between(mean_age, input$age[1], input$age[2]) | lower_age_range > input$age[1] | upper_age_range < input$age[2])
    }
    if (!is.null(input$ethnicity)) {
      rv$datasets <- rv$datasets %>% dplyr::filter(str_detect(ethnicity_group %>% str_to_lower(), input$ethnicity))
    }
    if (!is.null(input$device_brand)) {
      rv$datasets <- rv$datasets %>% dplyr::filter(str_detect(Device.brand, input$device_brand))
    }
    if (!is.null(input$device_model)) {
      rv$datasets <- rv$datasets %>% dplyr::filter(str_detect(Devie.model, input$device_model))
    }
    if (!is.null(input$placement)) {
      rv$datasets <- rv$datasets %>% dplyr::filter(str_detect(Placement, input$placement))
    }
  })
  
  shiny::observeEvent(input$reset, {
    if(input$continent == "All" & input$country == "All"){
      rv$datasets <- datasets
    }
    if(input$continent != "All"){
      rv$datasets <- datasets %>% dplyr::filter(Continent.of.data.collection == input$continent)
    }
    if(input$country != "All"){
      rv$datasets <- datasets %>% dplyr::filter(Country.of.data.collection == input$country)
    }
    shiny::updateSelectInput(inputId = "sex", selected = "All")
    ethnicity <- rv$datasets %>% 
      dplyr::mutate(ethnicity_group = stringr::str_to_lower(ethnicity_group)) %>% 
      dplyr::summarise(ethnicity_group = stringr::str_split(ethnicity_group, ",")) %>% tidyr::unnest(cols = dplyr::everything()) %>% dplyr::distinct()
    shiny::updateSelectInput(inputId = "ethnicity", choices = ethnicity, selected = rv$datasets %>% 
                               dplyr::mutate(ethnicity_group = stringr::str_to_lower(ethnicity_group)) %>% 
                               dplyr::summarise(ethnicity_group = stringr::str_split(ethnicity_group, ",")) %>% 
                               tidyr::unnest(cols = dplyr::everything()) %>% dplyr::distinct())
    device_brand <- rv$datasets %>% dplyr::select(Device.brand)
    shiny::updateSelectInput(inputId = "device_brand", choices = device_brand)
    device_model <- rv$datasets %>% dplyr::select(Devie.model)
    shiny::updateSelectInput(inputId = "device_model", choices = device_model)
    device_placement <- rv$datasets %>% dplyr::select(Placement)
    shiny::updateSelectInput(inputId = "placement", choices = device_placement)
  })
  
  ethnicity <- shiny::reactiveVal(NULL)
  device_brand <- shiny::reactiveVal(NULL)
  device_model <- shiny::reactiveVal(NULL)
  placement <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(input$filter, {
    ethnicity(input$ethnicity)
    device_brand(input$device_brand)
    device_model(input$device_model)
    placement(input$placement)
  })
  
  shiny::observeEvent(c(input$continent, input$sidebar), {
  if (input$continent == "All") {
    rv$datasets <- datasets
    countries <- c("All", rv$datasets %>%
                     dplyr::filter(!stringr::str_detect(Country.of.data.collection, ",")) %>%
                     dplyr::select(Country.of.data.collection) %>%
                     dplyr::filter(Country.of.data.collection != "13 European Countries") %>%
                     dplyr::distinct())
    shiny::updateSelectInput(inputId = "country", choices = countries)
    ethnicity <- rv$datasets %>%
      dplyr::mutate(ethnicity_group = stringr::str_to_lower(ethnicity_group)) %>%
      dplyr::summarise(ethnicity_group = stringr::str_split(ethnicity_group, ",")) %>%
      tidyr::unnest(cols = dplyr::everything()) %>%
      dplyr::distinct()
    shiny::updateSelectInput(inputId = "ethnicity", choices = ethnicity)
    device_brand <- rv$datasets %>% dplyr::select(Device.brand)
    shiny::updateSelectInput(inputId = "device_brand", choices = device_brand)
    device_model <- rv$datasets %>% dplyr::select(Devie.model)
    shiny::updateSelectInput(inputId = "device_model", choices = device_model)
    device_placement <- rv$datasets %>% dplyr::select(Placement)
    shiny::updateSelectInput(inputId = "placement", choices = device_placement)
  } else {
    rv$datasets <- datasets %>% 
      dplyr::filter(Continent.of.data.collection == input$continent)
    countries <- c("All", rv$datasets %>%
                     dplyr::filter(Continent.of.data.collection == input$continent) %>%
      dplyr::filter(!stringr::str_detect(Country.of.data.collection, ",")) %>%
      dplyr::select(Country.of.data.collection) %>%
      dplyr::filter(Country.of.data.collection != "13 European Countries") %>%
      dplyr::distinct())
    shiny::updateSelectInput(inputId = "country", choices = countries)
    ethnicity <- rv$datasets %>%
      dplyr::mutate(ethnicity_group = stringr::str_to_lower(ethnicity_group)) %>%
      dplyr::summarise(ethnicity_group = stringr::str_split(ethnicity_group, ",")) %>%
      tidyr::unnest(cols = dplyr::everything()) %>%
      dplyr::distinct()
    shiny::updateSelectInput(inputId = "ethnicity", choices = ethnicity)
    device_brand <- rv$datasets %>% dplyr::select(Device.brand)
    shiny::updateSelectInput(inputId = "device_brand", choices = device_brand)
    device_model <- rv$datasets %>% dplyr::select(Devie.model)
    shiny::updateSelectInput(inputId = "device_model", choices = device_model)
    device_placement <- rv$datasets %>% dplyr::select(Placement)
    shiny::updateSelectInput(inputId = "placement", choices = device_placement)
  }
})
  
  shiny::observeEvent(input$filter, {
    ethnicity <- rv$datasets %>% 
      dplyr::mutate(ethnicity_group = stringr::str_to_lower(ethnicity_group)) %>% 
      dplyr::summarise(ethnicity_group = stringr::str_split(ethnicity_group, ",")) %>% 
      tidyr::unnest(cols = dplyr::everything()) %>% 
      dplyr::distinct()
    shiny::updateSelectInput(inputId = "ethnicity", choices = ethnicity, selected = ethnicity())
    device_brand <- rv$datasets %>% dplyr::select(Device.brand)
    shiny::updateSelectInput(inputId = "device_brand", choices = device_brand, selected = device_brand())
    device_model <-  rv$datasets %>% dplyr::select(Devie.model)
    shiny::updateSelectInput(inputId = "device_model", choices = device_model, selected = device_model())
    device_placement <- rv$datasets %>% dplyr::select(Placement)
    shiny::updateSelectInput(inputId = "placement", choices = device_placement, selected = placement())
  })
  
  shiny::observeEvent(c(input$sidebar, input$continent), {
    if (input$continent == "All") {
      rv$map_filter <- continents %>%
        dplyr::select(Country) %>%
        unlist()
    } else {
      rv$datasets <- datasets %>%
        dplyr::filter(Continent.of.data.collection == input$continent)
      rv$map_filter <- continents %>%
        dplyr::filter(Continent == input$continent) %>%
        dplyr::select(Country) %>%
        unlist()
    }
    })
  
  shiny::observeEvent(c(input$sidebar, input$country), {
    rv$multi <- multi_country_prep(data = rv$datasets)
    rv$participant_country <- participant_country_summary(data = rv$datasets)
  })
  
  
  
  output$n_dataset <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Datasets available",
      value = rv$datasets %>% nrow(),
      color = "info",
      icon = icon("table"),
      width = 12,
      fill = TRUE
    )
  })
  
  output$paper_links <- DT::renderDataTable(
    {
      rv$datasets %>%
        dplyr::select(dataset_name, paper_url) %>%
        dplyr::mutate(
          dataset_name = paste0("<a href=\"", paper_url, "\" target=\"_blank\">", dataset_name, "</a>")
        ) %>%
        dplyr::select(dataset_name)
    },
    escape = FALSE,
    rownames = FALSE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = "200px", targets = "_all"))
    )
  )
  
  output$n_participants <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Number of participants",
      value =
        rv$datasets %>% dplyr::select(derived_sample) %>% dplyr::summarise(
          n = sum(derived_sample, na.rm = TRUE)
        ),
      color = "info",
      icon = shiny::icon("walking"),
      width = 12,
      fill = TRUE
    )
  })
  
  output$largest_dataset <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Most participants",
      value = rv$datasets %>% dplyr::select(dataset_name, derived_sample) %>% dplyr::group_by(dataset_name) %>%
        dplyr::summarise(n = sum(derived_sample, na.rm = TRUE)) %>% dplyr::arrange(-n) %>% head(1) %>% dplyr::select(1),
      color = "info",
      icon = shiny::icon("users"),
      width = 12,
      fill = TRUE
    )
  })
  
  output$modal_brand <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Most used device",
      value = names(sort(table(rv$datasets$Device.brand), decreasing = TRUE)[1]),
      width = 12,
      color = "info",
      icon = shiny::icon("tablet"),
      fill = TRUE
    )
  })
  
  output$modal_placement <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Most common wear location",
      value = names(sort(table(rv$datasets$Placement), decreasing = TRUE)[1]),
      width = 12,
      color = "info",
      icon = shiny::icon("child"),
      fill = TRUE
    )
  })
  
  output$open_access <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Number of open access datasets",
      value = rv$datasets %>% dplyr::count(Data.access.status) %>% dplyr::filter(Data.access.status == "Open") %>% dplyr::select(n),
      width = 12,
      color = "info",
      icon = shiny::icon("database"),
      fill = TRUE
    )
  })
  
  world_map <- shiny::reactive({
    map <- ggplot2::map_data("world") %>%
      dplyr::mutate(
        region = dplyr::if_else(region == "UK", "United Kingdom", region),
        region = dplyr::if_else(region == "USA", "United States of America", region)
      )
    
    map <- dplyr::full_join(map, rv$participant_country, by = c("region" = "Country.of.data.collection")) %>%
      dplyr::full_join(rv$multi, by = c("region" = "Country.of.data.collection")) %>%
      dplyr::filter(region %in% rv$map_filter) %>%
      dplyr::distinct()
    
    
    if (input$continent != "All") {
      if ("Oceania" %in% input$continent) {
        map <- map %>% dplyr::filter(long >= 0)
      } else {
        map <- map
      }
      if ("North America" %in%input$continent) {
        map <- map %>% dplyr::filter(long <= 0)
      } else {
        map <- map
      }
    }
    
    map
  })
  
  output$map <- plotly::renderPlotly({
    map <- ggplot2::ggplot(world_map(), ggplot2::aes(x = long, y = lat, group = group, fill = log(participants), text = paste(paste("Region:", region),
                                                                                                            paste("Number of datasets:", n),
                                                                                                            paste("Number of participants: ", participants),
                                                                                                            paste("Multi-country study: ", multi_country),
                                                                                                            sep = "\n"
    ))) +
      ggplot2::geom_polygon() +
      ggplot2::coord_map("rectangular", lat0 = 30) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
    plotly::ggplotly(map, tooltip = "text") %>%
      plotly::layout(
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
      )
  })
  
  output$observational <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Observational",
      value = if (input$study_type_toggle == "Datasets") {
        rv$datasets %>%
          dplyr::count(Study.type) %>%
          dplyr::filter(Study.type == "Observational") %>%
          dplyr::select(n) %>%
          unlist()
      } else {
        rv$datasets %>%
          dplyr::group_by(Study.type) %>%
          dplyr::summarise(
            participants = sum(derived_sample, na.rm = TRUE)
          ) %>%
          dplyr::filter(Study.type == "Observational") %>%
          dplyr::select(participants) %>%
          unlist()
      },
      width = 12,
      color = "info",
      icon = shiny::icon("glasses"),
      fill = TRUE
    )
  })
  
  output$interventional <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Interventional",
      value = if (input$study_type_toggle == "Datasets") {
        rv$datasets %>%
          dplyr::count(Study.type) %>%
          dplyr::filter(Study.type == "Interventional") %>%
          dplyr::select(n) %>%
          unlist()
      } else {
        rv$datasets %>%
          dplyr::group_by(Study.type) %>%
          dplyr::summarise(
            participants = sum(derived_sample, na.rm = TRUE)
          ) %>%
          dplyr::filter(Study.type == "Interventional") %>%
          dplyr::select(participants) %>%
          unlist()
      },
      width = 12,
      color = "info",
      icon = shiny::icon("tablet"),
      fill = TRUE
    )
  })
  
  output$longitudinal <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Longitudinal",
      value = if (input$study_type_toggle == "Datasets") {
        rv$datasets %>%
          dplyr::count(Study.type) %>%
          dplyr::filter(Study.type == "Longitudinal") %>%
          dplyr::select(n) %>%
          unlist()
      } else {
        rv$datasets %>%
          dplyr::group_by(Study.type) %>%
          dplyr::summarise(
            participants = sum(derived_sample, na.rm = TRUE)
          ) %>%
          dplyr::filter(Study.type == "Longitudinal") %>%
          dplyr::select(participants) %>%
          unlist()
      },
      width = 12,
      color = "info",
      icon = shiny::icon("clock"),
      fill = TRUE
    )
  })
  
  output$abstract <- bs4Dash::renderbs4InfoBox({
    bs4Dash::bs4InfoBox(
      title = "Abstracts only",
      value = if (input$study_type_toggle == "Datasets") {
        rv$datasets %>%
          dplyr::count(Study.type) %>%
          dplyr::filter(Study.type == "Abstract only") %>%
          dplyr::select(n) %>%
          unlist()
      } else {
        rv$datasets %>%
          dplyr::group_by(Study.type) %>%
          dplyr::summarise(
            participants = sum(derived_sample, na.rm = TRUE)
          ) %>%
          dplyr::filter(Study.type == "Abstract only") %>%
          dplyr::select(participants) %>%
          unlist()
      },
      width = 12,
      color = "info",
      icon = shiny::icon("sticky-note"),
      fill = TRUE
    )
  })
  
  output$device_brand <- shiny::renderPlot({
    if (input$device_toggle == "Datasets") {
      rv$datasets %>%
        dplyr::select(Device.brand) %>%
        dplyr::count(Device.brand) %>%
        dplyr::mutate(
          Device.brand = forcats::fct_lump_n(Device.brand, n = 10, w = n)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(Device.brand, n), n)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2:: scale_y_continuous(name = "Number of datasets")
    } else {
      rv$datasets %>%
        dplyr::select(Device.brand, derived_sample) %>%
        dplyr::group_by(Device.brand) %>%
        dplyr::summarise(
          total = sum(derived_sample, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          Device.brand = forcats::fct_lump_n(Device.brand, n = 10, w = total)
        ) %>%
        dplyr::group_by(Device.brand) %>%
        dplyr::summarise(
          total = sum(total, na.rm = TRUE)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = reorder(Device.brand, total), y = total)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of participants")
    }
  })
  
  output$device_placement <- shiny::renderPlot({
    if (input$device_toggle == "Datasets") {
      rv$datasets %>%
        dplyr::count(Placement) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(Placement, n), n)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of datasets")
    } else {
      rv$datasets %>%
        dplyr::select(Placement, derived_sample) %>%
        dplyr::group_by(Placement) %>%
        dplyr::summarise(
          derived_sample = sum(derived_sample, na.rm = TRUE)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = reorder(Placement, derived_sample), y = derived_sample)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of participants")
    }
  })
  
  output$largest_datasets <- shiny::renderPlot({
    rv$datasets %>%
      dplyr::select(dataset_name, derived_sample) %>%
      dplyr::arrange(-derived_sample) %>%
      dplyr::mutate(
        rank = 1:dplyr::n()
      ) %>%
      dplyr::filter(rank <= 15) %>%
      dplyr::group_by(dataset_name) %>%
      dplyr::summarise(
        derived_sample = sum(derived_sample, na.rm = TRUE)
      ) %>%
      ggplot2::ggplot(ggplot2::aes(reorder(dataset_name, derived_sample), derived_sample)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::scale_y_continuous(name = "Number of participants")
  })
  
  output$study_design <- shiny::renderPlot({
    if (input$study_type_toggle == "Datasets") {
      rv$datasets %>%
        dplyr::select(Study.type) %>%
        dplyr::count(Study.type) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(Study.type, n), n)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of datasets")
    } else {
      rv$datasets %>%
        dplyr::select(derived_sample, Study.type) %>%
        dplyr::group_by(Study.type) %>%
        dplyr::summarise(
          participants = sum(derived_sample, na.rm = TRUE)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(Study.type, participants), participants)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of participants")
    }
  })
  
  output$individual_health_outcomes <- shiny::renderPlot({
    if (input$health_outcomes_toggle == "Datasets") {
      rv$datasets %>%
        dplyr::select(Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol, Total.cholesterol, Triglyceride, VLDL, HbA1c, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "name", values_to = "value") %>%
        dplyr::mutate(
          value = dplyr::if_else(value == "Yes", 1, 0)
        ) %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          value = sum(value, na.rm = TRUE)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(name, value), value)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of studies")
    } else {
      rbind(
        rv$datasets %>%
          dplyr::filter(Height == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Weight == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Waist.circumference == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Hip.circumference == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Fat.mass == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Visceral.fat == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(SBP == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(DBP == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Resting.heart.rate == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(HDL.cholesterol == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(LDL.cholesterol == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Total.cholesterol == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Triglyceride == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(VLDL == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(HbA1c == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Glucose == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Insulin == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Oral.glucose.tolerance.test == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$datasets %>%
          dplyr::filter(Metabolic.syndrome == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          )
      ) %>%
        cbind(health_outcomes) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(health_outcomes, participants), participants)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of participants")
    }
  })
  
  output$grouped_health_outcomes <- renderPlot({
    if (input$health_outcomes_toggle == "Datasets") {
      rv$datasets %>%
        dplyr::mutate(
          anthropometry = dplyr::if_else(Height == "Yes" & Weight == "Yes" & Waist.circumference == "Yes" | Hip.circumference == "Yes" | Fat.mass == "Yes" | Visceral.fat == "Yes", "Yes", "No"),
          blood_pressure = dplyr::if_else(SBP == "Yes" | DBP == "Yes", "Yes", "No"),
          lipids = dplyr::if_else(HDL.cholesterol == "Yes" | LDL.cholesterol == "Yes" | Triglyceride == "Yes" | VLDL == "Yes", "Yes", "No"),
          glucose = dplyr::if_else(Glucose == "Yes" | Insulin == "Yes" | HbA1c == "Yes" | Oral.glucose.tolerance.test == "Yes", "Yes", "No")
        ) %>%
        dplyr::select(anthropometry, blood_pressure, lipids, glucose) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), values_to = "value", names_to = "name") %>%
        dplyr::mutate(
          value = dplyr::if_else(value == "Yes", 1, 0)
        ) %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          sum = sum(value, na.rm = TRUE)
        ) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(name, -sum), sum)) +
        ggplot2::geom_col() +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of datasets")
    } else {
      rv$health <- rv$datasets %>%
        dplyr::mutate(
          anthropometry = dplyr::if_else(Height == "Yes" & Weight == "Yes" & Waist.circumference == "Yes" | Hip.circumference == "Yes" | Fat.mass == "Yes" | Visceral.fat == "Yes", "Yes", "No"),
          blood_pressure = dplyr::if_else(SBP == "Yes" | DBP == "Yes", "Yes", "No"),
          lipids = dplyr::if_else(HDL.cholesterol == "Yes" | LDL.cholesterol == "Yes" | Triglyceride == "Yes" | VLDL == "Yes", "Yes", "No"),
          glucose = dplyr::if_else(Glucose == "Yes" | Insulin == "Yes" | HbA1c == "Yes" | Oral.glucose.tolerance.test == "Yes", "Yes", "No")
        ) %>%
        dplyr::select(derived_sample, anthropometry, blood_pressure, lipids, glucose)
      
      rbind(
        rv$health %>%
          dplyr::filter(anthropometry == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$health %>%
          dplyr::filter(blood_pressure == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$health %>%
          dplyr::filter(lipids == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          ),
        rv$health %>%
          dplyr::filter(glucose == "Yes") %>%
          dplyr::summarise(
            n = dplyr::n(),
            participants = sum(derived_sample, na.rm = TRUE)
          )
      ) %>%
        cbind(health_outcomes = colnames(rv$health[, 2:5])) %>%
        ggplot2::ggplot(ggplot2::aes(reorder(health_outcomes, -participants), participants)) +
        ggplot2::geom_col() +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_y_continuous(name = "Number of participants")
    }
  })
  
  sketch <- dataframe_container()
  
  shiny::observeEvent(input$dataset_summary, {
    if (length(input$table_rows_selected) == 1) {
      shiny::showModal(shiny::modalDialog(
        size = "l",
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "center",
            shiny::h1(rv$datasets[rv$sel, ] %>% dplyr::select(dataset_name) %>% unlist())
          ),
          shiny::column(
            width = 6, align = "center",
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Number of participants:", rv$datasets[rv$sel, ] %>% dplyr::select(derived_sample) %>% unlist(), sep = "\n"),
                icon = shiny::icon("running"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Mean age: ", rv$datasets[rv$sel, ] %>% dplyr::select(mean_age) %>% unlist(), sep = "\n"),
                icon = shiny::icon("calendar"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = shiny::HTML(
                  paste0("Percent male: ", rv$datasets[rv$sel, ] %>% dplyr::select(male_percent) %>% unlist()), "<br/>",
                  paste0("Percent female:", rv$datasets[rv$sel, ] %>% dplyr::select(female_percent) %>% unlist())
                ),
                icon = shiny::icon("venus-mars"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Ethnicities collected:", rv$datasets[rv$sel, ] %>% dplyr::select(ethnicity_group) %>% unlist(), sep = "\n"),
                icon = shiny::icon("globe"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Device used:", rv$datasets[rv$sel, ] %>% dplyr::select(Device.brand) %>% unlist(), sep = "\n"),
                icon = shiny::icon("tablet"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Device model:", rv$datasets[rv$sel, ] %>% dplyr::select(Devie.model) %>% unlist(), sep = "\n"),
                icon = shiny::icon("tablet"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            )
          ),
          shiny::column(
            width = 6, align = "center",
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Device wear location:", rv$datasets[rv$sel, ] %>% dplyr::select(Placement) %>% unlist(), sep = "\n"),
                icon = shiny::icon("child"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Dataset type:", rv$datasets[rv$sel, ] %>% dplyr::select(Study.type) %>% unlist(), sep = "\n"),
                icon = shiny::icon("table"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = shiny::HTML(paste0("Country of data collection:", "<br/>", rv$datasets[rv$sel, ] %>% dplyr::select(Country.of.data.collection) %>% unlist())),
                icon = shiny::icon("globe"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Dataset access status:", rv$datasets[rv$sel, ] %>% dplyr::select(Data.access.status) %>% unlist(), sep = "\n"),
                icon = shiny::icon("database"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Number of covariates collected:", rv$datasets[rv$sel, ] %>%
                                dplyr::select(Sex, Age, Ethnicity, Education.level, Household.income, Socio.economic.status, Smoking.status, 
                                              Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake) %>%
                                dplyr::mutate(across(.cols = dplyr::everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                dplyr::rowwise() %>%
                                dplyr::summarise(
                                  covariates = sum(c(Sex, Age, Ethnicity, Education.level, Household.income, Socio.economic.status, Smoking.status, 
                                                     Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake), na.rm = TRUE)
                                ) %>% unlist(), sep = "\n"),
                icon = shiny::icon("home"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            ),
            bs4Dash::renderbs4InfoBox(
              bs4Dash::bs4InfoBox(
                title = "",
                value = paste("Number of health outcomes collected:", rv$datasets[rv$sel, ] %>%
                                dplyr::select(
                                  Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol,
                                  Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                ) %>%
                                dplyr::mutate(across(.cols = dplyr::everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                dplyr::rowwise() %>%
                                dplyr::summarise(
                                  covariates = sum(c(
                                    Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol,
                                    Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                  ), na.rm = TRUE)
                                ) %>% unlist(), sep = "\n"),
                icon = shiny::icon("weight"),
                width = 12,
                color = "info",
                fill = TRUE
              )
            )
          )
        )
      ))
    } else {
      shiny::showModal(shiny::modalDialog(
        title = "Please select only one dataset to summarise",
        "Currently, summarising of only one dataset at a time is supported."
      ))
    }
  })
  
  shiny::observeEvent(input$dataset_compare, {
    if (length(input$table_rows_selected) == 2) {
      shiny::showModal(tags$div(id = "modal1", shiny::modalDialog(
        size = "l",
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "center",
            shiny::h4(rv$datasets[rv$sel[[1]], ] %>% dplyr::select(dataset_name) %>% unlist())
          ),
          shiny::column(
            width = 6, align = "center",
            shiny::h4(rv$datasets[rv$sel[[2]], ] %>% dplyr::select(dataset_name) %>% unlist())
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "center",
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of participants:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(derived_sample) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("running"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Mean age: ", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(mean_age) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("calendar"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = shiny::HTML(
                      paste0("Percent male: ", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(male_percent) %>% unlist()), "<br/>",
                      paste0("Percent female:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(female_percent) %>% unlist())
                    ),
                    icon = shiny::icon("venus-mars"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Ethnicities collected:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(ethnicity_group) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("globe"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Dataset type:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Study.type) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("table"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device used:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Device.brand) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("tablet"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device model:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Devie.model) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("tablet"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device wear location:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Placement) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("child"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Country of data collection:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Country.of.data.collection) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("globe"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Dataset access status:", rv$datasets[rv$sel[[1]], ] %>% dplyr::select(Data.access.status) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("database"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of covariates collected:", rv$datasets[rv$sel[[1]], ] %>%
                                    dplyr::select(Sex, Age, Ethnicity, Education.level, Household.income, Socio.economic.status, Smoking.status, Clinical.diagnosis.medical.history, 
                                                  Fruit.and.vegetable.intake) %>%
                                    dplyr::mutate(across(.cols = everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                    dplyr::rowwise() %>%
                                    dplyr::summarise(
                                      covariates = sum(c(Sex, Age, Ethnicity, Education.level, Household.income, Socio.economic.status, Smoking.status, 
                                                         Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake), na.rm = TRUE)
                                    ) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("home"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of health outcomes collected:", rv$datasets[rv$sel[[1]], ] %>%
                                    dplyr::select(
                                      Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, 
                                      LDL.cholesterol, Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                    ) %>%
                                    dplyr::mutate(across(.cols = everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                    dplyr::rowwise() %>%
                                    dplyr::summarise(
                                      covariates = sum(c(
                                        Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, 
                                        LDL.cholesterol, Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                      ), na.rm = TRUE)
                                    ) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("weight"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            tags$head(tags$style(".row{height:120px;}")),
            tags$head(tags$style("#modal1 .modal-body {min-height: 1600px;}"))
          ),
          shiny::column(
            width = 6, align = "center",
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of participants:", rv$datasets[rv$sel[[2]], ] %>% dplyr::select(derived_sample) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("running"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Mean age: ", rv$datasets[rv$sel[[2]], ] %>% dplyr::select(mean_age) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("calendar"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = shiny::HTML(
                      paste0("Percent male: ", rv$datasets[rv$sel[[2]], ] %>% dplyr::select(male_percent) %>% unlist()), "<br/>",
                      paste0("Percent female:", rv$datasets[rv$sel[[2]], ] %>% dplyr::select(female_percent) %>% unlist())
                    ),
                    icon = shiny::icon("venus-mars"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Ethnicities collected:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(ethnicity_group) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("globe"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Dataset type:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Study.type) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("table"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device used:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Device.brand) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("tablet"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device model:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Devie.model) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("tablet"),
                    width = 12,
                    color = "info",
                    fill = TRUE
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Device wear location:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Placement) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("child"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Country of data collection:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Country.of.data.collection) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("globe"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Dataset access status:", rv$datasets[rv$sel[[2]], ] %>% 
                                    dplyr::select(Data.access.status) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("database"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of covariates collected:", rv$datasets[rv$sel[[2]], ] %>%
                                    dplyr::select(Sex, Age, Ethnicity, Education.level, Household.income, Socio.economic.status, 
                                                  Smoking.status, Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake) %>%
                                    dplyr::mutate(across(.cols = dplyr::everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                    dplyr::rowwise() %>%
                                    dplyr::summarise(
                                      covariates = sum(c(Sex, Age, Ethnicity, Education.level, Household.income, 
                                                         Socio.economic.status, Smoking.status, Clinical.diagnosis.medical.history, 
                                                         Fruit.and.vegetable.intake), na.rm = TRUE)
                                    ) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("home"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            ),
            shiny::fluidRow(
              class = "row",
              shiny::column(
                align = "center", width = 12,
                bs4Dash::renderbs4InfoBox(
                  bs4Dash::bs4InfoBox(
                    title = "",
                    value = paste("Number of health outcomes collected:", rv$datasets[rv$sel[[2]], ] %>%
                                    dplyr::select(
                                      Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, 
                                      Resting.heart.rate, HDL.cholesterol, LDL.cholesterol, Total.cholesterol, Triglyceride, HbA1c, 
                                      VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                    ) %>%
                                    dplyr::mutate(across(.cols = dplyr::everything(), ~ dplyr::if_else(. == "Yes", 1, 0))) %>%
                                    dplyr::rowwise() %>%
                                    dplyr::summarise(
                                      covariates = sum(c(
                                        Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, 
                                        Resting.heart.rate, HDL.cholesterol, LDL.cholesterol, Total.cholesterol, Triglyceride, HbA1c,
                                        VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome
                                      ), na.rm = TRUE)
                                    ) %>% unlist(), sep = "\n"),
                    icon = shiny::icon("weight"),
                    width = 12,
                    fill = TRUE,
                    color = "info"
                  )
                )
              )
            )
          )
        )
      )))
    } else {
      shiny::showModal(shiny::modalDialog(
        title = "Please select two datasets to compare."
      ))
    }
  })
  
  shiny::observe({
    if (length(input$table_rows_selected) != 2) {
      shinyjs::hide("dataset_compare")
    } else {
      shinyjs::show("dataset_compare")
    }
  })
  
  shiny::observe({
    if (length(input$table_rows_selected) > 1) {
      shinyjs::hide("dataset_summary")
    } else {
      shinyjs::show("dataset_summary")
    }
  })
  
  shiny::observe({
    if (length(input$table_rows_selected) > 2) {
      rv$sel <- NULL
      shiny::showModal(shiny::modalDialog(
        title = "Please only select two datasets to compare",
        "Currently, comparing more than 2 datasets is not supported."
      ))
    } else {
      rv$sel <- input$table_rows_selected
    }
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(rv$datasets %>% dplyr::select(
      1:39, Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol,
      Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome, Sex, Age, Ethnicity, Education.level,
      Household.income, Socio.economic.status, Smoking.status, Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake
    ),
    container = sketch,
    selection = list(mode = "multiple", selected = rv$sel),
    rownames = FALSE,
    options = list(autoWidth = FALSE, scrollX = TRUE, processing = FALSE)
    )
  )
  
  shiny::observe({
    if (input$grouped_health_outcome_select == "All") {
      shinyjs::show("health_outcome_select")
    } else {
      shinyjs::hide("health_outcome_select")
    }
  })
  
  shiny::observeEvent(input$grouped_health_outcome_select, {
    if (input$grouped_health_outcome_select == "All") {
      output$health_datasets <- shiny::renderPlot({
        rv$datasets %>%
          dplyr::filter(get(input$health_outcome_select) == "Yes") %>%
          dplyr::arrange(-derived_sample) %>%
          dplyr::mutate(
            rank = 1:dplyr::n()
          ) %>%
          dplyr::filter(rank <= 15) %>%
          ggplot2::ggplot(ggplot2::aes(reorder(dataset_name, derived_sample), derived_sample)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::scale_x_discrete(name = NULL) +
          ggplot2::scale_y_continuous(name = "Number of participants")
      })
      
      output$health_device_brand <- shiny::renderPlot({
        if (input$health_device_toggle == "Datasets") {
          rv$datasets %>%
            dplyr::filter(get(input$health_outcome_select) == "Yes") %>%
            dplyr::select(Device.brand) %>%
            dplyr::count(Device.brand) %>%
            dplyr::mutate(
              Device.brand = forcats::fct_lump_n(Device.brand, n = 10, w = n)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(reorder(Device.brand, n), n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of datasets")
        } else {
          rv$datasets %>%
            dplyr::filter(get(input$health_outcome_select) == "Yes") %>%
            dplyr::select(Device.brand, derived_sample) %>%
            dplyr::group_by(Device.brand) %>%
            dplyr::summarise(
              total = sum(derived_sample, na.rm = TRUE)
            ) %>%
            dplyr::mutate(
              Device.brand = fct_lump_n(Device.brand, n = 10, w = total)
            ) %>%
            dplyr::group_by(Device.brand) %>%
            dplyr::summarise(
              total = sum(total, na.rm = TRUE)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Device.brand, total), y = total)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of participants")
        }
      })
      
      output$health_device_placement <- shiny::renderPlot({
        if (input$health_device_toggle == "Datasets") {
          rv$datasets %>%
            dplyr::filter(get(input$health_outcome_select) == "Yes") %>%
            dplyr::count(Placement) %>%
            ggplot2::ggplot(ggplot2::aes(reorder(Placement, n), n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of datasets")
        } else {
          rv$datasets %>%
            dplyr::filter(get(input$health_outcome_select) == "Yes") %>%
            dplyr::select(Placement, derived_sample) %>%
            dplyr::group_by(Placement) %>%
            dplyr::summarise(
              derived_sample = sum(derived_sample, na.rm = TRUE)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Placement, derived_sample), y = derived_sample)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of participants")
        }
      })
      
      output$health_search <- DT::renderDataTable(
        DT::datatable(rv$datasets %>% dplyr::select(
          1:39, Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol,
          Total.cholesterol, Triglyceride, HbA1c, VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome, Sex, Age, Ethnicity, Education.level,
          Household.income, Socio.economic.status, Smoking.status, Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake
        ) %>%
          dplyr::filter(get(input$health_outcome_select) == "Yes"),
        container = sketch,
        selection = list(mode = "multiple", selected = rv$sel),
        rownames = FALSE,
        options = list(autoWidth = FALSE, scrollX = TRUE, processing = FALSE)
        )
      )
    } else {
      output$health_datasets <- shiny::renderPlot({
        rv$datasets %>%
          dplyr::filter(get(input$grouped_health_outcome_select) == "Yes") %>%
          dplyr::arrange(-derived_sample) %>%
          dplyr::mutate(
            rank = 1:dplyr::n()
          ) %>%
          dplyr::filter(rank <= 15) %>%
          ggplot2::ggplot(ggplot2::aes(reorder(dataset_name, derived_sample), derived_sample)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::scale_x_discrete(name = NULL) +
          ggplot2::scale_y_continuous(name = "Number of participants")
      })
      
      output$health_device_brand <- shiny::renderPlot({
        if (input$health_device_toggle == "Datasets") {
          rv$datasets %>%
            dplyr::filter(get(input$grouped_health_outcome_select) == "Yes") %>%
            dplyr::select(Device.brand) %>%
            dplyr::count(Device.brand) %>%
            dplyr::mutate(
              Device.brand = forcats::fct_lump_n(Device.brand, n = 10, w = n)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(reorder(Device.brand, n), n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of datasets")
        } else {
          rv$datasets %>%
            dplyr::filter(get(input$grouped_health_outcome_select) == "Yes") %>%
            dplyr::select(Device.brand, derived_sample) %>%
            dplyr::group_by(Device.brand) %>%
            dplyr::summarise(
              total = sum(derived_sample, na.rm = TRUE)
            ) %>%
            dplyr::mutate(
              Device.brand = forcats::fct_lump_n(Device.brand, n = 10, w = total)
            ) %>%
            dplyr::group_by(Device.brand) %>%
            dplyr::summarise(
              total = sum(total, na.rm = TRUE)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Device.brand, total), y = total)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of participants")
        }
      })
      
      output$health_device_placement <- shiny::renderPlot({
        if (input$health_device_toggle == "Datasets") {
          rv$datasets %>%
            dplyr::filter(get(input$grouped_health_outcome_select) == "Yes") %>%
            dplyr::count(Placement) %>%
            ggplot2::ggplot(ggplot2::aes(reorder(Placement, n), n)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of datasets")
        } else {
          rv$datasets %>%
            dplyr::filter(get(input$grouped_health_outcome_select) == "Yes") %>%
            dplyr::select(Placement, derived_sample) %>%
            dplyr::group_by(Placement) %>%
            dplyr::summarise(
              derived_sample = sum(derived_sample, na.rm = TRUE)
            ) %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Placement, derived_sample), y = derived_sample)) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(name = NULL) +
            ggplot2::scale_y_continuous(name = "Number of participants")
        }
      })
      
      output$health_search <- DT::renderDataTable(
        DT::datatable(rv$datasets %>% dplyr::select(
          6:30, anthropometry, blood_pressure, lipids, glucose, Height, Weight, Waist.circumference, Hip.circumference, Fat.mass, 
          Visceral.fat, SBP, DBP, Resting.heart.rate, HDL.cholesterol, LDL.cholesterol, Total.cholesterol, Triglyceride, HbA1c, 
          VLDL, Glucose, Insulin, Oral.glucose.tolerance.test, Metabolic.syndrome, Sex, Age, Ethnicity, Education.level,
          Household.income, Socio.economic.status, Smoking.status, Clinical.diagnosis.medical.history, Fruit.and.vegetable.intake
        ) %>%
          dplyr::filter(get(input$grouped_health_outcome_select) == "Yes"),
        container = sketch,
        selection = list(mode = "multiple", selected = rv$sel),
        rownames = FALSE,
        options = list(autoWidth = FALSE, scrollX = TRUE, processing = FALSE)
        )
      )
    }
  })
}