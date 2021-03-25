selections <- data.frame(
  Group = c("a", "a", "a", "a", "a", "a", "b", "b", "c", "c"),
  Subgroup = c("a", "a", "a", "a", "b", "b", "NA", "NA", "NA", "NA"),
  Indicator = c("a", "a", "a", "a", "b", "c", "d", "d", "b", "b"),
  Breakdown = c("a", "b", "c", "d", "NA", "NA", "e", "f", "g", "h"),
  stringsAsFactors = FALSE
)

library(shiny)
library(dplyr)
library(purrr)
library(shinyjs)

module_select_ui <- function(id, name, choices){
  
  ns <- NS(id)
  
  selectInput(
    inputId = ns("selector"),
    label = name,
    choices = choices
  )
  
}

module_dropdowns_ui <- function(id){
  
  ns <- NS(id)
  
  list(
    useShinyjs(),
    module_select_ui(
      id = ns("group"),
      name = "Group",
      choices = NULL
    ),
    module_select_ui(
      id = ns("subg"),
      name = "Subgroup",
      choices = NULL
    ),
    module_select_ui(
      id = ns("ind"),
      name = "Indicator",
      choices = NULL
    ),
    module_select_ui(
      id = ns("bd"),
      name = "Breakdown",
      choices = NULL
    )
  )
  
  
}

module_select_server <- function(id, data, colname){
  
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(
        data(),
        {
          updateSelectInput(
            session, "selector", 
            choices = unique(pull(data(), {{ colname }} ))
          )
        }
      )
      
      observe( 
        {
          shinyjs::toggle(
            id = "selector",
            condition = input[["selector"]] != "NA"
          )
        }
      )
      
      filtered_data <- reactive(
        {
          req(input[["selector"]])
          filter(data(), {{ colname }} == input[["selector"]])
        }
      )
      
      list(
        value = reactive(input[["selector"]]),
        data = filtered_data
      )
    }
  )
}


module_dropdown_server <- function(id, data){
  
  
  moduleServer(
    id,
    function(input, output, session){
      
      group <- module_select_server("group", reactive(data), Group)
      subgroup <- module_select_server("subg", group$data, Subgroup)
      indicator <- module_select_server("ind", subgroup$data, Indicator)
      breakdown <- module_select_server("bd", indicator$data, Breakdown)
      
    }
  )
  
}


ui <- fluidPage(
  module_dropdowns_ui("grocery"),
  textOutput("grocery-test")
)

server <- function(input, output, session){
  
  test <- module_dropdown_server("grocery", selections)
  
}

shinyApp(ui, server)
