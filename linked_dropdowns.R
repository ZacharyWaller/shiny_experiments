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
    choices = NULL
  )
  
}

module_dropdowns_ui <- function(id, data){
  
  ns <- NS(id)
  
  names <- names(data)
  ids <- tolower(names)
  
  selectors <- map2(
    .x = ns(ids), .y = names,
    .f = module_select_ui
  )
  
  selectors
  
  
}

module_select_server <- function(data, id, colname){
  
  if (any(class(data) == "list")){
    
    data <- data$data
    
  }
  
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(
        data(),
        {
          updateSelectInput(
            session, "selector", 
            choices = unique(pull(data(), .data[[colname]] ))
          )
        }
      )
      
      observe( 
        {
          shinyjs::toggle(
            id = "selector",
            condition = input[["selector"]] != "NA" & length(unique(pull(data(), .data[[colname]] ))) > 1
          )
        }
      )
      
      filtered_data <- reactive(
        {
          req(input[["selector"]])
          filter(data(), .data[[colname]] == input[["selector"]])
        }
      )
      
      list(
        id = id,
        colname = colname,
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
      
      colnames <- names(data)
      ids <- tolower(colnames)
      n_cols <- length(colnames)
      
      dropdowns <- vector(mode = "list", length = n_cols)
      
      dropdowns <-  accumulate2(.x = ids, .y = colnames, .f = module_select_server, .init = reactive(data))
       
      
      dropdowns[2:n_cols]
    }
  )
}


ui <- fluidPage(
  useShinyjs(),
  module_dropdowns_ui("grocery", selections),
  textOutput("bob")
)

server <- function(input, output, session){
  
  test <- module_dropdown_server("grocery", selections)
  
  output$bob <- reactive({
    
    input$`grocery-indicator-selector`
    })
  
}

shinyApp(ui, server)
