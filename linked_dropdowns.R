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

module_select_server <- function(id, data, colname){
  
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(
        data(),
        {
          browser()
          updateSelectInput(
            session, "selector", 
            choices = unique(pull(data(), .data[[colname]] ))
          )
        }
      )
      
      observe( 
        {
          browser()
          shinyjs::toggle(
            id = "selector",
            condition = input[["selector"]] != "NA"
          )
        }
      )
      
      filtered_data <- reactive(
        {
          browser()
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
      
      dropdowns <- list()
      
#      group <- module_select_server(id[[1]], reactive(data), sym(names[[1]]))
      # group <- module_select_server(ids[[1]], reactive(data), colnames[[1]])
      # subgroup <- module_select_server(ids[[2]], group$data, colnames[[2]])
      # indicator <- module_select_server(ids[[3]], subgroup$data, colnames[[3]])
      # breakdown <- module_select_server(ids[[4]], indicator$data, colnames[[4]])

       browser()
      for (i in seq_along(colnames)) {

        if (i == 1){
         dropdowns[[i]] <- module_select_server(ids[[i]], reactive(data), colnames[[i]])

        } else {

          dropdowns[[i]] <- module_select_server(ids[[i]], dropdowns[[i - 1]]$data, colnames[[i]])

        }

      }

      
      dropdowns
      
    }
    
    
  )
  
}


ui <- fluidPage(
  useShinyjs(),
  module_dropdowns_ui("grocery", selections)
)

server <- function(input, output, session){
  
  test <- module_dropdown_server("grocery", selections)
  
}

shinyApp(ui, server)
