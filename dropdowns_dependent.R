library(shiny)
library(purrr)

# Intro ------------------------------------------------------------------------

# This is a teeny app with three tabs, each with a dropdown selector.
# Each dropdown selector would, by default, be independent of all the others.
# For the WICH tool (and probably some other apps) we'd like them to be 
# linked so that updating one updates all others (you could bypass this by 
# having just one global dropdown, but that limits where you can put the 
# dropdowns in the UI).

# The approach used here is to have on global value to store the current 
# selection. Each dropdown is tied to that global value in two ways:
# - If the user selects a new value from the dropdown, the global value is updated.
# - If the global value is changed by another dropdown then the value in this
#   dropdown changes to match it

# E.g. if the dropdown in tab 1 changes it will change the global value. The 
# change in global value will cause the values in tabs 2 and 3 to change

# An alternative is to have each dropdown be aware of each other dropdown, akin 
# to Hadley's suggestion here:
# https://community.rstudio.com/t/how-to-sync-selectizeinput-between-menu-items-for-navbarpage-in-r-shiny/38851/3
# This doesn't scale well though (you need n(n-1) comparisons for n tabs, as 
# opposed to the 2n comparisons in this method). This method is basically just 
# like raytong's in the above thread, but with modules!

# The values available from the dropdown menu
values <- c("Option A", "Option B", "Option C")

# UI module --------------------------------------------------------------------
# Layout of each dropdown
selector_module_ui <- function(id, name, values){
  
  ns <- NS(id)
  
  fluidRow(
    selectizeInput(
      ns("selector"), "Selector",
      choices = values,
      multiple = FALSE
    )
  )
}

# Server module ----------------------------------------------------------------
# Logic of each dropdown and global values
selector_module <- function(id, global_value){
      
  moduleServer(
    id,
    function(input, output, session){
      
      # If this dropdown's selected value is changed, update the global value
      observeEvent(
        input$selector,
        {
          if (global_value$value != input$selector) {
            global_value$value <- input$selector
            global_value$last <- id
            message("Global changed by ", id)
          }
        }
      )
      
      # If the global value changes change the value in the dropdown.
      observe({
        if (global_value$last != id){
          updateSelectizeInput(session, 'selector', selected = global_value$value)
          message("Selector ", id, " changed")
        }
      })
      
      global_value
    }
  )
}

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Set the initial value of the global_value
  global_value <- reactiveValues(value = "", last_change = "")
  
  # Walk through our modules to set release
  # Interestingly this only runs once, even though the logic continues to work
  # The fact I find this surprising probably suggests I have much to learn.
  tabs <- c("one", "two", "three")
  global_value <- walk(
    tabs,
    selector_module, global_value
  )
  
  # could also do it this way:
  # global_value <- selector_module("one", global_value)
  # global_value <- selector_module("two", global_value)
  # global_value <- selector_module("three", global_value)
}

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  navbarPage(
    "Demo",
    tabPanel(
      "Tab 1",
      selector_module_ui("one", "One", values)
    ),
    tabPanel(
      "Tab 2",
      selector_module_ui("two", "Two", values)
    ),
    tabPanel(
      "Tab 3",
      selector_module_ui("three", "Three", values)
    )
  )
)

# Run --------------------------------------------------------------------------
shinyApp(ui, server)

# To extend this example to more tabs and dropdowns you just need to add more 
# tabs and selector_module_ui's to the UI and add their IDs to the tabs variable
#