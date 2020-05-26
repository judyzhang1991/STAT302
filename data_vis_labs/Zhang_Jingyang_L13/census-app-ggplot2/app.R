
# Load packages ----

library(shiny)

library(maps)

library(maps)

library(tidyverse)

library(janitor)

library(sf)


# Read states names -----
states <- read_csv("data/states.csv") %>% 
    clean_names()


# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      selectInput("state", 
                  label = "Select a state or the contiguous 48 states to display",
                  choices = states$state,
                  selected = "South Dakota"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    
    args <- switch(input$var, 
                   "Percent White" = list("white", "darkgreen", "% White"),
                   "Percent Black" = list("black", "black", "% Black"),
                   "Percent Hispanic" = list("hispanic", "darkorange", "% Hispanic"),
                   "Percent Asian" = list("asian", "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    
    args$max <- input$range[2]
    
    args$state <- input$state
    
    
    # do.call constructs and executes a function call from a name or 
    ## a function and a list of args to be passed to it.
    
    do.call(percent_map, args)
    
  })
}

# Run app ----
shinyApp(ui, server)