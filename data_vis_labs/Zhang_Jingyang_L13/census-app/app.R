# -- STAT302 --
# -- May 25th 2020
# -- Jingyang Zhang
# -- Lab13 Exer1 census-app



# Load packages ----

library(shiny)

library(maps)

library(mapproj)

# Load data ----
counties <- readRDS("data/counties.rds")

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
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
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    
    args$max <- input$range[2]
    
    
    # do.call constructs and executes a function call from a name or 
    ## a function and a list of args to be passed to it.
    
    do.call(percent_map, args)
    
    #percent_map(unlist(args[1]), unlist(args[2]), unlist(args[3]), input$range[1], input$range[2])
    
  })
}

# Run app ----
shinyApp(ui, server)