# Load Packages
library(shiny)
library(tidyverse)
library(janitor)


# Read data
cdc <- read_delim("data/cdc.txt", "|") %>%
  clean_names() 



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CDC BRFSS: Histogram of Weight Grouped by Gender"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- cdc$weight
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot(cdc, aes(weight)) + 
      
      geom_histogram(aes(fill = gender), 
                     color = "black", 
                     position = "stack", 
                     breaks = bins) + 
      
      scale_fill_discrete(labels = c("Female", "Male")) + 
      
      labs(x = "Weight in Pounds",
           y = "Count",
           fill = "Gender") + 
      
      theme_minimal() + 
      
      theme(
        
        ### Legend ###
        legend.position = c(0.45, 0.8),
        
        legend.justification = c(0, 1)
        
      )
    
  })
  
}

shinyApp(ui = ui, server = server)

