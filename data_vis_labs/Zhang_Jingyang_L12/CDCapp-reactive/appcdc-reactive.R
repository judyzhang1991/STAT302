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
      
      # Input: Select box for variables
      selectInput("var", 
                  label = "Select Variable:",
                  choices = list("Actual Weight", 
                                 "Desired Weight",
                                 "Height"),
                  selected = "Actual Weight"),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30),
      
      
      # Input: Radio buttons for fill/legend variables
      radioButtons("choice", 
                  label = "Select Fill/Legend Variable:",
                  choices = list("General Health", 
                                 "Health Coverage",
                                 "Exercised in Past Month",
                                 "Smoked 100 Cigarettes",
                                 "Gender"),
                  selected = "Gender")
      
      
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
    
  
    vars    <- case_when(
      input$var == "Actual Weight" ~ list(cdc$weight, "Actual Weight in Pounds"),
      input$var == "Desired Weight" ~ list(cdc$wtdesire, "Desired Weight in Pounds"),
      input$var == "Height" ~ list(cdc$height, "Height in Inches")
    )
    
    legs <- case_when(
      input$choice == "General Health" ~ list(cdc$genhlth, 
                                              c("Excellent", 
                                                "Very Good", 
                                                "Good", 
                                                "Fair", 
                                                "Poor"), 
                                              "horizontal"),
      
      input$choice == "Health Coverage" ~ list(cdc$hlthplan, 
                                               c("Yes",
                                                 "No"),
                                               "horizontal"),
      
      input$choice == "Exercised in Past Month" ~ list(cdc$exerany, 
                                                       c("Yes",
                                                         "No"),
                                                       "horizontal"),
      
      
      input$choice == "Smoked 100 Cigarettes" ~ list(cdc$smoke100, 
                                                     c("Yes",
                                                       "No"),
                                                     "horizontal"),
      
      
      input$choice == "Gender" ~ list(cdc$gender, 
                                      c("Female",
                                        "Male"),
                                      "horizontal")
    )
    
  
    bins <- seq(min(unlist(vars[1])), max(unlist(vars[1])), length.out = input$bins + 1)
    
    ggplot(cdc, aes(unlist(vars[1]))) + 
      
      geom_histogram(aes(fill = unlist(legs[1])), 
                     color = "black", 
                     position = "stack", 
                     breaks = bins) + 
      
      scale_fill_discrete(labels = unlist(legs[2])) + 
      
      labs(x = unlist(vars[2]),
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

