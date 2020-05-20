# Load Packages
library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)


# Read data
cdc <- read_delim("data/cdc.txt", "|") %>%
  clean_names()

# Specify factor levels
cdc$exerany <- factor(cdc$exerany, levels = c(1, 0))

cdc$hlthplan <- factor(cdc$hlthplan, levels = c(1, 0))

cdc$smoke100 <- factor(cdc$smoke100, levels = c(1, 0))

cdc$genhlth <- factor(cdc$genhlth, levels = c("excellent", 
                                               "very good", 
                                               "good", 
                                               "fair", 
                                               "poor"))

cdc$gender <- factor(cdc$gender, levels = c("f", "m"))


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CDC BRFSS Histograms"),
  
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
      input$var == "Actual Weight" ~ list(cdc$weight, 
                                          seq(0, 500, by = 100), 
                                          "Actual Weight in Pounds"),
      
      input$var == "Desired Weight" ~ list(cdc$wtdesire, 
                                           seq(0, 600, by = 200),
                                           "Desired Weight in Pounds"),
      
      input$var == "Height" ~ list(cdc$height, 
                                   seq(50, 90, by = 10),
                                   "Height in Inches")
    )
    
    legs <- case_when(
      input$choice == "General Health" ~ list(cdc$genhlth, 
                                              c("Excellent", 
                                                "Very Good", 
                                                "Good", 
                                                "Fair", 
                                                "Poor"), 
                                              "General Health"),
      
      input$choice == "Health Coverage" ~ list(factor(cdc$hlthplan), 
                                               c("Yes",
                                                 "No"),
                                               "Health Coverage"),
      
      input$choice == "Exercised in Past Month" ~ list(factor(cdc$exerany), 
                                                       c("Yes",
                                                         "No"),
                                                       "Exercised in Past Month"),
      
      
      input$choice == "Smoked 100 Cigarettes" ~ list(factor(cdc$smoke100), 
                                                     c("Yes",
                                                       "No"),
                                                     "Smoked 100 Cigarettes"),
      
      
      input$choice == "Gender" ~ list(cdc$gender, 
                                      c("Female",
                                        "Male"),
                                      "Gender")
    )
    
  
    bins <- seq(min(unlist(vars[1])), max(unlist(vars[1])), length.out = input$bins + 1)
    
    ggplot(cdc, aes(unlist(vars[1]))) + 
      
      geom_histogram(aes(fill = unlist(legs[1])), 
                     color = "black", 
                     size = 1,
                     position = "stack", 
                     breaks = bins) + 
      
      scale_x_continuous(breaks = unlist(vars[2])) + 
      
      scale_fill_discrete(labels = unlist(legs[2])) + 
      
      guides(fill = guide_legend(title.position = "top")) + 
      
      labs(x = unlist(vars[3]),
           y = "Count",
           fill = legs[3]) + 
      
      theme_minimal() + 
      
      theme(
        
        ### Plot ###
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        
        
        
        ### Panel ###
        panel.grid.major = element_line(color = "#D2D2D2"),
        panel.border = element_blank(),
        
        panel.grid.minor = element_blank(),
        
        
        
        
        
        ### Legend ###
        legend.position = "top",
        
        legend.title.align = 0.5,
        
        legend.direction = "horizontal"
        
      )
    
  })
  
}

shinyApp(ui = ui, server = server)






  
  