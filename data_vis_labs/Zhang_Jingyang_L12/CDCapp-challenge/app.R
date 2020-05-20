# Load Packages
library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)
library(skimr)
library(knitr)


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
                  c("Actual Weight" = "weight",
                    "Desired Weight" = "wtdesire",
                    "Height" = "height"),
                  selected = "weight"),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30,
                  animate = TRUE),
      
     
      
      
      # Input: Radio buttons for fill/legend variables
      radioButtons("choice", 
                  label = "Select Fill/Legend Variable:",
                  c( "General Health" = "genhlth",
                    "Health Coverage" = "hlthplan",
                    "Exercised in Past Month" = "exerany",
                    "Smoked 100 Cigarettes" = "smoke100",
                    "Gender" = "gender",
                    "None" = "none"),
                  select = "none")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Summary", verbatimTextOutput("summary1"), verbatimTextOutput("summary2"))
                 
      )
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Reactive expression to generate the request skim summary
  f <- reactive({
    fill <- switch(input$choice,
                   genhlth = cdc$genhlth,
                   hlthplan = cdc$hlthplan,
                   exerany = cdc$exerany,
                   smoke100 = cdc$smoke100,
                   gender = cdc$gender,
                   none = NULL,
                   NULL)
    
    fill
    
  })
  
  v <- reactive({
    var <- switch(input$var,
                  weight = cdc$weight,
                  wtdesire = cdc$wtdesire,
                  height = cdc$height
                  )
    
    var
  })
  
  
  
  # Histogram of the CDC Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
  
    vars    <- case_when(
      input$var == "weight" ~ list(cdc$weight, 
                                          seq(0, 500, by = 100), 
                                          "Actual Weight in Pounds"),
      
      input$var == "wtdesire" ~ list(cdc$wtdesire, 
                                           seq(0, 600, by = 200),
                                           "Desired Weight in Pounds"),
      
      input$var == "height" ~ list(cdc$height, 
                                   seq(50, 90, by = 10),
                                   "Height in Inches")
    )
    
    legs <- case_when(
      input$choice == "genhlth" ~ list(cdc$genhlth, 
                                              c("Excellent", 
                                                "Very Good", 
                                                "Good", 
                                                "Fair", 
                                                "Poor"), 
                                              "General Health"),
      
      input$choice == "hlthplan" ~ list(factor(cdc$hlthplan), 
                                               c("Yes",
                                                 "No"),
                                               "Health Coverage"),
      
      input$choice == "exerany" ~ list(factor(cdc$exerany), 
                                                       c("Yes",
                                                         "No"),
                                                       "Exercised in Past Month"),
      
      
      input$choice == "smoke100" ~ list(factor(cdc$smoke100), 
                                                     c("Yes",
                                                       "No"),
                                                     "Smoked 100 Cigarettes"),
      
      
      input$choice == "gender" ~ list(cdc$gender, 
                                      c("Female",
                                        "Male"),
                                      "Gender")
    )
    
  
    bins <- seq(min(unlist(vars[1])), max(unlist(vars[1])), length.out = input$bins + 1)
    
    hist <- ggplot(cdc, aes(unlist(vars[1])))
    
    if(input$choice != "none"){
       hist <- hist + geom_histogram(aes(fill = unlist(legs[1])), 
                     color = "black", 
                     size = 1,
                     position = "stack", 
                     breaks = bins)}
    else{
      hist <- hist + geom_histogram(fill = "skyblue",
                                    color = "black",
                                    size = 1,
                                    breaks = bins)}
      
      hist + scale_x_continuous(breaks = unlist(vars[2])) + 
      
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
        
        #legend.justification = c(0, 0),
        
        legend.direction = "horizontal"
        
      )
    
  })
  
  # Generate a summary of variable ----
  output$summary1 <- renderPrint({
      skim_without_charts(v())
 
  })
  
  
  output$summary2 <- renderPrint({
    skim_without_charts(f()) 
    
    
    
  })
  
  

  
}

shinyApp(ui = ui, server = server)







  
  