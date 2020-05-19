library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("My Shiny App"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  
}

shinyApp(ui = ui, server = server)