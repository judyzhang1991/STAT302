library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
      code('install.package("shiny")'),
      br(),
      br(),
      br(),
      br(),
      img(src = "rstudio.png", height = 70, width = 200),
      br(),
      "Shiny is a product of ",
      span("RStduio", style = "color: blue")
    ),
    mainPanel(
      h1("Intoducing Shiny"),
      p("Shiny is a new package from RStudio that makes it ", em("incredibly easy"),
        "to build interactive web aplications with R."),
      br(),
      p("For an introduction and live examples, visit the ", 
        a("Shiny homepage.", href = "http://shiny.rstudio.com")),
      br(),
      
      h2("Features"),
      p("- Build useful web applications with only a few lines of code-no JavaScript required."),
      p("- Shiny applications are automatically 'live' in the same way that ",
        strong("spreadsheets"),
        " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
      
    )
  )
)








# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  
}

shinyApp(ui = ui, server = server)