
# Load packages
library(shiny)
library(tidyverse)
library(sf)


# Load data
sd_counties <- map_data("county", "South Dakota") %>%
  select(lon = long, lat, group, id = subregion)

# Define UI for app 
ui <- fluidPage(
  titlePanel("South Dakota"),
  sidebarLayout(
    sidebarPanel(
      h3("Fun Facts:"),
      p(strong("State Nickname: "), 
        "The Mount Rushmore State (since 1992) formerly the 
        Sunshine State (stupid Florida)"),
      
      p(strong("State Motto: "),
        "Under God the People Rule"),
      
      p(strong("Population: "), 
        "858,469 (2015 est)"
        ),
      
      p(strong("State Fossil: "),
        " Triceratops"),
      
      p(strong("State Insect: "),
        "Honey Bee"),
      
      p(strong("State Flag: ")),
  
      img(src = "sd_flag.JPG", height = 160, width = 250),
      
      br(),
      br(),
      
      p(strong("State Seal: ")),
      
      img(src = "sd_seal.png", height = 200, width = 200)
      
      
    ),
    mainPanel(
      
      # Output: SD Map ----
      plotOutput(outputId = "mapPlot"),
      
      p("* South Dakota is home to the world's only ",
        a("Corn Palace",
          href = "https://cornpalace.com/")),
      
      p("* South Dakota is ",
        span("not ", style = "color:red"),
        "the same as North Dakota"),
      
      p("* For more information visit ",
        a("South Dakota's Wikipedia Page", 
          href = "https://en.wikipedia.org/wiki/South_Dakota"))

    )
  )
)





# Define server logic 
server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    
    ggplot(sd_counties, aes(lon, lat)) +
      geom_polygon(aes(group = group), fill = "#ADD8E6", color = "black") +
      coord_quickmap() +
      theme_void()
  }
    
  )
  
}


shinyApp(ui = ui, server = server)

