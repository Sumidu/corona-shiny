#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
raw_data <- read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data") %>% 
    mutate(Meldedatum = as_datetime(Meldedatum))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Corona-App"),

    # Sidebar placeholder for UI Element
    sidebarLayout(
        sidebarPanel(
            uiOutput("country_pick"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactive data frame
    corona_data <- reactive({
        raw_data %>% filter(Bundesland %in% input$country_picker)
    })
    
    
    # UI Element
    output$country_pick <- renderUI({
        bundeslaender <- raw_data$Bundesland %>% unique()
        selectInput("country_picker", label = "Bundesland auswählen", choices = bundeslaender, selected = bundeslaender[1], multiple = T)    
    })

    # Output
    output$distPlot <- renderPlot({
        # Render our data
        corona_data() %>% 
            group_by(Meldedatum, Bundesland) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            ggplot() + 
            aes(x = Meldedatum, y = AnzahlFall, fill = Bundesland, color = Bundesland, group = Bundesland) + 
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
