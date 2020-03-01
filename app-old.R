library(shiny)
library(crosstalk)
library(DT)
library(leaflet)
library(tidyverse)

overall <- read_rds(path = "overall.rds")

pal <- colorNumeric(
    palette = "Reds",
    domain = overall$Aug)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("What's the best holiday destination from London?"),
    br(),
    br(),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(
            width = 4,
            offset = 1,
            sliderInput("temp",
                        "Temperature", 
                        min = 0,
                        max = 40,
                        value = c(15, 25), 
                        step = 1, 
                        dragRange = TRUE)
            ),
        column(
            width = 4, 
            offset = 1,
            sliderInput("duration",
                        "Flight Time (mins)",
                        min = 0,
                        max = 1080,
                        value = c(60, 300), 
                        step = 30, 
                        dragRange = TRUE)
            )
        ),

        # Show a plot of the generated distribution
        fluidRow(
            column(
                width = 8,
                leafletOutput("map_output")
            ),
            column(
                width = 4, 
                dataTableOutput("table_output")
            )
        ), 
    br()
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    new_data <- reactive({
        overall %>% 
            filter(
                between(
                    Aug, input$temp[1], input$temp[2]
                    ),
                between(
                    minutes, input$duration[1], input$duration[2]
                    )
                )
    })

    output$map_output <- renderLeaflet({
        leaflet(new_data()) %>% 
            addTiles() %>% 
            setView(
                lng = -25, 
                lat = 45, 
                zoom = 3) %>% 
            addCircles(
                lng = ~Longitude, 
                lat = ~Latitude, 
                weight = 2,
                radius = ~sqrt(minutes) * 10000, 
                popup = ~paste(City, "- time: ", round(minutes/60, 1), "hrs; temp: ", Aug, "C"), 
                color = ~pal(Aug)
            )
    })
    
    output$table_output <- renderDataTable ({
        datatable(
            new_data() %>% 
                select(
                    City = 'City', 
                    Time = 'minutes', 
                    Temp = 'Aug'
                    ), 
            rownames = FALSE,
            # extensions="Scroller", 
            # style="bootstrap", 
            # class="compact", 
            # width="100%",
            options=list(
                pageLength = 3,
                dom = 'tp',
                deferRender=TRUE, 
                scrollY=300, 
                scroller=TRUE
                )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
