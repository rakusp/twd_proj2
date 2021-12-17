library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

source("functions.R")

p_streaming <- read_json("data/patryk/StreamingHistory0.json")
p_streaming <- fix_streaming(p_streaming)

ui1 <- fluidPage(
    
    titlePanel("Test"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
           plotOutput("plot1")
        )
    )
)

ui2 <- fluidPage()

server <- function(input, output) {

    output$plot1 <- renderPlot({
        p_streaming %>% 
            ggplot() +
            geom_bar(aes(x=artistName)) +
            theme_bw() +
            coord_flip()
    })
}

app_ui <- navbarPage(
    title="Spotify",
    tabPanel("Strona 1", ui1),
    tabPanel("Strona 2", ui2),
)

shinyApp(ui = app_ui, server = server)
