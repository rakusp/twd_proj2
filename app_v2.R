library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

source("functions.R")

p_streaming <- read_json("data/patryk/StreamingHistory0.json")
j_streaming <- bind_rows(read_json("data/janek/StreamingHistory0.json"),
                         read_json("data/janek/StreamingHistory1.json"),
                         read_json("data/janek/StreamingHistory2.json"))
l_streaming <- read_json("data/lukasz/StreamingHistory0.json")

streaming <- bind_rows("p"=p_streaming, "j"=j_streaming, "l"=l_streaming,
                       .id="user")
streaming <- fix_streaming(streaming)

p_streaming <- fix_streaming(p_streaming)
j_streaming <- fix_streaming(j_streaming)
l_streaming <- fix_streaming(l_streaming)

ui1 <- fluidPage(
    
    titlePanel("W jakich godzinach najwięcej słuchamy muzyki?"),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("who", "Wybierz osoby:",
                               choiceNames=c("Patryk", "Łukasz", "Janek"),
                               choiceValues=c("p", "l", "j"),
                               selected=c("p", "l", "j")
                               )
        ),
        mainPanel(
           plotOutput("plot1")
        )
    )
)

ui2 <- fluidPage(
  
  #Jak sie uda to gatunki i albumy
  titlePanel("Ulubieni wykonawcy/utwory?"),
  sidebarLayout(
    sidebarPanel(
      selectInput("kategoria",
                  "Wybierz kategorię:",
                  c("Wykonawcy","Utwory"))),
      mainPanel(
        plotOutput("plot2"),
        shiny::tableOutput("table")
      )
  )
  
)

server <- function(input, output) {

    output$plot1 <- renderPlot({
        p <- streaming %>% 
            filter(user %in% input$who) %>% 
            group_by(user) %>% 
            mutate(totalTime = sum(msPlayed)) %>% 
            group_by(user, hour) %>% 
            summarise(avgTime = sum(msPlayed)/totalTime ) %>% 
            ggplot() +
            geom_line(aes(x=hour, y=avgTime, color=user), size=1.75) +
            theme_bw() +
            labs(
                x="Godzina",
                y="Procent całkowitego czasu słuchania"
            ) +
            scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                               breaks=seq(0, 0.13, by=0.01)) +
            scale_x_continuous(expand=c(0, 0, 0, 0),
                               breaks=seq(0, 23, by=1)) +
            theme(panel.grid.major.y = element_line(linetype=5),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank())
        
        p
    })
    
    output$plot2 <- renderPlot({
      if(input$kategoria == "Wykonawcy"){
        
        p <- p_streaming %>% group_by(artistName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
          
        j <- j_streaming %>% group_by(artistName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
        
        l <- l_streaming %>% group_by(artistName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
        
        streaming <- bind_rows("p"=p, "j"=j, "l"=l,
                               .id="user")
        ggplot(streaming, aes(x=artistName,y=(msPlayed/60000), fill = user)) +
          geom_col(position =  "dodge") + theme_bw() +
          labs(
            x="Wykonawca",
            y="Przesłuchane minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())
        
        
      }else if(input$kategoria == "Utwory"){
        p <- p_streaming %>% group_by(trackName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
        
        j <- j_streaming %>% group_by(trackName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
        
        l <- l_streaming %>% group_by(trackName) %>% 
          summarise(msPlayed = sum(msPlayed)) %>%
          arrange(-msPlayed) %>% head(3)
        
        streaming <- bind_rows("p"=p, "j"=j, "l"=l,
                               .id="user")
        ggplot(streaming, aes(x=trackName,y=(msPlayed/60000), fill = user)) +
          geom_col(position =  "dodge") + theme_bw() +
          labs(
            x="Utwor",
            y="Przesłuchane minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())
        
      }
    })
    
    output$table <- shiny::renderTable({if(input$kategoria == "Wykonawcy"){
      
      p <- p_streaming %>% group_by(artistName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      j <- j_streaming %>% group_by(artistName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      l <- l_streaming %>% group_by(artistName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      bind_rows("p"=p, "j"=j, "l"=l,
                             .id="user")
    
      
      
    }else if(input$kategoria == "Utwory"){
      p <- p_streaming %>% group_by(trackName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      j <- j_streaming %>% group_by(trackName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      l <- l_streaming %>% group_by(trackName) %>% 
        summarise(minutes = sum(msPlayed)/60000) %>%
        arrange(-minutes) %>% head(3)
      
      bind_rows("p"=p, "j"=j, "l"=l,
                             .id="user")
      
    }})
    
}

app_ui <- navbarPage(
    title="Spotify",
    tabPanel("Godziny słuchania", ui1),
    tabPanel("Najczęściej słuchane", ui2),
)

shinyApp(ui = app_ui, server = server)

