library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)

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
                  c("Wykonawcy","Utwory")),
      ),
      mainPanel(
        tabsetPanel(
        tabPanel("Wykres",
                 plotly::plotlyOutput("plot2"),
                 sliderInput("n", 
                             label = "Ilość:", 
                             value = 5,
                             min = 1,
                             max = 10),
                 actionButton(inputId = "start", label = "Start", icon = icon('play-circle')),
                 checkboxGroupInput("who2", "Wybierz osoby:",
                                    choiceNames=c("Patryk", "Łukasz", "Janek"),
                                    choiceValues=c("p", "l", "j"),
                                    selected=c("p", "l", "j")
                 ),
                 plotly::plotlyOutput("plot3")),
        tabPanel("Tabela",
                 fluidRow(
                   actionButton(inputId = "button_p", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-1/p200x200/117042018_2583202521944871_6439550514274272204_n.jpg?_nc_cat=109&ccb=1-5&_nc_sid=7206a8&_nc_ohc=QFaAaBs81foAX9puy1r&_nc_ht=scontent-ham3-1.xx&oh=00_AT9_MBy9ndkBmvrSfEiU61e6ODO0xvL68sqQsXmxxyb_dQ&oe=61F79921');  background-size: cover; background-position: center;"),
                   actionButton(inputId = "button_l", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/35237401_2001879876551410_721839996499132416_n.jpg?_nc_cat=104&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=Z8XbrlmH_TwAX-7-QXY&_nc_ht=scontent-ham3-1.xx&oh=00_AT-5GJS2QFCaFNJEeHDoVLmhPnL9LF-diIB6L1sFhn76Dw&oe=61F8B18A');  background-size: cover; background-position: center;"),
                   actionButton(inputId = "button_j", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/123193344_2840269192876438_7586431629465567622_n.jpg?_nc_cat=103&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=b5KeJi9WozQAX93Qn0Q&_nc_ht=scontent-ham3-1.xx&oh=00_AT-7Dk8EPiY94-U98NLYzsLvZWlYMCE9CJk_DkxG238MyQ&oe=61F76FDD');  background-size: cover; background-position: center;"),
                   textOutput("kto"),
                   dataTableOutput("tabela")
                 )
                 )
                
        )
      )
  )
  )
  

server <- function(input, output) {
  
  observeEvent(input$kategoria, {output$tabela <- renderDataTable({})
  output$kto <- renderText({""})})
  
  
  observeEvent(input$button_l,{
    output$kto <- renderText({"Łukasz"})
    if(input$kategoria == "Wykonawcy"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
    })
    }else if(input$kategoria == "Utwory"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
        })
      }
    })
  
  
  observeEvent(input$button_p,{
    output$kto <- renderText({"Patryk"})
    if(input$kategoria == "Wykonawcy"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoria == "Utwory"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  observeEvent(input$button_j,{
    output$kto <- renderText({"Janek"})
    if(input$kategoria == "Wykonawcy"){
      output$tabela <- renderDataTable({
        j_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoria == "Utwory"){
      output$tabela <- renderDataTable({
        j_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  
  
  
  
  
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
    
    output$plot2 <- plotly::renderPlotly({
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
                panel.grid.minor.x = element_blank()) + coord_flip()
        
        
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
                panel.grid.minor.x = element_blank()) + coord_flip()
        }
    })
    
    output$plot3 <- plotly::renderPlotly({
      if(input$kategoria == "Wykonawcy"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(artistName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(artistName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% 
          group_by(artistName, month) %>% summarise(Minuty = sum(msPlayed)/60000)
        
        plot_ly(data = top, x = ~month, y = ~Minuty, color = ~artistName, type = 'scatter',
                mode = 'lines')
      }else if(input$kategoria == "Utwory"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% 
          group_by(trackName, month) %>% summarise(Minuty = sum(msPlayed)/60000)
        
        plot_ly(data = top, x = ~month, y = ~Minuty, color = ~trackName, type = 'scatter',
                mode = 'lines')
      }
    })
    
    
    
    
    
}

app_ui <- navbarPage(
    title="Spotify",
    tabPanel("Godziny słuchania", ui1, icon = icon("clock")),
    tabPanel("Najczęściej słuchane", ui2, icon = icon("heart")),
)

shinyApp(ui = app_ui, server = server)

