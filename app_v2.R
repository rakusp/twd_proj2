library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)
library(spotifyr)
library(DT)

source("functions.R")
source("connectapi.R")

library(httr)

# przekopiować z password.txt
clientID = 'TAJNE'
secret = 'TAJNE'

response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

mytoken = content(response)$access_token

HeaderValue = paste0('Bearer ', mytoken)

# -----------PRZYKŁAD JAK UŻYWAĆ--------------
# artistID = "6QfFTZJHFSe9Xyes6DkAli" ---> wyszukujemy ID ReTo
# URI = paste0('https://api.spotify.com/v1/artists/', artistID)
# response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
# Artist = content(response2)
# Artist$followers$total   -->  liczba followersów ReTo


connect_with_api("28e3d7ae0359454888cc25815aa18732", "c0f866dbe2db4388b51d5f1f714ca8be") # oba z pliku password.txt

p_streaming <- read_json("data/patryk/StreamingHistory0.json")
j_streaming <- bind_rows(read_json("data/janek/StreamingHistory0.json"),
                         read_json("data/janek/StreamingHistory1.json"),
                         read_json("data/janek/StreamingHistory2.json"))
l_streaming <- read_json("data/Lukasz/StreamingHistory0.json")

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
  conditionalPanel(condition = "input.kategoria == 'Wykonawcy'",
                   titlePanel("Wykresy przedstawiające ulubionych wykonawców")
                   ),
  conditionalPanel(
                   condition = "input.kategoria == 'Utwory'",
                   titlePanel("Wykresy przedstawiające ulubione utwory")),
  sidebarLayout(
    sidebarPanel(
      selectInput("kategoria",
                  "Wybierz kategorię:",
                  c("Wykonawcy","Utwory")),
      sliderInput("n", 
                  label = "Ilość:", 
                  value = 5,
                  min = 1,
                  max = 10),
      checkboxGroupInput("who2", "Wybierz osoby:",
                         choiceNames=c("Patryk", "Łukasz", "Janek"),
                         choiceValues=c("p", "l", "j"),
                         selected=c("p", "l", "j"))
      ),
      mainPanel(
                 plotly::plotlyOutput("plot2"),
                 plotly::plotlyOutput("plot3"))
  )
  )

ui2a <- mainPanel(
  conditionalPanel(condition = "input.kategoriaa == 'Wykonawcy'",
                   titlePanel("Tabela przedstawiająca ulubionych wykonawców")
  ),
  conditionalPanel(
    condition = "input.kategoriaa == 'Utwory'",
    titlePanel("Tabela przedstawiająca ulubione utwory")),
          sidebarLayout(
            sidebarPanel(
            selectInput("kategoriaa",
                        "Wybierz kategorię:",
                        c("Wykonawcy","Utwory")),
            strong("Wybierz czyją tabelę chcesz zobaczyć:"),
            p(""),
            fixedRow(
               column(1,offset = 2,actionButton(inputId = "button_p", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-1/p200x200/117042018_2583202521944871_6439550514274272204_n.jpg?_nc_cat=109&ccb=1-5&_nc_sid=7206a8&_nc_ohc=QFaAaBs81foAX9puy1r&_nc_ht=scontent-ham3-1.xx&oh=00_AT9_MBy9ndkBmvrSfEiU61e6ODO0xvL68sqQsXmxxyb_dQ&oe=61F79921');  background-size: cover; background-position: center;")
               )
             ),
            fixedRow(
               column(1,offset = 2,actionButton(inputId = "button_l", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/35237401_2001879876551410_721839996499132416_n.jpg?_nc_cat=104&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=Z8XbrlmH_TwAX-7-QXY&_nc_ht=scontent-ham3-1.xx&oh=00_AT-5GJS2QFCaFNJEeHDoVLmhPnL9LF-diIB6L1sFhn76Dw&oe=61F8B18A');  background-size: cover; background-position: center;")
               )
             ),
            fixedRow(
               column(1,offset = 2,actionButton(inputId = "button_j", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/123193344_2840269192876438_7586431629465567622_n.jpg?_nc_cat=103&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=b5KeJi9WozQAX93Qn0Q&_nc_ht=scontent-ham3-1.xx&oh=00_AT-7Dk8EPiY94-U98NLYzsLvZWlYMCE9CJk_DkxG238MyQ&oe=61F76FDD');  background-size: cover; background-position: center;")
               )
                      )
            ),
            mainPanel(
              uiOutput("text_header"),
               dataTableOutput("tabela")
            )
          )
    )


ui3 <- fluidPage(
  titlePanel("Zgodność muzycznych preferencji"),
      tabsetPanel(
        tabPanel("Utwory", plotOutput("plot4")),
        tabPanel("Artyści", plotOutput("plot5"))
      )
)
  

server <- function(input, output) {
  
  rv <-reactiveVal("")
  observeEvent(input$button_l,{rv("Łukasz")})
  observeEvent(input$button_p,{rv("Patryk")})
  observeEvent(input$button_j,{rv("Janek")})
  output$text_header <- renderUI(h2(rv(),align = "center"))
  
  observeEvent(input$kategoriaa, {output$tabela <- renderDataTable({})
  rv("")})
  observeEvent(input$button_l,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
    })
    }else if(input$kategoriaa == "Utwory"){
      output$tabela <- renderDataTable({
        l_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
        })
      }
    })
  
  
  observeEvent(input$button_p,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoriaa == "Utwory"){
      output$tabela <- renderDataTable({
        p_streaming %>% group_by(trackName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }
  })
  
  observeEvent(input$button_j,{
    if(input$kategoriaa == "Wykonawcy"){
      output$tabela <- renderDataTable({
        j_streaming %>% group_by(artistName) %>% 
          summarise(`Przesłuchane minuty` = round(sum(msPlayed)/60000)) %>%
          arrange(-`Przesłuchane minuty`)
      })
    }else if(input$kategoriaa == "Utwory"){
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
    
    
    output$plot4 <- renderPlot({
      p <- p_streaming %>% select(trackName, artistName) %>% 
        unique()
      l <- l_streaming %>% select(trackName, artistName) %>% 
        unique()
      j <- j_streaming %>% select(trackName, artistName) %>% 
        unique()
      
      pl_len <- dim(full_join(p, l))[1]
      pj_len <- dim(full_join(p, j))[1]
      lj_len <- dim(full_join(l, j))[1]
      
      pl_common_len <- dim(inner_join(p, l))[1]
      pj_common_len <- dim(inner_join(p, j))[1]
      lj_common_len <- dim(inner_join(l, j))[1]
      
      df <- data.frame(who=c("Patryk i Łukasz", "Patryk i Janek", "Łukasz i Janek"),
                       perc=c(pl_common_len/pl_len*100,
                              pj_common_len/pj_len*100,
                              lj_common_len/lj_len*100))
      
      
      df %>% 
        ggplot() +
        geom_col(aes(x=who, y=perc), fill="lightblue") +
        theme_bw() +
        labs(
          title="Procent wspólnych utworów",
          x="Pomiędzy kim",
          y="Procent zgodności"
        ) +
        scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                           limits=c(0, 20)) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    })
    
    output$plot5 <- renderPlot({
      p <- p_streaming %>% select(artistName) %>% 
        unique()
      l <- l_streaming %>% select(artistName) %>% 
        unique()
      j <- j_streaming %>% select(artistName) %>% 
        unique()
      
      pl_len <- dim(full_join(p, l))[1]
      pj_len <- dim(full_join(p, j))[1]
      lj_len <- dim(full_join(l, j))[1]
      
      pl_common_len <- dim(inner_join(p, l))[1]
      pj_common_len <- dim(inner_join(p, j))[1]
      lj_common_len <- dim(inner_join(l, j))[1]
      
      df <- data.frame(who=c("Patryk i Łukasz", "Patryk i Janek", "Łukasz i Janek"),
                       perc=c(pl_common_len/pl_len*100,
                              pj_common_len/pj_len*100,
                              lj_common_len/lj_len*100))
      
      
      df %>% 
        ggplot() +
        geom_col(aes(x=who, y=perc), fill="lightblue") +
        theme_bw() +
        labs(
          title="Procent wspólnych artystów",
          x="Pomiędzy kim",
          y="Procent zgodności"
        ) + 
        scale_y_continuous(expand=expansion(add=c(0, 0.0058)),
                               limits=c(0, 20)) +
        theme(panel.grid.major.y = element_line(linetype=5),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
    })
    
    
    
    
}

app_ui <- navbarPage(
    title="Spotify",
    tabPanel("Godziny słuchania", ui1, icon = icon("clock")),
    navbarMenu("Najczęściej słuchane", 
               tabPanel("Wykresy", ui2, icon = icon("chart-line")),
               tabPanel("Tabela", ui2a, icon = icon("table")),
               icon = icon("heart")),
    tabPanel("Zgodność muzyki", ui3, icon = icon("handshake"))
)

shinyApp(ui = app_ui, server = server)

