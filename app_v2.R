library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(gganimate)
library(spotifyr)
library(DT)
library(VennDiagram)
library(shinycssloaders)

source("functions.R")
source("connectapi.R")

library(httr)

# przekopiować z password.txt
clientID = '28e3d7ae0359454888cc25815aa18732'
secret = 'c0f866dbe2db4388b51d5f1f714ca8be'

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

artists <- read.csv("data/artistID")
tracks <- read.csv("data/trackID")

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
                   h1("Wykresy przedstawiające ulubionych wykonawców", align = "center")
  ),
  conditionalPanel(
    condition = "input.kategoria == 'Utwory'",
    h1("Wykresy przedstawiające ulubione utwory", align = "center")),
  hr(),
  fluidRow(
    column(3,offset = 1,selectInput("kategoria",
                         "Wybierz kategorię:",
                         c("Wykonawcy","Utwory"))),
    column(3,offset = 1,sliderInput("n", 
                       label = "Ilość:", 
                       value = 5,
                       min = 1,
                       max = 10)
           ),
    column(3,offset = 1,checkboxGroupInput("who2", "Wybierz osoby:",
                                         choiceNames=c("Patryk", "Łukasz", "Janek"),
                                         choiceValues=c("p", "l", "j"),
                                         selected=c("p", "l", "j"))
    )
  ),
  hr(),
  fluidRow(
    column(6,h3("Ile minut słuchaliśmy naszych ulubionych wykonawców?",align="center"),
           plotly::plotlyOutput("plot2"), style='margin-bottom:30px;border:1px solid; padding: 10px;'),
    column(6,h3("Jak zminiał się czas słuchania na przestrzeni całego roku?",align="center"),
           plotly::plotlyOutput("plot3"), style='margin-bottom:30px;border:1px solid; padding: 10px;')
  ),
  fluidRow(
    column(6,plotly::plotlyOutput("plot6"),style='margin-bottom:30px;border:1px solid; padding: 10px;')
    )
)

ui2a <- fluidPage(
  conditionalPanel(condition = "input.kategoriaa == 'Wykonawcy'",
                   h1("Tabela przedstawiająca ulubionych wykonawców",align = "center")
  ),
  conditionalPanel(
    condition = "input.kategoriaa == 'Utwory'",
    h1("Tabela przedstawiająca ulubione utwory", align = "Center")),
    hr(),
          sidebarLayout(
            sidebarPanel(width = 3,
            selectInput("kategoriaa",
                        "Wybierz kategorię:",
                        c("Wykonawcy","Utwory")),
            strong("Wybierz czyją tabelę chcesz zobaczyć:"),
            p(""),
            fixedRow(
               column(1,offset = 3,actionButton(inputId = "button_p", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-1/p200x200/117042018_2583202521944871_6439550514274272204_n.jpg?_nc_cat=109&ccb=1-5&_nc_sid=7206a8&_nc_ohc=QFaAaBs81foAX9puy1r&_nc_ht=scontent-ham3-1.xx&oh=00_AT9_MBy9ndkBmvrSfEiU61e6ODO0xvL68sqQsXmxxyb_dQ&oe=61F79921');  background-size: cover; background-position: center;")
               )
             ),
            fixedRow(
               column(1,offset = 3,actionButton(inputId = "button_l", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/35237401_2001879876551410_721839996499132416_n.jpg?_nc_cat=104&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=Z8XbrlmH_TwAX-7-QXY&_nc_ht=scontent-ham3-1.xx&oh=00_AT-5GJS2QFCaFNJEeHDoVLmhPnL9LF-diIB6L1sFhn76Dw&oe=61F8B18A');  background-size: cover; background-position: center;")
               )
             ),
            fixedRow(
               column(1,offset = 3,actionButton(inputId = "button_j", label = NULL, style = "width: 150px; height: 150px;
                    background: url('https://scontent-ham3-1.xx.fbcdn.net/v/t1.6435-9/123193344_2840269192876438_7586431629465567622_n.jpg?_nc_cat=103&ccb=1-5&_nc_sid=09cbfe&_nc_ohc=b5KeJi9WozQAX93Qn0Q&_nc_ht=scontent-ham3-1.xx&oh=00_AT-7Dk8EPiY94-U98NLYzsLvZWlYMCE9CJk_DkxG238MyQ&oe=61F76FDD');  background-size: cover; background-position: center;")
               )
                      )
            ),
            mainPanel(
              fixedRow(
                column(12, offset = 1,uiOutput("text_header"))
                ),
               fixedRow(
                 column(12, offset = 1,dataTableOutput("tabela")))
              
            )
          )
    )


ui3 <- fluidPage(
  fluidRow(
    titlePanel("Zgodność muzycznych preferencji"),
    align="center"
  ),
  fluidRow(
    column(
      plotOutput("plot4") %>% withSpinner(type=2, color.background="White"),
      width=6
    ),
    column(
      plotOutput("plot5") %>% withSpinner(type=2, color.background="White"),
      width=6
    )
  ),
  fluidRow(
    titlePanel("Ulubione gatunki muzyczne"),
    align="center"
  ),
  fluidRow(
    plotOutput("genres") %>% withSpinner(type=2, color.background="White"),
    align="center"
  )
  # style = "overflow-y: auto;" 
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
        
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(artistName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(artistName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% group_by(artistName,user) %>% summarise(Minuty = sum(msPlayed)/60000) %>% 
          mutate(Minuty = round(Minuty,0))%>%
          mutate(Użytkownik = case_when(user == "l" ~ "Łukasz",
                                        user == "j" ~ "Janek",
                                        user == "p" ~ "Patryk"))
        
        colnames(top) <- c("Wykonawca","user","Minuty","Użytkownik")
        
        
        ggplot(top, aes(x=Wykonawca,y=Minuty, fill = Użytkownik)) +
          geom_col(position =  "dodge") + theme_bw() +
          labs(
            x="Wykonawca",
            y="Minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank()) + coord_flip()
        
        
      }else if(input$kategoria == "Utwory"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% group_by(trackName,user) %>% summarise(Minuty = sum(msPlayed)/60000)%>%
          mutate(Minuty = round(Minuty,0))%>%
          mutate(Użytkownik = case_when(user == "l" ~ "Łukasz",
                                        user == "j" ~ "Janek",
                                        user == "p" ~ "Patryk"))
        
        colnames(top) <- c("Utwór","user","Minuty","Użytkownik")
        
        ggplot(top, aes(x=Utwór,y=Minuty, fill = Użytkownik)) +
          geom_col(position =  "dodge") + theme_bw() +
          labs(
            x="Utwor",
            y="Minuty"
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
          group_by(artistName, month) %>% summarise(Minuty = round(sum(msPlayed)/60000),0)
          colnames(top) <- c("Wykonawca","Miesiąc","Minuty")
        
        
          ggplot(top, aes(x=Miesiąc,y=Minuty, color = Wykonawca)) +
            geom_line() + theme_bw() +
            labs(
              x="Miesiąc",
              y="Minuty"
            ) +
            theme(panel.grid.major.y = element_line(linetype=5),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank())+
            scale_x_continuous(breaks = seq(0,12,by=1))
      }else if(input$kategoria == "Utwory"){
        top <- streaming %>% filter(user %in% input$who2) %>% 
          group_by(trackName) %>%  summarise(Minuty = sum(msPlayed)/60000) %>% 
          arrange(-Minuty) %>% select(trackName) %>% head(input$n) %>% left_join(streaming) %>% 
          filter(user %in% input$who2) %>% 
          group_by(trackName, month) %>% summarise(Minuty = round(sum(msPlayed)/60000),0)
        colnames(top) <- c("Utwór","Miesiąc","Minuty")
        
        
        ggplot(top, aes(x=Miesiąc,y=Minuty, color = Utwór)) +
          geom_line() + theme_bw() +
          labs(
            x="Miesiąc",
            y="Minuty"
          ) +
          theme(panel.grid.major.y = element_line(linetype=5),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())+
          scale_x_continuous(breaks = seq(0,12,by=1))
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
    
    output$genres <- renderPlot({
      temp <- function(streaming){
        streaming %>%
          group_by(artistName) %>%
          summarise(totalTime = sum(msPlayed)) %>%
          arrange(-totalTime) %>%
          head(25) %>%
          select(artistName)
      }
      
      p_art <- temp(p_streaming)
      j_art <- temp(j_streaming)
      l_art <- temp(l_streaming)
      
      p_gen <- c()
      j_gen <- c()
      l_gen <- c()
      
      for(i in 1:25) {
        p_gen <- c(p_gen, unlist(getArtistInfo(p_art[[i, 1]])[["genres"]]))
        j_gen <- c(j_gen, unlist(getArtistInfo(j_art[[i, 1]])[["genres"]]))
        l_gen <- c(l_gen, unlist(getArtistInfo(l_art[[i, 1]])[["genres"]]))
      }
      
      data = list(Patryk=unique(p_gen), Łukasz=unique(l_gen),
                  Janek=unique(j_gen))
      
      
      grid.newpage()
      v <- venn.diagram(data, filename = NULL,col=c("#440154ff", '#21908dff', '#fde725ff'),
                        fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
                        fontfamily = "sans",
                        cex=1,
                        cat.cex = 3,
                        cat.default.pos = "outer",
                        cat.fontfamily = "sans",
                        cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
                        cat.pos=c(315, 45, 0),
                        rotation.degree=360)
      v[[7]]$label <- stringr::str_to_title(paste(setdiff(setdiff(l_gen, p_gen), j_gen), collapse="\n"))
      v[[8]]$label <- stringr::str_to_title(paste(setdiff(setdiff(j_gen, p_gen), l_gen), collapse="\n"))
      v[[9]]$label <- stringr::str_to_title(paste(setdiff(intersect(p_gen, l_gen), j_gen), collapse="\n"))
      v[[10]]$label <- stringr::str_to_title(paste(intersect(intersect(p_gen, l_gen), j_gen), collapse="\n"))
      v[[11]]$label <- stringr::str_to_title(paste(setdiff(intersect(p_gen, j_gen), l_gen), collapse="\n"))
      v[[12]]$label <-stringr::str_to_title(paste(setdiff(setdiff(p_gen, l_gen), j_gen), collapse="\n"))
      grid.draw(v)
      
    }, height=900, width=900)
    
    
    
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

