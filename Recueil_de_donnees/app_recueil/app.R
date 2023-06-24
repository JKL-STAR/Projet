library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
#install.packages('rsconnect')
library(rsconnect)
## Partie UI

ui = dashboardPage(
  dashboardHeader(
    title = "Game of Thrones",
    titleWidth = 300
  ),
  
  ## Menu des pages
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("INTRODUCTION", tabName = "vue", icon = icon("dashboard")),
      sidebarMenu(
        
        id = "tabs",
        menuItem("DATAFRAME", icon = icon("table"),
                 
                 ## Sous-menu
                  menuSubItem("Données ",  tabName = "dataframe"),
                  menuSubItem("Données spatiales", tabName = "dataframe1"),
                  menuSubItem("Exploration", tabName = "dataf")),
        menuItem("GRAPHIQUE", icon = icon("bar-chart-o"),
                 menuSubItem("Graphiques ",  tabName = "graphique"),
                 menuSubItem("Histogramme ",  tabName = "graphique1")),
    
      menuItem("CARTE", tabName = "map", icon = icon("map")),
      menuItem("LIEUX FICTIFS", tabName = "extension", icon = icon("list-ol"))
    )
  )),
  ## Le corps de l'application
  
  dashboardBody(
    tabItems(
      
      tabItem("vue",
              
              fluidRow(
              column(6,img(src = "image1.jpeg", height=300, width=350)),
              column(6,
                     
                  
                    p( h2("UNIVERSITE DE PARIS"),
                     h3("Master 2 IMB"),
                     h3("Projet Recueil de données"),
                     h4("BARKAT Salima, BODIAN Pierre Denis, ELION Lessy-Lafoi, LOUIS James Kelson"))
                     )),
              
              p(h4("Game of Thrones également appelée Le Trône de fer
                 selon le titre français de l'œuvre romanesque 
                 dont elle est adaptée, est une série télévisée 
                 américaine de Fantasy crée par David Benioff et D. B. Weiss.
                 
          
                 Le but de notre projet est d'étudier des données de la série,
                 plus précisément en créant une application Shiny
                 oú l'accès sera plus facile pour l'utilisateur.
                 Pour cela, nous commençons par une exploration des 
                 données, puis nous effectuons une représentation graphique afin 
                 de trouver des réponses pour des questions clés, enfin 
                 nous allons représenter les continents fictifs oú se déroule
                 la série."))),
              
 
      
      ## PAGE DATAFRAME
      
      tabItem(
        "dataframe",
        
        selectInput('num1',"Choisir un dataframe",choices=list("scenes"="scenes","appearances"="appearances", "populations"="populations", "episodes"="episodes","characters"="characters")),
        
        numericInput(
          'num2',
          "Nombre d'observations",
          value=10,
          min = 1,
          max = 400000,
          step = NA,
          width = NULL
        ),
        tabsetPanel(type = "tabs",
                    tabPanel("Table", DT::dataTableOutput("plot1")),
                    tabPanel("Summary", verbatimTextOutput("plot2")),
                    tabPanel("str", verbatimTextOutput("plot5"))
                    
                    
        )),
      
      tabItem(
        "dataframe1",
        
        selectInput('num3',"Choisir un dataframe",choices=list("locations"="locations",
                                                               "lakes"="lakes",
                                                               "conts"="conts",
                                                               "land"="land",
                                                               "wall"="wall",
                                                               "islands"="islands",
                                                               "kingdoms"="kingdoms",
                                                               "landscapes"="landscapes",
                                                               "roads"="roads",
                                                               "rivers"="rivers")),
        
        numericInput(
          'num4',
          "Nombre d'observations",
          value=10,
          min = 1,
          max = 400000,
          step = NA,
          width = NULL
        ),
        tabsetPanel(type = "tabs",
                    tabPanel("Table", DT::dataTableOutput("plot3")),
                    tabPanel("Summary", verbatimTextOutput("plot4")),
                    tabPanel("str", verbatimTextOutput("plot6"))
                    
                    
        )),
      
      tabItem("dataf",
              
              
              tabPanel("Exploration 2", 
                       h4("1) Le nombre de personnages morts dans l’ensemble de la série. "),
                       textOutput("nb_mort"),
                       h4("2) Les 5 plus grands meurtriers de la série.  "),
                       tableOutput("meurtrier"),
                       h4("3) La durée de la scène la plus longue."),
                       tableOutput("dur_sc"),
                       h4("4) Lieu le plus visité en nombre de scènes."),
                       tableOutput("lieu"),
                       h4("5) Lieu précis oú le plus de personnages meurent."),
                       tableOutput("lieu2"),
                       h4("6) L’épisode oú Jon Snow est le plus longtemps visible. "),
                       tableOutput("epi_jon"),
                       h4("7) Le nombre de personnages qui passent plus de 30 minutes 
                                     à l’écran sur l’ensemble des saisons."),
                       textOutput("perso"),
                       h4("8) Les deux personnages qui passent le plus de scènes ensemble."),
                       tableOutput("perso2"),
                       h4("9) Les deux personnages qui passent le plus de temps ensemble."),
                       tableOutput("perso3")
                       
              )
              
              
              ),
      
      # PAGE GRAPHIQUE
      
      tabItem("graphique",
              tabsetPanel(type = "tabs",
                          tabPanel("Exploration ", 
                                   
                                   
                                   
                             fluidPage( " "     ,sidebarLayout(sidebarPanel(radioButtons("radio1",'Choix',
                                                choices = list("Durée des scènes" = '1',
                                                
                                                               "Durée des scènes par épisodes"='3',
                                                               "Temps d'apparition cumulé par personnage et saison"='4',
                                                               "Temps de présence par épisode de John Snow"="5"
                                                           ))),
                                   
                                   mainPanel(plotOutput("dist"))))
                                   
                                   
                                   )
                          
                          
                          
              )),
    tabItem("graphique1",
            
            
            
            fluidPage( " ",
                sidebarLayout(sidebarPanel(
                  sliderInput("id",label="histogramme des durées des scènes",
                              min =10, value=1000,max=nrow(scenes))
                  
                ),
                                              
                    mainPanel(plotOutput("slide"))))
            
            
            
            ),
      
    

    ## Page Carte
      
      tabItem("map",
              
              fluidPage(
                
                # Application title
                titlePanel("Carte"),
                
                sidebarLayout(
                  
                  # Sidebar with a slider input
                  sidebarPanel(
                    radioButtons("radio2",'Choix',
                                 choices = list("Univers Got" = '1',
                                                "Temps de présence des personnages principaux" = '2'
                                        
                                 ))
                  ),
                  
                  
                  mainPanel(
                    plotOutput("map1")
                  )
                )
              )
              
              
        
        
      ),
    tabItem("extension", 
             h3("The World"),
            img(src = "carte_complet.png", height=300, width=500),
            h3("Port Real"),
            (img(src = "port_real.png", height=300, width=500)),
            h3("Bravoos"),
            (img(src = "bravos.png", height=300, width=500)),
            h3("Dothraki"),
            (img(src = "dothraki.png", height=300, width=500)),
            h3("Winterfell"),
            (img(src = "winterfell.jpg", height=300, width=500))
            
            )
            
      
      
      
    ),
  title = "Texas Housing",
  skin = "blue"
))

## Partie server

server = function(input, output) {
  
  
  # reactive expression 1
  plot_reactive1 <-reactive({  
    
    
    switch( input$num1,
            appearances = appearances,
            scenes = scenes,
            populations = populations,
            episodes=episodes,
            characters=characters)
  })
  
  inputId2_reactive1 <- reactive({
    
    input$num2
  })
  
  
  # Affichage des tableaux 
  output$plot1 <- DT::renderDataTable({
    DT::datatable(plot_reactive1()[1:inputId2_reactive1(),])
  })
  
  # Affichage des résumés des tableaux
  output$plot2 <- renderPrint({
    summary(plot_reactive1())
  })
  
  output$plot5 <- renderPrint({
    str(plot_reactive1())
  })

  ####### reactive expression 2
  plot_reactive2 <-reactive({  
    
    
    switch( input$num3,
            locations=locations,
            lakes=lakes,
            conts=conts,
            land=land,
            wall=wall,
            islands=islands,
            kingdoms=kingdoms,
            landscapes=landscapes,
            roads=roads,
            rivers=rivers)
  })
  
  inputId2_reactive2 <- reactive({
    
    input$num4
  })
  
  
  # Affichage des tableaux 
  output$plot3 <- DT::renderDataTable({
    DT::datatable(plot_reactive2()[1:inputId2_reactive2(),])
  })
  
  # Affichage des résumés des tableaux
  output$plot4 <- renderPrint({
    summary(plot_reactive2())
  })
  output$plot6 <- renderPrint({
    str(plot_reactive2())
  })
  
  
  
  
  
  
  
  
  # Expresssion reactive qui enregistre le choix du bouton radio
  plot_reactive11 <-reactive({  
    
    
    switch( input$radio1,
            '1' = p3,
  
            '3' = p1,
            '4' = p2,
            "5"=p7
          )
  })
  
  
  output$dist <- renderPlot({
    
    plot_reactive11()})
  
  
  output$nb_mort <-renderText({
    as.character(sum(scenes$nbdeath) )
  })
  
  output$meurtrier <-({
    renderTable(sort(table(characters$killedBy),decreasing = TRUE)[1:5])
  })
  
  output$dur_sc <- renderTable({
    scenes[which.max(scenes$duration),]
  })
  
  output$lieu <- renderTable({
    
    scenes %>% 
      group_by(location) %>% 
      summarise(nbsc = n()) %>% 
      arrange(desc(nbsc)) %>%
      head
  })
  
  output$lieu2 <- renderTable({
    scenes %>% 
      group_by(subLocation) %>% 
      summarise(nbd=sum(nbdeath)) %>% 
      arrange(desc(nbd))%>%
      head
   
  })
  
  output$slide <- renderPlot(
    {hist(scenes$duration[1:input$id], xlab="",
          main="Histogramme des durées des scènes", col="#c994c7")})
  
  
  
  output$epi_jon <- renderTable({
   
    appearances %>%filter(name=="Jon Snow") %>% 
      left_join(scenes) %>% left_join(episodes) %>% 
      group_by(name,episodeId,episodeTitle) %>% 
      summarise(screenTime=sum(duration)) %>% 
      arrange(desc(screenTime)) %>% head(1)
    
    
  })
  output$perso <- renderText({
    
    appearances %>% left_join(scenes)  %>% 
      group_by(name) %>% 
      summarise(screenTime=sum(duration)) %>% 
      filter(screenTime>30*60) %>% 
      nrow()
  })
  
  output$perso2 <- renderTable({
    
    appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% 
      filter(name.x!=name.y) %>% 
      group_by(name.x,name.y) %>% 
      summarise(nbs=n()) %>% 
      arrange(desc(nbs)) %>%
      head
    
  })
  
  output$perso3 <- renderTable({
    
    appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% 
      filter(name.x!=name.y) %>% 
      left_join(scenes %>% select(sceneId,duration)) %>%
      group_by(name.x,name.y) %>% 
      summarise(commonTime=sum(duration)) %>% 
      arrange(desc(commonTime))%>%
      head
    
    
  })
  
  
  # Expresssion reactive qui enregistre le choix du bouton radio2 
  plot_reactive12 <-reactive({  
    
    
    switch( input$radio2,
            '1' = p8,
            '2' = p9

    )
  })
  
  
  output$map1 <- renderPlot({
    
    plot_reactive12()})
  
}

shinyApp(ui=ui, server=server)

