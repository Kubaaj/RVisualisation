###########################################################
#                   Visualizations in R                   #
#                      Jakub Ajchel                       #
#                     Jerzy Bednarski                     #
#                        Project                          #
###########################################################


library(xts)
library(ggplot2)
library(dplyr)
library(colourpicker) 
library(RColorBrewer)
library(data.table)
library(DT)
library(tibble)
library(plotly)
library(gganimate)
library(ggthemes)

#data <- prepare_data()

#setwd("C:\\Users\\jakub\\Desktop\\WNE_Data_Science\\RVisualisations\\project")

data<-read.csv("my_data_new.csv")

# Definicja logiki serwera

server <- function(input,    # zawiera elementy zdefiniowane w części ui
                   output) { # zawiera elementy generowane w części server,
                             # które będą wyświetlane w głównym panelu (mainPanel)

  
  # najpierw filtrujemy tylko po podanym kraju
  
  dane_kraj <- eventReactive(
    eventExpr = input$kraj, 
    valueExpr = { 
      data %>% 
        filter(country == input$kraj) 
      })
  

  output$przedzial_dat <- renderUI({
    
    min_ <- 1960
    max_ <- 2018
    
    # slider - suwak
    sliderInput(inputId = "przedzial_dat", 
                label = "Select years for analysis:", 
                min = min_, # wartośc minimalna
                max = max_, # wartośc maksymalna
                step = 1,   # możliwa zmiana (krok)
                sep = "",   # separator tysięcy w wyświetlanych wartościach
                ticks = FALSE, # czy wyświetlać pomocnicze znaczniki
                value = c(min_, max_)) # zakres wartości
    })
  
  
  # potem generujemy po kliknięciu dane dla wybranego zakresu dat
  
  wybraneDane <- eventReactive(
    eventExpr = input$pokaz_wykres,
    valueExpr = { 
      # pobieramy dane                        
      dane_wybrane <- data %>% 
        filter(country == input$kraj) %>% 
        filter(year >= input$przedzial_dat[[1]]) %>% 
        filter(year <= input$przedzial_dat[[2]])
      
      
      if (input$statystyka == "gdp") {
        dane_wybrane$statystyka <- dane_wybrane$gdp
      } else if (input$statystyka == "gini") {
        dane_wybrane$statystyka <- dane_wybrane$gini
      } else if (input$statystyka == "hdi") {
        dane_wybrane$statystyka <- dane_wybrane$hdi
      } else if (input$statystyka == "pollution") {
        dane_wybrane$statystyka <- dane_wybrane$pollution
      } else if (input$statystyka == "urban") {
        dane_wybrane$statystyka <- dane_wybrane$urban
      }
      
      return(dane_wybrane)
      
    }) # koniec eventReactive
  
  
  wykresLiniowy <- eventReactive(
    eventExpr = input$pokaz_wykres,
    valueExpr = {
      
      wykres <- ggplot(data = wybraneDane()) 
      wykres <- wykres +
        geom_line(aes(x = year,
                      y = statystyka,
                      color = "line")) +
        labs(x = "year",
             y = input$statystyka,
             title = "Country development over years")
      
      if (input$theme == "standard"){
        wykres <- wykres + scale_color_manual(values = "black") + 
          theme(plot.title = element_text(color = 'tomato4', size = 15, face = 'bold.italic'),
                                         legend.position = "none",
                                         axis.title = element_text(face = "bold", color = "tomato4"),
                                         axis.text.x = element_text(face = "bold", color = "tomato4"), # Options for labels of the x axis
                                         axis.text.y = element_text(face = "bold", color = "tomato4", angle = 45))
      } else if (input$theme == "night") {
        wykres <- wykres + scale_color_manual(values = "white")+
          theme(line = element_line(color = "white", size = 0.5, linetype = 1, lineend = "butt"),
                                 plot.background=element_rect(fill = "gray"),
                                 panel.background = element_rect(fill = 'black'),
                                 plot.title = element_text(color = 'tomato4', size = 15, face = 'bold.italic'),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 legend.position = "none",  
                                 axis.title = element_text(face = "bold", color = "tomato4"),
                                 axis.text.x = element_text(face = "bold", color = "tomato4"), # Options for labels of the x axis
                                 axis.text.y = element_text(face = "bold", color = "tomato4", angle = 45))
      } else if (input$theme == "economist") {
        wykres <- wykres + theme_economist() + scale_colour_economist() + theme(legend.position = "none")
      } else if (input$theme == "solarized") {
        wykres <- wykres + theme_solarized() + scale_colour_solarized() + theme(legend.position = "none")
      }

      wykresLiniowy = wykres
      # co ma zwrócić nasz eventReactive
      return(wykresLiniowy)
      
    }) # koniec eventReactive
  
  output$przedzial_dat2 <- renderUI({
    
    min_ <- 1960
    
    max_ <- 2018
    
    # slider - suwak
    sliderInput(inputId = "przedzial_dat2", 
                label = "Select years for analysis:", 
                min = min_, # wartośc minimalna
                max = max_, # wartośc maksymalna
                step = 1,   # możliwa zmiana (krok)
                sep = "",   # separator tysięcy w wyświetlanych wartościach
                ticks = FALSE, # czy wyświetlać pomocnicze znaczniki
                value = c(min_, max_)) # zakres wartości
  })
  
  wybraneDaneScatter <- eventReactive(
    eventExpr = input$show_graph_scatter,
    valueExpr = { 
      # pobieramy dane                        
      dane_wybrane2 <- data %>% 
        filter(country %in% input$kraj2) %>% 
        filter(year >= input$przedzial_dat2[[1]]) %>% 
        filter(year <= input$przedzial_dat2[[2]])
      
      #dane_wybrane2$year <- as.Date(dane_wybrane2$year)
      
      if (input$statystyka2 == "gdp") {
        dane_wybrane2$statystyka <- dane_wybrane2$gdp
      } else if (input$statystyka2 == "gini") {
        dane_wybrane2$statystyka <- dane_wybrane2$gini
      } else if (input$statystyka2 == "hdi") {
        dane_wybrane2$statystyka <- dane_wybrane2$hdi
      } else if (input$statystyka2 == "pollution") {
        dane_wybrane2$statystyka <- dane_wybrane2$pollution
      } else if (input$statystyka2 == "urban") {
        dane_wybrane2$statystyka <- dane_wybrane2$urban
      }
      
      if (input$statystyka2_x == "gdp") {
        dane_wybrane2$statystyka_x <- dane_wybrane2$gdp
      } else if (input$statystyka2_x == "gini") {
        dane_wybrane2$statystyka_x <- dane_wybrane2$gini
      } else if (input$statystyka2_x == "hdi") {
        dane_wybrane2$statystyka_x <- dane_wybrane2$hdi
      } else if (input$statystyka2_x == "pollution") {
        dane_wybrane2$statystyka_x <- dane_wybrane2$pollution
      } else if (input$statystyka2_x == "urban") {
        dane_wybrane2$statystyka_x <- dane_wybrane2$urban
      }
    
      return(dane_wybrane2)
      
    }) # koniec eventReactive
  
  wykresPunktowy <- eventReactive(
    eventExpr = input$show_graph_scatter,
    valueExpr = {
      
      wykres_scatter <- ggplot(data = wybraneDaneScatter(), aes(x = statystyka_x, y = statystyka, col = country, shape = continent)) 
      wykres_scatter <- wykres_scatter +
        geom_point(aes(frame = year)) 
      #wykres_scatter <- ggplotly(wykres_scatter)
      # co ma zwrócić nasz eventReactive
      return(wykres_scatter)
      
    }) # koniec eventReactive
  
  
  # renderujemy wykresy jako elementy w output
  # NOWE! wykres w plotly renderujemy za pomocą renderPlotly!!!!
  output$wykresLiniowy <- renderPlot(wykresLiniowy())
  
  output$wykresPunktowy <- renderPlotly(ggplotly(wykresPunktowy()))
  
  #output$wykres_boxplot <- renderPlotly(ggplotly(wykresik_boxplot()))
  
  #output$wykres_density <- renderPlotly(ggplotly(wykresik_density()))
  
  
  # Zapisanie danych do pliku csv
  
  output$zapiszDaneCSV <- downloadHandler(
    
    # podajemy nazwę pliku jako funkcję
    # nazwa (może być dynamiczna, zależna od danych)
    
    filename = function() paste0("Wykres_", input$kraj, ".csv"),
    
    # podajemy zawartość jako funkcję
    
    content = function(file) {
      write.csv(data.frame(wybraneDane()),  # zapisujemy wybrane dane
                # konwertując obiekt xts na data.frame
                # wtedy daty będą zapisane jako nazwy wierszy
                file, 
                row.names = TRUE # dlatego zostawiamy nazwy wierszy 
                )
      }
    ) # koniec downloadHandler
  
  # data.table renderowany za pomocą renderDatatable()
  
  output$tabelaDanych <- renderDataTable(data.frame(wybraneDane()) %>% 
                                           select(-country))
  
  
} # koniec definicji server
