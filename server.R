###########################################################
#                   Visualizations in R                   #
#                      Jakub Ajchel                       #
#                     Jerzy Bednarski                     #
#                        Project                          #
###########################################################
<<<<<<< HEAD

list.of.packages <- c("ggplot2","dplyr","colourpicker","RColorBrewer","data.table","DT","tibble","plotly","gganimate","ggthemes","rgeos","treemapify","treemap","rnaturalearth","sf","countrycode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

=======
library(xts)
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
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
<<<<<<< HEAD
library(treemapify)
library(treemap)
library(rnaturalearth)
library(sf)
library(countrycode)
library(rgeos)

=======
library("rnaturalearth")
library("sf")
library(countrycode)
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
#data <- prepare_data()

setwd("C:\\Users\\jakub\\Desktop\\WNE_Data_Science\\RVisualisations\\project")

<<<<<<< HEAD
data<-read_csv("my_data_new.csv")
=======
data<-read.csv("my_data_new.csv")
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
world <- ne_countries(scale = "medium", returnclass = "sf")
DATA <-fread("my_data_new.csv")
cc <- countrycode::codelist

countries <- unique(DATA$country)
mapping <- data.table(countries,iso = countrycode::codelist$iso3c[match(countries,countrycode::codelist$country.name.en)])

country = c("Czech Republic", "Russian Federation", "Korea, Rep.", "Egypt, Arab Rep.", "Congo, Rep.","Venezule" )
iso = c("CZE", "RUS", "KOR", "EGY", "COG", "VEN")

manual_mapping <- data.table(countries = country, iso)
mapping <- rbind(mapping[!is.na(iso),], manual_mapping)
mapping[,country:= countries]
mapping[,countries:=NULL]
DATA <- merge(DATA,mapping, by= "country" , all.x = T)
rm(mapping, country, iso, countries)
<<<<<<< HEAD
=======

>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06

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
  
  ############### 2 ###############
  
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
      if (input$isanimation == "yes") {
        dane_wybrane2 <- data %>% 
        filter(country %in% input$kraj2) %>% 
        filter(year >= input$przedzial_dat2[[1]]) %>% 
        filter(year <= input$przedzial_dat2[[2]])
      } else if (input$country_region_continent == "country"){
        dane_wybrane2 <- data %>% 
          filter(country %in% input$kraj2) %>% 
          filter(year == input$rok_scatter)
      } else if (input$country_region_continent == "continent"){
        dane_wybrane2 <- data %>% 
          filter(year == input$rok_scatter)
      } else if (input$country_region_continent == "region"){
        dane_wybrane2 <- data %>% 
          filter(year == input$rok_scatter)
      }
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
      if (input$isanimation == "yes") {
      wykres_scatter <- ggplot(data = wybraneDaneScatter(), aes(x = statystyka_x, y = statystyka, col = country, shape = continent)) 
      wykres_scatter <- wykres_scatter +
<<<<<<< HEAD
        geom_point(aes(frame = year)) + 
        xlab(input$statystyka2_x) + 
        ylab(input$statystyka2)
=======
        geom_point(aes(frame = year)) #+
        # xlab(input$statystyka2_x) +
        # ylab(input$statystyka2)
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
      #wykres_scatter <- ggplotly(wykres_scatter)
      # co ma zwrócić nasz eventReactive
      return(wykres_scatter) }
      
    }) # koniec eventReactive
  
<<<<<<< HEAD
  wykresPunktowyNoAnim <- eventReactive(
    eventExpr = input$show_graph_scatter,
    valueExpr = {
      if (input$isanimation == "no") {
        
      if(input$country_region_continent == "country"){
        wykres_scatter_no_anim <- ggplot(data = wybraneDaneScatter(), aes(x = statystyka_x,y=statystyka,label=country,col=continent,shape=region))
      } else if (input$country_region_continent == "continent"){
        xD = wybraneDaneScatter()
        xD <- xD %>% group_by(continent) %>% 
          summarise(statystyka_x = mean(statystyka_x, na.rm=TRUE), statystyka = mean(statystyka, na.rm=TRUE))
        wykres_scatter_no_anim <- ggplot(data = xD , aes(x = statystyka_x, y = statystyka, col = continent))
      } else if (input$country_region_continent == "region"){
        xD = wybraneDaneScatter()
        xD <- xD %>% group_by(region) %>% 
          summarise(statystyka_x = mean(statystyka_x, na.rm=TRUE), statystyka = mean(statystyka, na.rm=TRUE))
        wykres_scatter_no_anim <- ggplot(data = xD , aes(x = statystyka_x, y = statystyka, col = region))
      }

        
      wykres_scatter_no_anim <- wykres_scatter_no_anim +
        geom_point() + 
        xlab(input$statystyka2_x) + 
        ylab(input$statystyka2)
      
      # co ma zwrócić nasz eventReactive
      return(wykres_scatter_no_anim)}
      
    }) # koniec eventReactive
  
  
  
  
  ############### 3 ###############
  
  wybraneDaneTreeMap <- eventReactive(
    eventExpr = input$show_treemap,
    valueExpr = { 
      # pobieramy dane                        
      dane_wybrane_tm <- data %>% 
        filter(year == input$rok)
      
      
      if (input$statystyka_tm == "gdp") {
        dane_wybrane_tm$statystyka <- dane_wybrane_tm$gdp
      } else if (input$statystyka_tm == "gini") {
        dane_wybrane_tm$statystyka <- dane_wybrane_tm$gini
      } else if (input$statystyka_tm == "hdi") {
        dane_wybrane_tm$statystyka <- dane_wybrane_tm$hdi
      } else if (input$statystyka_tm == "pollution") {
        dane_wybrane_tm$statystyka <- dane_wybrane_tm$pollution
      } else if (input$statystyka_tm == "urban") {
        dane_wybrane_tm$statystyka <- dane_wybrane_tm$urban
      }
      
        if (input$statystyka_tm_2 == "gdp"){
        dane_wybrane_tm$statystyka2 <- dane_wybrane_tm$gdp
      } else if (input$statystyka_tm_2 == "gini") {
        dane_wybrane_tm$statystyka2 <- dane_wybrane_tm$gini
      } else if (input$statystyka_tm_2 == "hdi") {
        dane_wybrane_tm$statystyka2 <- dane_wybrane_tm$hdi
      } else if (input$statystyka_tm_2 == "pollution") {
        dane_wybrane_tm$statystyka2 <- dane_wybrane_tm$pollution
      } else if (input$statystyka_tm_2 == "urban") {
        dane_wybrane_tm$statystyka2 <- dane_wybrane_tm$urban
      }
      return(dane_wybrane_tm)
      
    }) # koniec eventReactive
  
  
  wykresTreeMapContinent <- eventReactive(
    eventExpr = input$show_treemap,
    valueExpr = {
      wykres_tree_map <- ggplot(data = wybraneDaneTreeMap(), aes(area = statystyka, label = country, fill = continent)) 
      wykresTreeMapContinent <- wykres_tree_map +
        geom_treemap() +
        geom_treemap_text(grow = T, reflow = T, place = "topleft", colour = "white") +
        facet_wrap( ~continent)

      if (input$theme_tm == "magma"){
        wykresTreeMapContinent <- wykresTreeMapContinent + scale_fill_viridis_d(option = "magma", na.value = "grey50")
      } else if (input$theme_tm == "inferno"){
        wykresTreeMapContinent <- wykresTreeMapContinent + scale_fill_viridis_d(option = "inferno", na.value = "grey50")
      } else if (input$theme_tm == "plasma"){
        wykresTreeMapContinent <- wykresTreeMapContinent + scale_fill_viridis_d(option = "plasma", na.value = "grey50")
      } else if (input$theme_tm == "viridis"){
        wykresTreeMapContinent <- wykresTreeMapContinent + scale_fill_viridis_d(option = "viridis", na.value = "grey50")
      } else if (input$theme_tm == "cividis"){
        wykresTreeMapContinent <- wykresTreeMapContinent + scale_fill_viridis_d(option = "cividis", na.value = "grey50")
      }
        
      return(wykresTreeMapContinent)
      
    }) # koniec eventReactive
  
  wykresTreeMap <- eventReactive(
    eventExpr = input$show_treemap,
    valueExpr = {
      wykres_tree_map2 <- ggplot(data = wybraneDaneTreeMap(), aes(area = statystyka, label = country, fill = statystyka2)) 
      wykresTreeMap <- wykres_tree_map2 +
        geom_treemap() +
        geom_treemap_text(grow = T, reflow = T, place = "topleft", colour = "white") + 
        labs(fill = input$statystyka_tm_2)
  
      if (input$theme_tm == "magma"){
        wykresTreeMap <- wykresTreeMap + scale_fill_viridis_c(option = "magma", na.value = "grey50")
      } else if (input$theme_tm == "inferno"){
        wykresTreeMap <- wykresTreeMap + scale_fill_viridis_c(option = "inferno", na.value = "grey50")
      } else if (input$theme_tm == "plasma"){
        wykresTreeMap <- wykresTreeMap + scale_fill_viridis_c(option = "plasma", na.value = "grey50")
      } else if (input$theme_tm == "viridis"){
        wykresTreeMap <- wykresTreeMap + scale_fill_viridis_c(option = "viridis", na.value = "grey50")
      } else if (input$theme_tm == "cividis"){
        wykresTreeMap <- wykresTreeMap + scale_fill_viridis_c(option = "cividis", na.value = "grey50")
      }
      return(wykresTreeMap)
      
    }) # koniec eventReactive
  
  
  ############### 4 ###############
=======
  # mapy
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
  
  wykresMapa <- eventReactive(
    eventExpr = input$show_map,
    valueExpr = {
      subset <- DATA[year == input$map_year,.(iso, gdp, gini, hdi, pollution, urban)]
      
      world_isos <- as.data.table(world[,c('name','iso_a3')])
      
      dt <- merge(world_isos[!is.na(iso_a3),], subset, by.x = 'iso_a3', by.y = 'iso', all.x = T)
      dt <- rbind(dt,world_isos[is.na(iso_a3)], fill =T)
      dt <- st_sf(dt)
      
      ggplot(data = dt) +
        geom_sf(aes(fill = get(input$map_stat))) +
        scale_fill_viridis_c(option = input$map_color) +
        guides(fill = guide_legend(title = input$map_stat))
    }
  )
  
<<<<<<< HEAD
  ############### 5 ###############
=======
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
  
  wykresBar <- eventReactive(
    eventExpr = input$show_bar,
    valueExpr = {
      DATA[eval(bquote(year == .(as.numeric(input$bar_year)) &
                         .(as.name(input$bar_stat)) >= quantile(.(as.name(input$bar_stat)), .(input$bar_slider[1]/100), na.rm = T) &
                         .(as.name(input$bar_stat)) <= quantile(.(as.name(input$bar_stat)), .(input$bar_slider[2]/100), na.rm = T))),]  %>%
        ggplot() +
        geom_bar(aes(x = continent, fill = continent)) +
        theme(axis.text.x = element_blank())
    }
  )
<<<<<<< HEAD
  
  ##############################################
  
=======
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
  
  # renderujemy wykresy jako elementy w output
  # NOWE! wykres w plotly renderujemy za pomocą renderPlotly!!!!
  ### 1 ###
  output$wykresLiniowy <- renderPlot(wykresLiniowy())
  
  ### 2 ###
  output$wykresPunktowy <- renderPlotly(ggplotly(wykresPunktowy()))
  output$wykresPunktowyNoAnim <- renderPlotly(ggplotly(wykresPunktowyNoAnim()))

  ### 3 ###
  output$wykresTreeMapContinent <- renderPlot(wykresTreeMapContinent())
  output$wykresTreeMap <- renderPlot(wykresTreeMap())
  
  ### 4 ###
  output$mapa <- renderPlot(wykresMapa())
  
<<<<<<< HEAD
  ### 5 ###
  output$continent_bar <- renderPlot(wykresBar())
  
=======
  output$mapa <- renderPlot(wykresMapa())
>>>>>>> cedb295d2f05a70157b8e05e6da42a26e2ae6f06
  # Zapisanie danych do pliku csv
  output$continent_bar <- renderPlot(wykresBar())
  
  
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
