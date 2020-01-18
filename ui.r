#-------------------------------------------------------------------------#
#                           USER INTERFACE                                #
#-------------------------------------------------------------------------#
list.of.packages <- c("RColorBrewer","data.table","DT","shinythemes","shinyWidgets", "shinydashboard","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(readr)
library(RColorBrewer) # dla palet kolorów
library(shinyWidgets) # dla prettyCheckbox()
library(shinydashboard) 
library(shinythemes) # dodatkowe szablony: https://rstudio.github.io/shinythemes/
library(data.table)
library(DT) # do renderDataTable()
# DT: An R interface to the DataTables library
# https://rstudio.github.io/DT/
library(plotly)

setwd("C:\\Users\\jakub\\Desktop\\WNE_Data_Science\\RVisualisations\\project")
data<-read_csv("my_data_new.csv")

ui <- 
  navbarPage(title = "Economy comparison",
             theme = shinythemes::shinytheme("sandstone"),
             # inne: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, 
             #      readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
             position = "fixed-top",

             # definiujemy pierwszą zakładkę
             tabPanel(title = "Line plot",

    # Sidebar layout
  sidebarLayout(

    sidebarPanel(
      tags$style(type="text/css", "body {padding-top: 70px;}"),
    
      # wybór kraju
      selectInput(inputId = "kraj", 
                  label = "Select country:",
                  # teraz lista wartości zależna od danych
                  choices = unique(sort(data$country)),
                  multiple = FALSE # czy dopuszczalny wybór wielu opcji
      ),
      
      uiOutput("przedzial_dat"),
      
      selectInput(inputId = "statystyka", 
                  label = "Select statistics:",
                  # teraz lista wartości zależna od danych
                  choices = c("gdp", "gini", "hdi", "pollution", "urban"),
                  multiple = FALSE # czy dopuszczalny wybór wielu opcji
      ),
      selectInput(inputId = "theme", 
                  label = "Select theme of plot:",
                  # teraz lista wartości zależna od danych
                  choices = c("standard", "night", "economist", "solarized"),
                  multiple = FALSE # czy dopuszczalny wybór wielu opcji
      ),
      
      helpText(h4("After selecting the parameters, please push the SELECT button")),
      actionButton(inputId = "pokaz_wykres",
                   label = "SELECT"),
      
      helpText(h4("If you want to save selected data into CSV file, please select the button below.")),
      
      # zapisywanie do CSV
      downloadButton(outputId = "zapiszDaneCSV", 
                     label = "Save data (csv)")
      
      ), # koniec sidebarPanel

    # Główny panel do wyświetlana wyników (outputs)
    
    mainPanel( 
      #DT::dataTableOutput('tabelaDanych')
      plotOutput("wykresLiniowy")
      ) # koniec mainPanel
  ) # koniec sidebarLayout
), # koniec tabPanel "Wybór danych"

# Druga zakładka

tabPanel(title = "Comparison animation",
         # Sidebar layout with input and output definitions
         sidebarLayout(
           sidebarPanel(
             
             # zdefiniujmy szerokość panelu
             width = 3,
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             
             radioButtons(inputId = "isanimation", 
                          label = "Should the plot be animated?",
                          choices = c("Yes" = "yes",
                                      "No" = "no"),
                          # domyślny wybór
                          selected = "yes"),
             
             conditionalPanel(
               condition = "input.country_region_continent.indexOf('country') > -1 || input.isanimation.indexOf('yes') > -1",
               
             # wybór kraju
             selectInput(inputId = "kraj2", 
                         label = "Select countries:",
                         # teraz lista wartości zależna od danych
                         choices = unique(sort(data$country)),
                         multiple = TRUE # czy dopuszczalny wybór wielu opcji
             )),
             
             conditionalPanel(
               condition = "input.isanimation.indexOf('no') > -1",
               selectInput(inputId = "rok_scatter", 
                           label = "Select year:",
                           # teraz lista wartości zależna od danych
                           choices = unique(sort(data$year)),
                           multiple = FALSE # czy dopuszczalny wybór wielu opcji
               )
             ),
             conditionalPanel(
               condition = "input.isanimation.indexOf('yes') > -1",
               uiOutput("przedzial_dat2")
             ),
             
             #colourpicker::colourInput(inputId = "kolor", 
              #                         label = "Select color (not working atm):",
              #                        palette = "limited",
              #                       allowedCols = c(brewer.pal(9, 'Set1')),
              #                      returnName = TRUE),
             
             # zastosuj radioButtons
             radioButtons(inputId = "statystyka2", 
                          label = "Select statistics on y axis:",  
                          
                          choices = c("Gross Domestic Product" = "gdp",
                                      "Gini Index" = "gini",
                                      "Human Development Index" = "hdi",
                                      "Pollution" = "Pollution",
                                      "Urban percentage" = "urban"
                                      ),
                          selected = "gdp"
           ),
           
           radioButtons(inputId = "statystyka2_x", 
                        label = "Select statistics on x axis:",  
                        
                        choices = c("Gross Domestic Product" = "gdp",
                                    "Gini Index" = "gini",
                                    "Human Development Index" = "hdi",
                                    "Pollution" = "Pollution",
                                    "Urban percentage" = "urban"
                                    ),
                        selected = "gdp"
           ),
             
           conditionalPanel(
             condition = "input.isanimation.indexOf('no') > -1",
             radioButtons(inputId = "country_region_continent", 
                         label = "Do you want to show distinction for:",
                         # teraz lista wartości zależna od danych
                         choices = c("country", "region", "continent"),
                         selected = "country" # czy dopuszczalny wybór wielu opcji
             )
           ),
            
             helpText(h4("After selecting the parameters, please push the SELECT button")),
             actionButton(inputId = "show_graph_scatter",
                          label = "SELECT")
         ),
         
           mainPanel( 
             
             conditionalPanel(
               condition = "input.isanimation.indexOf('yes') > -1",
               plotlyOutput("wykresPunktowy")
               ),
             conditionalPanel(
               condition = "input.isanimation.indexOf('no') > -1",
               plotlyOutput("wykresPunktowyNoAnim")
             )
             
             ) # koniec mainPanel
           
         ) # koniec sidebarLayout
         ), # koniec tabPanel "Wykresy"

tabPanel(title = "TreeMap",
         
         # Sidebar layout
         sidebarLayout(
           
           sidebarPanel(
             tags$style(type="text/css", "body {padding-top: 70px;}"), # wybór kraju

             selectInput(inputId = "rok", 
                         label = "Select year:",
                         # teraz lista wartości zależna od danych
                         choices = unique(sort(data$year)),
                         multiple = FALSE # czy dopuszczalny wybór wielu opcji
             ),
             
             selectInput(inputId = "statystyka_tm", 
                         label = "Select statistics:",
                         # teraz lista wartości zależna od danych
                         choices = c("gdp", "gini", "hdi", "pollution", "urban"),
                         multiple = FALSE # czy dopuszczalny wybór wielu opcji
             ),
             selectInput(inputId = "theme_tm", 
                         label = "Select theme of plot:",
                         # teraz lista wartości zależna od danych
                         choices = c("magma", "inferno", "plasma", "vividis", "cividis"),
                         multiple = FALSE # czy dopuszczalny wybór wielu opcji
             ),
             
             radioButtons(inputId = "facet_wrap", 
                                label = "Should treemap be divided by continent?",
                                choices = c("Yes" = "yes",
                                            "No" = "no"),
                                # domyślny wybór
                                selected = "yes"),
             
             conditionalPanel(
               condition = "input.facet_wrap.indexOf('no') > -1",
               selectInput(inputId = "statystyka_tm_2", 
                           label = "Select the fill:",
                           # teraz lista wartości zależna od danych
                           choices = c("gdp", "gini", "hdi", "pollution", "urban"),
                           multiple = FALSE # czy dopuszczalny wybór wielu opcji
               )
             ),
             
             helpText(h4("After selecting the parameters, please push the SELECT button")),
             actionButton(inputId = "show_treemap",
                          label = "SELECT")
            
           ), # koniec sidebarPanel
           
           # Główny panel do wyświetlana wyników (outputs)
           
           mainPanel( 
             #DT::dataTableOutput('tabelaDanych')
             conditionalPanel(
               condition = "input.facet_wrap.indexOf('no') > -1",
                plotOutput("wykresTreeMap")
               ),
             conditionalPanel(
               condition = "input.facet_wrap.indexOf('yes') > -1",
               plotOutput("wykresTreeMapContinent")
             )
           ) # koniec mainPanel
         ) # koniec sidebarLayout
), # koniec tabPanel "TreeMap"

tabPanel(title = "Maps",
         # Sidebar layout with input and output definitions
         sidebarLayout(
           sidebarPanel(
             width = 3,
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             radioButtons(inputId = "map_stat", 
                          label = "Select statistics:",  
                          choices = c("Gross Domestic Product" = "gdp",
                                      "Gini Index" = "gini",
                                      "Human Development Index" = "hdi",
                                      "Pollution" = "pollution",
                                      "Urban percentage" = "urban"
                                      ),
                          selected = "gdp"
             ),
             selectInput(inputId = "map_year", 
                         label = "Select year:",
                         choices = unique(sort(data$year)),
                         multiple = FALSE
             ),
             selectInput(inputId = "map_color", 
                         label = "Select color:",
                         choices = c("magma","inferno","plasma","viridis","cividis"),
                         multiple = FALSE
             ),
             actionButton(inputId = "show_map",
                          label = "SELECT"
             )
           ),
           mainPanel( 
             plotOutput("mapa")
           )
         )
) , # koniec tabPanel "Maps"

tabPanel(title = "Continents",
         sidebarLayout(
           sidebarPanel(
             width = 3,
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             radioButtons(inputId = "bar_stat", 
                          label = "Select statistics:",  
                          choices = c("Gross Domestic Product" = "gdp",
                                      "Gini Index" = "gini",
                                      "Human Development Index" = "hdi",
                                      "Pollution" = "pollution",
                                      "Urban percentage" = "urban"
                                      ),
                          selected = "gdp"
             ),
             selectInput(inputId = "bar_year", 
                         label = "Select year:",
                         choices = unique(sort(data$year)),
                         multiple = FALSE
             ),
             sliderInput("bar_slider", 
                         label = "Select range:", 
                         min = 0, 
                         max = 100, 
                         value = c(0, 100)),
             actionButton(inputId = "show_bar",
                          label = "SELECT"
             )
           ),
           mainPanel( 
             plotOutput("continent_bar")
           )
         )
) # koniec tabPanel "Continents"

) # koniec navbarPage
