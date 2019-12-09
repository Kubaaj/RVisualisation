#-------------------------------------------------------------------------#
#                           USER INTERFACE                                #
#-------------------------------------------------------------------------#


library(RColorBrewer) # dla palet kolorów
library(shinyWidgets) # dla prettyCheckbox()
library(shinydashboard) 
library(shinythemes) # dodatkowe szablony: https://rstudio.github.io/shinythemes/
library(data.table)
library(DT) # do renderDataTable()
# DT: An R interface to the DataTables library
# https://rstudio.github.io/DT/
library(plotly)

data<-read.csv("my_data_new.csv")

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
             
             # wybór kraju
             selectInput(inputId = "kraj2", 
                         label = "Select country:",
                         # teraz lista wartości zależna od danych
                         choices = unique(sort(data$country)),
                         multiple = TRUE # czy dopuszczalny wybór wielu opcji
             ),
             
             uiOutput("przedzial_dat2"),
             
             
             colourpicker::colourInput(inputId = "kolor", 
                                       label = "Select color (not working atm):",
                                       palette = "limited",
                                       allowedCols = c(brewer.pal(9, 'Set1')),
                                       returnName = TRUE),
             
             # zastosuj radioButtons
             radioButtons(inputId = "statystyka2", 
                          label = "Select statistics on y axis:",  
                          
                          choices = c("Gross Domestic Product" = "gdp",
                                      "Gini Index" = "gini",
                                      "Human Development Index" = "hdi",
                                      "Pollution" = "Pollution",
                                      "Urban percentage" = "urban"),
                          selected = "gdp"
           ),
           
           radioButtons(inputId = "statystyka2_x", 
                        label = "Select statistics on x axis:",  
                        
                        choices = c("Gross Domestic Product" = "gdp",
                                    "Gini Index" = "gini",
                                    "Human Development Index" = "hdi",
                                    "Pollution" = "Pollution",
                                    "Urban percentage" = "urban"),
                        selected = "gdp"
           ),
             
             checkboxGroupInput(inputId = "wykresy", 
                                label = "Select kind of the plot:",
                                choices = c("Standard plot" = "standard",
                                            "Wykres z podziałem na kontynenty" = "continent",
                                            "Wykres z podziałem na regiony" = "region"),
                                # domyślny wybór
                                selected = "standard"),
             
             conditionalPanel(
             condition = "input.wykresy.indexOf('continent') > -1",

             # wybór kontynentu
             selectInput(inputId = "kontynent", 
                         label = "Select continent:",
                         # teraz lista wartości zależna od danych
                         choices = unique(sort(data$continent)),
                         multiple = TRUE # czy dopuszczalny wybór wielu opcji
             )),
           
             conditionalPanel(
             condition = "input.wykresy.indexOf('region') > -1",
             
             # wybór regionu
             selectInput(inputId = "region", 
                         label = "Select region:",
                         # teraz lista wartości zależna od danych
                         choices = unique(sort(data$region)),
                         multiple = TRUE # czy dopuszczalny wybór wielu opcji
             )),
            
             helpText(h4("After selecting the parameters, please push the SELECT button")),
             actionButton(inputId = "show_graph_scatter",
                          label = "SELECT")
         ),
         
           mainPanel( 
             
             conditionalPanel(
               condition = "input.wykresy.indexOf('standard') > -1",
               plotlyOutput("wykresPunktowy")
               ),
             conditionalPanel(
               condition = "input.wykresy.indexOf('continent') > -1",
               plotlyOutput("wykresPunktowyContinent")
             ),
             conditionalPanel(
               condition = "input.wykresy.indexOf('region') > -1",
               plotOutput("wykresPunktowyRegion")
             )
             
             ) # koniec mainPanel
           
         ) # koniec sidebarLayout
         )# koniec tabPanel "Wykresy"
,
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
                                      "Urban percentage" = "urban"),
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
) 
)# koniec navbarPage