library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(plotly)
library(shinydashboard)
library(shinyWidgets)


## UI ##########################################################################

ui <- bootstrapPage(
  navbarPage(#theme = shinytheme("flatly"), collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">USA Homicides</a>'), id="nav",
    windowTitle = "USA Homicides",
    
    tabPanel("Introduction",
             sidebarLayout(
               sidebarPanel(
                 h2("Introduction"),
                 p(style="text-align: justify;", intro_text),
                 h3("Navigation"),
                 p(style="text-align: justify;", tab_text),
                 h3("About the Data"),
                 p(style="text-align: justify;", dataset_info),
                 htmlOutput("column_list"),
                 p(style="text-align: justify;", other_data_text, a(href="https://www.icip.iastate.edu/tables/population/states-estimates", "Annual Estimates of the Total Population for States (Iowa State University)"), "to augment the map plot."),
                 width = 3),
               
               mainPanel(
                 h3("Overview"),
                 br(),
                 plotlyOutput("main_plot", width="100%"),
                 hr(),
                 p(style="text-align: justify;", summary_text),
                 hr(),
                 DTOutput('data_preview'),
                 hr(),
                 width=8)
             )),
    
    tabPanel("USA Homicide Incidents",
             div(class="outer",
                 tags$head(includeCSS("styles.css")),
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel(id = "controls", class = "panel panel-default",
                               bottom = 55, left = 55, width = 350, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               
                               span(tags$i(h6("Use the slider to view counts of the data, as well as manipulate the map.")), style="color:#045a8d"),
                               h2(textOutput("total_homicide_count"), align = "right"),
                               h3(textOutput("total_homicide_year_count"), align = "right"),
                               
                               sliderTextInput("plot_year",
                                               label = h5("Select Year"),
                                               choices = seq(1980,2014,1),
                                               selected = 1980,
                                               grid = FALSE,
                                               animate=animationOptions(interval = 1000, loop = FALSE))
                               
                 ),
                 
             )
    ),
    
    tabPanel("Region Plots",
             
             sidebarLayout(
               sidebarPanel(
                 
                 span(tags$i(h6("Please select the options from below. You can select one State and as many Weapons. To view data for All States, select All States. To view data for All Weapons, select All Weapons.")), style="color:#045a8d"),
                 span(tags$i(h6("")), style="color:#045a8d"),
                 span(tags$i(h6("")), style="color:#045a8d"),
                 span(tags$i(h6("")), style="color:#045a8d"),
                 
                 pickerInput("state_select", "State:",
                             choices = as.character(state_list),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = "All States",
                             multiple = FALSE),
                 
                 pickerInput("weapon_select", "Weapon:",
                             choices = as.character(weapon_list),
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = "All Weapons",
                             multiple = TRUE),
                 
                 sliderInput("minimum_year",
                             "Minimum Year:",
                             min = overall_minimum_year,
                             max = overall_maximum_year,
                             value = overall_minimum_year)
               ),
               mainPanel(plotlyOutput("region_plot"))
             )
    ),
    
    tabPanel("Comparisons",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("metric", h3("Select Metric:"),
                              c("Victim Age" = "Age",
                                "Victim Sex" = "Sex",
                                "Victim Race" = "Race")), width=2),
               
               mainPanel(plotlyOutput("comparison_plot"), width=10)
             )
    ),
    
    #tabPanel
    tabPanel(title = "Data Table", 
             
             fluidRow(
               column(2, selectInput("rel", "Relationship:", c("All", unique(as.character(dataset$Relationship))))),
               column(2, selectInput("yr", "Year:",  c("All",  unique(as.character(dataset$Year)))) ),
               column(2, selectInput("wpn", "Weapon:",  c("All", unique(as.character(dataset$Weapon)))) ),
               column(2, selectInput("vics", "Victim.Sex:",  c("All", unique(as.character(dataset$Victim.Sex))))),
               column(2, selectInput("state", "State:", c("All", unique(as.character(dataset$State))))),
               #
               column(2, selectInput("crms", "Crime.Solved:",  c("All", unique(as.character(dataset$Crime.Solved))))),
               column(2, selectInput("vicr", "Victim.Race:", c("All", unique(as.character(dataset$Victim.Race))))),
               column(2, selectInput("vice", "Victim.Ethnicity:",  c("All", unique(as.character(dataset$Victim.Ethnicity))))),
               column(2, selectInput("recs", "Record.Source:", c("All", unique(as.character(dataset$Record.Source))))),
               column(2, selectInput("pers", "Perpetrator.Sex:",  c("All", unique(as.character(dataset$Perpetrator.Sex))))),
               column(2, selectInput("perr", "Perpetrator.Race:", c("All", unique(as.character(dataset$Perpetrator.Race))))),
               
               
               column(2, selectInput("crmt", "Crime.Type:", c("All", unique(as.character(dataset$Crime.Type)))))
             ),
             
             DT::dataTableOutput("table")),  
    
    
    #tabPanel
    tabPanel("About",
             br(),
             column(1),
             column(8, 
                    # h5('This app was developed '),
                    p('This app was built in R and RStudio with shiny, shinydashboard, the tidyverse, and many more packages by Ornella Yema and Temitayo Ologun.'),
                    p("It was the result of Chase Romano's Visual Analytics course at the University of 
                 North Carolina Charlotte through the Data Science and Business Analytics MS program."),
                    p('Reference: http://www.murderdata.com/'),
                    br(),
                    HTML('<a href="https://github.com/Temitayoologun" style="color: #e36209">View Code on GitHub</a>')
             ),
             column(3))
    
  )
  
)
