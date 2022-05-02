###################
# server.R
# 
# For all your server needs 
###################

library(shinydashboard)
library(shiny)

server <- function(input, output, session) {
  
  ###########################################################################
  ###########################################################################
  
  # REGION PLOT
  
  ###########################################################################
  
  country_reactive_db = reactive({
    temp <- region_summary[region_summary$City == 'All Cities', c(1,2,4,5)]
    temp <- temp %>% as.data.frame() %>%
      filter(State %in% input$state_select,
             Weapon %in% input$weapon_select,
             Year >= as.numeric(input$minimum_year))
    return(distinct(temp))
  })
  
  tryCatch({
    output$region_plot <- plotly::renderPlotly({
      g <- ggplot(country_reactive_db(), aes(x = Year, y = Count, colour = Weapon)) + xlab("Year")  +
        labs(title=paste0("Weapons used in ", input$state_select))
      g1 <- g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
        ylab("Count") + theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
      ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
    })}, error = function(e){
      print("No data available for this combination. Please tweak the variables.")}
  )
  
  
  ###########################################################################
  ###########################################################################
  
  # COMPARISON PLOT
  
  ###########################################################################
  
  comparison_data <- reactive({
    
    if(input$metric == 'Age') {
      comparison_age$Metric <- as.numeric(comparison_age$Metric)
      return(comparison_age)
    }
    
    if(input$metric == 'Race') {
      return(comparison_race)
    }
    
    if(input$metric == 'Sex') {
      return(comparison_sex)
    }
    
    
  })
  
  output$comparison_plot <- plotly::renderPlotly({
    c <- ggplot(comparison_data(), aes(x=Metric, y=Count, fill=Metric)) + geom_col(position = "dodge")  +
      scale_y_continuous(labels = comma)
    if(input$metric == 'Age'){
      c <- c + scale_x_continuous(breaks = scales::breaks_pretty(10))
    }
    c <- c + ylab("Count") + xlab(input$metric) + theme_minimal()   + labs(title=paste0("Comparison based on ", input$metric))
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))
    ggplotly(c, tooltip = c("text")) %>% layout(legend = list(font = list(size=11))) 
  })
  
  ###########################################################################
  ###########################################################################
  
  # MAP PLOT
  
  ###########################################################################
  
  
  output$map <- renderLeaflet({ 
    m()
  })
  
  radius_data <- reactive({
    temp <- main_page_summary[main_page_summary$Year == input$plot_year, ]
    return(temp)
  })
  
  states <- reactive({
    states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
    states$homicides_per_100k <- as.numeric(radius_data()$Homicides_Per_100K)
    breaks <- c(0, 1, 2, 5, 10, 20, 50, 100, Inf)
    states$interval <- findInterval(states$homicides_per_100k, breaks)
    states$bins <- breaks[states$interval + 1]
    return(states)
  })
  
  pal <- reactive({
    colorBin("YlOrRd", domain = states()$bins, bins = c(0, 1, 2, 5, 10, 20, 50, 100, Inf))
  })
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g homicides per 100k people.</sup>",
      states()$name, states()$homicides_per_100k
    ) %>% lapply(htmltools::HTML)
  })
  
  m <- reactive({
    leaflet(states()) %>%
      setView(-96, 37.8, 5) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal()(bins),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.2,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = FALSE),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomright", pal = pal(), values = states()$homicides_per_100k,
                title = "<small>Homicides per 100k</small>")
  })
  
  output$total_homicide_count <- renderText({
    paste0(total_homicide_count, " homicides")
  })
  
  output$total_homicide_year_count <- renderText({
    x <- total_homicide_year_count[total_homicide_year_count$Year == input$plot_year, 2]
    paste0(x, " in ", input$plot_year)
  })
  
  year_plot_data <- reactive({
    return(total_homicide_year_count)
  })
  
  observeEvent(input$plot_year, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      
      addCircleMarkers(data = radius_data(), lat = radius_data()$latitude, lng = radius_data()$longitude, weight = 1, radius = ~(Count)^(1/2),
                       fillOpacity = 0.1, color = "#cc4c02",
                       label = sprintf("%g homicides in %s.</sup>",radius_data()$Count, input$plot_year) %>% lapply(htmltools::HTML))
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#cc4c02"),
      textsize = "25px", direction = "auto")
    
  })
  
  output$main_plot <- plotly::renderPlotly({
    ggplot(year_plot_data(), aes(x = Year, y = Count)) + xlab("Year") +
      geom_line(alpha=0.8, colour = "#E69F00", size=1) + geom_point(size = 2, alpha = 0.8, colour = "#E69F00") +
      theme_minimal() + xlab("Years") + ylab("Homicides") + ggtitle("Homicides (1980 - 2014)") +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  })
  
  ###########################################################################
  ###########################################################################
  
  output$data_preview <- DT::renderDataTable({
    summary <- main_page_summary
    summary <- summary %>% filter(!State %in% c('Puerto Rico', 'Rhode Island'))
    summary$latitude <- NULL
    summary$longitude <- NULL
    summary$Population <- as.numeric(summary$Population)
    summary$Homicides_Per_100K <- as.numeric(summary$Homicides_Per_100K)
    colnames(summary) <- c("State", "Year", "Homicide Count", "Total Population", "Homicides Per 100k")
    datatable(summary, rownames = TRUE)
  })
  
  output$column_list <- renderText({
    original_columns
  })
  
  ###############################################
  ###############################################
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dataset
    if (input$rel != "All") {
      data <- data[dataset$Relationship == input$rel,]
    }
    if (input$crmt != "All") {
      data <- data[data$Crime.Type == input$crmt,]
    }
    if (input$wpn != "All") {
      data <- data[data$Weapon == input$wpn,]
    }
    if (input$vics != "All") {
      data <- data[data$Victim.Sex == input$vics,]
    }
    
    if (input$yr != "All") {
      data <- data[data$Year == input$yr,]
    }
    if (input$state != "All") {
      data <- data[data$State == input$state,]
    }
    
    #
    if (input$crms != "All") {
      data <- data[data$Crime.Solved == input$crms,]
    }
    if (input$vicr != "All") {
      data <- data[data$Victim.Race == input$vicr,]
    }
    
    
    if (input$vice != "All") {
      data <- data[data$Victim.Ethnicity == input$vice,]
    }
    
    if (input$pers != "All") {
      data <- data[data$Perpetrator.Sex == input$pers,]
    }
    if (input$perr != "All") {
      data <- data[data$Perpetrator.Race == input$perr,]
    }
    if (input$recs != "All") {
      data <- data[data$Record.Source == input$recs,]
    }
    data
  }))
  
  
}
