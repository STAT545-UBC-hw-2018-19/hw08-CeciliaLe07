library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(highcharter)

happiness <- read.csv("happiness.csv")

ui <- dashboardPage(
  dashboardHeader(
    title  = "World Hapiness"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ranking", tabName = "stats", icon = icon("bar-chart-o")),
      menuItem("Prediction", tabName = "preds", icon = icon("pie-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tabItems(
      tabItem(tabName = "stats",
              fluidRow(
                tags$div(
                  class="col-md-6",
                  tags$div(class="my_div", style = "margin-bottom: 19px;",
                    radioButtons("year", label = h3("Choose year"),
                               choices = list("2015" = 2015, "2016" = 2016, "2017" = 2017), 
                               selected = 2015,
                               inline = TRUE)
                    ),
                  tags$div(
                    class="my_title",
                    h4("Map")
                  ),
                  tags$div(
                    class="my_content",
                    leafletOutput("global_data")
                  )
                         ),
                tags$div(
                  class="col-md-6",
                  tags$div(class="my_div", style = "padding-bottom: 0px;",
                  selectInput("continent",label = h3("Choose continent"),
                              choices=c("All","Africa","Americas","Asia","Europe"))
                  ),
                  tags$div(
                    class="my_title",
                    h4("Distribution of happiness scores")
                  ),
                  tags$div(
                    class="my_content",
                    highchartOutput("histogram")
                    )
                ),
                tags$div(
                  class="col-md-12", style="margin-top: 15px;",
                  tags$div(
                    class="my_title",
                    h4("Ranking")
                    ),
                  tags$div(
                    class="my_content",
                    dataTableOutput("global_rank")
                    )
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "preds",
              h2("Widgets tab content")
      )
    )
  )
 
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    if(input$continent!="All"){
      happiness %>% 
        filter(!is.na(latitude),
               !is.na(longitude)) %>%   
        filter(Continent == input$continent,
               Year == input$year) %>% 
        mutate(Rank = sort(Score, decreasing = TRUE,index.return = TRUE)$ix)
      
    }else{
      happiness %>% 
        filter(Year == input$year)
      }
  })
  
  output$global_data <- renderLeaflet({
    
    content <- paste0(filtered_data()$Country,
                      "  ",
                      "#",
                      filtered_data()$Rank,
                      "<br/>",
                      "Score: ",
                      round(filtered_data()$Score,4)
    )
    
    filtered_data() %>%  
        leaflet()  %>%   
        addTiles() %>%  
        addCircles(lng = ~longitude, lat = ~latitude, popup= ~content, color = "#af4592")

  })
  
  output$histogram <- renderHighchart({
    hchart(name = "Score of Happiness",
           filtered_data()$Score) 
  })
  
  output$global_rank <- renderDataTable({
    filtered_data() %>% 
      select(Rank, Country, Economy,	Family,	Health,	Freedom, Trust,	Generosity) %>% 
    head()
  },options = list(paging = FALSE, searching = FALSE))
  
}

shinyApp(ui = ui, server = server)
