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
      menuItem("Explore data", tabName = "data", icon = icon("bar-chart-o")),
      menuItem("Ranking", tabName = "rank", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tabItems(
      #Starts tab content
      tabItem(tabName = "rank",
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
                  tags$div(style="float:right; margin-top: -13px;",
                           numericInput("position", label = "", value = 5)
                  ),
                  tags$div(
                    class="my_title",
                    h4("Ranking: choose the number of positions to show")
                    ),
                  tags$div(
                    class="my_content",
                    textOutput("warning"),
                    dataTableOutput("global_rank")
                    )
                )
              )
      ),
      #Starts the other tab content
      tabItem(tabName = "data",
              tags$div(class="my_title", style="background: #F9F9F9;",
                       h2("Welcome to this shiny app!"),
                       h4("You will find information about happiness scores around the world for
                           since 2015 to 2017")
              ),
              fluidRow(style = "margin-top: 15px;",
                       tags$div(class = "col-md-4",
                                tags$div(class="my_title",
                                         h4("Continent"),
                                         tags$div(class="my_content", style = "margin-left: -19px;",
                                                  checkboxGroupInput("continents", label = "", 
                                                                     choices = list("Africa", "Americas", "Asia", "Europe"),
                                                                     selected = "Africa")
                                         )
                                )
                       ),
                       tags$div(class="col-md-4",
                                tags$div(class="my_title",
                                         h4("Choose a Region"),
                                         tags$div(class="my_content", style = "margin-left: -19px; margin-left: -19px; padding-bottom: 22px;",
                                                  uiOutput("region")
                                         )
                                )
                       ),
                       tags$div(class="col-md-4",
                                tags$div(class="my_title",
                                         h4("Choose a Variable"),
                                         tags$div(class="my_content", style = "margin-left: -19px; padding-bottom: 22px;",
                                                  selectInput("variable", label = "", 
                                                              choices = list("Happiness"="Score",
                                                                             "Economy",
                                                                             "Family",
                                                                             "Health",
                                                                             "Freedom",
                                                                             "Trust",
                                                                             "Generosity")
                                                  )
                                         )
                                )
                       )
              ),
              fluidRow(style="margin-top: 15px;",
                       tags$div(class="col-md-4",
                                tags$div(class="my_title",
                                         h4("Description"),
                                         tags$div(class="my_content", style="margin-left: -19px; height: 432px;",
                                                  textOutput("description")
                                         )
                                )
                       ),
                       tags$div(class="col-md-8",
                                tags$div(class="my_title",
                                         h4("Score by country (sample)"),
                                         tags$div(class="my_content", style="margin-left: -19px;",
                                                  plotOutput("score_plot")
                                         )
                                )
                       )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  exploring_data <- reactive({
    happiness %>%
      filter(Continent%in%input$continents&Region!="")
  })
  
  #Dynamic selectInput with regions acording to selected continents
  output$region <- renderUI({
    
    my_options <- unique(as.character(exploring_data()$Region))
    names(my_options)<- my_options
      
    selectInput("regions", label = "",
                 choices = my_options, 
                 selected = 1)
  })
  
  output$description <- renderText({
    
      if(input$variable=="Score"){
        return("A metric measured in 2015 by asking the sampled people the question:
               'How would you rate your happiness on a scale of 0 to 10 where 10 is the happiest.'")
      }
      
      if(input$variable=="Economy"){
        return("The extent to which GDP contributes to the calculation of the Happiness Score")
      }
      
      if(input$variable=="Family"){
        return("The extent to which Family contributes to the calculation of the Happiness Score")
      }
      
      if(input$variable=="Health"){
        return("(Life Expectancy) The extent to which Life expectancy contributed to the calculation of the Happiness Score")
      }
      
      if(input$variable=="Freedom"){
        return("The extent to which Freedom contributed to the calculation of the Happiness Score.")
      }
      
      if(input$variable=="Trust"){
        return("(Government Corruption)The extent to which Perception of Corruption contributes to Happiness Score.")
      }
      
      if(input$variable=="Generosity"){
        return("The extent to which Generosity contributed to the calculation of the Happiness Score.")
      }
    })
  
  output$score_plot <- renderPlot({
    
    selected_variable <- input$variable
    data_for_plot <- exploring_data() %>% 
                      filter(Region%in%input$regions) %>% 
                      select(Country,selected_variable)
    
    #In case the region has less than 8 countries
    local_length <- min(8,dim(data_for_plot)[1])
    sample_index <- sample(1:local_length,local_length,replace=FALSE)
    
    ggplot(data_for_plot[sample_index,],
           aes(Country,
               data_for_plot[sample_index,which(colnames(data_for_plot)==selected_variable)],
               fill=Country)) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Historical score of",ifelse(selected_variable=="Score","Hapiness",selected_variable))) +
      ylab("Score Mean") +
      theme(text = element_text(size=15),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    })
  
  
  filtered_data <- reactive({
    
    if(input$continent!="All"){
      happiness %>% 
        filter(!is.na(latitude),
               !is.na(longitude)) %>%   
        filter(Continent == input$continent,
               Year == input$year) %>% 
        #Calculating the rank of selected countries
        mutate(Rank = sort(Score, decreasing = TRUE,index.return = TRUE)$ix)
      
    }else{
      happiness %>% 
        filter(Year == input$year)
      }
  })
  
  output$global_data <- renderLeaflet({
    
    #Creating the content of the tooltips in graph
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
  
  #To avoid an understandable error when input$position is empty
  output$warning <- renderText({
    if(is.na(input$position)){
      "Please choose the number of positions!"
    }
  })
  
  output$global_rank <- renderDataTable({
    
    if(is.na(input$position)==FALSE){
      filtered_data() %>% 
        select(Rank, Country, Economy,	Family,	Health,	Freedom, Trust,	Generosity) %>% 
        head(n = input$position)
    }else{
      return(NULL)
    }
    
  },options = list(paging = TRUE, searching = TRUE, pageLength = 5))
  
}

shinyApp(ui = ui, server = server)
