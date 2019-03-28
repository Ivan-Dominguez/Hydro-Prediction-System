library(shiny)
library(shinydashboard)
library(plotly)

current_date<-as.character(Sys.Date())

ui <- dashboardPage(
  
  dashboardHeader(title = "HYDRO", titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    menuItem("Dashboard", tabName = "dashboardTab", icon = icon("dashboard")),
    textInput("date_txt", "Enter date (YYYY-MM-DD)", "2018-03-02"),
    actionButton("run_btn","Run Prediction"),
    p("Run prediction for the entered date")
  ),
  
  dashboardBody(
    
    fluidRow(valueBoxOutput("peak", width = 4)),
    
    tabItem(tabName = "first", h2("Daily Prediction")),
    fluidRow(box(plotlyOutput("plot"), width=12, height=500))
    # fluidRow(
    #   box(
    #     title = "Daily Prediction"
    #     ,status = "primary"
    #     ,solidHeader = TRUE
    #     ,collapsible = TRUE
    #     ,plotOutput("plot", height=500, width=15)
    #     ,height=500, width=15
    #   )
    #)
    
  ),
  
  #set up progress bar
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);;
           left: calc(50% - 400px);;
           }
           "
      )
    )
  )
)


#SERVER
server <- function(input, output) { 
 
  #run predictions
  predictions_list<-reactive({
    
    #wait until Run button is clicked
    if(input$run_btn > 0)
    {
      isolate(
        predictions<-make_predictions(input$date_txt)
      )
    }
  })
  
  
  #plot
  output$plot <- 
    renderPlotly({
      
      #progress bar
      withProgress(message = 'Calculation in progress.',
                   detail = ' This may take a while...', value = 0, {
                     
                     #wait until Run button is clicked
                     if(input$run_btn > 0)
                       {
                         isolate(
                           daily_plot(predictions_list(), input$date_txt)
                         )
                       }
                    })
    })
  
  
  #peak time box
  output$peak <- renderValueBox({
    
    #wait until Run button is clicked
    if(input$run_btn > 0)
    {
      list<-predictions_list()
      
      isolate(
        valueBox(
          formatC(strftime(list$xcoord_list$mean,format="%H:%M:%S"), format="s")
          ,paste('Predicted Peak Time')
          ,icon = icon("stats",lib='glyphicon')
          ,color = "red")
      )
    }
  })
  
  
  
}


shinyApp(ui, server)

