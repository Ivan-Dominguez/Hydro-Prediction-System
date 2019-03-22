library(shiny)
library(shinydashboard)
library(plotly)

current_date<-as.character(Sys.Date())

ui <- dashboardPage(
  
  dashboardHeader(title = "HYDRO", titleWidth = 100),
  
  dashboardSidebar(
    width = 200,
    menuItem("Dashboard", tabName = "dashboardTab", icon = icon("dashboard")),
    textInput("date_txt", "Enter date (YYYY-MM-DD)", "2018-04-02"),
    actionButton("run_btn","Run Prediction"),
    p("Run prediction for the entered date")
  ),
  
  dashboardBody(
    tabItem(tabName = "first", h2("Daily Prediction")),
    fluidRow(box(plotlyOutput("plot"), width=15, height=500))
  ),
  
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

                           predictions_list<- make_predictions(input$date_txt))
                          
                           daily_plot(predictions_list, input$date_txt)
                       }
                     
                    })
    })
}


shinyApp(ui, server)

