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
    
    actionButton("run_btn","Start"),
    p("  Run prediction for the entered date"),
    
    checkboxInput("cubist_chk", "Cubist", TRUE),
    checkboxInput("xgb_chk", "XGBoost", TRUE),
    checkboxInput("dl_chk", "Deep Learning", TRUE),
    checkboxInput("rf_chk", "Random Forest", TRUE),
    checkboxInput("avg_chk", "Average", TRUE),
    checkboxInput("test_chk", "Test Data", TRUE),
    
    actionButton("modify_btn","Modify Graph"),
    
    p("Remove curves from graph")
  ),
  
  dashboardBody(
    
    #top boxes
    fluidRow(
      valueBoxOutput("pred_peak", width = 4),
      valueBoxOutput("real_peak", width = 4),
      valueBoxOutput("error", width = 4)
      ),
    
    tabItem(tabName = "first", h2("Daily Prediction")),
    fluidRow(box(plotlyOutput("plot"), width=12, height=500))
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
                         #isolate(
                           checkbox_list<-list("cubist_chk"=input$cubist_chk,
                                                 "xgb_chk"= input$xgb_chk,
                                                 "dl_chk"= input$dl_chk,
                                                 "rf_chk"= input$rf_chk,
                                                 "avg_chk"=input$avg_chk,
                                                 "test_chk"=input$test_chk)
                           
                           plot2(predictions_list(), input$date_txt,checkbox_list)
                         #)
                       }
                    })
    })
  
  
  #pred_peak time box
  output$pred_peak <- renderValueBox({
    
    #wait until Run button is clicked
    if(input$run_btn > 0)
    {
      list<-predictions_list()
      
      valueBox(
          formatC(strftime(list$xcoord_list$mean,format="%H:%M:%S"), format="s")
          ,paste('Predicted Peak Time')
          ,icon = icon("stats",lib='glyphicon')
          ,color = "light-blue")
    }else{
      valueBox(
        formatC("-", format="s")
        ,paste('Predicted Peak Time')
        ,icon = icon("stats",lib='glyphicon')
        ,color = "light-blue")
    }
  })
  
  
  #real_peak time box
  output$real_peak <- renderValueBox({
    
    #wait until Run button is clicked
    if(input$run_btn > 0)
    {
      list<-predictions_list()
      
      valueBox(
        formatC(strftime(list$xcoord_list$test, format="%H:%M:%S"), format="s")
        ,paste('Real Peak Time')
        ,icon = icon("clock")
        ,color = 'red')
    }else{
      valueBox(
        formatC("-", format="s")
        ,paste('Real Peak Time')
        ,icon = icon("clock")
        ,color = 'red')
    }
  })
  
  #error box
  output$error <- renderValueBox({
    
    #wait until Run button is clicked
    if(input$run_btn > 0)
    {
      list<-predictions_list()
      error_in_min<-difftime(list$xcoord_list$mean, list$xcoord_list$test,units="mins" )
      
      valueBox(
        formatC(paste(error_in_min, " minutes"), format="s")
        ,paste('Error')
        ,icon = icon("times")
        ,color = "black")
    }else{
      valueBox(
        formatC("-", format="s")
        ,paste('Error')
        ,icon = icon("times")
        ,color = "black")
    }
  })
  
}


shinyApp(ui, server)

