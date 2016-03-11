library(shiny)
library(ggplot2)
library(reshape)

# Load model into the local environment
source("model.R", local = TRUE)

# Define server logic required to generate the plot
shinyServer(function(input, output, session) {
  
  #Session store is a reactive values ~list
  store              <- reactiveValues()
  store$summaryData  <- data.frame()
  
  # Capture input variables in a reactive expression
  runArgs   <- reactive({
    
    # Bind initial state and parameter inputs    
    list( state = vapply( names(state)
                        , function(name) { input[[name]] }
                        , FUN.VALUE = numeric(1)
                        )
        , parameters = vapply( names(parameters)
                             , function(name) { input[[name]] }
                             , FUN.VALUE = numeric(1)
                             )
        
        )
  })
  
  # Run the model in a reactive expression
  runModel  <- reactive({
    
    args <- runArgs()
    
    # Run the simulation; convert result to a data.frame    
    data.frame( solver( y     = args$state
                      , times = seq( time["start"]
                                   , input$time.end
                                   , by = abs(input$time.end - time["start"]) / 100
                                   )
                      , func  = model
                      , parms = args$parameters
                      )
              )
    
  })
  
  observeEvent(input$simStart, {
    updateSliderInput( session
                     , inputId = 'simRun'
                     , min     = input$simStart
                     , step    = (input$simEnd - input$simStart) / simluationSteps
                     )
  })
  
  observeEvent(input$simEnd, {
    updateSliderInput( session
                     , inputId = 'simRun'
                     , value   = input$simEnd
                     , max     = input$simEnd
                     , step    = (input$simEnd - input$simStart) / simluationSteps
                     )
  })
  
  observeEvent(input$simParameter, {
    updateNumericInput(session, 'simEnd', value = input[[input$simParameter]])
  })
  
  observeEvent(input$simRun, {
    
    updateNumericInput(session, input$simParameter, value = input$simRun)
    if(input$tabs == "Simulation") {
      # Update with run updates
      args    <- runArgs()
      result  <- runModel()
      
      newRow <- nrow(store$summaryData) + 1
      
      # Capture each model initial state
      for (state in names(args$state)) {
        store$summaryData[newRow, state] <- args$state[[state]]
      }
      
      # Capture each model parameter
      for (parameter in names(args$parameters)) {
        store$summaryData[newRow, parameter] <- args$parameters[[parameter]]
      }
      
      # Capture each summary calculation
      for (summary in names(state.summary)) {
        store$summaryData[newRow, summary] <- state.summary[[summary]](result)
      }
      
      store$summaryData[newRow, 'series'] <- input$simSeries
      
    }
    
  })
  
  observeEvent( input$resetSummary, {
    store$summaryData <- store$summaryData[nrow(store$summaryData), names(store$summaryData)]
  })
  
  # Simulation plot
  output$modelPlot <- renderPlot({
    
    ggplot(melt(runModel(), id = "time"))               +
      geom_line( aes(time, value, colour = variable) )  +
      ylab("[variable]")                                +
      ylim(input$ymax[1], input$ymax[2])
    
  })
  
  # Summary plot
  output$summaryPlot <- renderPlot({
    if (nrow(store$summaryData) > 0) {
      ggplot( data.frame( x = store$summaryData[[input$simParameter]]
                        , y = store$summaryData[[input$simPlot]]
                        , series = store$summaryData$series
                        )
                        , aes(x, y)
            )                          +
        geom_point()                   +
        geom_line(aes(color = series)) +
        xlab(input$simParameter)       +
        ylab(input$simPlot)
      
    }
  
  })
  
  # Summary download link
  output$downloadSummaryData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      }
    , content = function(con) {
        write.csv(store$summaryData, con)
      }
    )
  
})