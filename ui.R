library(shiny)
library(markdown)

# Load model into the local environment
source("model.R", local = TRUE)

# Build an input UI from the model
modelInputs <- list(
  
    # Sidebar header text
    helpText(HTML(markdownToHTML(text = sidebarHeader, fragment.only = TRUE)))
    
    # state input boxes
  , lapply( names(state)
          , function(name) { 
              numericInput( name
                          , stateFormat(name)
                          , state[name]
                          , step = state[name] / 2
                          )
              }
          )
    
    # parameter input boxes
  , lapply( names(parameters)
          , function(name) {
              numericInput( name
                          , parameterFormat(name)
                          , parameters[name]
                          , step = parameters[name] / 2
                          )
              }
          )
    
    # Time scale adjustment
  , sliderInput( "time.end" 
               , "Time scale"
               , min   = time["end"] * 0.1
               , max   = time["end"] * 10
               , value = time["end"]
               , step  = time["end"] * 0.1
               )
    
    # Save to summary button    
  , br()
    
    # Sidebar footer text
  , helpText(HTML(markdownToHTML(text = sidebarFooter, fragment.only = TRUE)))
    
  )

controls    <- tagList(modelInputs)
kineticsTab <- tabPanel( "Kinetic time course"
                       , plotOutput("modelPlot")
                       , wellPanel( sliderInput( "ymax"
                                               , "Y-axis scale:"
                                               , min = 0
                                               , max = max(state)
                                               , value = c(0, 0.1 * max(state))
                                               )
                                  )
                       )

summaryVars        <- c(names(parameters), names(state))
names(summaryVars) <- c(parameterFormat(names(parameters)), stateFormat(names(state)))
simControls  <- div( fluidRow( column( 3
                                      , selectInput( 'simParameter'
                                                   , 'Variable'
                                                   , choices = summaryVars
                                                   )
                                      )
                              , column( 3
                                      , numericInput( 'simStart'
                                                    , 'Start value'
                                                    , 0
                                                    )
                                      )
                              , column( 3
                                      , numericInput( 'simEnd'
                                                    , 'End'
                                                    , 1
                                                    )
                                      )
                              , column( 3
                                      , selectInput( 'simPlot'
                                                   , "Plot"
                                                   , choices = names(state.summary)
                                                   )
                                      )
                              )
                    , fluidRow( column( 3
                                      , textInput( 'simSeries'
                                                 , 'Series name'
                                                 , "series1"
                                                 )
                                      )
                              , column( 9
                                      , sliderInput( 'simRun'
                                                   , "Run simulation"
                                                   , min     = 0
                                                   , max     = 1
                                                   , value   = 1
                                                   , animate = animationOptions(interval = simulationTime / simluationSteps)
                                                   )
                                      )
                              )
                    , fluidRow( actionButton("resetSummary", "Clear data")
                              , downloadButton('downloadSummaryData', 'Download Data')
                              )
                    )

simTab       <- tabPanel( "Simulation"
                        , br()
                        , wellPanel(simControls)
                        , plotOutput('summaryPlot')
                        # , actionButton("resetSummary", "Clear data")
                        # , downloadButton('downloadSummaryData', 'Download Data')
                        )
#                           # TODO implement a less ugly horizontal well 
#                           , wellPanel( class = "well container-fluid"
#                                        , div( class = "row-fluid"
#                                               , div( class = "span5"
#                                                      , selectInput( "summaryY"
#                                                                     , "Summarize:"
#                                                                     , choices = names(state.summary)
#                                                      )
#                                               )
#                                               , div( class = "span5"
#                                                      , selectInput("summaryX"
#                                                                    , "As a function of:"
#                                                                    , choices = c( stateFormat(names(state))
#                                                                                   , parameterFormat(names(parameters))
#                                                                    )
#                                                      )
#                                               )
#                                        )
#                           )
#                           
#                           , 
#                           , actionButton("resetSummary", "Clear data")
#                           , downloadButton('downloadSummaryData', 'Download Data')
# )

shinyUI( fluidPage( titlePanel(headerText)
                  , fluidRow( column( 3
                                    , wellPanel(controls)
                                    )
                            , column( 9
                                    , tabsetPanel(kineticsTab, simTab, id = 'tabs')
                                    )
                            )
                  )
       )
