library(shiny)
library(plyr)
library(markdown)

# Load model into the local environment
source("model.R", local = TRUE)

# Helper functions
toHTML <- function(s) {
  
  #If s is a path to a local file, load it and return our result of its contents
  if (file.exists(s)) {
    toHTML(paste(readLines(s), collapse="\n"))
  } else {
    HTML(markdownToHTML(text = s, fragment.only = TRUE))
  }
  
}

# Build an input UI from the model
modelInputs <- list(
  
    # Sidebar header text
    helpText(toHTML(sidebarHeader))
    
    # state input boxes
  , lapply( names(state)
          , function(name) { 
              numericInput( name
                          , stateFormat(name)
                          , state[name]
                          , step = state[name] / 2
                          )
          })
    
    # parameter input boxes
  , lapply( names(parameters)
          , function(name) { 
              numericInput( name
                          , parameterFormat(name)
                          , parameters[name]
                          , step = parameters[name] / 2
                          )
          })
    
    # Time scale adjustment
  , sliderInput( "time.end" 
               , "Time scale"
               , min = 0
               , max = time["end"] * 10
               , value = time["end"]
               , step = time["end"] * 0.1
               )
    
    # Save to summary button    
  , br()
    
    # Sidebar footer text
  , helpText(toHTML(sidebarFooter))
    
  )

# Define UI layout for the application
shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel(headerText)
  
    # Sidebar with a slider input for state and parameter variables
  , splat(sidebarPanel)(modelInputs)
  
    # Output panel
  , mainPanel(
    
    tabsetPanel(
        
        tabPanel( "Simulation"
                , plotOutput("modelPlot")
                , wellPanel(
                    sliderInput( "ymax"
                               , "Y-axis scale:"
                               , min = 0
                               , max = max(state)
                               , value = 0.1 * max(state)
                               )
                          )
                )
      
      , tabPanel( "Summary"
                  
                  # TODO implement a less ugly horizontal well 
                , wellPanel( class = "well container-fluid"
                           , div( class = "row-fluid"
                                , div( class = "span5"
                                       , selectInput( "summaryY"
                                                      , "Summarize:"
                                                      , choices = names(state.summary)
                                       )
                                )
                                , div( class = "span5"
                                       , selectInput("summaryX"
                                                     , "As a function of:"
                                                     , choices = c( stateFormat(names(state))
                                                                  , parameterFormat(names(parameters))
                                                                  )
                                                     )
                                       )
                                )
                           )
                 
                 , plotOutput('summaryPlot')
                 , actionButton("resetSummary", "Clear")
                 , downloadButton('downloadSummaryData', 'Download')
                 )
        
        , tabPanel( "Model"
                     
                  , helpText(toHTML(modelDescription))
                  )
        
      )
    )
))
