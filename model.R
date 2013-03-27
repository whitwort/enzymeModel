# We'll use the deSolve package for our integration engine
library(deSolve)

# define the solver that we want to use
solver <- ode

# Kinetic parameters
parameters <- c(
    kon   = 1000
  , koff  = 10
  , kcat  = 1
)

# Initial values of state variables
state <- c(
    E   = 0.001
  , S   = 0.01
  , ES  = 0
  , P   = 0
) 

# Time window and step size
time <- c(
    start = 0
  , end   = 10
  , step  = 0.01
)

# deSolve functional interface; t is the model time passed by the library
model <- function(t, state, parameters) {
  
  # We'll bind state and parameter variables to clean up our model code block
  with(as.list(c(state, parameters)), {
    
    # This function returns an ordered list of rate of change calculations: the
    # order should match that of the state vector
    return(list(c(
      
        dE  <- (koff*ES) - (kon*E*S) + (kcat*ES)
      , dS  <- (koff*ES) - (kon*E*S)
      , dES <- (kon*E*S) - (koff*ES) - (kcat*ES)
      , dP  <- (kcat*ES)
      
    )))
    
  })
  
}

# Named vector of functions that are availabe as response variable choices on
# the summary tab.  Functions should take one argument, the results data.frame,
# with a $time variable and named variables for all of the model states.
rate <- function(v, t) {
  
  # TODO fix implementation with framework update
  # For now, we just do a regression on the first 5% of data points
  return(
    tryCatch({
      initial <- 1:(round(length(t) * 0.05))
      fit     <- lm(v ~ t, list(v = v[initial], t = t[initial]))
      coefficients(fit)[[2]] 
      }
    , error = function(e) { NA }                  
    )
  )
  
}

state.summary <- c(
    "Initial d[E]/dt"    = function(r) { rate(r$E, r$time)  }
  , "Initial d[S]/dt"    = function(r) { rate(r$S, r$time)  }
  , "Initial d[ES]/dt"   = function(r) { rate(r$ES, r$time) }
  , "Initial d[P]/dt"    = function(r) { rate(r$P, r$time)  }
  )

# Header element describing this model
headerText    <- "Enzymatic Catalysis Model"

# Descriptive text blocks:  these strings are sent through the R markdown 
# preprocessor before being embeded in the UI.  For a full description of the
# syntax see:  http://www.rstudio.com/ide/docs/r_markdown
#
# If the string contains a path to local file, it's contents are loaded instead. 
# 
sidebarHeader     <- "The simulation will update as you change the input parameters below."
sidebarFooter     <- "Version 0.3.  [Source code](https://github.com/whitwort/enzymeModel) available on github."
modelDescription  <- "MODEL.md"

# Label formatters
stateFormat       <- function(name) { paste("Initial [", name, "] (mM)", sep = "") }
parameterFormat   <- function(name) { paste("Rate of ", name, " (s-1)", sep = "")  }
