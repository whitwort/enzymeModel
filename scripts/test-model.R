# Little development helper function to run the model in an interactive session
# (not used by the server).  To test your model, do `r <- runModel()`.
source("./model.R")

runModel <- function() {
  result <- solver(  
          y     = state
        , times = seq(time["start"], time["end"], by = time["step"])
        , func  = model
        , parms = parameters
      )
  
  print(head(result))
  print(tail(result))
  print(summary(result))
  plot(result)
  
  return(result)
}

r <- runModel()
