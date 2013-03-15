# This is a total hack that dirties ui.R and server.R so that they are reloaded
# by the framework.
dirtify <- function(files = c("./ui.R", "./server.R")) {
  for (file in files) {
    system( paste("touch", file) )
  }
}

dirtify()