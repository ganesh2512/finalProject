library(shiny)
library(reactlog)

# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)

# run a shiny app
app <- system.file("app.R", package = "finalProjectShiny")
runApp(app)

# once app has closed, display reactlog from shiny
shiny::showReactLog()
