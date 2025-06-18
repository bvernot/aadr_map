library(shiny)
library(reactlog)
library(here)

# tell shiny to log all reactivity
reactlog_enable()

# run a shiny app
# app <- system.file("aadr_map_app/app.R", package = "shiny")
runApp('aadr_map_app')

# once app has closed, display reactlog from shiny
shiny::reactlogShow()
