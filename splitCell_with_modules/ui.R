library(shiny)
source("./modules/getThreshold.R")

# UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Using Modules"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("density"),
    densitySlider("dens")
  )
)