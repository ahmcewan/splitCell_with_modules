library(shiny)
library(dplyr)
library(DT)
library(reshape2)
library(openxlsx)
source("./modules/getThreshold.R")


# UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Using Modules"),
  
  # Show a plot of the generated distribution
  mainPanel(
    densitySlider("dens"),
    densityDapiSlider("densDapi")
    # fileInput("files",label = h4("Upload Data"), multiple = TRUE, accept = c(".csv")),
    # fluidRow(column(3, uiOutput("edu")),
    #          column(3, uiOutput("dapi")),
    #          column(3, uiOutput("foci")),
    #          column(3, uiOutput("dapi_int"))),
    # DT::dataTableOutput("userChoiceTbl", width = "50%"),
    # tags$b("Cells Selected:"),
    # verbatimTextOutput("selectedInfo"), 
    # fluidRow(column(6,h4("Density Plot of Mean Edu Intensity"),plotOutput("edu_plot")),
    #          column(6,h4("Density Plot of Sum Dapi Intensity"),plotOutput("dapi_plot"))),
    # fluidRow(column(6,uiOutput("slider_edu")),
    #          column(2,uiOutput("slider_sub")),
    #          column(2,uiOutput("slider_dapi")),
    #          column(2,uiOutput("slider_cut"))),
    # fluidRow(column(12,h4("Proportion of Cells",tableOutput("population_stats")))),
    # downloadButton("downloadData", "Download")
  )
)