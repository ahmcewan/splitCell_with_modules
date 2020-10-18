library(shiny)


# Functions
localMetric <- function(dat, type.metrix) {
    if(type.metrix == "max"){
        x = -Inf
    } else {
        x = Inf
    }
    local.metrix <- diff(c(x, dat)) > 0L
    rle(local.metrix)$lengths
    local.metrix <- cumsum(rle(local.metrix)$lengths)
    local.metrix <- local.metrix[seq.int(1L, length(local.metrix), 2L)]
    if (dat[[1]] == dat[[2]]) {
        local.metrix <- local.metrix[-1]
    }
    return(local.metrix)
}


# 
densitySlider <- function(dens, label = "Density Plot") {
    ns <- NS(dens)
    tagList(
        sliderInput(ns("densitySlider"), label = "Threshold", 0, 100, 1),
        textOutput(ns("number"))
    )
}

densityServer <- function(input, output, session){
    output$number <- renderText({input$densitySlider
})
    reactive({input$densitySlider + 100})

}


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

# Server
server <- function(input, output, session) {

    num <- callModule(densityServer,"dens")
    
    output$density <- renderPlot({
        end <- num()
        dat <- rnorm(1:end)
        dat.dens <- density(dat)$y
        plot(dat.dens, type="l")
        abline(v=end, col="blue")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
