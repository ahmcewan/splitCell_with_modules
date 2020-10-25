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