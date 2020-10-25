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

# modules 

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

