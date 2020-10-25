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

#Table used to select control wells
toggleTable <- matrix(" ", nrow = 8, ncol = 12, dimnames = list(c(LETTERS[1:8]), seq.int(1, 12, 1)))

# reshape data
reshape_cols <- function(x, cols=cols){
  x$id <- with(x, ave(rep(1, nrow(x)), WellName, FUN = seq_along))
  x <- dcast(data = x,formula = id~WellName,fun.aggregate = sum,value.var =cols,fill=-1)
  x[x == -1] <-  NA
  x <- x[-1]
  dat <- x[,order(as.character(sub("[0-9]","",names(x))),as.integer(sub("[A-Z]", "", names(x))))]
  return(dat)
}

# reshape data to use for density plot
subset_file <- reactive({
  validate(
    need(input$edu != "", ""),
    need(input$dapi != "", ""),
    need(input$foci != "", ""),
    need(input$dapi_int != "", "")
  )
  if(input$edu %in% c("","select") | input$dapi %in% c("","select") | input$foci %in% c("","select") | input$dapi_int %in% c("","select")){
    return(NULL)
  }else{
    rawData = dataset()$df
    dat.cols <- c("WellName", input$edu, input$dapi, input$foci, input$dapi_int)
    dat.all <- dataset()$df[,colnames(dataset()$df) %in% dat.cols]
    dat.cont <- dat.all
    dat.all.count <- reshape_cols(dat.cont, cols=input$dapi_int)
    if (length(input$userChoiceTbl_cells_selected)!=0){
      dat.cont <- dat.cont[rawData$WellName %in% cont_wells()$coordinates,]
    }
  }
  list(dat.cont=dat.cont, 
       dat.all=dat.all,
       dat.all.count=dat.all.count)
})


dat_g1 <- reactive({
  validate(
    need(input$edu != "", ""),
    need(input$dapi != "", ""),
    need(input$foci != "", ""),
    need(input$dapi_int != "", "")
  )
  if (input$edu %in% c("","select") | input$dapi %in% c("","select") | input$foci %in% c("","select")| input$dapi_int %in% c("","select")){
    return(NULL)
  }else{
    if(nrow(subset_file()$dat.cont)==0){
      edu_values <- subset_file()$dat.all[[input$edu]]
    }else{
      edu_values <- subset_file()$dat.cont[[input$edu]]
    }
    edu_dens <- density(edu_values, na.rm=TRUE)
    dens <- edu_dens$y
    edu.max.x <-localMaxima(dens)[1]
    edu.min.x <-localMinima(dens)
    edu.min.thres <-edu.min.x[min(which(edu.min.x > edu.max.x))]
    edu.min.x <- edu_dens$x[edu.min.thres]
    
    
    list(edu_values=edu_values, 
         dens=dens,
         edu_dens=edu_dens,
         edu.min.x=edu.min.x,
         edu.max.x=edu.max.x,
         edu.min.thres=edu.min.thres)
  }
})


# modules 

#Edu mean intensity
densitySlider <- function(dens, label = "Density Plot") {
  ns <- NS(dens)
  tagList(
    plotOutput(ns("density")),
    sliderInput(ns("densitySlider"), label = "Threshold", min=0, max=150, 1),
    textOutput(ns("number"))
  )
}

densityServer <- function(input, output, session){
  output$number <- renderText({input$densitySlider
  })
  output$density <- renderPlot({
    dat <- rnorm(1:100)
    dat.dens <- density(dat)$y
    plot(dat.dens, type="l")
    abline(v=50, col="blue")
    
  })
  reactive({input$densitySlider + 100})
}

#dapi mean intensity
densityDapiSlider <- function(densDapi, label = "Edu Density Plot") {
  ns <- NS(densDapi)
  tagList(
    plotOutput(ns("densityDapi")),
    sliderInput(ns("densityDapiSlider"), label = "Dapi Threshold 1", min=0, max=150, 1),
    sliderInput(ns("densityDapiSlider"), label = "Dapi Threshold 2", min=0, max=150, 1),
    sliderInput(ns("densityDapiSlider"), label = "Dapi Threshold 3", min=0, max=150, 1),
    textOutput(ns("numberDapi"))
  )
}

densityDapiServer <- function(input, output, session){
  output$numberDapi <- renderText({input$densityDapiSlider
  })
  output$densityDapi <- renderPlot({
    dat <- rnorm(1:100)
    dat.dens <- density(dat)$y
    plot(dat.dens, type="l")
    abline(v=50, col="blue")

  })
  reactive({input$densitySliderDapi + 100})
}






