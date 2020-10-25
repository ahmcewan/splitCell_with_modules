server <- function(input, output, session) {
  
  callModule(densityServer,"dens")
  callModule(densityDapiServer,"densDapi")
  
  dataset <- reactive({
    validate(
      need(input$files != "", "")
    )
    path_list <- as.list(input$files$datapath)
    tbl_list <- lapply(input$files$datapath, read.table, header=TRUE, sep=",")
    df <- do.call(rbind, tbl_list)
    df <- df[order(df$WellName),]
    col <- colnames(df)
    updateSelectInput(session, inputId ="edu", choices =c("select" , col ))
    updateSelectInput(session, inputId ="dapi", choices =c("select" , col ))
    updateSelectInput(session, inputId ="foci", choices =c("select" , col ))
    updateSelectInput(session, inputId ="dapi_int", choices =c("select" , col ))
    list(df=data.frame(df),
         path_list=path_list)
    
  })
  
  #update selectInput wells
  output$edu <- renderUI({
    selectInput("edu","Edu Mean",choices = as.vector(dataset()$col))
  })
  output$dapi <- renderUI({
    selectInput("dapi","Dapi sum",choices = as.vector(dataset()$col))
  })
  output$foci <- renderUI({
    selectInput("foci","Num spots",choices = as.vector(dataset()$col))
  })
  output$dapi_int <- renderUI({
    selectInput("dapi_int","H2AX",choices = as.vector(dataset()$col))
  })
  output$userChoiceTbl <- DT::renderDataTable({
    datatable(toggleTable,options = list(dom = 't',ordering = F),selection = list(target = 'cell'),class = 'cell-border compact') %>% formatStyle(1:12, cursor = 'pointer')
  })
  
  
  cont_wells <- reactive({
    validate(
      need(input$userChoiceTbl_cells_selected != "", "Please select control cells if applicable")
    )
    wells <- input$userChoiceTbl_cells_selected
    let <- LETTERS[wells[,1]]
    num <- sort(wells[,2])
    coordinates <- paste0(let,num)
    list(coordinates = coordinates)
  })
  
}