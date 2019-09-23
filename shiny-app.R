library(shiny)

values <- reactiveValues(dataset=NULL,toShowColumns=NULL,matrixToPrint=NULL)

ui <- fluidPage(
  navbarPage(title = "Tarea académica 1"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput("principalFile", "Escoge el archivo CSV:",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput("fileEncoding", "File encoding:",
                  c("Latin 1" = "latin1",
                    "Por defecto" = "default")),
      actionButton("loadDataset","Cargar datos"),
      tags$hr(),
      actionButton("showTable","Mostrar tabla"),
      checkboxGroupInput("fieldsCBDataset", "Columnas a mostrar:",c()),
      tags$hr(),
      helpText("Nota: Si no cargas un dataset, no ocurrirá nada al apretar el botón."),
      tags$hr(),
      tags$h3("Normalización"),
      selectInput("fieldsDataset", "Campos",
                  c("No hay opciones")),
      helpText("Nota: Solo se pueden normalizar los campos Q6"),
      actionButton("normalizeData","Normalizar"),
      downloadLink("downloadData", "Descargar archivo .dat"),
      tags$hr(),
      tags$h3("Categórico a numérico"),
      helpText("Nota: Esto convertirá todos las columnas Q6"),
      actionButton("tranformData","Transformar"),
      tags$hr(),
      tags$h3("Matriz de distancia"),
      helpText("Nota: Distancia Euclideana"),
      actionButton("matrixDistanceBtn","Generar Matriz de distancia"),
      downloadLink("downloadDataMatrix", "Descargar archivo .dmta")
    ),
    
    #Main
    mainPanel(
      tableOutput("contents"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  #React to button 1
  observeEvent(input$loadDataset, {
    
    inFile = input$principalFile
    #if NULL do nothing
    if(is.null(inFile)){
      return(NULL)
    }
    #Read csv
    
    if(input$fileEncoding =="latin1"){
      values$dataset = read.csv(inFile$datapath,fileEncoding = input$fileEncoding)
      updateSelectInput(session, "fieldsDataset", choices = names(values$dataset))
      updateCheckboxGroupInput(session, "fieldsCBDataset", choices = names(values$dataset))
    } else {
      values$dataset = read.csv(inFile$datapath)
      updateSelectInput(session, "fieldsDataset", choices = names(values$dataset))
      updateCheckboxGroupInput(session, "fieldsCBDataset", choices = names(values$dataset))
    }
  })
  
  #React to showTable
  observeEvent(input$fieldsCBDataset, {
    values$toShowColumns = input$fieldsCBDataset
  })
  
  observeEvent(input$showTable,{
    output$contents <- renderTable({
      if (is.null(values$dataset))
        return(NULL)
      
      columns = values$toShowColumns
      
      values$matrixToPrint = values$dataset[columns]
      #data.frame(dataset$Internal.ID,dataset$Q1..GOING.OUT.)
      #values$dataset[[columns]]
    })  
  })
  
  #React to normalize
  observeEvent(input$normalizeData, {
    
    normalize = function(x) {
      return (((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))))
    }
    
    toNormalize = input$fieldsDataset
    
    ## Clean variable
    # Not numbers to => NA
    values$dataset[[toNormalize]] = as.numeric(as.character(values$dataset[[toNormalize]]))
    #Delete values > 100 age
    if(toNormalize == "Q3..AGE"){
      values$dataset[[toNormalize]] = ifelse(values$dataset[[toNormalize]] > 100, mean(values$dataset[[toNormalize]], na.rm = T), values$dataset[[toNormalize]])  
    }
    # if NA => mean
    if(toNormalize == "Q3..AGE"){
      values$dataset[[toNormalize]] = ifelse(is.na(values$dataset[[toNormalize]]), mean(values$dataset[[toNormalize]], na.rm = T), values$dataset[[toNormalize]])
    }
    # Round age 0 decimals
    if(toNormalize == "Q3..AGE"){
      values$dataset[[toNormalize]] = as.numeric(format(round(values$dataset[[toNormalize]], 0)))
    }
    values$dataset[[toNormalize]] =  normalize(values$dataset[[toNormalize]])
    result = values$dataset[[toNormalize]]
    nameExtention = paste(toNormalize,".dmat",sep="")
    #data1 = saveRDS(result, file = nameExtention)
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(nameExtention,"-", Sys.Date(), ".dat", sep="")
      },
      content = function(file) {
        write(result,file)
      }
    )
  })
  
  #React to transform data
  observeEvent(input$tranformData,{
    
    f = function(x){
      as.numeric(factor(x, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3)))
    }
    toNormalize = input$fieldsDataset
    values$dataset[["Q6...100.Grand.Bar"]] = f(values$dataset[["Q6...100.Grand.Bar"]])
    
    #dataset$Q6...100.Grand.Bar=factor(dataset$Q6...100.Grand.Bar, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3))
    
  })
  
  observeEvent(input$matrixDistanceBtn, {
    print("entre")
    tableDistance = values$matrixToPrint
    
    tableDistance = na.omit(tableDistance)
    
    # Distancia euclideana # ALGORITMO
    matrixTable = as.matrix(dist(tableDistance,method = "euclidean"))
    
    View(matrixTable)
    
    output$downloadDataMatrix <- downloadHandler(
      filename = function() {
        paste("matrix-table","-", Sys.Date(), ".dmat", sep="")
      },
      content = function(file) {
        write(matrixTable,file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)

