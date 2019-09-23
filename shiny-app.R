library(shiny)
options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.

values <- reactiveValues(dataset=NULL,toShowColumns=NULL,matrixToPrint=NULL)

ui <- fluidPage(
  #headerPanel(title = "Tarea académica 1"),
  
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
      checkboxGroupInput(inputId = "fieldsCBDataset", "Columnas a mostrar:",c()),
      tags$hr(),
      helpText("Nota: Si no cargas un dataset, no ocurrirá nada al apretar el botón."),
      tags$hr(),
      tags$h3("Normalización"),
      selectInput("normalizationMethods", "Metodos",
                  c("No hay opciones")),
      selectInput("fieldsDataset", "Campos",
                  c("No hay opciones")),
      helpText("Nota: Solo se pueden normalizar los campos Q6"),
      actionButton("normalizeData","Normalizar"),
      downloadLink("downloadData", "Descargar archivo .dat"),
      tags$hr(),
      tags$h3("Categórico a numérico"),
      selectInput("fieldsDatasetNorm", "Campos",
                  c("No hay opciones")),
      actionButton("tranformData","Transformar"),
      tags$hr(),
      tags$h3("Matriz de distancia"),
      selectInput("distanceMethods", "Metodos",
                  c("No hay opciones")),
      helpText("Por defecto: Distancia Euclideana"),
      actionButton("matrixDistanceBtn","Generar Matriz de distancia"),
      downloadLink("downloadDataMatrix", "Descargar archivo .dmat"),
      style = "overflow-y:scroll; max-height: 100vh"
    ),
    
    #Main
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", tableOutput("contents")),
        tabPanel("Gráfico", 
                 plotOutput(outputId = "plot", brush = "plot_brush"),
                 fluidRow(tableOutput("data_brush"))
                 ),
        tabPanel("Tabla resultado", plotOutput("resultTable"))
      ),
      height = "100%",
      style = "overflow-y:scroll; max-height: 100vh;"
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
      updateSelectInput(session, "fieldsDatasetNorm", choices = names(values$dataset))
      updateCheckboxGroupInput(session, "fieldsCBDataset", choices = names(values$dataset))
      updateSelectInput(session, "normalizationMethods",
                        choices = c("Normalizacion lineal" = "linealNorm"
                                    ,"Normalizacion por el valor maximo de los elementos" = "maxEleNorm"))
      updateSelectInput(session, "distanceMethods",
                        choices = c("Distancia Euclideana" = "euclidean"
                                    ,"Distancia Minkowski" = "minkowski"
                                    ,"Distancia Manhattan" = "manhattan"
                                    ,"Distancia Mahalanobis" = "mahalanobis"))
    } else {
      values$dataset = read.csv(inFile$datapath)
      updateSelectInput(session, "fieldsDataset", choices = names(values$dataset))
      updateSelectInput(session, "fieldsDatasetNorm", choices = names(values$dataset))
      updateCheckboxGroupInput(session, "fieldsCBDataset", choices = names(values$dataset))
      updateSelectInput(session, "normalizationMethods",
                        choices = c("Normalizacion lineal" = "linealNorm"
                                    ,"Normalizacion por el valor maximo de los elementos" = "maxEleNorm"))
      updateSelectInput(session, "distanceMethods",
                        choices = c("Distancia Euclideana" = "euclidean"
                                    ,"Distancia Minkowski" = "minkowski"
                                    ,"Distancia Manhattan" = "manhattan"
                                    ,"Distancia Mahalanobis" = "mahalanobis"))
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
    
    #linealNorm
    normalize = function(x) {
      return (((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))))
    }
    #maxEleNorm
    normalize2 = function(x) {
      return(x / max(x, na.rm = T))
    }
    
    toNormalize = input$fieldsDataset
    method = input$normalizationMethods
    
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
    
    ##choose method
    if(method == "maxEleNorm"){
      values$dataset[[toNormalize]] =  normalize2(values$dataset[[toNormalize]])  
    } else {
      values$dataset[[toNormalize]] =  normalize(values$dataset[[toNormalize]])
    }
    
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
    
    fTransform = function(x){
      as.numeric(factor(x, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3)))
    }
    toTransform = input$fieldsDatasetNorm
    print("TO")
    print(toTransform)
    values$dataset[[toTransform]] = fTransform(values$dataset[[toTransform]])
    
    #dataset$Q6...100.Grand.Bar=factor(dataset$Q6...100.Grand.Bar, levels = c('JOY','MEH','DESPAIR'),labels = c(1,2,3))
    
  })
  
  observeEvent(input$matrixDistanceBtn, {
    tableDistance = values$matrixToPrint
    
    tableDistance = na.omit(tableDistance)
    
    View(tableDistance)
    # Distancia euclideana # ALGORITMO
    
    methodDistance = input$distanceMethods
    if(methodDistance == "minkowski"){
      matrixTable = as.matrix(dist(tableDistance,method = "minkowski"))
    }else if(methodDistance == "manhattan"){
      matrixTable = as.matrix(dist(tableDistance,method = "manhattan"))
    }else if(methodDistance == "mahalanobis"){
      #Reimplementar esto
      matrixTable = as.matrix(dist(tableDistance,method = "manhattan"))
    }else{
      matrixTable = as.matrix(dist(tableDistance,method = "euclidean"))
    }
    
    View(matrixTable)
    
    #output$resultTable
    
    output$plot = renderPlot({
      #plot(matrixTable)
      data(matrixTable, package = "ggplot2")
      # Scatterplot
      gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
        geom_point(aes(col=state, size=popdensity)) +
        geom_smooth(method="loess", se=F) +
        xlim(c(0, 0.1)) +
        ylim(c(0, 500000)) +
        labs(subtitle="Area Vs Population",
             y="Population",
             x="Area",
             title="Scatterplot",
             caption = "Source: midwest")
      plot(gg)
    })
    
    output$data_brush = renderTable({
      n = nrow(brushedPoints(dataset, brush = input$plot_brush))
      if(n == 0){
        return()
      }
      else
        brushedPoints(dataset,brush = input$plot_brush)
    })
    
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

