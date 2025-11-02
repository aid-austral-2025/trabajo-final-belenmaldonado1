producto <- c("soja", "maiz", "trigo", "girasol", "cebada", "sorgo")
frecuencia <- c("diario", "semanal")
total <- c("por producto", "total")

# Shiny ------------------------------------------------------------------------

# interfaz del usuario
interfaz <- fluidPage(
  
  title = "Tablero SIO Granos",
  h2("Tablero SIO Granos"),
  h4("Trabajo final para la materia Análisis Inteligente de Datos"),
  p("Este tablero sirve para monitorear las", strong("operaciones en el mercado local"), "de 
  los principales cultivos agrícolas de Argentina (soja, maíz, trigo, girasol, cebada y sorgo). 
  Los datos se obtienen a través de la plataforma ", a("SIO Granos", href = "https://www.siogranos.com.ar/#1"),
  ", un sistema unificado de información obligatoria de las operaciones de compraventa de granos 
  que conforman el mercado físico."),
  
  sidebarLayout(
    sidebarPanel("Panel de selección", width = 3,
                 
                 numericInput(inputId = "id_year",
                              label = "Año",
                              value = 2015,
                              min = min(datos_total$ANIO_OPERACION),
                              max = max(datos_total$ANIO_OPERACION)
                 ),
                 
                 
                 pickerInput(
                   inputId = "id_producto",
                   label = "Producto",
                   choices = producto,
                   multiple = FALSE,
                   options = list('live-search' = TRUE)
                 ),
                 
                 pickerInput(
                   inputId = "id_frecuencia",
                   label = "Frecuencia",
                   choices = frecuencia,
                   multiple = FALSE,
                   options = list('live-search' = TRUE)
                 ),
                 
                 pickerInput(
                   inputId = "id_total",
                   label = "Total",
                   choices = total,
                   multiple = FALSE,
                   options = list('live-search' = TRUE)
                 )
                 
    ),
    
    mainPanel(width = 9,
              plotlyOutput(outputId = "grafico"),
              DTOutput(outputId = "tabla"))))

# función server
servidor <- function(input, output) {
  
  year_reactivo <- reactive({input$id_year})
  
  producto_reactivo <- reactive({input$id_producto})
  
  output$grafico <- renderPlotly({
    
    if(input$id_total == "por producto") {
      
      if(input$id_frecuencia == "diario") {
        ggplotly(funcion_grafico_diario_producto(producto_reactivo(), year_reactivo()))
        
      } else if(input$id_frecuencia == "semanal") {
        ggplotly(funcion_grafico_semanal_producto(producto_reactivo(), year_reactivo()))
      }
      
    } else if(input$id_total == "total") {
      if(input$id_frecuencia == "diario") {
        ggplotly(funcion_grafico_diario_total(year_reactivo()))
        
      } else if (input$id_frecuencia == "semanal") {
        ggplotly(funcion_grafico_semanal_total(year_reactivo()))
      }
    }
  })
  
  output$tabla <- renderDT({
    if(input$id_total == "por producto") {
      
      if (input$id_frecuencia == "diario") {
        get(paste0("pricing_diario_", tolower(input$id_producto), "_", input$id_year))
        
      } else if (input$id_frecuencia == "semanal") {
        get(paste0("pricing_semanal_", tolower(input$id_producto), "_", input$id_year))
      }
      
    } else if (input$id_total == "total") {
      if (input$id_frecuencia == "diario") {
        get(paste0("pricing_diario_total_", input$id_year))
        
      } else if (input$id_frecuencia == "semanal") {
        get(paste0("pricing_semanal_total_", input$id_year))
      }
    }
  })
}       

# publicación
shinyApp(ui = interfaz, server = servidor)