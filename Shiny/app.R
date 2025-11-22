# Librerías --------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)
library(scales)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(sf)
library(leaflet)
library(tools)
library(readxl)
library(tidyverse)
library(leaflet)

# Pricing diario y semanal -----------------------------------------------------
ruta <- "Datos limpios/"
archivos <- list.files(ruta, pattern = "*.xlsx$", full.names = TRUE)

for (archivo in archivos) {
  nombre <- file_path_sans_ext(basename(archivo))
  assign(nombre, read_excel(archivo))
}

# Origen de la producción ------------------------------------------------------
## Datos de producción ---------------------------------------------------------
produccion <- read_delim("Mapa/Estimaciones.csv",
                         delim = ";",
                         locale = locale(encoding = "Windows-1252")) %>%
  mutate(Producción = case_when(Producción == "SD" ~ "0",
                                TRUE ~ Producción),
         Producción = as.numeric(Producción))

produccion <- produccion %>% 
  select(Cultivo,
         Campaña,
         Provincia,
         Departamento,
         Producción) %>% 
  rename(CULTIVO = Cultivo,
         COSECHA = Campaña,
         PROVINCIA = Provincia,
         DEPARTAMENTO = Departamento,
         TONELADAS = Producción) %>% 
  mutate(CULTIVO = case_when(CULTIVO == "Cebada total" ~ "cebada",
                             CULTIVO == "Girasol" ~ "girasol",
                             CULTIVO == "Maíz" ~ "maiz",
                             CULTIVO == "Soja total" ~ "soja",
                             CULTIVO == "Sorgo" ~ "sorgo",
                             TRUE ~ "trigo")) %>% 
  mutate(DEPARTAMENTO = str_to_upper(DEPARTAMENTO),
         DEPARTAMENTO = chartr("ÁÉÍÓÚÑ", "AEIOUN", DEPARTAMENTO),
         PROVINCIA = str_to_upper(PROVINCIA),
         PROVINCIA = chartr("ÁÉÍÓÚÑ", "AEIOUN", PROVINCIA)) %>% 
  group_by(CULTIVO,
           COSECHA,
           PROVINCIA,
           DEPARTAMENTO) %>% 
  summarise(TONELADAS = sum(TONELADAS), .groups = "drop")

# Mapa -------------------------------------------------------------------------
mapa <- read_sf(dsn = "Mapa",
                layer = "departamento") %>% 
  rename(DEPARTAMENTO = nam,
         PROVINCIA = fdc) %>% 
  mutate(PROVINCIA = case_when(PROVINCIA == "IDE Cordoba" ~ "CORDOBA",
                               PROVINCIA == "Catastro" ~ "CORDOBA",
                               PROVINCIA == "IDE Salta" ~ "SALTA",
                               PROVINCIA == "IDE Rio Negro" ~ "RIO NEGRO",
                               PROVINCIA == "IDE Catamarca" ~ "CATAMARCA",
                               PROVINCIA == "IDE Tucuman" ~ "TUCUMAN",
                               PROVINCIA == "IDE Chaco" ~ "CHACO",
                               PROVINCIA == "IDE Mendoza" ~ "MENDOZA",
                               PROVINCIA == "ARBA - Gerencia de Servicios Catastrales" ~ "BUENOS AIRES",
                               PROVINCIA == "Direc. Grl. de Inmuebles" & str_starts(in1, "38") ~ "JUJUY",
                               PROVINCIA == "Direc. Grl. de Inmuebles" & str_starts(in1, "66") ~ "SALTA",
                               PROVINCIA == "ATER - Direc. de Catastro" ~ "ENTRE RIOS",
                               PROVINCIA == "SCAR" ~ "TIERRA DEL FUEGO",
                               PROVINCIA == "Ministerio de Ecología" ~ "MISIONES",
                               PROVINCIA == "Gerencia de Catastro Pcial." ~ "TIERRA DEL FUEGO",
                               PROVINCIA == "Direc. Pcial. de Catastro y Cartografía" ~ "CHACO",
                               PROVINCIA == "Direc. de Geodesia y Catastro" ~ "SAN LUIS",
                               PROVINCIA == "Servicio de Catastro e Información Territorial" ~ "SANTA FE",
                               PROVINCIA == "Direc. Pcial. de Catastro e Inf. Territorial" ~ "NEUQUEN",
                               PROVINCIA == "Direc. de Catastro" & gna == "Comuna" ~ "CABA",
                               PROVINCIA == "Direc. de Catastro" & gna == "Departamento" ~ "SAN JUAN",
                               PROVINCIA == "Catastro Provinciales" ~ "SANTIAGO DEL ESTERO",
                               PROVINCIA == "Direc. Grl. de Catastro" ~ "LA PAMPA",
                               PROVINCIA == "IGN" & str_starts(in1, "26") ~ "CHUBUT",
                               PROVINCIA == "IGN" & str_starts(in1, "18") ~ "CORRIENTES",
                               PROVINCIA == "IGN" & str_starts(in1, "34") ~ "FORMOSA",
                               PROVINCIA == "IGN" & str_starts(in1, "46") ~ "LA RIOJA",
                               PROVINCIA == "IGN" & str_starts(in1, "78") ~ "SANTA CRUZ",
                               PROVINCIA == "IGN" & str_starts(in1, "94") ~ "TIERRA DEL FUEGO",
                               TRUE ~ PROVINCIA)) %>% 
  mutate(DEPARTAMENTO = str_to_upper(DEPARTAMENTO),
         DEPARTAMENTO = chartr("ÁÉÍÓÚÑ", "AEIOUN", DEPARTAMENTO))

# Funciones --------------------------------------------------------------------
## Gráficos pricing diario por producto ----------------------------------------
funcion_grafico_diario_producto <- function(p, y) {
  
  datos_seleccionados <- get(paste0("pricing_diario_", p, "_", y)) %>%
    filter(TOTAL > 0,                                         # valores positivos
           wday(FECHA_OPERACION, week_start = 1) <= 5) %>%    # días de semana pq los fines de semana casi no se anotan operaciones
    select(FECHA_OPERACION, 
           CONTRATO_PRECIO_HECHO,
           FIJACIONES) %>%
    pivot_longer(cols = c(CONTRATO_PRECIO_HECHO, FIJACIONES),
                 names_to = "TIPO_CONTRATO",
                 values_to = "TONELADAS") %>%
    mutate(TIPO_CONTRATO = case_when(TIPO_CONTRATO == "CONTRATO_PRECIO_HECHO" ~ "Contratos a precio hecho",
                                     TRUE ~ "Fijaciones")) %>% 
    group_by(FECHA_OPERACION) %>%
    mutate(TOTAL_DIA = sum(TONELADAS),
           TONELADAS_fmt = number(TONELADAS, big.mark = "."),
           TOTAL_DIA_fmt = number(TOTAL_DIA, big.mark = "."),
           texto = paste0("<b>Fecha: </b>", FECHA_OPERACION, "<br>",
                          "<b>", TIPO_CONTRATO, ": </b>", TONELADAS_fmt, "<br>",
                          "<b>Total: </b>", TOTAL_DIA_fmt)) %>%
    ungroup() %>% 
    mutate(TIPO_CONTRATO = factor(TIPO_CONTRATO, 
                                  levels = c("Fijaciones", "Contratos a precio hecho")))
  
  # establezco colores según el producto
  colores <- switch(tolower(p),
                    "soja" = c("Contratos a precio hecho" = "#9BBB59", "Fijaciones" = "#C4D79B"),
                    "maiz"  = c("Contratos a precio hecho" = "#F79646", "Fijaciones" = "#FABF8F"),
                    "trigo"  = c("Contratos a precio hecho" = "#FFC000", "Fijaciones" = "#FFD966"),
                    "girasol"  = c("Contratos a precio hecho" = "#A6A6A6", "Fijaciones" = "#BFBFBF"),
                    "cebada"  = c("Contratos a precio hecho" = "#4BACC6", "Fijaciones" = "#92CDDC"),
                    "sorgo"  = c("Contratos a precio hecho" = "#1F497D", "Fijaciones" = "#8DB4E2"),
                    c("Contratos a precio hecho" = "#0D0D0D", "Fijaciones" = "#808080")
  )
  
  grafico <- datos_seleccionados %>%
    ggplot(aes(x = factor(FECHA_OPERACION), # utilizo la fecha como factor pq sino muestra todas las fechas, y quedan huecos los días de fines de semana
               y = TONELADAS, 
               fill = TIPO_CONTRATO, 
               text = texto)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("<b>Pricing diario de ", p, " en el año ", y),
         x = "Día", 
         y = "Toneladas") +
    scale_y_continuous(labels = label_number(big.mark = ".")) +
    # el siguiente código lo formuló la IA "grok" para poder mostrar las fechas cada 7 días
    scale_x_discrete(breaks = levels(factor(datos_seleccionados$FECHA_OPERACION))[seq(1, nlevels(factor(datos_seleccionados$FECHA_OPERACION)), by = 7)],
                     labels = format(as.Date(levels(factor(datos_seleccionados$FECHA_OPERACION))[seq(1, nlevels(factor(datos_seleccionados$FECHA_OPERACION)), by = 7)]), "%d-%m")) +
    scale_fill_manual(values = colores) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"))
  
  return(ggplotly(grafico, tooltip = "text"))
  
}

## Gráficos pricing diario todos los productos ---------------------------------
funcion_grafico_diario_total <- function(y) {
  
  datos_seleccionados <- get(paste0("pricing_diario_total_", y)) %>%
    filter(wday(FECHA_OPERACION, week_start = 1) <= 5) %>%    # días de semana pq los fines de semana casi no se anotan operaciones
    pivot_longer(cols = c(CEBADA, GIRASOL, MAIZ, SOJA, SORGO, TRIGO),
                 names_to = "PRODUCTO",
                 values_to = "TONELADAS") %>%
    group_by(FECHA_OPERACION) %>%
    mutate(TOTAL_DIA = sum(TONELADAS),
           TONELADAS_fmt = number(TONELADAS, big.mark = "."),
           TOTAL_DIA_fmt = number(TOTAL_DIA, big.mark = "."),
           texto = paste0("<b>Fecha: </b>", FECHA_OPERACION, "<br>",
                          "<b>", PRODUCTO, ": </b>", TONELADAS_fmt, "<br>",
                          "<b>Total: </b>", TOTAL_DIA_fmt)) %>%
    ungroup() %>% 
    mutate(PRODUCTO = factor(PRODUCTO, 
                             levels = c("SORGO", "CEBADA", "GIRASOL", "TRIGO", "MAIZ", "SOJA")))
  
  grafico <- datos_seleccionados %>%
    ggplot(aes(x = factor(FECHA_OPERACION), # utilizo la fecha como factor pq sino muestra todas las fechas, y quedan huecos los días de fines de semana
               y = TONELADAS, 
               fill = PRODUCTO, 
               text = texto)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("<b>Pricing diario total en el año ", y),
         x = "Día", 
         y = "Toneladas") +
    scale_y_continuous(labels = label_number(big.mark = ".")) +
    # el siguiente código lo formuló la IA "grok" para poder mostrar las fechas cada 7 días
    scale_x_discrete(breaks = levels(factor(datos_seleccionados$FECHA_OPERACION))[seq(1, nlevels(factor(datos_seleccionados$FECHA_OPERACION)), by = 7)],
                     labels = format(as.Date(levels(factor(datos_seleccionados$FECHA_OPERACION))[seq(1, nlevels(factor(datos_seleccionados$FECHA_OPERACION)), by = 7)]), "%d-%m")) +
    scale_fill_manual(values = c("SOJA" = "#9BBB59", 
                                 "MAIZ" = "#F79646",
                                 "TRIGO" = "#FFC000",
                                 "GIRASOL" = "#A6A6A6",
                                 "CEBADA" = "#4BACC6",
                                 "SORGO" = "#1F497D")) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"))
  
  return(ggplotly(grafico, tooltip = "text"))
  
}

## Gráficos pricing semanal por producto ---------------------------------------
funcion_grafico_semanal_producto <- function(p, y) {
  
  datos_seleccionados <- get(paste0("pricing_semanal_", p, "_", y)) %>%
    select(SEMANA_INICIO, 
           CONTRATO_PRECIO_HECHO,
           FIJACIONES) %>%
    pivot_longer(cols = c(CONTRATO_PRECIO_HECHO, FIJACIONES),
                 names_to = "TIPO_CONTRATO",
                 values_to = "TONELADAS") %>%
    mutate(TIPO_CONTRATO = case_when(TIPO_CONTRATO == "CONTRATO_PRECIO_HECHO" ~ "Contratos a precio hecho",
                                     TRUE ~ "Fijaciones")) %>% 
    group_by(SEMANA_INICIO) %>%
    mutate(TOTAL_SEMANA = sum(TONELADAS),
           TONELADAS_fmt = number(TONELADAS, big.mark = "."),
           TOTAL_SEMANA_fmt = number(TOTAL_SEMANA, big.mark = "."),
           texto = paste0("<b>Fecha: </b>", SEMANA_INICIO, "<br>",
                          "<b>", TIPO_CONTRATO, ": </b>", TONELADAS_fmt, "<br>",
                          "<b>Total: </b>", TOTAL_SEMANA_fmt)) %>%
    ungroup() %>% 
    mutate(TIPO_CONTRATO = factor(TIPO_CONTRATO, 
                                  levels = c("Fijaciones", "Contratos a precio hecho")))
  
  # establezco colores según el producto
  colores <- switch(tolower(p),
                    "soja" = c("Contratos a precio hecho" = "#9BBB59", "Fijaciones" = "#C4D79B"),
                    "maiz"  = c("Contratos a precio hecho" = "#F79646", "Fijaciones" = "#FABF8F"),
                    "trigo"  = c("Contratos a precio hecho" = "#FFC000", "Fijaciones" = "#FFD966"),
                    "girasol"  = c("Contratos a precio hecho" = "#A6A6A6", "Fijaciones" = "#BFBFBF"),
                    "cebada"  = c("Contratos a precio hecho" = "#4BACC6", "Fijaciones" = "#92CDDC"),
                    "sorgo"  = c("Contratos a precio hecho" = "#1F497D", "Fijaciones" = "#8DB4E2"),
                    c("Contratos a precio hecho" = "#0D0D0D", "Fijaciones" = "#808080")
  )
  
  grafico <- datos_seleccionados %>%
    ggplot(aes(x = factor(SEMANA_INICIO),
               y = TONELADAS, 
               fill = TIPO_CONTRATO, 
               text = texto)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("<b>Pricing semanal de ", p, " en el año ", y),
         x = "Primer día de la semana", 
         y = "Toneladas") +
    scale_y_continuous(labels = label_number(big.mark = ".")) +
    scale_x_discrete(breaks = levels(factor(datos_seleccionados$SEMANA_INICIO))[seq(1, nlevels(factor(datos_seleccionados$SEMANA_INICIO)), by = 1)],
                     labels = format(as.Date(levels(factor(datos_seleccionados$SEMANA_INICIO))[seq(1, nlevels(factor(datos_seleccionados$SEMANA_INICIO)), by = 1)]), "%d-%m")) +
    scale_fill_manual(values = colores) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"))
  
  return(ggplotly(grafico, tooltip = "text"))
  
}

## Gráficos pricing semanal todos los productos --------------------------------
funcion_grafico_semanal_total <- function(y) {
  
  datos_seleccionados <- get(paste0("pricing_semanal_total_", y)) %>%
    pivot_longer(cols = c(CEBADA, GIRASOL, MAIZ, SOJA, SORGO, TRIGO),
                 names_to = "PRODUCTO",
                 values_to = "TONELADAS") %>%
    group_by(SEMANA_INICIO) %>%
    mutate(TOTAL_SEMANA = sum(TONELADAS),
           TONELADAS_fmt = number(TONELADAS, big.mark = "."),
           TOTAL_SEMANA_fmt = number(TOTAL_SEMANA, big.mark = "."),
           texto = paste0("<b>Fecha: </b>", SEMANA_INICIO, "<br>",
                          "<b>", PRODUCTO, ": </b>", TONELADAS_fmt, "<br>",
                          "<b>Total: </b>", TOTAL_SEMANA_fmt)) %>%
    ungroup() %>% 
    mutate(PRODUCTO = factor(PRODUCTO, 
                             levels = c("SORGO", "CEBADA", "GIRASOL", "TRIGO", "MAIZ", "SOJA")))
  
  grafico <- datos_seleccionados %>%
    ggplot(aes(x = factor(SEMANA_INICIO),
               y = TONELADAS, 
               fill = PRODUCTO, 
               text = texto)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("<b>Pricing semanal total en el año ", y),
         x = "Primer día de la semana", 
         y = "Toneladas") +
    scale_y_continuous(labels = label_number(big.mark = ".")) +
    scale_x_discrete(breaks = levels(factor(datos_seleccionados$SEMANA_INICIO))[seq(1, nlevels(factor(datos_seleccionados$SEMANA_INICIO)), by = 1)],
                     labels = format(as.Date(levels(factor(datos_seleccionados$SEMANA_INICIO))[seq(1, nlevels(factor(datos_seleccionados$SEMANA_INICIO)), by = 1)]), "%d-%m")) +
    scale_fill_manual(values = c("SOJA" = "#9BBB59", 
                                 "MAIZ" = "#F79646",
                                 "TRIGO" = "#FFC000",
                                 "GIRASOL" = "#A6A6A6",
                                 "CEBADA" = "#4BACC6",
                                 "SORGO" = "#1F497D")) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
          axis.text.y = element_text(color = "black"))
  
  return(ggplotly(grafico, tooltip = "text"))
  
}

## Mapa ------------------------------------------------------------------------
funcion_grafico_mapa <- function(x, y, z) {
  colores <- switch(x,
                    "soja" = c("#C4D79B", "#375623"),
                    "maiz" = c("#FABF8F", "#833C0C"),
                    "trigo" = c("#FFD966", "#BF8F00"),
                    "girasol" = c("#D9D9D9", "#595959"),
                    "cebada" = c("#92CDDC", "#215967"),
                    "sorgo" = c("#8DB4E2", "#203764"),
                    c("#808080", "#0D0D0D"))
  
  datos_seleccionados <- produccion %>%
    filter(CULTIVO == x, COSECHA == y, PROVINCIA == z)
  
  mapa_cultivo <- mapa %>%
    left_join(datos_seleccionados, by = c("DEPARTAMENTO", "PROVINCIA")) %>%
    filter(PROVINCIA == z) %>%
    mutate(TONELADAS = ifelse(TONELADAS == 0, NA, TONELADAS))
  
  # Si no hay datos, forzamos todo a NA para que la paleta funcione y quede blanco
  hay_datos <- any(!is.na(mapa_cultivo$TONELADAS))
  
  paleta_colores <- colorNumeric(
    palette = c(colores[1], colores[2]),
    domain = if(hay_datos) mapa_cultivo$TONELADAS else c(0, 1),
    na.color = "white")
  
  grafico <- leaflet(mapa_cultivo) %>%
    addPolygons(fillColor = ~paleta_colores(TONELADAS),
                weight = 1,
                opacity = 1, 
                color = "black",
                fillOpacity = 0.8,
                label = ~paste0(DEPARTAMENTO, ", Toneladas: ", 
                                ifelse(is.na(TONELADAS), "0", 
                                       format(TONELADAS, big.mark = ".", decimal.mark = ",")))) %>%
    addLegend(pal = paleta_colores, values = ~TONELADAS, 
              title = "Toneladas", position = "bottomright")
  
  return(grafico)
  
}

# Datos para widgets -----------------------------------------------------------
producto <- c("soja", "maiz", "trigo", "girasol", "cebada", "sorgo")
frecuencia <- c("diario", "semanal")
total <- c("por producto", "total")
campania <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20",
              "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
provincia <- c("BUENOS AIRES", "CORDOBA", "ENTRE RIOS", "LA PAMPA", "SANTA FE",
               "SANTIAGO DEL ESTERO", "TUCUMAN", "SAN LUIS", "SALTA", "CHACO", 
               "FORMOSA", "CATAMARCA", "CORRIENTES", "JUJUY", "MISIONES")

# Shiny ------------------------------------------------------------------------
# interfaz del usuario
interfaz <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  title = "Monitor de cultivos",
  h2("Monitor de cultivos"),
  h4("Trabajo final para la materia Análisis Inteligente de Datos"),
  p("Este tablero sirve para monitorear las", strong("operaciones en el mercado local"), "de 
  los principales cultivos agrícolas de Argentina (soja, maíz, trigo, girasol, cebada y sorgo). 
  Los datos se obtienen a través de la plataforma ", a("SIO Granos", href = "https://www.siogranos.com.ar/#1"),
  ", un sistema unificado de información obligatoria de las operaciones de compraventa de granos 
  que conforman el mercado físico."),
  p("También se puede analizar el", strong("origen de la producción por campaña"), ". En este caso, los datos se obtienen
    a través de la página de la ", a("Secretaría de Agricultura, Ganadería y Pesca (SAGyP)", href = "https://datosestimaciones.magyp.gob.ar/reportes.php?reporte=Estimaciones"),
    "."),
  
  tabsetPanel(
    tabPanel("Pricing de granos",
             
             sidebarLayout(
               sidebarPanel("Panel de selección", width = 3,
                            
                                             numericInput(inputId = "id_year",
                                                          label = "Año",
                                                          value = 2015,
                                                          min = 2015,
                                                          max = 2025
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
                              ),
                            
                            pickerInput(
                              inputId = "id_producto",
                              label = "Producto",
                              choices = producto,
                              multiple = FALSE,
                              options = list('live-search' = TRUE))
                            ),
               
               mainPanel(width = 9,
                         plotlyOutput(outputId = "grafico"),
                         DTOutput(outputId = "tabla")))),
    
    tabPanel("Origen de la producción",
             
             sidebarLayout(
               sidebarPanel("Panel de selección", width = 3,
                            
                            pickerInput(
                              inputId = "id_producto_origen",
                              label = "Producto",
                              choices = producto,
                              multiple = FALSE,
                              options = list('live-search' = TRUE)),
                            
                            pickerInput(
                              inputId = "id_campaña_origen",
                              label = "Campaña",
                              choices = campania,
                              multiple = FALSE,
                              options = list('live-search' = TRUE)),
                            
                            pickerInput(
                              inputId = "id_provincia_origen",
                              label = "Provincia",
                              choices = provincia,
                              multiple = FALSE,
                              options = list('live-search' = TRUE))
                            
                            ),
               
               mainPanel(width = 9,
                         leafletOutput("grafico_origen"))
               )
             )
    )
)

# función server
servidor <- function(input, output) {
  
  year_reactivo <- reactive({input$id_year})
  
  producto_reactivo <- reactive({input$id_producto})
  
  producto_origen_reactivo <- reactive({input$id_producto_origen})
  
  campaña_origen_reactivo <- reactive({input$id_campaña_origen})
  
  provincia_origen_reactivo <- reactive({input$id_provincia_origen})
  
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
  
  output$grafico_origen <- renderLeaflet({
    
    req(input$id_producto_origen, input$id_campaña_origen, input$id_provincia_origen)
    
    funcion_grafico_mapa(input$id_producto_origen, input$id_campaña_origen,
                         input$id_provincia_origen)
    
    
  })
  
}       

# publicación
shinyApp(ui = interfaz, server = servidor)