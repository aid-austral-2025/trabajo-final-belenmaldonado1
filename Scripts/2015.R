# Lectura de datos año 2015 ----------------------------------------------------

archivos_2015 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2015.*\\.csv$",
  full.names = TRUE)

datos_2015 <- archivos_2015 %>%
  map_dfr(~ read_delim(file = .x,
                       delim = ";",
                       locale = locale(encoding = "UTF-16LE"),
                       col_types = cols(`FECHA OPERACION` = col_character(),     # leo las fechas como caracter pq no las reconoce como fecha
                                        `FECHA CONCERTACION` = col_character(),
                                        OPERACION = col_character(),
                                        TIPO = col_character(),
                                        PRECIO = col_character(),
                                        PRODUCTO = col_character(),
                                        `CANT. (TN)` = col_number(),
                                        `CALIDAD ADICIONAL` = col_character(),
                                        `PROCEDENCIA PCIA` = col_character(),
                                        `PROCEDENCIA LOCALID.` = col_character(),
                                        `PRECIO/TN MONEDA` = col_character(),
                                        `PRECIO/TN MONTO` = col_number(),
                                        `LUGAR ENTREGA` = col_character(),
                                        `FECHA ENTR. DESDE` = col_character(),
                                        `FECHA ENTR. HASTA` = col_character(),
                                        `CONDICION PAGO` = col_character(),
                                        `ES FINAL` = col_character(),
                                        `COSECHA` = col_character())))

datos_2015 <- datos_2015 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

# Transformaciones para las columnas que necesito en formato fecha -------------

datos_2015$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2015$`FECHA OPERACION`))
datos_2015$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2015$`FECHA CONCERTACION`))
datos_2015$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. DESDE`))
datos_2015$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. HASTA`))

# Renombrar columnas -----------------------------------------------------------

datos_2015 <- datos_2015 %>% 
  rename(FECHA_OPERACION = `FECHA OPERACION`,
         FECHA_CONCERTACION = `FECHA CONCERTACION`,
         TONELADAS = `CANT. (TN)`,
         PROCEDENCIA_PROVINCIA = `PROCEDENCIA PCIA`,
         PROCEDENCIA_LOCALIDAD = `PROCEDENCIA LOCALID.`,
         MONEDA = `PRECIO/TN MONEDA`,
         PRECIO_TN = `PRECIO/TN MONTO`,
         ENTREGA = `LUGAR ENTREGA`,
         ENTREGA_DESDE = `FECHA ENTR. DESDE`,
         ENTREGA_HASTA = `FECHA ENTR. HASTA`,
         CONDICION_PAGO = `CONDICION PAGO`,
         ES_FINAL = `ES FINAL`)
                            
                            
                            
                            
                            
