# Lectura de datos año 2024 ----------------------------------------------------

archivos_2024 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2024.*\\.csv$",
  full.names = TRUE)

datos_2024 <- archivos_2024 %>%
  map_dfr(~ read_delim(file = .x,
                       delim = ";",
                       locale = locale(encoding = "UTF-16LE",
                                       decimal_mark = ",",
                                       grouping_mark = "."),
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

datos_2024 <- datos_2024 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

# Transformaciones para las columnas que necesito en formato fecha -------------

datos_2024$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2024$`FECHA OPERACION`))
datos_2024$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2024$`FECHA CONCERTACION`))
datos_2024$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2024$`FECHA ENTR. DESDE`))
datos_2024$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2024$`FECHA ENTR. HASTA`))

# Renombrar columnas -----------------------------------------------------------

datos_2024 <- datos_2024 %>% 
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

# Agregar columnas que necesito después ----------------------------------------

datos_2024 <- datos_2024 %>% 
  mutate(ANIO_OPERACION = year(FECHA_OPERACION),
         MES_OPERACION = month(FECHA_OPERACION),
         SEMANA_OPERACION = week(floor_date(FECHA_OPERACION, unit = "week", week_start = 1)), # separa las semanas tomando los lunes como primer día
         DIA_OPERACION = day(FECHA_OPERACION),
         ANIO_CONCERTACION = year(FECHA_CONCERTACION),
         MES_CONCERTACION = month(FECHA_CONCERTACION),
         DIA_CONCERTACION = day(FECHA_CONCERTACION),
         ANIO_ENTREGA_DESDE = year(ENTREGA_DESDE),
         MES_ENTREGA_DESDE = month(ENTREGA_DESDE),
         DIA_ENTREGA_DESDE = day(ENTREGA_DESDE),
         ANIO_ENTREGA_HASTA = year(ENTREGA_HASTA),
         MES_ENTREGA_HASTA = month(ENTREGA_HASTA),
         DIA_ENTREGA_HASTA = day(ENTREGA_HASTA)) %>% 
  select(FECHA_OPERACION,
         ANIO_OPERACION,
         MES_OPERACION,
         SEMANA_OPERACION,
         DIA_OPERACION,
         FECHA_CONCERTACION,
         ANIO_CONCERTACION,
         MES_CONCERTACION,
         DIA_CONCERTACION,
         OPERACION,
         TIPO,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         CALIDAD,
         PROCEDENCIA_PROVINCIA,
         PROCEDENCIA_LOCALIDAD,
         MONEDA,
         PRECIO_TN,
         ENTREGA,
         ENTREGA_DESDE,
         ANIO_ENTREGA_DESDE,
         MES_ENTREGA_DESDE,
         DIA_ENTREGA_DESDE,
         ENTREGA_HASTA,
         ANIO_ENTREGA_HASTA,
         MES_ENTREGA_HASTA,
         DIA_ENTREGA_HASTA,
         everything())