# Lectura de datos año 2022 ----------------------------------------------------

archivos_2022 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2022.*\\.csv$",
  full.names = TRUE)

datos_2022 <- archivos_2022 %>%
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

datos_2022 <- datos_2022 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

# Transformaciones para las columnas que necesito en formato fecha -------------

datos_2022$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2022$`FECHA OPERACION`))
datos_2022$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2022$`FECHA CONCERTACION`))
datos_2022$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. DESDE`))
datos_2022$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. HASTA`))

# Renombrar columnas -----------------------------------------------------------

datos_2022 <- datos_2022 %>% 
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

datos_2022 <- datos_2022 %>% 
  mutate(ANIO_OPERACION = year(FECHA_OPERACION),
         MES_OPERACION = month(FECHA_OPERACION),
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
         DIA_OPERACION,
         FECHA_CONCERTACION,
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
