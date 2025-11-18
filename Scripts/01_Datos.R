# 2015 -------------------------------------------------------------------------
archivos_2015 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2015.*\\.csv$",
  full.names = TRUE)

datos_2015 <- archivos_2015 %>%
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

datos_2015 <- datos_2015 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2015$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2015$`FECHA OPERACION`))
datos_2015$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2015$`FECHA CONCERTACION`))
datos_2015$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. DESDE`))
datos_2015$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
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

## Agregar columnas que necesito después ---------------------------------------
datos_2015 <- datos_2015 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2015 <- datos_2015 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2015-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# 2016 -------------------------------------------------------------------------
archivos_2016 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2016.*\\.csv$",
  full.names = TRUE)

datos_2016 <- archivos_2016 %>%
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

datos_2016 <- datos_2016 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2016$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2016$`FECHA OPERACION`))
datos_2016$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2016$`FECHA CONCERTACION`))
datos_2016$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2016$`FECHA ENTR. DESDE`))
datos_2016$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2016$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2016 <- datos_2016 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2016 <- datos_2016 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2016 <- datos_2016 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2016-01-03") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# 2017 -------------------------------------------------------------------------
archivos_2017 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2017.*\\.csv$",
  full.names = TRUE)

datos_2017 <- archivos_2017 %>%
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

datos_2017 <- datos_2017 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2017$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2017$`FECHA OPERACION`))
datos_2017$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2017$`FECHA CONCERTACION`))
datos_2017$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2017$`FECHA ENTR. DESDE`))
datos_2017$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2017$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2017 <- datos_2017 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2017 <- datos_2017 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2017 <- datos_2017 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2017-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

## Filtro de datos con fecha pq hay datos mal cargados -------------------------
datos_2017 <- datos_2017 %>%
  filter(!is.na(FECHA_OPERACION))

# 2018 -------------------------------------------------------------------------
archivos_2018 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2018.*\\.csv$",
  full.names = TRUE)

datos_2018 <- archivos_2018 %>%
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

datos_2018 <- datos_2018 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2018$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2018$`FECHA OPERACION`))
datos_2018$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2018$`FECHA CONCERTACION`))
datos_2018$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2018$`FECHA ENTR. DESDE`))
datos_2018$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2018$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2018 <- datos_2018 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2018 <- datos_2018 %>% 
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

## Filtro de datos con fecha pq hay datos mal cargados -------------------------
datos_2018 <- datos_2018 %>%
  filter(!is.na(FECHA_OPERACION))

# 2019 -------------------------------------------------------------------------
archivos_2019 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2019.*\\.csv$",
  full.names = TRUE)

datos_2019 <- archivos_2019 %>%
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

datos_2019 <- datos_2019 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2019$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2019$`FECHA OPERACION`))
datos_2019$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2019$`FECHA CONCERTACION`))
datos_2019$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2019$`FECHA ENTR. DESDE`))
datos_2019$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2019$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2019 <- datos_2019 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2019 <- datos_2019 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2019 <- datos_2019 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 53     
                                      & FECHA_OPERACION <= as.Date("2019-01-06") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# 2020 -------------------------------------------------------------------------
archivos_2020 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2020.*\\.csv$",
  full.names = TRUE)

datos_2020 <- archivos_2020 %>%
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

datos_2020 <- datos_2020 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2020$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2020$`FECHA OPERACION`))
datos_2020$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2020$`FECHA CONCERTACION`))
datos_2020$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2020$`FECHA ENTR. DESDE`))
datos_2020$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2020$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2020 <- datos_2020 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2020 <- datos_2020 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2020 <- datos_2020 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2020-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# 2021 -------------------------------------------------------------------------
archivos_2021 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2021.*\\.csv$",
  full.names = TRUE)

datos_2021 <- archivos_2021 %>%
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

datos_2021 <- datos_2021 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2021$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2021$`FECHA OPERACION`))
datos_2021$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2021$`FECHA CONCERTACION`))
datos_2021$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2021$`FECHA ENTR. DESDE`))
datos_2021$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2021$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2021 <- datos_2021 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2021 <- datos_2021 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2021 <- datos_2021 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2021-01-03") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# 2022 -------------------------------------------------------------------------
archivos_2022 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2022.*\\.csv$",
  full.names = TRUE)

datos_2022 <- archivos_2022 %>%
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

datos_2022 <- datos_2022 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2022$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2022$`FECHA OPERACION`))
datos_2022$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2022$`FECHA CONCERTACION`))
datos_2022$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. DESDE`))
datos_2022$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
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


## Agregar columnas que necesito después ---------------------------------------
datos_2022 <- datos_2022 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2022 <- datos_2022 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2022-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

# 2023 -------------------------------------------------------------------------
archivos_2023 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2023.*\\.csv$",
  full.names = TRUE)

datos_2023 <- archivos_2023 %>%
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

datos_2023 <- datos_2023 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2023$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2023$`FECHA OPERACION`))
datos_2023$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2023$`FECHA CONCERTACION`))
datos_2023$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2023$`FECHA ENTR. DESDE`))
datos_2023$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2023$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2023 <- datos_2023 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2023 <- datos_2023 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2023 <- datos_2023 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2023-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

# 2024 -------------------------------------------------------------------------
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

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2024$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2024$`FECHA OPERACION`))
datos_2024$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2024$`FECHA CONCERTACION`))
datos_2024$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2024$`FECHA ENTR. DESDE`))
datos_2024$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2024$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
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

## Agregar columnas que necesito después ---------------------------------------
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

# 2025 -------------------------------------------------------------------------
archivos_2025 <- list.files(
  path = "Datos crudos/", 
  pattern = "^2025.*\\.csv$",
  full.names = TRUE)

datos_2025 <- archivos_2025 %>%
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

datos_2025 <- datos_2025 %>% 
  select(-`CALIDAD ADICIONAL`,
         -`NRO INSTANCIA OPERACION`,
         -`...21`)

## Transformaciones para las columnas que necesito en formato fecha ------------
datos_2025$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2025$`FECHA OPERACION`))
datos_2025$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2025$`FECHA CONCERTACION`))
datos_2025$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2025$`FECHA ENTR. DESDE`))
datos_2025$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2025$`FECHA ENTR. HASTA`))

## Renombrar columnas ----------------------------------------------------------
datos_2025 <- datos_2025 %>% 
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

## Agregar columnas que necesito después ---------------------------------------
datos_2025 <- datos_2025 %>% 
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

## Corrijo las semanas pq los primeros días del año me los toma como la última semana del año anterior
datos_2025 <- datos_2025 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 53     
                                      & FECHA_OPERACION <= as.Date("2025-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

# Todos los datos --------------------------------------------------------------
datos_total <- bind_rows(datos_2015,
                         datos_2016,
                         datos_2017,
                         datos_2018,
                         datos_2019,
                         datos_2020,
                         datos_2021,
                         datos_2022,
                         datos_2023,
                         datos_2024,
                         datos_2025)

# Producción -------------------------------------------------------------------
produccion <- read_delim("Mapa/Estimaciones.csv",
                         delim = ";",
                         locale = locale(encoding = "Windows-1252")) %>%
  mutate(Producción = case_when(Producción == "SD" ~ "0",
                                TRUE ~ Producción),
         Producción = as.numeric(Producción))