# Librerías --------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(sf)
library(leaflet)
library(spData)
library(tools)
library(units)
library(Rcpp)

# Lectura de datos -------------------------------------------------------------
## 2015 ------------------------------------------------------------------------
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

datos_2015$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2015$`FECHA OPERACION`))
datos_2015$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2015$`FECHA CONCERTACION`))
datos_2015$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. DESDE`))
datos_2015$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2015$`FECHA ENTR. HASTA`))

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

datos_2015 <- datos_2015 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2015-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## 2016 ------------------------------------------------------------------------
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

datos_2016$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2016$`FECHA OPERACION`))
datos_2016$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2016$`FECHA CONCERTACION`))
datos_2016$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2016$`FECHA ENTR. DESDE`))
datos_2016$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2016$`FECHA ENTR. HASTA`))

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

datos_2016 <- datos_2016 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2016-01-03") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## 2017 ------------------------------------------------------------------------
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

datos_2017$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2017$`FECHA OPERACION`))
datos_2017$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2017$`FECHA CONCERTACION`))
datos_2017$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2017$`FECHA ENTR. DESDE`))
datos_2017$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2017$`FECHA ENTR. HASTA`))

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

datos_2017 <- datos_2017 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2017-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

datos_2017 <- datos_2017 %>%
  filter(!is.na(FECHA_OPERACION))

## 2018 ------------------------------------------------------------------------
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

datos_2018$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2018$`FECHA OPERACION`))
datos_2018$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2018$`FECHA CONCERTACION`))
datos_2018$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2018$`FECHA ENTR. DESDE`))
datos_2018$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2018$`FECHA ENTR. HASTA`))

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

datos_2018 <- datos_2018 %>%
  filter(!is.na(FECHA_OPERACION))

## 2019 ------------------------------------------------------------------------
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

datos_2019$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2019$`FECHA OPERACION`))
datos_2019$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2019$`FECHA CONCERTACION`))
datos_2019$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2019$`FECHA ENTR. DESDE`))
datos_2019$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2019$`FECHA ENTR. HASTA`))

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

datos_2019 <- datos_2019 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 53     
                                      & FECHA_OPERACION <= as.Date("2019-01-06") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## 2020 ------------------------------------------------------------------------
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

datos_2020$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2020$`FECHA OPERACION`))
datos_2020$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2020$`FECHA CONCERTACION`))
datos_2020$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2020$`FECHA ENTR. DESDE`))
datos_2020$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2020$`FECHA ENTR. HASTA`))

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

datos_2020 <- datos_2020 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2020-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## 2021 ------------------------------------------------------------------------
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

datos_2021$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2021$`FECHA OPERACION`))
datos_2021$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2021$`FECHA CONCERTACION`))
datos_2021$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2021$`FECHA ENTR. DESDE`))
datos_2021$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2021$`FECHA ENTR. HASTA`))

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

datos_2021 <- datos_2021 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION <= as.Date("2021-01-03") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## 2022 ------------------------------------------------------------------------
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

datos_2022$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2022$`FECHA OPERACION`))
datos_2022$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2022$`FECHA CONCERTACION`))
datos_2022$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. DESDE`))
datos_2022$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2022$`FECHA ENTR. HASTA`))

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

datos_2022 <- datos_2022 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2022-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

## 2023 ------------------------------------------------------------------------
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

datos_2023$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2023$`FECHA OPERACION`))
datos_2023$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2023$`FECHA CONCERTACION`))
datos_2023$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2023$`FECHA ENTR. DESDE`))
datos_2023$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2023$`FECHA ENTR. HASTA`))

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

datos_2023 <- datos_2023 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 52     
                                      & FECHA_OPERACION == as.Date("2023-01-01") ~ 1,
                                      TRUE ~ SEMANA_OPERACION))

## 2024 ------------------------------------------------------------------------
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

## 2025 ------------------------------------------------------------------------
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

datos_2025$`FECHA OPERACION` <- as.Date(dmy_hms(datos_2025$`FECHA OPERACION`))
datos_2025$`FECHA CONCERTACION` <- as.Date(dmy_hms(datos_2025$`FECHA CONCERTACION`))
datos_2025$`FECHA ENTR. DESDE` <- as.Date(dmy_hms(datos_2025$`FECHA ENTR. DESDE`))
datos_2025$`FECHA ENTR. HASTA` <- as.Date(dmy_hms(datos_2025$`FECHA ENTR. HASTA`))

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

datos_2025 <- datos_2025 %>%    
  mutate(SEMANA_OPERACION = case_when(SEMANA_OPERACION == 53     
                                      & FECHA_OPERACION <= as.Date("2025-01-05") ~ 1,
                                      TRUE ~ SEMANA_OPERACION + 1))

## total ------------------------------------------------------------------------
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

## Producción -------------------------------------------------------------------
produccion <- read_delim("Mapa/Estimaciones.csv",
                         delim = ";",
                         locale = locale(encoding = "Windows-1252")) %>%
  mutate(Producción = case_when(Producción == "SD" ~ "0",
                                TRUE ~ Producción),
         Producción = as.numeric(Producción))

# Pricing diario de granos -----------------------------------------------------
## 2015 ------------------------------------------------------------------------
pricing_diario_soja_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2015 <- datos_2015 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2015 <- datos_2015 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2015$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2015$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2015$PRODUCTO <- "MAIZ"
pricing_diario_soja_2015$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2015$PRODUCTO <- "SORGO"
pricing_diario_trigo_2015$PRODUCTO <- "TRIGO"

pricing_diario_total_2015 <- bind_rows(pricing_diario_cebada_2015,
                                       pricing_diario_girasol_2015,
                                       pricing_diario_maiz_2015,
                                       pricing_diario_soja_2015,
                                       pricing_diario_sorgo_2015,
                                       pricing_diario_trigo_2015)

pricing_diario_total_2015 <- pricing_diario_total_2015 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2016 ------------------------------------------------------------------------
pricing_diario_soja_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2016 <- datos_2016 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2016 <- datos_2016 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2016$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2016$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2016$PRODUCTO <- "MAIZ"
pricing_diario_soja_2016$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2016$PRODUCTO <- "SORGO"
pricing_diario_trigo_2016$PRODUCTO <- "TRIGO"

pricing_diario_total_2016 <- bind_rows(pricing_diario_cebada_2016,
                                       pricing_diario_girasol_2016,
                                       pricing_diario_maiz_2016,
                                       pricing_diario_soja_2016,
                                       pricing_diario_sorgo_2016,
                                       pricing_diario_trigo_2016)

pricing_diario_total_2016 <- pricing_diario_total_2016 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2017 ------------------------------------------------------------------------
pricing_diario_soja_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2017 <- datos_2017 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2017 <- datos_2017 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2017$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2017$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2017$PRODUCTO <- "MAIZ"
pricing_diario_soja_2017$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2017$PRODUCTO <- "SORGO"
pricing_diario_trigo_2017$PRODUCTO <- "TRIGO"

pricing_diario_total_2017 <- bind_rows(pricing_diario_cebada_2017,
                                       pricing_diario_girasol_2017,
                                       pricing_diario_maiz_2017,
                                       pricing_diario_soja_2017,
                                       pricing_diario_sorgo_2017,
                                       pricing_diario_trigo_2017)

pricing_diario_total_2017 <- pricing_diario_total_2017 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2018 ------------------------------------------------------------------------
pricing_diario_soja_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2018 <- datos_2018 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2018 <- datos_2018 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2018$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2018$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2018$PRODUCTO <- "MAIZ"
pricing_diario_soja_2018$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2018$PRODUCTO <- "SORGO"
pricing_diario_trigo_2018$PRODUCTO <- "TRIGO"

pricing_diario_total_2018 <- bind_rows(pricing_diario_cebada_2018,
                                       pricing_diario_girasol_2018,
                                       pricing_diario_maiz_2018,
                                       pricing_diario_soja_2018,
                                       pricing_diario_sorgo_2018,
                                       pricing_diario_trigo_2018)

pricing_diario_total_2018 <- pricing_diario_total_2018 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2019 ------------------------------------------------------------------------
pricing_diario_soja_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2019 <- datos_2019 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2019 <- datos_2019 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2019$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2019$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2019$PRODUCTO <- "MAIZ"
pricing_diario_soja_2019$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2019$PRODUCTO <- "SORGO"
pricing_diario_trigo_2019$PRODUCTO <- "TRIGO"

pricing_diario_total_2019 <- bind_rows(pricing_diario_cebada_2019,
                                       pricing_diario_girasol_2019,
                                       pricing_diario_maiz_2019,
                                       pricing_diario_soja_2019,
                                       pricing_diario_sorgo_2019,
                                       pricing_diario_trigo_2019)

pricing_diario_total_2019 <- pricing_diario_total_2019 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2020 ------------------------------------------------------------------------
pricing_diario_soja_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2020 <- datos_2020 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2020 <- datos_2020 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2020$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2020$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2020$PRODUCTO <- "MAIZ"
pricing_diario_soja_2020$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2020$PRODUCTO <- "SORGO"
pricing_diario_trigo_2020$PRODUCTO <- "TRIGO"

pricing_diario_total_2020 <- bind_rows(pricing_diario_cebada_2020,
                                       pricing_diario_girasol_2020,
                                       pricing_diario_maiz_2020,
                                       pricing_diario_soja_2020,
                                       pricing_diario_sorgo_2020,
                                       pricing_diario_trigo_2020)

pricing_diario_total_2020 <- pricing_diario_total_2020 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2021 ------------------------------------------------------------------------
pricing_diario_soja_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2021 <- datos_2021 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2021 <- datos_2021 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2021$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2021$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2021$PRODUCTO <- "MAIZ"
pricing_diario_soja_2021$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2021$PRODUCTO <- "SORGO"
pricing_diario_trigo_2021$PRODUCTO <- "TRIGO"

pricing_diario_total_2021 <- bind_rows(pricing_diario_cebada_2021,
                                       pricing_diario_girasol_2021,
                                       pricing_diario_maiz_2021,
                                       pricing_diario_soja_2021,
                                       pricing_diario_sorgo_2021,
                                       pricing_diario_trigo_2021)

pricing_diario_total_2021 <- pricing_diario_total_2021 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2022 ------------------------------------------------------------------------
pricing_diario_soja_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2022 <- datos_2022 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2022 <- datos_2022 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2022$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2022$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2022$PRODUCTO <- "MAIZ"
pricing_diario_soja_2022$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2022$PRODUCTO <- "SORGO"
pricing_diario_trigo_2022$PRODUCTO <- "TRIGO"

pricing_diario_total_2022 <- bind_rows(pricing_diario_cebada_2022,
                                       pricing_diario_girasol_2022,
                                       pricing_diario_maiz_2022,
                                       pricing_diario_soja_2022,
                                       pricing_diario_sorgo_2022,
                                       pricing_diario_trigo_2022)

pricing_diario_total_2022 <- pricing_diario_total_2022 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2023 ------------------------------------------------------------------------
pricing_diario_soja_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2023 <- datos_2023 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2023 <- datos_2023 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2023$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2023$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2023$PRODUCTO <- "MAIZ"
pricing_diario_soja_2023$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2023$PRODUCTO <- "SORGO"
pricing_diario_trigo_2023$PRODUCTO <- "TRIGO"

pricing_diario_total_2023 <- bind_rows(pricing_diario_cebada_2023,
                                       pricing_diario_girasol_2023,
                                       pricing_diario_maiz_2023,
                                       pricing_diario_soja_2023,
                                       pricing_diario_sorgo_2023,
                                       pricing_diario_trigo_2023)

pricing_diario_total_2023 <- pricing_diario_total_2023 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2024 ------------------------------------------------------------------------
pricing_diario_soja_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2024 <- datos_2024 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2024 <- datos_2024 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2024$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2024$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2024$PRODUCTO <- "MAIZ"
pricing_diario_soja_2024$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2024$PRODUCTO <- "SORGO"
pricing_diario_trigo_2024$PRODUCTO <- "TRIGO"

pricing_diario_total_2024 <- bind_rows(pricing_diario_cebada_2024,
                                       pricing_diario_girasol_2024,
                                       pricing_diario_maiz_2024,
                                       pricing_diario_soja_2024,
                                       pricing_diario_sorgo_2024,
                                       pricing_diario_trigo_2024)

pricing_diario_total_2024 <- pricing_diario_total_2024 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

## 2025 ------------------------------------------------------------------------
pricing_diario_soja_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_maiz_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_trigo_2025 <- datos_2025 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_girasol_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2025 <- datos_2025 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_sorgo_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0)) %>% 
  group_by(FECHA_OPERACION) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(FECHA_OPERACION,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_diario_cebada_2025$PRODUCTO <- "CEBADA"
pricing_diario_girasol_2025$PRODUCTO <- "GIRASOL"
pricing_diario_maiz_2025$PRODUCTO <- "MAIZ"
pricing_diario_soja_2025$PRODUCTO <- "SOJA"
pricing_diario_sorgo_2025$PRODUCTO <- "SORGO"
pricing_diario_trigo_2025$PRODUCTO <- "TRIGO"

pricing_diario_total_2025 <- bind_rows(pricing_diario_cebada_2025,
                                       pricing_diario_girasol_2025,
                                       pricing_diario_maiz_2025,
                                       pricing_diario_soja_2025,
                                       pricing_diario_sorgo_2025,
                                       pricing_diario_trigo_2025)

pricing_diario_total_2025 <- pricing_diario_total_2025 %>% 
  select(FECHA_OPERACION,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(FECHA_OPERACION)

# Pricing semanal de granos ----------------------------------------------------
## 2015 ------------------------------------------------------------------------
pricing_semanal_soja_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION,
                                    unit = "week",
                                    week_start = 1),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2015 <- datos_2015 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2015 <- datos_2015 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2015 <- datos_2015 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Contrato"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2014-12-29") ~ as.Date("2015-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2015) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2015$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2015$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2015$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2015$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2015$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2015$PRODUCTO <- "TRIGO"

pricing_semanal_total_2015 <- bind_rows(pricing_semanal_cebada_2015,
                                        pricing_semanal_girasol_2015,
                                        pricing_semanal_maiz_2015,
                                        pricing_semanal_soja_2015,
                                        pricing_semanal_sorgo_2015,
                                        pricing_semanal_trigo_2015)

pricing_semanal_total_2015 <- pricing_semanal_total_2015 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2016 ------------------------------------------------------------------------
pricing_semanal_soja_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION,
                                    unit = "week",
                                    week_start = 1),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2016 <- datos_2016 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2016 <- datos_2016 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2016 <- datos_2016 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2015-12-28") ~ as.Date("2016-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2016) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2016$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2016$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2016$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2016$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2016$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2016$PRODUCTO <- "TRIGO"

pricing_semanal_total_2016 <- bind_rows(pricing_semanal_cebada_2016,
                                        pricing_semanal_girasol_2016,
                                        pricing_semanal_maiz_2016,
                                        pricing_semanal_soja_2016,
                                        pricing_semanal_sorgo_2016,
                                        pricing_semanal_trigo_2016)

pricing_semanal_total_2016 <- pricing_semanal_total_2016 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2017 ------------------------------------------------------------------------
pricing_semanal_soja_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION,
                                    unit = "week",
                                    week_start = 1),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2017 <- datos_2017 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2017 <- datos_2017 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2017 <- datos_2017 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2016-12-26") ~ as.Date("2017-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2017) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2017$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2017$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2017$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2017$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2017$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2017$PRODUCTO <- "TRIGO"

pricing_semanal_total_2017 <- bind_rows(pricing_semanal_cebada_2017,
                                        pricing_semanal_girasol_2017,
                                        pricing_semanal_maiz_2017,
                                        pricing_semanal_soja_2017,
                                        pricing_semanal_sorgo_2017,
                                        pricing_semanal_trigo_2017)

pricing_semanal_total_2017 <- pricing_semanal_total_2017 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2018 ------------------------------------------------------------------------
pricing_semanal_soja_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2018 <- datos_2018 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2018 <- datos_2018 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2018 <- datos_2018 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2018) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2018$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2018$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2018$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2018$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2018$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2018$PRODUCTO <- "TRIGO"

pricing_semanal_total_2018 <- bind_rows(pricing_semanal_cebada_2018,
                                        pricing_semanal_girasol_2018,
                                        pricing_semanal_maiz_2018,
                                        pricing_semanal_soja_2018,
                                        pricing_semanal_sorgo_2018,
                                        pricing_semanal_trigo_2018)

pricing_semanal_total_2018 <- pricing_semanal_total_2018 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2019 ------------------------------------------------------------------------
pricing_semanal_soja_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2019 <- datos_2019 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2019 <- datos_2019 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2019 <- datos_2019 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2018-12-31") ~ as.Date("2019-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2019) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2019$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2019$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2019$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2019$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2019$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2019$PRODUCTO <- "TRIGO"

pricing_semanal_total_2019 <- bind_rows(pricing_semanal_cebada_2019,
                                        pricing_semanal_girasol_2019,
                                        pricing_semanal_maiz_2019,
                                        pricing_semanal_soja_2019,
                                        pricing_semanal_sorgo_2019,
                                        pricing_semanal_trigo_2019)

pricing_semanal_total_2019 <- pricing_semanal_total_2019 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2020 ------------------------------------------------------------------------
pricing_semanal_soja_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2020 <- datos_2020 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2020 <- datos_2020 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2020 <- datos_2020 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2019-12-30") ~ as.Date("2020-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2020) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2020$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2020$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2020$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2020$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2020$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2020$PRODUCTO <- "TRIGO"

pricing_semanal_total_2020 <- bind_rows(pricing_semanal_cebada_2020,
                                        pricing_semanal_girasol_2020,
                                        pricing_semanal_maiz_2020,
                                        pricing_semanal_soja_2020,
                                        pricing_semanal_sorgo_2020,
                                        pricing_semanal_trigo_2020)

pricing_semanal_total_2020 <- pricing_semanal_total_2020 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2021 ------------------------------------------------------------------------
pricing_semanal_soja_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2021 <- datos_2021 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2021 <- datos_2021 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2021 <- datos_2021 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2020-12-28") ~ as.Date("2021-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2021) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2021$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2021$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2021$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2021$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2021$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2021$PRODUCTO <- "TRIGO"

pricing_semanal_total_2021 <- bind_rows(pricing_semanal_cebada_2021,
                                        pricing_semanal_girasol_2021,
                                        pricing_semanal_maiz_2021,
                                        pricing_semanal_soja_2021,
                                        pricing_semanal_sorgo_2021,
                                        pricing_semanal_trigo_2021)

pricing_semanal_total_2021 <- pricing_semanal_total_2021 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2022 ------------------------------------------------------------------------
pricing_semanal_soja_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-03"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2022 <- datos_2022 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2022 <- datos_2022 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2022 <- datos_2022 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2021-12-27") ~ as.Date("2022-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2022) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2022$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2022$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2022$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2022$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2022$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2022$PRODUCTO <- "TRIGO"

pricing_semanal_total_2022 <- bind_rows(pricing_semanal_cebada_2022,
                                        pricing_semanal_girasol_2022,
                                        pricing_semanal_maiz_2022,
                                        pricing_semanal_soja_2022,
                                        pricing_semanal_sorgo_2022,
                                        pricing_semanal_trigo_2022)

pricing_semanal_total_2022 <- pricing_semanal_total_2022 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2023 ------------------------------------------------------------------------
pricing_semanal_soja_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_maiz_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_trigo_2023 <- datos_2023 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_girasol_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2023 <- datos_2023 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_sorgo_2023 <- datos_2023 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2022-12-26") ~ as.Date("2023-01-02"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2023) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL) %>% 
  arrange(SEMANA_OPERACION)

pricing_semanal_cebada_2023$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2023$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2023$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2023$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2023$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2023$PRODUCTO <- "TRIGO"

pricing_semanal_total_2023 <- bind_rows(pricing_semanal_cebada_2023,
                                        pricing_semanal_girasol_2023,
                                        pricing_semanal_maiz_2023,
                                        pricing_semanal_soja_2023,
                                        pricing_semanal_sorgo_2023,
                                        pricing_semanal_trigo_2023)

pricing_semanal_total_2023 <- pricing_semanal_total_2023 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2024 ------------------------------------------------------------------------
pricing_semanal_soja_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_maiz_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_trigo_2024 <- datos_2024 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_girasol_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_cebada_2024 <- datos_2024 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_sorgo_2024 <- datos_2024 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2024) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_cebada_2024$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2024$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2024$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2024$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2024$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2024$PRODUCTO <- "TRIGO"

pricing_semanal_total_2024 <- bind_rows(pricing_semanal_cebada_2024,
                                        pricing_semanal_girasol_2024,
                                        pricing_semanal_maiz_2024,
                                        pricing_semanal_soja_2024,
                                        pricing_semanal_sorgo_2024,
                                        pricing_semanal_trigo_2024)

pricing_semanal_total_2024 <- pricing_semanal_total_2024 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

## 2025 ------------------------------------------------------------------------
pricing_semanal_soja_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_maiz_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "MAIZ",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_trigo_2025 <- datos_2025 %>% 
  filter(PRODUCTO %in% c("TRIGO PAN", "TRIGO CAND."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_girasol_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "GIRASOL",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_cebada_2025 <- datos_2025 %>% 
  filter(PRODUCTO %in% c("CEBADA CERV.", "CEBADA FORR."),
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_sorgo_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "SORGO",
         ES_FINAL == "SI") %>% 
  select(FECHA_OPERACION,
         SEMANA_OPERACION,
         ANIO_OPERACION,
         OPERACION,
         PRECIO,
         PRODUCTO,
         TONELADAS,
         ES_FINAL) %>%
  mutate(CONTRATO = case_when(OPERACION == "Contrato"
                              & PRECIO == "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         CONTRATO_ANULACION = case_when(OPERACION == "Anulación"
                                        & PRECIO == "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         CONTRATO_RECTIFICACION = case_when(OPERACION == "Rectificación"
                                            & PRECIO == "Precio Hecho"~ TONELADAS,
                                            TRUE ~ 0),
         FIJACION = case_when(OPERACION == "Fijación"
                              & PRECIO != "Precio Hecho" ~ TONELADAS,
                              TRUE ~ 0),
         FIJACION_ANULACION = case_when(OPERACION == "Anulación Fijación"
                                        & PRECIO != "Precio Hecho" ~ TONELADAS,
                                        TRUE ~ 0),
         FIJACION_RECTIFICACION = case_when(OPERACION == "Rectificación Fijación"
                                            & PRECIO != "Precio Hecho" ~ TONELADAS,
                                            TRUE ~ 0),
         SEMANA_INICIO = floor_date(FECHA_OPERACION, unit = "week", week_start = 1),
         SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"), 
                                   TRUE ~ SEMANA_INICIO)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES,
         ANIO_OPERACION = 2025) %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         CONTRATO,
         CONTRATO_RECTIFICACION,
         CONTRATO_ANULACION,
         CONTRATO_PRECIO_HECHO,
         FIJACION,
         FIJACION_RECTIFICACION,
         FIJACION_ANULACION,
         FIJACIONES,
         TOTAL)

pricing_semanal_cebada_2025$PRODUCTO <- "CEBADA"
pricing_semanal_girasol_2025$PRODUCTO <- "GIRASOL"
pricing_semanal_maiz_2025$PRODUCTO <- "MAIZ"
pricing_semanal_soja_2025$PRODUCTO <- "SOJA"
pricing_semanal_sorgo_2025$PRODUCTO <- "SORGO"
pricing_semanal_trigo_2025$PRODUCTO <- "TRIGO"

pricing_semanal_total_2025 <- bind_rows(pricing_semanal_cebada_2025,
                                        pricing_semanal_girasol_2025,
                                        pricing_semanal_maiz_2025,
                                        pricing_semanal_soja_2025,
                                        pricing_semanal_sorgo_2025,
                                        pricing_semanal_trigo_2025)

pricing_semanal_total_2025 <- pricing_semanal_total_2025 %>% 
  select(ANIO_OPERACION,
         SEMANA_OPERACION,
         SEMANA_INICIO,
         TOTAL,
         PRODUCTO) %>%
  pivot_wider(names_from = PRODUCTO,
              values_from = TOTAL) %>% 
  mutate(CEBADA = if_else(is.na(CEBADA), 0, CEBADA),
         GIRASOL = if_else(is.na(GIRASOL), 0, GIRASOL),
         MAIZ = if_else(is.na(MAIZ), 0, MAIZ),
         SOJA = if_else(is.na(SOJA), 0, SOJA),
         SORGO = if_else(is.na(SORGO), 0, SORGO),
         TRIGO = if_else(is.na(TRIGO), 0, TRIGO),
         TOTAL = CEBADA + GIRASOL + MAIZ + SOJA + SORGO + TRIGO) %>% 
  arrange(SEMANA_OPERACION)

# Origen de la producción ------------------------------------------------------
## Datos de producción ---------------------------------------------------------
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

## Mapa ------------------------------------------------------------------------
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
                               PROVINCIA == "Direc. Grl. de Inmuebles" ~ "JUJUY",
                               PROVINCIA == "ATER - Direc. de Catastro" ~ "ENTRE RIOS",
                               PROVINCIA == "SCAR" ~ "TIERRA DEL FUEGO",
                               PROVINCIA == "Ministerio de Ecología" ~ "MISIONES",
                               PROVINCIA == "Gerencia de Catastro Pcial." ~ "TIERRA DEL FUEGO",
                               PROVINCIA == "Direc. Pcial. de Catastro y Cartografía" ~ "BUENOS AIRES",
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
funcion_grafico_mapa <- function(x, y) {
  colores <- switch(x,
                    "soja" = c("#C4D79B", "#375623"),
                    "maiz" = c("#FABF8F", "#833C0C"),
                    "trigo" = c("#FFD966", "#BF8F00"),
                    "girasol" = c("#D9D9D9", "#595959"),
                    "cebada" = c("#92CDDC", "#215967"),
                    "sorgo" = c("#8DB4E2", "#203764"),
                    c("#808080", "#0D0D0D"))
  
  datos_seleccionados <- produccion %>%
    filter(CULTIVO == x, COSECHA == y)
  
  mapa_cultivo <- mapa %>%
    left_join(datos_seleccionados, by = c("DEPARTAMENTO", "PROVINCIA")) %>%
    filter(!PROVINCIA %in% c("CHUBUT", "SANTA CRUZ", "TIERRA DEL FUEGO")) %>%
    mutate(fill_val = ifelse(TONELADAS == 0, NA, TONELADAS))
  
  grafico <- ggplot(mapa_cultivo) +
    geom_sf(aes(fill = fill_val, text = paste("Departamento:", DEPARTAMENTO,
                                              "\nProvincia:", PROVINCIA,
                                              "\nToneladas:", format(TONELADAS, big.mark = ".", decimal.mark = ",")))) +
    scale_fill_gradient(low = colores[1], high = colores[2], na.value = "white", 
                        name = "Toneladas") +
    theme_bw()
  
  return(grafico)
  
}

# Datos para widgets -----------------------------------------------------------
producto <- c("soja", "maiz", "trigo", "girasol", "cebada", "sorgo")
frecuencia <- c("diario", "semanal")
total <- c("por producto", "total")
campania <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20",
              "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")

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
                              options = list('live-search' = TRUE)
                              )
                            ),
               
               mainPanel(width = 9,
                         plotlyOutput("grafico_origen"))
               )
             )
    )
)

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
  
  output$grafico_origen <- renderPlotly({
    
    req(input$id_producto_origen, input$id_campaña_origen)
    
    ggplotly(funcion_grafico_mapa(input$id_producto_origen, input$id_campaña_origen), 
             tooltip = "text")
    
  })
  
}       

# publicación
shinyApp(ui = interfaz, server = servidor)