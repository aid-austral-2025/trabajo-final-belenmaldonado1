# Pricing semanal de granos 2015 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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


## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2016 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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


## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2017 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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


## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2018 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2019 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2020 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2021 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2022 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2023 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2024 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Pricing semanal de granos 2025 -----------------------------------------------
## SOJA ------------------------------------------------------------------------
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

## MAÍZ ------------------------------------------------------------------------
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

## TRIGO -----------------------------------------------------------------------
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

## GIRASOL ---------------------------------------------------------------------
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

## CEBADA ----------------------------------------------------------------------
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

## SORGO -----------------------------------------------------------------------
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

## TOTAL -----------------------------------------------------------------------
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

# Histórico --------------------------------------------------------------------
## SOJA ------------------------------------------------------------------------
pricing_semanal_historico_soja <- bind_rows(pricing_semanal_soja_2015,
                                            pricing_semanal_soja_2016,
                                            pricing_semanal_soja_2017,
                                            pricing_semanal_soja_2018,
                                            pricing_semanal_soja_2019,
                                            pricing_semanal_soja_2020,
                                            pricing_semanal_soja_2021,
                                            pricing_semanal_soja_2022,
                                            pricing_semanal_soja_2023,
                                            pricing_semanal_soja_2024,
                                            pricing_semanal_soja_2025)

## MAIZ ------------------------------------------------------------------------
pricing_semanal_historico_maiz <- bind_rows(pricing_semanal_maiz_2015,
                                            pricing_semanal_maiz_2016,
                                            pricing_semanal_maiz_2017,
                                            pricing_semanal_maiz_2018,
                                            pricing_semanal_maiz_2019,
                                            pricing_semanal_maiz_2020,
                                            pricing_semanal_maiz_2021,
                                            pricing_semanal_maiz_2022,
                                            pricing_semanal_maiz_2023,
                                            pricing_semanal_maiz_2024,
                                            pricing_semanal_maiz_2025)

## TRIGO -----------------------------------------------------------------------
pricing_semanal_historico_trigo <- bind_rows(pricing_semanal_trigo_2015,
                                             pricing_semanal_trigo_2016,
                                             pricing_semanal_trigo_2017,
                                             pricing_semanal_trigo_2018,
                                             pricing_semanal_trigo_2019,
                                             pricing_semanal_trigo_2020,
                                             pricing_semanal_trigo_2021,
                                             pricing_semanal_trigo_2022,
                                             pricing_semanal_trigo_2023,
                                             pricing_semanal_trigo_2024,
                                             pricing_semanal_trigo_2025)

## GIRASOL ---------------------------------------------------------------------
pricing_semanal_historico_girasol <- bind_rows(pricing_semanal_girasol_2015,
                                               pricing_semanal_girasol_2016,
                                               pricing_semanal_girasol_2017,
                                               pricing_semanal_girasol_2018,
                                               pricing_semanal_girasol_2019,
                                               pricing_semanal_girasol_2020,
                                               pricing_semanal_girasol_2021,
                                               pricing_semanal_girasol_2022,
                                               pricing_semanal_girasol_2023,
                                               pricing_semanal_girasol_2024,
                                               pricing_semanal_girasol_2025)

## CEBADA ----------------------------------------------------------------------
pricing_semanal_historico_cebada <- bind_rows(pricing_semanal_cebada_2015,
                                              pricing_semanal_cebada_2016,
                                              pricing_semanal_cebada_2017,
                                              pricing_semanal_cebada_2018,
                                              pricing_semanal_cebada_2019,
                                              pricing_semanal_cebada_2020,
                                              pricing_semanal_cebada_2021,
                                              pricing_semanal_cebada_2022,
                                              pricing_semanal_cebada_2023,
                                              pricing_semanal_cebada_2024,
                                              pricing_semanal_cebada_2025)

## SORGO -----------------------------------------------------------------------
pricing_semanal_historico_sorgo <- bind_rows(pricing_semanal_sorgo_2015,
                                             pricing_semanal_sorgo_2016,
                                             pricing_semanal_sorgo_2017,
                                             pricing_semanal_sorgo_2018,
                                             pricing_semanal_sorgo_2019,
                                             pricing_semanal_sorgo_2020,
                                             pricing_semanal_sorgo_2021,
                                             pricing_semanal_sorgo_2022,
                                             pricing_semanal_sorgo_2023,
                                             pricing_semanal_sorgo_2024,
                                             pricing_semanal_sorgo_2025)

## TOTAL -----------------------------------------------------------------------
pricing_semanal_historico <- bind_rows(pricing_semanal_total_2015,
                                       pricing_semanal_total_2016,
                                       pricing_semanal_total_2017,
                                       pricing_semanal_total_2018,
                                       pricing_semanal_total_2019,
                                       pricing_semanal_total_2020,
                                       pricing_semanal_total_2021,
                                       pricing_semanal_total_2022,
                                       pricing_semanal_total_2023,
                                       pricing_semanal_total_2024,
                                       pricing_semanal_total_2025)

# GUARDO LOS DATOS -------------------------------------------------------------
## 2015 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2015, "Datos limpios/Pricing semanal/2015/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2015, "Datos limpios/Pricing semanal/2015/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2015, "Datos limpios/Pricing semanal/2015/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2015, "Datos limpios/Pricing semanal/2015/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2015, "Datos limpios/Pricing semanal/2015/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2015, "Datos limpios/Pricing semanal/2015/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2015, "Datos limpios/Pricing semanal/2015/Total.xlsx")

## 2016 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2016, "Datos limpios/Pricing semanal/2016/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2016, "Datos limpios/Pricing semanal/2016/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2016, "Datos limpios/Pricing semanal/2016/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2016, "Datos limpios/Pricing semanal/2016/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2016, "Datos limpios/Pricing semanal/2016/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2016, "Datos limpios/Pricing semanal/2016/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2016, "Datos limpios/Pricing semanal/2016/Total.xlsx")

## 2017 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2017, "Datos limpios/Pricing semanal/2017/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2017, "Datos limpios/Pricing semanal/2017/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2017, "Datos limpios/Pricing semanal/2017/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2017, "Datos limpios/Pricing semanal/2017/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2017, "Datos limpios/Pricing semanal/2017/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2017, "Datos limpios/Pricing semanal/2017/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2017, "Datos limpios/Pricing semanal/2017/Total.xlsx")

## 2018 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2018, "Datos limpios/Pricing semanal/2018/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2018, "Datos limpios/Pricing semanal/2018/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2018, "Datos limpios/Pricing semanal/2018/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2018, "Datos limpios/Pricing semanal/2018/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2018, "Datos limpios/Pricing semanal/2018/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2018, "Datos limpios/Pricing semanal/2018/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2018, "Datos limpios/Pricing semanal/2018/Total.xlsx")

## 2019 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2019, "Datos limpios/Pricing semanal/2019/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2019, "Datos limpios/Pricing semanal/2019/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2019, "Datos limpios/Pricing semanal/2019/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2019, "Datos limpios/Pricing semanal/2019/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2019, "Datos limpios/Pricing semanal/2019/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2019, "Datos limpios/Pricing semanal/2019/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2019, "Datos limpios/Pricing semanal/2019/Total.xlsx")

## 2020 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2020, "Datos limpios/Pricing semanal/2020/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2020, "Datos limpios/Pricing semanal/2020/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2020, "Datos limpios/Pricing semanal/2020/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2020, "Datos limpios/Pricing semanal/2020/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2020, "Datos limpios/Pricing semanal/2020/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2020, "Datos limpios/Pricing semanal/2020/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2020, "Datos limpios/Pricing semanal/2020/Total.xlsx")

## 2021 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2021, "Datos limpios/Pricing semanal/2021/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2021, "Datos limpios/Pricing semanal/2021/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2021, "Datos limpios/Pricing semanal/2021/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2021, "Datos limpios/Pricing semanal/2021/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2021, "Datos limpios/Pricing semanal/2021/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2021, "Datos limpios/Pricing semanal/2021/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2021, "Datos limpios/Pricing semanal/2021/Total.xlsx")

## 2022 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2022, "Datos limpios/Pricing semanal/2022/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2022, "Datos limpios/Pricing semanal/2022/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2022, "Datos limpios/Pricing semanal/2022/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2022, "Datos limpios/Pricing semanal/2022/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2022, "Datos limpios/Pricing semanal/2022/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2022, "Datos limpios/Pricing semanal/2022/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2022, "Datos limpios/Pricing semanal/2022/Total.xlsx")

## 2023 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2023, "Datos limpios/Pricing semanal/2023/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2023, "Datos limpios/Pricing semanal/2023/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2023, "Datos limpios/Pricing semanal/2023/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2023, "Datos limpios/Pricing semanal/2023/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2023, "Datos limpios/Pricing semanal/2023/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2023, "Datos limpios/Pricing semanal/2023/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2023, "Datos limpios/Pricing semanal/2023/Total.xlsx")

## 2024 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2024, "Datos limpios/Pricing semanal/2024/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2024, "Datos limpios/Pricing semanal/2024/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2024, "Datos limpios/Pricing semanal/2024/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2024, "Datos limpios/Pricing semanal/2024/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2024, "Datos limpios/Pricing semanal/2024/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2024, "Datos limpios/Pricing semanal/2024/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2024, "Datos limpios/Pricing semanal/2024/Total.xlsx")

## 2025 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2025, "Datos limpios/Pricing semanal/2025/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2025, "Datos limpios/Pricing semanal/2025/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2025, "Datos limpios/Pricing semanal/2025/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2025, "Datos limpios/Pricing semanal/2025/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2025, "Datos limpios/Pricing semanal/2025/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2025, "Datos limpios/Pricing semanal/2025/Trigo.xlsx")
write_xlsx(pricing_semanal_total_2025, "Datos limpios/Pricing semanal/2025/Total.xlsx")

## Histórico -------------------------------------------------------------------
write_xlsx(pricing_semanal_historico_soja, "Datos limpios/Pricing semanal/Histórico/Soja.xlsx")
write_xlsx(pricing_semanal_historico_maiz, "Datos limpios/Pricing semanal/Histórico/Maiz.xlsx")
write_xlsx(pricing_semanal_historico_trigo, "Datos limpios/Pricing semanal/Histórico/Trigo.xlsx")
write_xlsx(pricing_semanal_historico_girasol, "Datos limpios/Pricing semanal/Histórico/Girasol.xlsx")
write_xlsx(pricing_semanal_historico_cebada, "Datos limpios/Pricing semanal/Histórico/Cebada.xlsx")
write_xlsx(pricing_semanal_historico_sorgo, "Datos limpios/Pricing semanal/Histórico/Sorgo.xlsx")
write_xlsx(pricing_semanal_historico, "Datos limpios/Pricing semanal/Histórico/Pricing semanal histórico.xlsx")


# GUARDO LOS DATOS PARA SHINY --------------------------------------------------
## 2015 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2015, "Shiny/Datos limpios/pricing_semanal_cebada_2015.xlsx")
write_xlsx(pricing_semanal_girasol_2015, "Shiny/Datos limpios/pricing_semanal_girasol_2015.xlsx")
write_xlsx(pricing_semanal_maiz_2015, "Shiny/Datos limpios/pricing_semanal_maiz_2015.xlsx")
write_xlsx(pricing_semanal_soja_2015, "Shiny/Datos limpios/pricing_semanal_soja_2015.xlsx")
write_xlsx(pricing_semanal_sorgo_2015, "Shiny/Datos limpios/pricing_semanal_sorgo_2015.xlsx")
write_xlsx(pricing_semanal_trigo_2015, "Shiny/Datos limpios/pricing_semanal_trigo_2015.xlsx")
write_xlsx(pricing_semanal_total_2015, "Shiny/Datos limpios/pricing_semanal_total_2015.xlsx")

## 2016 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2016, "Shiny/Datos limpios/pricing_semanal_cebada_2016.xlsx")
write_xlsx(pricing_semanal_girasol_2016, "Shiny/Datos limpios/pricing_semanal_girasol_2016.xlsx")
write_xlsx(pricing_semanal_maiz_2016, "Shiny/Datos limpios/pricing_semanal_maiz_2016.xlsx")
write_xlsx(pricing_semanal_soja_2016, "Shiny/Datos limpios/pricing_semanal_soja_2016.xlsx")
write_xlsx(pricing_semanal_sorgo_2016, "Shiny/Datos limpios/pricing_semanal_sorgo_2016.xlsx")
write_xlsx(pricing_semanal_trigo_2016, "Shiny/Datos limpios/pricing_semanal_trigo_2016.xlsx")
write_xlsx(pricing_semanal_total_2016, "Shiny/Datos limpios/pricing_semanal_total_2016.xlsx")

## 2017 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2017, "Shiny/Datos limpios/pricing_semanal_cebada_2017.xlsx")
write_xlsx(pricing_semanal_girasol_2017, "Shiny/Datos limpios/pricing_semanal_girasol_2017.xlsx")
write_xlsx(pricing_semanal_maiz_2017, "Shiny/Datos limpios/pricing_semanal_maiz_2017.xlsx")
write_xlsx(pricing_semanal_soja_2017, "Shiny/Datos limpios/pricing_semanal_soja_2017.xlsx")
write_xlsx(pricing_semanal_sorgo_2017, "Shiny/Datos limpios/pricing_semanal_sorgo_2017.xlsx")
write_xlsx(pricing_semanal_trigo_2017, "Shiny/Datos limpios/pricing_semanal_trigo_2017.xlsx")
write_xlsx(pricing_semanal_total_2017, "Shiny/Datos limpios/pricing_semanal_total_2017.xlsx")

## 2018 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2018, "Shiny/Datos limpios/pricing_semanal_cebada_2018.xlsx")
write_xlsx(pricing_semanal_girasol_2018, "Shiny/Datos limpios/pricing_semanal_girasol_2018.xlsx")
write_xlsx(pricing_semanal_maiz_2018, "Shiny/Datos limpios/pricing_semanal_maiz_2018.xlsx")
write_xlsx(pricing_semanal_soja_2018, "Shiny/Datos limpios/pricing_semanal_soja_2018.xlsx")
write_xlsx(pricing_semanal_sorgo_2018, "Shiny/Datos limpios/pricing_semanal_sorgo_2018.xlsx")
write_xlsx(pricing_semanal_trigo_2018, "Shiny/Datos limpios/pricing_semanal_trigo_2018.xlsx")
write_xlsx(pricing_semanal_total_2018, "Shiny/Datos limpios/pricing_semanal_total_2018.xlsx")

## 2019 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2019, "Shiny/Datos limpios/pricing_semanal_cebada_2019.xlsx")
write_xlsx(pricing_semanal_girasol_2019, "Shiny/Datos limpios/pricing_semanal_girasol_2019.xlsx")
write_xlsx(pricing_semanal_maiz_2019, "Shiny/Datos limpios/pricing_semanal_maiz_2019.xlsx")
write_xlsx(pricing_semanal_soja_2019, "Shiny/Datos limpios/pricing_semanal_soja_2019.xlsx")
write_xlsx(pricing_semanal_sorgo_2019, "Shiny/Datos limpios/pricing_semanal_sorgo_2019.xlsx")
write_xlsx(pricing_semanal_trigo_2019, "Shiny/Datos limpios/pricing_semanal_trigo_2019.xlsx")
write_xlsx(pricing_semanal_total_2019, "Shiny/Datos limpios/pricing_semanal_total_2019.xlsx")

## 2020 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2020, "Shiny/Datos limpios/pricing_semanal_cebada_2020.xlsx")
write_xlsx(pricing_semanal_girasol_2020, "Shiny/Datos limpios/pricing_semanal_girasol_2020.xlsx")
write_xlsx(pricing_semanal_maiz_2020, "Shiny/Datos limpios/pricing_semanal_maiz_2020.xlsx")
write_xlsx(pricing_semanal_soja_2020, "Shiny/Datos limpios/pricing_semanal_soja_2020.xlsx")
write_xlsx(pricing_semanal_sorgo_2020, "Shiny/Datos limpios/pricing_semanal_sorgo_2020.xlsx")
write_xlsx(pricing_semanal_trigo_2020, "Shiny/Datos limpios/pricing_semanal_trigo_2020.xlsx")
write_xlsx(pricing_semanal_total_2020, "Shiny/Datos limpios/pricing_semanal_total_2020.xlsx")

## 2021 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2021, "Shiny/Datos limpios/pricing_semanal_cebada_2021.xlsx")
write_xlsx(pricing_semanal_girasol_2021, "Shiny/Datos limpios/pricing_semanal_girasol_2021.xlsx")
write_xlsx(pricing_semanal_maiz_2021, "Shiny/Datos limpios/pricing_semanal_maiz_2021.xlsx")
write_xlsx(pricing_semanal_soja_2021, "Shiny/Datos limpios/pricing_semanal_soja_2021.xlsx")
write_xlsx(pricing_semanal_sorgo_2021, "Shiny/Datos limpios/pricing_semanal_sorgo_2021.xlsx")
write_xlsx(pricing_semanal_trigo_2021, "Shiny/Datos limpios/pricing_semanal_trigo_2021.xlsx")
write_xlsx(pricing_semanal_total_2021, "Shiny/Datos limpios/pricing_semanal_total_2021.xlsx")

## 2022 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2022, "Shiny/Datos limpios/pricing_semanal_cebada_2022.xlsx")
write_xlsx(pricing_semanal_girasol_2022, "Shiny/Datos limpios/pricing_semanal_girasol_2022.xlsx")
write_xlsx(pricing_semanal_maiz_2022, "Shiny/Datos limpios/pricing_semanal_maiz_2022.xlsx")
write_xlsx(pricing_semanal_soja_2022, "Shiny/Datos limpios/pricing_semanal_soja_2022.xlsx")
write_xlsx(pricing_semanal_sorgo_2022, "Shiny/Datos limpios/pricing_semanal_sorgo_2022.xlsx")
write_xlsx(pricing_semanal_trigo_2022, "Shiny/Datos limpios/pricing_semanal_trigo_2022.xlsx")
write_xlsx(pricing_semanal_total_2022, "Shiny/Datos limpios/pricing_semanal_total_2022.xlsx")

## 2023 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2023, "Shiny/Datos limpios/pricing_semanal_cebada_2023.xlsx")
write_xlsx(pricing_semanal_girasol_2023, "Shiny/Datos limpios/pricing_semanal_girasol_2023.xlsx")
write_xlsx(pricing_semanal_maiz_2023, "Shiny/Datos limpios/pricing_semanal_maiz_2023.xlsx")
write_xlsx(pricing_semanal_soja_2023, "Shiny/Datos limpios/pricing_semanal_soja_2023.xlsx")
write_xlsx(pricing_semanal_sorgo_2023, "Shiny/Datos limpios/pricing_semanal_sorgo_2023.xlsx")
write_xlsx(pricing_semanal_trigo_2023, "Shiny/Datos limpios/pricing_semanal_trigo_2023.xlsx")
write_xlsx(pricing_semanal_total_2023, "Shiny/Datos limpios/pricing_semanal_total_2023.xlsx")

## 2024 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2024, "Shiny/Datos limpios/pricing_semanal_cebada_2024.xlsx")
write_xlsx(pricing_semanal_girasol_2024, "Shiny/Datos limpios/pricing_semanal_girasol_2024.xlsx")
write_xlsx(pricing_semanal_maiz_2024, "Shiny/Datos limpios/pricing_semanal_maiz_2024.xlsx")
write_xlsx(pricing_semanal_soja_2024, "Shiny/Datos limpios/pricing_semanal_soja_2024.xlsx")
write_xlsx(pricing_semanal_sorgo_2024, "Shiny/Datos limpios/pricing_semanal_sorgo_2024.xlsx")
write_xlsx(pricing_semanal_trigo_2024, "Shiny/Datos limpios/pricing_semanal_trigo_2024.xlsx")
write_xlsx(pricing_semanal_total_2024, "Shiny/Datos limpios/pricing_semanal_total_2024.xlsx")

## 2025 ------------------------------------------------------------------------
write_xlsx(pricing_semanal_cebada_2025, "Shiny/Datos limpios/pricing_semanal_cebada_2025.xlsx")
write_xlsx(pricing_semanal_girasol_2025, "Shiny/Datos limpios/pricing_semanal_girasol_2025.xlsx")
write_xlsx(pricing_semanal_maiz_2025, "Shiny/Datos limpios/pricing_semanal_maiz_2025.xlsx")
write_xlsx(pricing_semanal_soja_2025, "Shiny/Datos limpios/pricing_semanal_soja_2025.xlsx")
write_xlsx(pricing_semanal_sorgo_2025, "Shiny/Datos limpios/pricing_semanal_sorgo_2025.xlsx")
write_xlsx(pricing_semanal_trigo_2025, "Shiny/Datos limpios/pricing_semanal_trigo_2025.xlsx")
write_xlsx(pricing_semanal_total_2025, "Shiny/Datos limpios/pricing_semanal_total_2025.xlsx")

## Histórico -------------------------------------------------------------------
write_xlsx(pricing_semanal_historico_soja, "Shiny/Datos limpios/pricing_semanal_historico_soja.xlsx")
write_xlsx(pricing_semanal_historico_maiz, "Shiny/Datos limpios/pricing_semanal_historico_maiz.xlsx")
write_xlsx(pricing_semanal_historico_trigo, "Shiny/Datos limpios/pricing_semanal_historico_trigo.xlsx")
write_xlsx(pricing_semanal_historico_girasol, "Shiny/Datos limpios/pricing_semanal_historico_girasol.xlsx")
write_xlsx(pricing_semanal_historico_cebada, "Shiny/Datos limpios/pricing_semanal_historico_cebada.xlsx")
write_xlsx(pricing_semanal_historico_sorgo, "Shiny/Datos limpios/pricing_semanal_historico_sorgo.xlsx")
write_xlsx(pricing_semanal_historico, "Shiny/Datos limpios/pricing_semanal_historico.xlsx")