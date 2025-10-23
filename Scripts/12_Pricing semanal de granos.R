# Pricing semanal de granos 2025 -----------------------------------------------
## SOJA ------------------------------------------------------------------------

pricing_semanal_soja_2025 <- datos_2025 %>% 
  filter(PRODUCTO == "SOJA",
         ES_FINAL == "SI") %>% 
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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
  select(SEMANA_OPERACION,
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
         SEMANA_INICIO = floor_date(ymd(paste(ANIO_OPERACION, 1, 1, sep = "-"))           # agrego la fecha del lunes de la semana
                                    + weeks(SEMANA_OPERACION - 1),
                                    unit = "week",
                                    week_start = 1)) %>%
  group_by(SEMANA_OPERACION,
           SEMANA_INICIO) %>%
  summarise(CONTRATO = sum(CONTRATO, na.rm = TRUE),
            CONTRATO_ANULACION = sum(CONTRATO_ANULACION, na.rm = TRUE),
            CONTRATO_RECTIFICACION = sum(CONTRATO_RECTIFICACION, na.rm = TRUE),
            FIJACION = sum(FIJACION, na.rm = TRUE),
            FIJACION_ANULACION = sum(FIJACION_ANULACION, na.rm = TRUE),
            FIJACION_RECTIFICACION = sum(FIJACION_RECTIFICACION, na.rm = TRUE)) %>% 
  mutate(SEMANA_INICIO = case_when(SEMANA_INICIO == as.Date("2024-12-30") ~ as.Date("2025-01-01"),
                                   TRUE ~ SEMANA_INICIO),
         CONTRATO_PRECIO_HECHO = CONTRATO + CONTRATO_RECTIFICACION - CONTRATO_ANULACION,
         FIJACIONES = FIJACION + FIJACION_RECTIFICACION - FIJACION_ANULACION,
         TOTAL = CONTRATO_PRECIO_HECHO + FIJACIONES) %>% 
  select(SEMANA_OPERACION,
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

# Guardo los datos ------------------------------------------------------------- 

write_xlsx(pricing_semanal_cebada_2025, "Datos limpios/Pricing semanal/2025/Cebada.xlsx")
write_xlsx(pricing_semanal_girasol_2025, "Datos limpios/Pricing semanal/2025/Girasol.xlsx")
write_xlsx(pricing_semanal_maiz_2025, "Datos limpios/Pricing semanal/2025/Maiz.xlsx")
write_xlsx(pricing_semanal_soja_2025, "Datos limpios/Pricing semanal/2025/Soja.xlsx")
write_xlsx(pricing_semanal_sorgo_2025, "Datos limpios/Pricing semanal/2025/Sorgo.xlsx")
write_xlsx(pricing_semanal_trigo_2025, "Datos limpios/Pricing semanal/2025/Trigo.xlsx")
