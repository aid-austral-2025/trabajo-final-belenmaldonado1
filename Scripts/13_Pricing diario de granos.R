# Pricing diario de granos 2025 ------------------------------------------------
## SOJA ------------------------------------------------------------------------

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

## MAÍZ ------------------------------------------------------------------------

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

## TRIGO -----------------------------------------------------------------------

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

## GIRASOL ---------------------------------------------------------------------

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

## CEBADA ----------------------------------------------------------------------

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

## SORGO -----------------------------------------------------------------------

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

# Guardo los datos ------------------------------------------------------------- 

write_xlsx(pricing_diario_cebada_2025, "Datos limpios/Pricing diario/2025/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2025, "Datos limpios/Pricing diario/2025/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2025, "Datos limpios/Pricing diario/2025/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2025, "Datos limpios/Pricing diario/2025/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2025, "Datos limpios/Pricing diario/2025/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2025, "Datos limpios/Pricing diario/2025/Trigo.xlsx")
