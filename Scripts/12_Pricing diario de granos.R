# Pricing diario de granos 2015 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2015, "Datos limpios/Pricing diario/2015/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2015, "Datos limpios/Pricing diario/2015/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2015, "Datos limpios/Pricing diario/2015/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2015, "Datos limpios/Pricing diario/2015/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2015, "Datos limpios/Pricing diario/2015/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2015, "Datos limpios/Pricing diario/2015/Trigo.xlsx")
write_xlsx(pricing_diario_total_2015, "Datos limpios/Pricing diario/2015/Total.xlsx")

# Pricing diario de granos 2016 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2016, "Datos limpios/Pricing diario/2016/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2016, "Datos limpios/Pricing diario/2016/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2016, "Datos limpios/Pricing diario/2016/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2016, "Datos limpios/Pricing diario/2016/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2016, "Datos limpios/Pricing diario/2016/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2016, "Datos limpios/Pricing diario/2016/Trigo.xlsx")
write_xlsx(pricing_diario_total_2016, "Datos limpios/Pricing diario/2016/Total.xlsx")

# Pricing diario de granos 2017 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2017, "Datos limpios/Pricing diario/2017/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2017, "Datos limpios/Pricing diario/2017/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2017, "Datos limpios/Pricing diario/2017/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2017, "Datos limpios/Pricing diario/2017/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2017, "Datos limpios/Pricing diario/2017/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2017, "Datos limpios/Pricing diario/2017/Trigo.xlsx")
write_xlsx(pricing_diario_total_2017, "Datos limpios/Pricing diario/2017/Total.xlsx")

# Pricing diario de granos 2018 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2018, "Datos limpios/Pricing diario/2018/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2018, "Datos limpios/Pricing diario/2018/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2018, "Datos limpios/Pricing diario/2018/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2018, "Datos limpios/Pricing diario/2018/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2018, "Datos limpios/Pricing diario/2018/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2018, "Datos limpios/Pricing diario/2018/Trigo.xlsx")
write_xlsx(pricing_diario_total_2018, "Datos limpios/Pricing diario/2018/Total.xlsx")

# Pricing diario de granos 2019 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2019, "Datos limpios/Pricing diario/2019/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2019, "Datos limpios/Pricing diario/2019/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2019, "Datos limpios/Pricing diario/2019/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2019, "Datos limpios/Pricing diario/2019/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2019, "Datos limpios/Pricing diario/2019/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2019, "Datos limpios/Pricing diario/2019/Trigo.xlsx")
write_xlsx(pricing_diario_total_2019, "Datos limpios/Pricing diario/2019/Total.xlsx")

# Pricing diario de granos 2020 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2020, "Datos limpios/Pricing diario/2020/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2020, "Datos limpios/Pricing diario/2020/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2020, "Datos limpios/Pricing diario/2020/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2020, "Datos limpios/Pricing diario/2020/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2020, "Datos limpios/Pricing diario/2020/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2020, "Datos limpios/Pricing diario/2020/Trigo.xlsx")
write_xlsx(pricing_diario_total_2020, "Datos limpios/Pricing diario/2020/Total.xlsx")

# Pricing diario de granos 2021 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2021, "Datos limpios/Pricing diario/2021/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2021, "Datos limpios/Pricing diario/2021/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2021, "Datos limpios/Pricing diario/2021/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2021, "Datos limpios/Pricing diario/2021/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2021, "Datos limpios/Pricing diario/2021/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2021, "Datos limpios/Pricing diario/2021/Trigo.xlsx")
write_xlsx(pricing_diario_total_2021, "Datos limpios/Pricing diario/2021/Total.xlsx")

# Pricing diario de granos 2022 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2022, "Datos limpios/Pricing diario/2022/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2022, "Datos limpios/Pricing diario/2022/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2022, "Datos limpios/Pricing diario/2022/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2022, "Datos limpios/Pricing diario/2022/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2022, "Datos limpios/Pricing diario/2022/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2022, "Datos limpios/Pricing diario/2022/Trigo.xlsx")
write_xlsx(pricing_diario_total_2022, "Datos limpios/Pricing diario/2022/Total.xlsx")

# Pricing diario de granos 2023 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos 

write_xlsx(pricing_diario_cebada_2023, "Datos limpios/Pricing diario/2023/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2023, "Datos limpios/Pricing diario/2023/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2023, "Datos limpios/Pricing diario/2023/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2023, "Datos limpios/Pricing diario/2023/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2023, "Datos limpios/Pricing diario/2023/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2023, "Datos limpios/Pricing diario/2023/Trigo.xlsx")
write_xlsx(pricing_diario_total_2023, "Datos limpios/Pricing diario/2023/Total.xlsx")

# Pricing diario de granos 2024 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos

write_xlsx(pricing_diario_cebada_2024, "Datos limpios/Pricing diario/2024/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2024, "Datos limpios/Pricing diario/2024/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2024, "Datos limpios/Pricing diario/2024/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2024, "Datos limpios/Pricing diario/2024/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2024, "Datos limpios/Pricing diario/2024/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2024, "Datos limpios/Pricing diario/2024/Trigo.xlsx")
write_xlsx(pricing_diario_total_2024, "Datos limpios/Pricing diario/2024/Total.xlsx")

# Pricing diario de granos 2025 ------------------------------------------------
## SOJA

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

## MAÍZ

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

## TRIGO

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

## GIRASOL

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

## CEBADA

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

## SORGO

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

## TOTAL

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

# Guardo los datos

write_xlsx(pricing_diario_cebada_2025, "Datos limpios/Pricing diario/2025/Cebada.xlsx")
write_xlsx(pricing_diario_girasol_2025, "Datos limpios/Pricing diario/2025/Girasol.xlsx")
write_xlsx(pricing_diario_maiz_2025, "Datos limpios/Pricing diario/2025/Maiz.xlsx")
write_xlsx(pricing_diario_soja_2025, "Datos limpios/Pricing diario/2025/Soja.xlsx")
write_xlsx(pricing_diario_sorgo_2025, "Datos limpios/Pricing diario/2025/Sorgo.xlsx")
write_xlsx(pricing_diario_trigo_2025, "Datos limpios/Pricing diario/2025/Trigo.xlsx")
write_xlsx(pricing_diario_total_2025, "Datos limpios/Pricing diario/2025/Total.xlsx")

# Histórico --------------------------------------------------------------------

## SOJA
pricing_diario_historico_soja <- bind_rows(pricing_diario_soja_2015,
                                           pricing_diario_soja_2016,
                                           pricing_diario_soja_2017,
                                           pricing_diario_soja_2018,
                                           pricing_diario_soja_2019,
                                           pricing_diario_soja_2020,
                                           pricing_diario_soja_2021,
                                           pricing_diario_soja_2022,
                                           pricing_diario_soja_2023,
                                           pricing_diario_soja_2024,
                                           pricing_diario_soja_2025)

write_xlsx(pricing_diario_historico_soja,
           "Datos limpios/Pricing diario/Pricing diario histórico_Soja.xlsx")

## MAIZ
pricing_diario_historico_maiz <- bind_rows(pricing_diario_maiz_2015,
                                           pricing_diario_maiz_2016,
                                           pricing_diario_maiz_2017,
                                           pricing_diario_maiz_2018,
                                           pricing_diario_maiz_2019,
                                           pricing_diario_maiz_2020,
                                           pricing_diario_maiz_2021,
                                           pricing_diario_maiz_2022,
                                           pricing_diario_maiz_2023,
                                           pricing_diario_maiz_2024,
                                           pricing_diario_maiz_2025)

write_xlsx(pricing_diario_historico_maiz,
           "Datos limpios/Pricing diario/Pricing diario histórico_Maiz.xlsx")

## TRIGO
pricing_diario_historico_trigo <- bind_rows(pricing_diario_trigo_2015,
                                            pricing_diario_trigo_2016,
                                            pricing_diario_trigo_2017,
                                            pricing_diario_trigo_2018,
                                            pricing_diario_trigo_2019,
                                            pricing_diario_trigo_2020,
                                            pricing_diario_trigo_2021,
                                            pricing_diario_trigo_2022,
                                            pricing_diario_trigo_2023,
                                            pricing_diario_trigo_2024,
                                            pricing_diario_trigo_2025)

write_xlsx(pricing_diario_historico_trigo,
           "Datos limpios/Pricing diario/Pricing diario histórico_Trigo.xlsx")

## GIRASOL
pricing_diario_historico_girasol <- bind_rows(pricing_diario_girasol_2015,
                                              pricing_diario_girasol_2016,
                                              pricing_diario_girasol_2017,
                                              pricing_diario_girasol_2018,
                                              pricing_diario_girasol_2019,
                                              pricing_diario_girasol_2020,
                                              pricing_diario_girasol_2021,
                                              pricing_diario_girasol_2022,
                                              pricing_diario_girasol_2023,
                                              pricing_diario_girasol_2024,
                                              pricing_diario_girasol_2025)

write_xlsx(pricing_diario_historico_girasol,
           "Datos limpios/Pricing diario/Pricing diario histórico_Girasol.xlsx")

## CEBADA
pricing_diario_historico_cebada <- bind_rows(pricing_diario_cebada_2015,
                                             pricing_diario_cebada_2016,
                                             pricing_diario_cebada_2017,
                                             pricing_diario_cebada_2018,
                                             pricing_diario_cebada_2019,
                                             pricing_diario_cebada_2020,
                                             pricing_diario_cebada_2021,
                                             pricing_diario_cebada_2022,
                                             pricing_diario_cebada_2023,
                                             pricing_diario_cebada_2024,
                                             pricing_diario_cebada_2025)

write_xlsx(pricing_diario_historico_cebada,
           "Datos limpios/Pricing diario/Pricing diario histórico_Cebada.xlsx")


## SORGO
pricing_diario_historico_sorgo <- bind_rows(pricing_diario_sorgo_2015,
                                            pricing_diario_sorgo_2016,
                                            pricing_diario_sorgo_2017,
                                            pricing_diario_sorgo_2018,
                                            pricing_diario_sorgo_2019,
                                            pricing_diario_sorgo_2020,
                                            pricing_diario_sorgo_2021,
                                            pricing_diario_sorgo_2022,
                                            pricing_diario_sorgo_2023,
                                            pricing_diario_sorgo_2024,
                                            pricing_diario_sorgo_2025)

write_xlsx(pricing_diario_historico_sorgo,
           "Datos limpios/Pricing diario/Pricing diario histórico_Sorgo.xlsx")

## TOTAL
pricing_diario_historico <- bind_rows(pricing_diario_total_2015,
                                      pricing_diario_total_2016,
                                      pricing_diario_total_2017,
                                      pricing_diario_total_2018,
                                      pricing_diario_total_2019,
                                      pricing_diario_total_2020,
                                      pricing_diario_total_2021,
                                      pricing_diario_total_2022,
                                      pricing_diario_total_2023,
                                      pricing_diario_total_2024,
                                      pricing_diario_total_2025)

write_xlsx(pricing_diario_historico, "Datos limpios/Pricing diario/Pricing diario histórico.xlsx")
