# MAPA DE ORIGEN DE LA PRODUCCIÓN ----------------------------------------------
# Trabajo los datos de producción ----------------------------------------------
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

saveRDS(pricing_diario_historico, "Shiny/Data/pricing_diario_historico.rds")

saveRDS(produccion, "Shiny/Data/produccion.rds")

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

saveRDS(mapa, "Shiny/Data/mapa.rds")