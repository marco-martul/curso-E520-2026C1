setwd("C:/Users/user/Desktop/Curso-E520-MarcoMartul/repo-curso-2026/curso-E520-2026C1")
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

anac_2025 <- read_csv2(file = "202512-informe-ministerio-actualizado-dic-final.csv")
anac_2024 <- read_csv2(file = "202412-informe-ministerio-actualizado-dic-final.csv")
anac_2023 <- read_csv2(file = "202312-informe-ministerio-actualizado-dic.csv")
anac_2022 <- read_csv2(file = "202212-informe-ministerio-actualizado-dic-final.csv")
anac_2021 <- read_csv2(file = "202112-informe-ministerio-actualizado-dic-final.csv")
anac_2020 <- read_csv2(file = "202012-informe-ministerio-actualizado-dic-final.csv")
anac_2019 <- read_csv2(file = "201912-informe-ministerio-actualizado-dic-final.csv")

glimpse(anac_2025)

aeropuertos <-
  read_csv(file = "C:/Users/user/Desktop/Curso-E520-MarcoMartul/repo-curso-2026/econ-520/clases/proyectos/tareas/iata-icao.csv")
glimpse(aeropuertos)

limpiar_anac <- function(df, anio) {
  df |>
    mutate(across(everything(), as.character)) |>
    mutate(
      anio = anio,
      tipo_vuelo = factor(`Clase de Vuelo (todos los vuelos)`),
      clasif_vuelos = factor(`Clasificación Vuelo`),
      tipo_movimiento = factor(`Tipo de Movimiento`),
      aeropuerto = factor(Aeropuerto),
      origen_destino = factor(`Origen / Destino`),
      aerolinea = factor(`Aerolinea Nombre`),
      calidad_dato = factor(`Calidad dato`),
      aeronave = factor(Aeronave)
    )
}
anac_2019 <- limpiar_anac(anac_2019, 2019)
anac_2020 <- limpiar_anac(anac_2020, 2020)
anac_2021 <- limpiar_anac(anac_2021, 2021)
anac_2022 <- limpiar_anac(anac_2022, 2022)
anac_2023 <- limpiar_anac(anac_2023, 2023)
anac_2024 <- limpiar_anac(anac_2024, 2024)
anac_2025 <- limpiar_anac(anac_2025, 2025)
anac_completo <- bind_rows(anac_2019, anac_2020, anac_2021, anac_2022, anac_2023, anac_2024, anac_2025)
glimpse(anac_completo)

#sacar columnas duplicadas

anac_completo <- anac_completo |>
  select(-c(`Clase de Vuelo (todos los vuelos)`, `Clasificación Vuelo`, `Tipo de Movimiento`, Aeropuerto, `Origen / Destino`, `Aerolinea Nombre`, `Calidad dato`, Aeronave))
glimpse(anac_completo)

vuelos_anio <- anac_completo |>
  group_by(anio) |>
  summarise(vuelos = n()) |>
  arrange(anio)
ggplot(vuelos_anio, aes(x = anio, y = vuelos)) + 
  geom_line() +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3)
  )+
  geom_point() +
  labs(title = "Cantidad de vuelos por año (miles)", x = "Año", y = "Cantidad de vuelos") +
  theme_minimal()
 
#b- ¿Cuánto tiempo se tarda en recuperar flujos pre-pandemia?
#tomo 4 anios recuperar los flujos pre-pandemia

#c- ¿Diferencias en patrones de viaje?

vuelos_aeropuerto <- anac_completo |>
  group_by(anio, aeropuerto) |>
  summarise(vuelos = n()) |>
  arrange(anio, desc(vuelos)) |>
  group_by(anio) |>
  slice_head(n = 5)
ggplot(vuelos_aeropuerto, aes(x = anio, y = vuelos, fill = aeropuerto)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 aeropuertos por año", x = "anio", y = "Cantidad de vuelos") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Aeropuerto")
  
  
#aeroparque es mucho mas visitado luego de la pandemia, mientras que ezeiza se mantiene 
# con un volumen similar, lo que puede indicar que hubo menor flujo de vuelos internacionales




