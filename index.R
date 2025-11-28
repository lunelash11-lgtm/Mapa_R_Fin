library(leaflet)
library(dplyr)
library(htmlwidgets)  # Para guardar el mapa como HTML

# ================================
# 🌍 LISTA COMPLETA DE LUGARES
# ================================
lugares <- data.frame(
  nombre = c(
    "Santorini (Grecia)", "Parque del Café (Colombia)", "Coliseo Romano (Roma)",
    "La Muralla China (China)", "Machu Picchu (Perú)", "Tokio (Japón)",
    "Alaska (EE.UU)", "Salto del Ángel (Venezuela)", "Mar Muerto (Israel/Jordania)",
    "Ejército de Terracota (China)", "Hawái (EE.UU)", "Danco Coast (Antártida)",
    "Stonehenge (Reino Unido)", "Pirámide de Giza (Egipto)", "Caño Cristales (Meta, Colombia)",
    "Desierto de Tatacoa (Huila, Colombia)", "Taj Mahal (India)", "Laguna Azul (Islandia)",
    "Big Ben (Reino Unido)"
  ),
  lat = c(
    37.65647222, 4.543055556, 41.89022222, 40.432, -13.17413889,
    35.68711111, 63.81111111, 5.970138889, 31.57655556,
    34.38436111, 19.87408333, -64.69566667, 51.17902778,
    29.97752778, 2.264277778, 3.2325, 27.17483333,
    63.87980333, 51.50075
  ),
  lon = c(
    25.53125, -75.77063889, 12.49222222, 116.5703056, -72.5415,
    139.7600833, -154.5152222, -62.53627778, 35.47658333,
    109.2783611, -159.2949167, -61.99977778, -1.826166667,
    31.13252778, -72.79444444, -75.16866667, 78.04208333,
    -22.44452778, -0.124583333
  )
)


lugares <- lugares %>%
  mutate(
    continente = case_when(
      grepl("Colombia|Venezuela|EE.UU|Perú", nombre) ~ "América",
      grepl("Grecia|Roma|Reino Unido|Egipto|Islandia", nombre) ~ "Europa",
      grepl("China|Japón|India", nombre) ~ "Asia",
      grepl("Israel|Jordania", nombre) ~ "Asia",
      grepl("Antártida", nombre) ~ "Antártida",
      TRUE ~ "Otros"
    )
  )

# Colores por continente
pal <- colorFactor(
  palette = c("purple", "blue", "red", "green", "orange"),
  levels = c("América", "Europa", "Asia", "Antártida", "Otros")
)


map <- leaflet(lugares) |> 
  addProviderTiles("CartoDB.Positron") |> 
  addAwesomeMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~paste0(
      "<b style='font-size:16px;'>", nombre, "</b><br><br>",
      "🌍 <b>Lat:</b> ", lat, "<br>",
      "🌍 <b>Lon:</b> ", lon, "<br><br>",
      "<img src='https://source.unsplash.com/400x250/?", nombre, "' width='250'/>"
    ),
    icon = ~makeAwesomeIcon(
      icon = "star",
      markerColor = "darkpurple",
      iconColor = "white"
    )
  ) |>
  addPolylines(
    lng = lugares$lon,
    lat = lugares$lat,
    color = "darkmagenta",
    weight = 4,
    opacity = 0.7
  ) |>
  addCircles(
    lng = ~lon, lat = ~lat,
    color = ~pal(continente),
    opacity = 0.7,
    fillOpacity = 0.2,
    radius = 200000
  ) |>
  setView(lng = 0, lat = 20, zoom = 2)


htmlwidgets::saveWidget(map, "mi_mapa_creativo.html", selfcontained = TRUE)

map
