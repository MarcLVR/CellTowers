library(leaflet);library(sf)
library(ggplot2);library(dplyr)
library(units);library(htmlwidgets)
library(giscoR);library(rnaturalearthdata)
library(scales);library(rnaturalearth)
library(plotly);library(quarto)

df2 <- read.csv("C:/Users/marc/Desktop/UOC/DataViz/P2/Africa towers.csv")
df_egypt <- df2[df2$Country == "Egypt", ]
df_egypt <- df_egypt[df_egypt$LON >= 30.6 & df_egypt$LON <= 33.2, ]
df_egypt <- df_egypt[, !(names(df_egypt) %in% c("created", "updated", "averageSignal", "Continent"))]

rivers <- ne_download(scale = 10,
                      type = "rivers_lake_centerlines",
                      category = "physical",
                      returnclass = "sf")
# Filter all segments related to the Nile system
nile_parts <- rivers %>%
  filter(name %in% c(
    "Nile"
  ))

# Combine into a single multiline geometry
nile <- st_union(nile_parts)

# Convert towers to sf points (keeping original LAT and LON)
towers_sf <- st_as_sf(df_egypt, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

# Reproject both geometries to metric CRS for distance computation
towers_proj <- st_transform(towers_sf, 3857)
nile_proj <- st_transform(nile, 3857)

# Filter towers within 10 km of the Nile
nearby_index <- st_is_within_distance(towers_proj, nile_proj, dist = 10000)
towers_near_nile <- towers_proj[lengths(nearby_index) > 0, ]

# Drop geometry but keep original columns
towers_near_nile_df <- st_drop_geometry(towers_near_nile)

# Paso 1: Agrupar en intervalos
plot_rango <- towers_near_nile_df %>%
  mutate(RangeGroup = cut(RANGE,
                          breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf),
                          labels = c("0–500", "501–1000", "1001–2000", "2001–5000", "5001–10000", "10001+"),
                          right = TRUE)) %>%
  count(RangeGroup) %>%
  ggplot(aes(x = RangeGroup, y = n, text = paste("Número de torres:", n))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Número de torres por rango de cobertura",
       x = "Rango de cobertura (m)",
       y = "Número de torres") +
  theme_minimal()

# Convertir a gráfico interactivo
plot_rango <- ggplotly(plot_rango, tooltip = "text")



# Crear gráfico con ggplot2
plot_radio <- towers_near_nile_df %>%
  count(radio) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = reorder(radio, -percentage),
             y = percentage,
             text = paste0("Radio: ", radio,
                           "<br>Número de torres: ", n,
                           "<br>Porcentaje: ", percent(percentage, accuracy = 0.1)))) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Porcentaje de torres por tipo de radio",
       x = "Tipo de radio",
       y = "Porcentaje") +
  theme_minimal()

# Convertir a gráfico interactivo
plot_radio <- ggplotly(plot_radio, tooltip = "text")


plot_cid <- towers_near_nile_df %>%
  count(CID) %>%
  top_n(10, n) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = reorder(as.factor(CID), -percentage),
             y = percentage,
             text = paste0("CID: ", CID,
                           "<br>Número de torres: ", n,
                           "<br>Porcentaje: ", percent(percentage, accuracy = 0.1)))) +
  geom_bar(stat = "identity", fill = "purple") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Top 10 CIDs con más torres",
       x = "CID",
       y = "Porcentaje") +
  theme_minimal()

plot_cid   <- ggplotly(plot_cid, tooltip = "text")


plot_network <- towers_near_nile_df %>%
  count(Network) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = reorder(Network, -percentage),
             y = percentage,
             fill = Network,
             text = paste0("Operador: ", Network,
                           "<br>Número de torres: ", n,
                           "<br>Porcentaje: ", percent(percentage, accuracy = 0.1)))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Porcentaje de torres por operador de red",
       x = "Operador",
       y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta leyenda redundante

plot_network <- ggplotly(plot_network, tooltip = "text")

