# Install necessary packages if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(sf)) install.packages("sf")
if (!require(rnaturalearth)) install.packages("rnaturalearth")
if (!require(rnaturalearthhires)) {
  install.packages("devtools")
  devtools::install_github("ropensci/rnaturalearthhires")
}
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(readr)
library(dplyr)

# Load Switzerland outline (large scale)
switz <- ne_countries(scale = "large", country = "Switzerland", returnclass = "sf")
cantons <- ne_states(country = "Switzerland", returnclass = "sf")

# Load your point data
df <- read.csv2("C:/Users/js22i117/OneDrive - Universitaet Bern/Dokumente/IVA_Projekt/Data_processing/SIV_farms/01_oridata/Map_data.csv")
colnames(df) <- c("lat", "lon", "label")
df$label <- as.numeric(df$label)
df$lat <- as.numeric(df$lat)
df$lon <- as.numeric(df$lon)

# Create the plot object
p <- ggplot(data = switz) +
  geom_sf(fill = "white", color = "black", linewidth = 1) +
  geom_sf(data = cantons, fill = NA, color = "gray40", linewidth = 0.5) +
  geom_point(
    data = df,
    aes(x = lon, y = lat, color = as.factor(label)),
    size = 2,
    alpha = 0.5,
    show.legend = FALSE,
    position = position_jitter(width = 0.01, height = 0.01)
  ) +
  scale_color_manual(values = c("green", "red")) +
  coord_sf(xlim = c(5.8, 10.5), ylim = c(45.7, 47.9), expand = FALSE) +
  theme_void()

# Export as SVG
ggsave(
  filename = "switzerland_points.svg",
  plot = p,
  device = "svg",
  width = 8,
  height = 6,
  units = "in"
)