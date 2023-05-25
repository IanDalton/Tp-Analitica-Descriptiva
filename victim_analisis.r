# Reading Excel file
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

loadfonts(device = "win")
setwd("S:/Github/Tp-Analitica-Descriptiva")
data <- read_excel("dataset terrorismo.xlsx")

colors <- c(
    "#F8766D", "#CD9600", "#53B400",
    "#00C094", "#00B6EB", "#A58AFF", "#FB61D7",
    "#FFA600", "#E76BF3", "#9590FF", "#FF62BC",
    "#7CAE00", "#00BF7D", "#F564E3",
    "#FF9E6E", "#C77CFF", "#00BA38"
)

# Analizing the target type

data %>%
    group_by(targtype1_txt) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(targtype1_txt, -nAttacks), y = nAttacks)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Tipo de objetivo",
        y = "Cantidad de ataques",
        title = "Top 5 tipos de objetivos más atacados",
    )
# From private citizens and property, the top 5 subtargets are:

data %>%
    filter(targtype1_txt == "Private Citizens & Property") %>%
    group_by(targsubtype1_txt) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(targsubtype1_txt, -nAttacks), y = nAttacks)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Subtipo de objetivo",
        y = "Cantidad de ataques",
        title = "Top 5 subtipos de objetivos más atacados",
    )
# Heatmap per country that shows the most attacked target type per country



world <- ne_countries(scale = "medium", returnclass = "sf")
world$region <- world$name

mapdata <- left_join(world, data %>%
    group_by(country_txt, targtype1_txt) %>%
    mutate(country_txt = ifelse(country_txt == "United States", "USA", country_txt)) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    group_by(country_txt) %>%
    slice(1),
by = c("region" = "country_txt"), sort = FALSE
)

ggplot() +
    geom_sf(data = mapdata, aes(fill = targtype1_txt), color = "black") +
    coord_sf() +
    scale_fill_manual(values = colors) +
    labs(
        x = "Longitud",
        y = "Latitud",
        title = "Tipo de objetivo más atacado por país",
        fill = "Tipo de objetivo"
    )
