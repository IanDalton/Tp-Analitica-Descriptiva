# Reading Excel file
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(stringr)
loadfonts(device = "win")
setwd("S:/Github/Tp-Analitica-Descriptiva")
data <- read_excel("dataset terrorismo.xlsx")


# Creating a heatmap of where the attacks happened and the amount of attacks in each country

world_coordinate <- map_data("world")

# Creating data_total to get the amount of attacks in each country
# But before ploting I remove the outliers in n

data_total <- data %>%
    group_by(country_txt, longitude, latitude) %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    count()
data_in_america <- data %>%
    filter(region %in% c(1, 2, 3)) %>%
    group_by(country_txt, longitude, latitude) %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    count()

ggplot() +
    geom_map(
        data = world_coordinate, map = world_coordinate,
        aes(long, lat, map_id = region),
        color = "black", fill = "lightyellow"
    ) +
    stat_summary_2d(
        data = data_total,
        aes(longitude, latitude, z = n),
        alpha = 1,
        fun = sum,
        binwidth = c(1, 1)
    ) +
    scale_fill_gradient(low = "green", high = "red")

# creating a heatmap of the amount of attacks in American continent
america_map <- map_data("world", region = c("Canada", "United States", "Mexico", "Central America", "South America"))

ggplot() +
    geom_map(
        data = america_map, map = america_map,
        aes(long, lat, map_id = region),
        color = "black", fill = "lightyellow"
    ) +
    stat_summary_hex(
        data = data_in_america,
        aes(longitude, latitude, z = n),
        alpha = 1,
        fun = sum,
        binwidth = c(2, 2),
        geom = "hex"
    )

mapdata <- map_data("world")


mapdata <- left_join(mapdata,
        data %>%
            group_by(country_txt) %>%
            count() %>%
            mutate( country_txt = if( country_txt == "United States"){"USA"}else{country_txt}),
        by = c("region" = "country_txt")
    )
mapdata1 <- mapdata %>%
    filter(!is.na(n))

ggplot(mapdata1, aes(long,lat,group = group)) +
    geom_map(
        data = world_coordinate, map = world_coordinate,
        aes(long, lat, map_id = region),
        color = "black", fill = "white"
    )+
    geom_polygon(aes(fill = log(n)), color = "black")+
    scale_fill_gradient(low = "green", high = "red")+
    labs(
        x = "Longitud",
        y = "Latitud",
        title = "Cantidad de ataques por país",
        fill = "Cantidad de ataques",
        caption = "NOTA: Se muestra el logaritmo de la cantidad de ataques por país"
    ) +
    theme(text = element_text(family = "Comic Sans MS"))
    
#creating a heatmap with the atackers

mapdata <- map_data("world")

mapdata <- left_join(mapdata,
        data %>%
            group_by(natlty1_txt) %>%
            filter(!is.na(natlty1_txt)) %>%
            count() %>%
            mutate( natlty1_txt = if( natlty1_txt == "United States"){"USA"}else{natlty1_txt}) %>%
            filter(!is.na(n)),
        by = c("region" = "natlty1_txt")
    )
ggplot(mapdata, aes(long,lat,group = group)) +
    geom_map(
        data = world_coordinate, map = world_coordinate,
        aes(long, lat, map_id = region),
        color = "black", fill = "white"
    )+
    geom_polygon(aes(fill = log(n)), color = "black")+
    scale_fill_gradient(low = "green", high = "red")+
    labs(
        x = "Longitud",
        y = "Latitud",
        title = "Cantidad de victimas por país",
        fill = "Cantidad de victimas",
        caption = "NOTA: Se muestra el logaritmo de la cantidad de victimas por país"
    )

# Creating a heatmap with the amount of victims in each country

mapdata <- map_data("world")

mapdata <- left_join(mapdata,
        data %>%
            group_by(natlty1_txt) %>%
            filter(!is.na(natlty1_txt)) %>%
            summarise(n = sum(nkill)) %>%
            mutate( natlty1_txt = if( natlty1_txt == "United States"){"USA"}else{natlty1_txt}) %>%
            filter(!is.na(n)),
        by = c("region" = "natlty1_txt")
    )

ggplot(mapdata, aes(long,lat,group = group)) +
    geom_map(
        data = world_coordinate, map = world_coordinate,
        aes(long, lat, map_id = region),
        color = "black", fill = "white"
    )+
    geom_polygon(aes(fill = log(n)), color = "black")+
    scale_fill_gradient(low = "green", high = "red")+
    labs(
        x = "Longitud",
        y = "Latitud",
        title = "Cantidad de victimas por país",
        fill = "Cantidad de victimas",
        caption = "NOTA: Se muestra el logaritmo de la cantidad de victimas por país"
    )