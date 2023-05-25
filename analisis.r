# Reading Excel file
library(readxl)
library(dplyr)
library(ggplot2)
library(tydiverse)
library(extrafont)
loadfonts(device = "win")
setwd("S:/Github/Tp-Analitica-Descriptiva")
data <- read_excel("dataset terrorismo.xlsx")

# Showing the amount of attacks by month

data %>%
    group_by(imonth) %>%
    filter(imonth != 0) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = imonth, y = n)) +
    geom_col() +
    labs(
        x = "Mes",
        y = "Cantidad de ataques",
        title = "Cantidad de ataques por mes",
        subtitle = "Testeado por ANOVA"
    )

# Showing the amount of attacks by day

data %>%
    group_by(iday) %>%
    filter(iday != 0) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = iday, y = n)) +
    geom_col() +
    labs(
        x = "Día",
        y = "Cantidad de ataques",
        title = "Cantidad de ataques por día",
        subtitle = "Testeado por ANOVA"
    )



data_us <- data %>%
    filter(country_txt == "United States")

# Showing the evolution of the attacks in the US over time

data_us %>%
    group_by(iyear) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = iyear, y = n)) +
    geom_line() +
    labs(
        x = "Año",
        y = "Cantidad de ataques",
        title = "Cantidad de ataques en Estados Unidos",
        subtitle = "Testeado por ANOVA"
    )

# Testing for mean difference between the attacks over the years in the US

data_us %>%
    group_by(iyear) %>%
    summarise(n = n()) %>%
    aov(n ~ iyear, data = .) %>%
    summary()

# Showing the amount of attacks divided by groups (gname)

data %>%
    group_by(gname) %>%
    summarise(n = n()) %>%
    filter(gname != "Unknown") %>%
    arrange(desc(n)) %>%
    top_n(10) %>%
    ggplot(aes(x = reorder(gname, -n), y = n, fill = as.factor(gname))) +
    geom_col() +
    labs(
        x = "Grupo",
        y = "Cantidad de ataques",
        title = "Cantidad de ataques por grupo",
        subtitle = "Testeado por ANOVA",
        caption = "Nota: Se descartaron los ataques causados por grupos desconocidos y se muestra el top 10",
        fill = "Nombre del grupo"
    ) +
    theme(axis.text.x = element_blank())

# Doing a chow test to see if there is a structural break in the amount of attacks in the US

data_us %>%
    group_by(iyear) %>%
    summarise(n = n()) %>%
    chow.test(n ~ iyear, data = .)

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
