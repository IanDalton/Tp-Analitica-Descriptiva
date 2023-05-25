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

# getting the top 5 groups that have claimed the most attacks and showing them as a table

data %>%
    filter(gname != "Unknown") %>%
    group_by(gname) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(gname, -nAttacks), y = nAttacks)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Grupo",
        y = "Cantidad de ataques",
        title = "Top 5 grupos con más ataques",
        caption = "NOTA: Se descartaron los casos en los que el grupo no fue identificado"
    )
#Testing for that
data %>%
    filter(gname != "Unknown") %>%
    group_by(gname, iyear) %>%
    summarise(nAttacks = n()) %>%
    aov(nAttacks ~ gname, data = .) %>%
    summary()

# getting the top 5 groups that have claimed the most victims and showing them as a table

data %>%
    group_by(gname) %>%
    summarise(nKills = sum(nkill)) %>%
    arrange(desc(nKills)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(gname, -nKills), y = nKills)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Grupo",
        y = "Cantidad de fallecidos",
        title = "Top 5 grupos con más fallecidos"
    )
#Testing for that
data %>%
    filter(nkill != 0) %>%
    group_by(gname, iyear) %>%
    summarise(nKills = sum(nkill)) %>%
    aov(nKills ~ gname, data = .) %>%
    summary()

data %>%
    group_by(gname) %>%
    summarise(nWound = sum(nwound)) %>%
    arrange(desc(nWound)) %>%
    head(5) %>%
    mutate(gname = str_wrap(gname, width = 10)) %>%
    ggplot(aes(x = reorder(gname, -nWound), y = nWound)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Grupo",
        y = "Cantidad de heridos",
        title = "Top 5 grupos con más heridos"
    )
#Testing for that
data %>%
    filter(nwound != 0) %>%
    group_by(gname, iyear) %>%
    summarise(nWound = sum(nwound)) %>%
    aov(nWound ~ gname, data = .) %>%
    summary()

# Showing the evolution over time of the top 5 groups with the most attacks

top_5 <- data %>%
    filter(gname != "Unknown") %>%
    group_by(gname) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    head(5)
data %>%
    filter(gname %in% top_5$gname) %>%
    group_by(iyear, gname) %>%
    summarise(nAttacks = n()) %>%
    arrange(desc(nAttacks)) %>%
    ggplot(aes(x = iyear, y = nAttacks, color = gname)) +
    geom_line() +
    labs(
        x = "Año",
        y = "Cantidad de ataques",
        title = "Evolución de los ataques de los 5 grupos con más ataques",
    )
# Piechart showing the atack types of Al-Qaida and Aum Shinri kyo

data %>%
    filter(gname == "Al-Qaida") %>%
    group_by(attacktype1_txt) %>%
    summarise(nAttacks = n()) %>%
    ggplot(aes(x = "", y = nAttacks, fill = attacktype1_txt)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    labs(
        x = "",
        y = "",
        title = "Tipos de ataques de Al-Qaida",
        fill = "Tipo de ataque"
    )
# Showing the same data but over time using bars
data %>%
    filter(gname == "Al-Qaida") %>%
    group_by(iyear, attacktype1_txt) %>%
    summarise(nAttacks = n()) %>%
    ggplot(aes(x = iyear, y = nAttacks, fill = attacktype1_txt)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Año",
        y = "Cantidad de ataques",
        title = "Evolución de los tipos de ataques de Al-Qaida",
        fill = "Tipo de ataque"
    )
# Now the same thing but for Aum Shinri kyo
data %>%
    filter(gname == "Aum Shinri Kyo") %>%
    group_by(attacktype1_txt) %>%
    summarise(nAttacks = n()) %>%
    ggplot(aes(x = "", y = nAttacks, fill = attacktype1_txt)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    labs(
        x = "",
        y = "",
        title = "Tipos de ataques de Aum Shinri Kyo",
        fill = "Tipo de ataque"
    )
# Showing the same data but over time using bars
data %>%
    filter(gname == "Aum Shinri Kyo") %>%
    group_by(iyear, attacktype1_txt) %>%
    summarise(nAttacks = n()) %>%
    ggplot(aes(x = iyear, y = nAttacks, fill = attacktype1_txt)) +
    geom_bar(stat = "identity") +
    labs(
        x = "Año",
        y = "Cantidad de ataques",
        title = "Evolución de los tipos de ataques de Aum Shinri Kyo",
        fill = "Tipo de ataque"
    )
