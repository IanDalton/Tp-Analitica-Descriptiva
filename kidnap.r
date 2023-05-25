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

#showing all Kidnaping cases

kidnappings = data %>%
    filter(attacktype1_txt == "Hostage Taking (Kidnapping)")

#Showing a graph bar with the results of the kidnappings that the ransom was payed

kidnappings %>%
    filter(ransompaid > 0) %>%
    group_by(hostkidoutcome_txt) %>%
    filter(!hostkidoutcome_txt %in% c("Unknown", NA)) %>%
    count() %>%
    ggplot(aes(x = hostkidoutcome_txt, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        x = "Resultado",
        y = "Cantidad de secuestros",
        title = "Resultado de los secuestros en los que se pagó rescate",
        caption = "NOTA: Se descartaron los casos en los que no estaba marcado el resultado"
    )
    

