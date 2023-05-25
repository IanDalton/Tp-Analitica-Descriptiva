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


data %>%
    group_by(region_txt, iyear) %>%
    count() %>%
    aov(n ~ region_txt, data = .) %>%
    summary()

#Testing for mean difference between the kidnappings over the years between the countries

data %>%
    filter(ishostkid == 1) %>%
    group_by(country_txt, iyear) %>%
    count() %>%
    aov(n ~ country_txt, data = .) %>%
    summary()

# Testing for mean difference between the kidnapping result over the years

data %>%
    filter(ishostkid == 1) %>%
    group_by(hostkidoutcome_txt, iyear) %>%
    count() %>%
    aov(n ~ hostkidoutcome_txt, data = .) %>%
    summary()

# Testing for mean difference between the kidnapping result over the years where the ransom was paid

data %>%
    filter(ishostkid == 1, !is.na(ransompaid)) %>%
    group_by(hostkidoutcome_txt, iyear) %>%
    count() %>%
    aov(n ~ hostkidoutcome_txt, data = .) %>%
    summary()

#Testing for the mean difference between the attack types over the years

data %>%
    group_by(attacktype1_txt, iyear) %>%
    count() %>%
    aov(n ~ attacktype1_txt, data = .) %>%
    summary()