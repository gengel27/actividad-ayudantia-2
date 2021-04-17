pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)
getwd()
datos <- read.csv("/Users/gabrielengel/Downloads/Ayudantia_DataMining01_2021-main 2/Ayudantia 2/Primer_Tiempo2020.csv")
datos
install.packages("tidyverse")
install.packages("utf8")
install.packages("lmtest")
library(lmtest)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(utf8)

summary(datos)
datos<- datos[,!(colnames(datos) %in% c("id_partido", "fasepartido", "local", "tiempo","formationUsed", "torneo"))]
datos

datos_ac_pases <- datos[order(datos$accuratePass, decreasing = TRUE),]
datos_ac_pases

datos_pases = datos_ac_pases[,colnames(datos) %in% c("equipo", "partido", "accuratePass", "totalPass", "precision_pases")]
datos_pases = datos_pases[order(datos_pases$precision_pases, decreasing = TRUE),]

datos_tiros <- NULL
datos_tiros = datos_ac_pases[,colnames(datos) %in% c("equipo", "partido", "goals", "ontargetScoringAtt", "totalScoringAtt", "blockedScoringAtt", "shotOffTarget", "precision_tiros")]
datos_tiros = datos_tiros[order(datos_tiros$goals, decreasing = TRUE),]
datos_tiros

cobresal <- filter(datos, equipo == "Cobresal")
cobresal_tiros <- filter(datos_tiros, equipo == "Cobresal")
cobresal_pases <- filter(datos_pases, equipo == "Cobresal")



cobresal_pases <- cobresal_pases[,!(colnames(cobresal_pases) %in% c("equipo"))] 

Promedios_Pas <- c("Promedio Pases",mean(cobresal_pases$accuratePass),mean(cobresal_pases$totalPass),mean(cobresal_pases$precision_pases))
cobresal_pases <- rbind(cobresal_pases, Promedios_Pas)

Max_Pas <- c("Max Pases",max(cobresal_pases$accuratePass),max(cobresal_pases$totalPass),max(cobresal_pases$precision_pases))
cobresal_pases <- rbind(cobresal_pases, Max_Pas)

Min_Pas <- c("Min Pases",min(cobresal_pases$accuratePass),min(cobresal_pases$totalPass),min(cobresal_pases$precision_pases))
cobresal_pases <- rbind(cobresal_pases, Min_Pas)
cobresal_pases

pases_cobresal <- cobresal$accuratePass
cobresal2 <- cobresal[order(cobresal$accuratePass, decreasing = FALSE),]

dotchart(cobresal$totalPass, labels = utf8_encode(cobresal$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
dotchart(cobresal$accuratePass, labels = utf8_encode(cobresal$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
dotchart(cobresal$totalPass, labels = utf8_encode(cobresal$partido), cex=0.5, xlab = "Pases", ylab = "Partido")
dotchart(cobresal$totalPass, labels = utf8_encode(cobresal$partido), main="Pases Acertados Cobresal", pch = 16, col=c("darkblue","dodgerblue"),lcolor="gray90", cex=0.8, xlab = "Pases", ylab = "Partido", cex.main=2,cex.lab=1.5)

texto <- datos$partido
texto <- char_tolower(texto)
texto <- iconv(texto, to = "ASCII//TRANSLIT")

a <- dfm(texto, remove = c(stopwords("es"), "vs", "Universidad"))
dim(a)


