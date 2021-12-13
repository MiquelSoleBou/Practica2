

setwd("C:\\Users/mique/Downloads")

library(dplyr)
library(data.table)
library(lubridate)
require(glue)

#--------------------- Read inputs -----------------------------------------
WineQuality <- read.csv("winequality-red.csv", header = TRUE, blank.lines.skip = TRUE) 

print(head(WineQuality))

# Comprovem si els diferents camps del Dataset contenen nuls:
WineQuality2 <- apply(WineQuality, 2, function(x) any(is.na(x)))
WineQuality2

# Hem vist que no. En el 

# Comprovem si contenen 0s:
WineQuality3 <- apply(WineQuality, 2, function(x) any(x==0))
WineQuality3

maximAcidNitric<-max(WineQuality$citric.acid)


# Establim un criteri per determinar outliers de +/- 2 desviacions mitjanes


for (col in 1:ncol(WineQuality)){
  
  # Emmagatzemem el nom de la columna en una variable
  nomCol <- names(WineQuality)[col]
  
  # Creem el nom de la nova columna
  outlierColumnName <- paste("Outlier.", nomCol, sep="")
  
  # Apliquem un TRUE/FALSE a la nova columna creada, en funció de si sobrepassa les dues SD
  WineQuality[,outlierColumnName] <- abs(scale(WineQuality[,nomCol])) > 2
  
  # Emmagatzemem els noms de les columnes en una llista
  nomsColumnes <- NULL
  
  nomsColumnes <- append(nomsColumnes, outlierColumnName)
  
}

outlierColumnName

nomsColumnes

# Comprovem el percentatge de files que es desvien per cada variable
ifelse( WineQuality$WithinHorizon[row] == 1, max( NettingTable$Forecast[row],NettingTable$Orders[row] ), NettingTable$Forecast[row] )

ifelse(WineQuality)

>#Criterio distancia Mahalanobis (los dos outliers más extremos)
  
n.outliers <- 2

m.dist.order <- order(mahalanobis(WineQuality, colMeans(WineQuality), cov(WineQuality)), decreasing=TRUE)
is.outlier <- rep(FALSE, nrow(WineQuality))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16

pch

plot(WineQuality, pch=pch)


