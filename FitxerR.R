

setwd("C:\\Users/mique/Downloads")

library(dplyr)
library(data.table)
library(lubridate)
require(glue)

#--------------------- Read inputs -----------------------------------------
WineQuality <- read.csv("winequality-red.csv", header = TRUE, blank.lines.skip = TRUE) 

print(head(WineQuality))

summary(WineQuality)

# Comprovem si els diferents camps del Dataset contenen nuls:
WineQuality2 <- apply(WineQuality, 2, function(x) any(is.na(x)))
WineQuality2

# Hem vist que no. En el 

# Comprovem si contenen 0s:
WineQuality3 <- apply(WineQuality, 2, function(x) any(x==0))
WineQuality3

maximAcidNitric<-max(WineQuality$citric.acid)

# ==============================================
# DETECCIÓ I SUBSTITUCIÓ D'OUTLIERS
# ==============================================

# Establim un criteri per determinar outliers de +/- 2 desviacions mitjanes

# Inicialitzem la llista de noms de columnes
nomsColumnes <- NULL

for (col in 1:ncol(WineQuality)){
  
  # Emmagatzemem el nom de la columna en una variable
  nomCol <- names(WineQuality)[col]
  
  #print(nomCol)
  
  # Creem el nom de la nova columna
  outlierColumnName <- paste("Outlier.", nomCol, sep="")
  
  # Emmagatzemem els noms de les columnes en una llista
  nomsColumnes <- append(nomsColumnes, outlierColumnName)
  
  # Apliquem un TRUE/FALSE a la nova columna creada, en funció de si sobrepassa les tres SD
  WineQuality[,outlierColumnName] <- ifelse( abs(scale(WineQuality[,nomCol])) > 3, 1, 0)
  
  # Calculem la mitjana excloent els Outliers
  WithoutOutliers <- filter(WineQuality, WineQuality[,outlierColumnName] == 0)
  
  avg = mean(WithoutOutliers[,nomCol])

  # Substituim els valors extrems per la mitjana
  WineQuality[,nomCol] <- ifelse( abs(scale(WineQuality[,nomCol])) > 3, avg, WineQuality[,nomCol])
  
}

# ========================================================
# COMPROVACIÓ DEL VOLUM D'OUTLIERS OBTINGUTS
# ========================================================
# Comprovem el percentatge de files que es desvien per cada variable, segons el nombre de desviacions mitjanes que establim
# com a llindar pel que considerem Outlier
WineQualityOutliers <- WineQuality %>% dplyr:: select(starts_with("Outlier"))

totalRows <- count(WineQualityOutliers) 

outlierPercentages = data.frame()

# Per cada columna, generem una columna percentatge
for (col in 1:ncol(WineQualityOutliers)){
  
  # Emmagatzemem el nom de la columna en una variable
  nomCol <- names(WineQualityOutliers)[col]
  
  suma <- length(WineQualityOutliers[,nomCol][WineQualityOutliers[,nomCol]==TRUE])
  
  outlierPercentages[1,nomCol] <- (suma / totalRows)*100
  
}

# Sumem totes les columnes indicadores d'outliers en un nou DF
DFSuma <- WineQualityOutliers %>% transmute(suma = Outlier.fixed.acidity + Outlier.volatile.acidity + Outlier.citric.acid + Outlier.residual.sugar + Outlier.chlorides
                                             + Outlier.free.sulfur.dioxide + Outlier.total.sulfur.dioxide + Outlier.density + Outlier.pH + Outlier.sulphates
                                             + Outlier.alcohol + Outlier.quality)

# COmprovem quin percentatge de registres queden afectats per algun outlier
NumOutliers = sum(DFSuma$suma, na.rm = TRUE)
PercentatgeOutliers = NumOutliers*100 / totalRows

# Imprimim el resultat
PercentatgeOutliers

# Ens apareix un 12% de registres que contenen algún Outlier, segons el tall de 3 desviacions mitjanes.
# Substituim cada valor extrem per la mitjana d'aquell atribut.





















