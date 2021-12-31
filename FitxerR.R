# ==============================================
# DEFINIM RUTA I IMPORTEM LLIBRERIES
# ==============================================
#setwd("C:\\Users/mique/Downloads")
setwd("C:\\Users/tuneu/Downloads")

library(dplyr)
library(data.table)
library(lubridate)
library(Hmisc)
library(HH)
library(corrplot)
library(ResourceSelection)
library(pROC)
library(DT)
require(glue)


# ==============================================
# LLEGIM LES DADES
# ==============================================
WineQuality <- read.csv("winequality-red.csv", header = TRUE, blank.lines.skip = TRUE) 

print(head(WineQuality))

summary(WineQuality)

# Comprovem si els diferents camps del Dataset contenen nuls:
WineQuality2 <- apply(WineQuality, 2, function(x) any(is.na(x)))
WineQuality2

# Hem vist que no en contenen.

# Comprovem si contenen 0s:
WineQuality3 <- apply(WineQuality, 2, function(x) any(x==0))
WineQuality3

maximAcidNitric<-max(WineQuality$citric.acid)

# ==============================================
# DETECCIÓ I SUBSTITUCIÓ D'OUTLIERS
# ==============================================

# Establim un criteri per determinar outliers de +/- 3 desviacions mitjanes

# Inicialitzem la llista de noms de columnes
nomsColumnes <- NULL

WineQualityColNames <- colnames(WineQuality)

for (col in 1:ncol(WineQuality)){
  
  # Emmagatzemem el nom de la columna en una variable
  nomCol <- names(WineQuality)[col]
  
  #print(nomCol)
  
  # Creem el nom de la nova columna
  outlierColumnName <- paste("Outlier.", nomCol, sep="")
  
  # Emmagatzemem els noms de les columnes en una llista
  nomsColumnes <- append(nomsColumnes, outlierColumnName)
  
  # Apliquem un TRUE/FALSE a la nova columna creada, en funció de si sobrepassa les 3 SD
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

WineQuality <- subset(WineQuality, select = WineQualityColNames)

# ========================================================
# COMPROVACIÓ DE NORMALITAT
# ========================================================

WineQuality$quality <- as.numeric(WineQuality$quality)

hist.data.frame(WineQuality)

qqnorm(WineQuality$density)
qqline(WineQuality$density)

qqnorm(WineQuality$pH)
qqline(WineQuality$pH)

lshap <- lapply(WineQuality, shapiro.test)

for(l in lshap){
  print(l)
}

shapiro.test(WineQuality$density)
shapiro.test(WineQuality$pH)



# ========================================================
# HOMOGENEÏTAT DE LA VARIÀNCIA
# ========================================================

lhov <- c()


hov(WineQuality$quality ~ WineQuality$alcohol)
hov(WineQuality$quality ~ WineQuality$volatile.acidity)
hov(WineQuality$quality ~ WineQuality$sulphates)
hov(WineQuality$quality ~ WineQuality$citric.acid)

# ========================================================
# CORRELACIÓ ENTRE VARIABLES
# ========================================================

correlation <- cor(WineQuality)
round(correlation, 2)
corrplot(correlation, type="upper", order = "hclust", tl.col="black", tl.srt=45)

#Veiem en la matriu de correlació que les variables que semblen afectar més la qualitat del vi són alcohol i volatile.acidity.
#Seguides de sulphates i citric.acid.


# ========================================================
# REGRESSIÓ LINEAL
# ========================================================

#Comencem fent regressió lineal amb la variable alcohol.

alcoholModel <- lm(quality ~ alcohol, WineQuality)
summary(alcoholModel)

#Els resultats ens mostren un coeficient de determinació de 0.2309, afegim la variable volatile.acidity al model i comprovem si ha millroat.

alcVolatileModel <- lm(quality ~ alcohol+volatile.acidity, WineQuality)
summary(alcVolatileModel)

#Amb aquesta modificació el coeficient de determinació millora 

alcVolSulphatesModel <- lm(quality ~ alcohol+volatile.acidity+sulphates, WineQuality)
summary(alcVolSulphatesModel)

alcVolSulCitricModel <- lm(quality ~ alcohol+volatile.acidity+sulphates+citric.acid, WineQuality)
summary(alcVolSulCitricModel)

tab <- matrix( c(summary(alcoholModel)$r.squared,summary(alcVolatileModel)$r.squared,summary(alcVolSulphatesModel)$r.squared,summary(alcVolSulCitricModel)$r.squared), ncol=1, byrow=TRUE  )
colnames(tab) <- c('Coeficient de determinació')
rownames(tab) <- c('Alcohol', 'Alcohol + Volatile','Alc + Vol + Sulphates','Alc + Vol + Sul + Citric')


datatable(tab)

plot(alcVolSulphatesModel, which=c(1,2))

# ========================================================
# REGRESSIÓ LOGÍSTICA
# ========================================================

#Es preté fer una classificació dels vins entre "bons" i "dolents", considerant com a bons tots aquells que superin
#la puntuació de 5 i de "dolents" tots aquells que tinguin una puntuació diferent a 5. Per dur a terme aquest anàlisi
#generarem diferents models de regressió logística.

WineQuality$quality_cat <- cut(WineQuality$quality, breaks=c(0,5,10), labels=c("dolent", "bo"))

regressionAlcModel <- glm(quality_cat ~ alcohol , data = WineQuality, family=binomial)
summary(regressionAlcModel)

exp(coefficients(regressionAlcModel))

hoslem.test(WineQuality$quality_cat, fitted(regressionAlcModel))

prob = predict(regressionAlcModel, WineQuality, type="response")
r=roc(WineQuality$quality_cat, prob, data=WineQuality)
plot(r)
auc(r)

regressionAlcVolModel <- glm(quality_cat ~ alcohol+ volatile.acidity , data = WineQuality, family=binomial)
summary(regressionAlcVolModel)

exp(coefficients(regressionAlcVolModel))

hoslem.test(WineQuality$quality_cat, fitted(regressionAlcVolModel))

prob = predict(regressionAlcVolModel, WineQuality, type="response")
r=roc(WineQuality$quality_cat, prob, data=WineQuality)
plot(r)
auc(r)


regressionAlcVolSulModel <- glm(quality_cat ~ alcohol+ volatile.acidity+sulphates, data = WineQuality, family=binomial)
summary(regressionAlcVolSulModel)

exp(coefficients(regressionAlcVolSulModel))

hoslem.test(WineQuality$quality_cat, fitted(regressionAlcVolSulModel))

prob = predict(regressionAlcVolSulModel, WineQuality, type="response")
r=roc(WineQuality$quality_cat, prob, data=WineQuality)
plot(r)
auc(r)

# ========================================================
# GUARDEM EL DATAFRAME EN CSV
# ========================================================

write.csv(WineQuality, "WineQuality-clean.csv", row.names=FALSE)
