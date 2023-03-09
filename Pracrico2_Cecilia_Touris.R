# CARGANDO ARCHIVO 
setwd("~/POSGRADO - CIENCIA DE DATOS/DATA SCIENCE PARA EMPRESAS")

# MAGENES Y RESETEAR ENTORNO GRAFICO
par(mar=c(0,0,0,0))
dev.off()

df <- read.table(file="House_Rent_Dataset.csv", header=T, sep = ",")
View(df)
str(df)
dim(df)
summary(df)

# ELIMINAR NA

!is.na(df)
df = na.omit(df)
View(df)

# CONVERSION DE DATOS
df$Floor = as.factor(df$Floor) ##
df$Area.Type = as.factor(df$Area.Type)
df$Area.Locality = as.factor(df$Area.Locality) ##
df$City = as.factor(df$City)
df$Furnishing.Status = as.factor(df$Furnishing.Status)
df$Tenant.Preferred = as.factor(df$Tenant.Preferred) ####
df$Point.of.Contact = as.factor(df$Point.of.Contact) ##

#OUTLIERS
#Rent
outliers = boxplot(df$Rent)$out
outliers
which(df$Rent %in% outliers)
df[which(df$Rent %in% outliers),]
df <- df[-which(df$Rent %in% outliers),]
boxplot(df$Rent)
#Bathroom
outliers = boxplot(df$Bathroom)$out
outliers
which(df$Bathroom %in% outliers)
df[which(df$Bathroom %in% outliers),]
df <- df[-which(df$Bathroom %in% outliers),]
boxplot(df$Bathroom)
#Size
outliers = boxplot(df$Size)$out
outliers
which(df$Size %in% outliers)
df[which(df$Size %in% outliers),]
df <- df[-which(df$Size %in% outliers),]
boxplot(df$Size)
#BHK
outliers = boxplot(df$BHK)$out
outliers
which(df$BHK %in% outliers)
df[which(df$BHK %in% outliers),]
df <- df[-which(df$BHK %in% outliers),]
boxplot(df$BHK)


# ANALISIS EXPLORATORIO

library(ggplot2)
library(lattice)
library(caret)
library(gapminder)
library(tidyverse)
library(dplyr)


# 1.3 BOXPLOT

df$BHK = as.factor(df$BHK)
ggplot(df,aes(y = Rent, fill = Rent, color = BHK)) + geom_boxplot()
df$Size = as.factor(df$Size)
ggplot(df,aes(y = Rent, fill = Rent, color = Size)) + geom_boxplot()
df$Bathroom = as.factor(df$Bathroom)
ggplot(df,aes(y = Rent, fill = Rent, color = Bathroom)) + geom_boxplot()
df$BHK = as.numeric(df$BHK)
df$Size = as.numeric(df$Size)
df$Bathroom = as.numeric(df$Bathroom)


ggplot(df,aes(y = Rent, fill = Rent, color = City)) + geom_boxplot()                      
ggplot(df,aes(y = Rent, fill = Rent, color = Area.Type)) + geom_boxplot() 
ggplot(df,aes(y = Rent, fill = Rent, color = Furnishing.Status)) + geom_boxplot() 
ggplot(df,aes(y = Rent, fill = Rent, color = Tenant.Preferred)) + geom_boxplot() 
ggplot(df,aes(y = Rent, fill = Rent, color = Point.of.Contact)) + geom_boxplot()


# 1.4 HISTOGRAM
p <- ggplot(df, aes(x=Size)) +  geom_histogram(bins=10)
p
q <- ggplot(df, aes(x=Rent)) +  geom_histogram(bins=10)
q
ggplot(df,aes(x = Rent, fill = Rent, color = City)) + geom_histogram(bins = 50) #City     
ggplot(df,aes(x = Rent, fill = Rent, color = Area.Type)) + geom_histogram(bins = 50) #Area.Type 
ggplot(df,aes(x = Rent, fill = Rent, color = Furnishing.Status)) + geom_histogram(bins = 50) #Furnishing.Status


# GEOMBAR

ggplot(df,aes(x = Rent, y = Bathroom, fill = Rent, color = City)) + geom_bar(stat="identity")
ggplot(df,aes(x = Floor, y = Rent, fill = Rent)) + geom_bar(stat="identity")
ggplot(df,aes(x = Area.Locality, y = Rent, fill = Rent)) + geom_bar(stat="identity")


# DENSITY 

ggplot(df,aes(x = Size,fill = City)) + geom_density(alpha=0.35)
ggplot(df,aes(x = Rent,fill = City)) + geom_density(alpha=0.35)


#EXPLORACION CON SCATTERPLOT
##BHK

x <- df$BHK
y <- df$Rent
plot(x, y, main = "BHK_Rent",
     xlab = "BHK", ylab = "Rent",
     pch = 5, frame = T)
# Agrego linea de regresion
plot(x, y, main = "BHK_Rent",
     xlab = "BHK", ylab = "Rent",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = df), col = "blue")
#Cambio la regresion
plot(x, y, main = "BHK_Rent",
     xlab = "BHK", ylab = "Rent",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "blue")

## Bathroom

x <- df$Bathroom
y <- df$Rent
plot(x, y, main = "Bathroom_Rent",
     xlab = "Bathroom", ylab = "Rent",
     pch = 5, frame = T)
# Agrego linea de regresion
plot(x, y, main = "Bathroom_Rent",
     xlab = "Bathroom", ylab = "Rent",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = df), col = "green")
#Cambio la regresion
plot(x, y, main = "Bathroom_Rent",
     xlab = "Bathroom", ylab = "Rent",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "green")


## Size

x <- df$Size
y <- df$Rent
plot(x, y, main = "Size_Rent",
     xlab = "Size", ylab = "Rent",
     pch = 5, frame = T)
# Agrego linea de regresion
plot(x, y, main = "Size_Rent",
     xlab = "Size", ylab = "Rent",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = df), col = "red")
#Cambio la regresion
plot(x, y, main = "Size_Rent",
     xlab = "Size", ylab = "Rent",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "red")


library(PerformanceAnalytics)
chart.Correlation(df[2:4])


# CONCLUSIÓN ANALISIS EXPLORATORIO: 
#1- sacar las variables: Posted.On, Floor, Area.Type y Area.Locality dado a que 
#son variables que no aportan mucho al modelo (Posted.On y Area.Type) o que son muy complejas
#de trabajar por su cantidad enorme de valores diferentes (Floor y Area.Locality)
#2- normalizar la variable Rent aplicando un logaritmo de datos de manera que el modelo funcione mejor
#3- las variables BHK y Bathroom estan muy corelacionadas asi que sacaré alguna de las dos en caso de 
#utilizar un modelo que asi lo requiera. En caso de usar Random Forest, las dejaré a ambas.


#1- 
df_clean = df[,c(-1,-5,-6,-7)]
View(df_clean)


# 2-
df_clean$Rent <- log(df_clean$Rent)
q <- ggplot(df, aes(x=Rent)) +  geom_histogram(bins=60)
q

#3-
df_clean_NoBHK = df_clean[,c(-1)]
View(df_clean_NoBHK)




# Preprocesamiento

# Vimos dos formas 

# 1: Funcion preProcess de Caret

preprocessParams <- preProcess(df_clean[,c(1,2,3,7)], method=c("center", "scale"))

print(preprocessParams)

transformed <- predict(preprocessParams, df_clean[,c(1,2,3,7)])

summary(transformed)


# Separar datos para entrenar y testear: 2 formas

# 1: Separar entre train y test

data_index = createDataPartition(df_clean_NoBHK$Rent,p=0.80,list = FALSE)

data_test <- df_clean_NoBHK[-data_index, ]
data_train <- df_clean_NoBHK[data_index, ]



# Entrenar el algoritmo
library(randomForest)

fit_rf = train(Rent~.,data=data_train, method = "rf", metric = "RMSE")
predictions_rf = predict(fit_rf, data_test$Rent)

library(xgboost)

fit_xgbLinear = train(Rent~.,data=data_train, method = "xgbLinear", metric = "RMSE")
predictions_fit_xgbLinear = predict(fit_xgbLinear, data_test$Rent)

library(adaboost)

fit_ranger = train(Rent~.,data=data_train, method = "ranger", metric = "RMSE")
predictions_fit_ranger = predict(fit_ranger, data_test$Rent)

str(df_clean)







