#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Febrero 2024
#### Iniciamos ####
# Se está realizando un estudio sobre la producción de madera que se obtiene de cierta especie
# arbórea. Esta especie alcanza la mayor producción en zonas costeras o de baja altitud, mientras
# que la producción es menor en zonas más elevadas. Para constatar este hecho y evaluarlo de
# manera empírica, se registran las producciones de treinta parcelas semejantes situadas a diferentes
# altitudes. Los datos se encuentran disponibles en el ﬁchero "madera.txt".

# Ingreso de datos
library(readr)
madera <- read_table("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/madera.txt")
attach(madera)
#Modelo de regresión
modelo<-lm(produccion~altitud)
summary(modelo)
# Gráfica de dispersión
plot(produccion,altitud,pch=16,
     main="Diagrama de dispersión, Altitud vs Producción", col="blue" )
abline(modelo, col="red")
# se observa que la tendencia es no lineal y que 
# cuando x es bajo, hay mas dispersion de Y en la gráfica

# Ahora ¿cómo están los residuos?
res<-modelo$residuals
plot(res~produccion, pch=16, col="black");abline(h=0, col="red")

# vemos que el grafico de residuos presenta tendencia, como una U
# por tanto no hay normalidad, 
# y la dispersion de errores no es constante

hist(y);rug(y)# no existe normalidad

#DATOS TRANSFORMADOS DEL MODELO: Transformacion Log

yt<-log(y)
modelot<-lm(yt~x)
summary(modelot) #mejoro el R cuadrado y el RSS

par(mfrow=c(1,2))
plot(yt~x);abline(modelot)#el modelo es ahora LINEAL

plot(y~x)
abline(modelo)
# veamos los residuos
rest<-modelot$residuals
plot(rest~x);abline(h=0) #vemos que ahora ya no hay 
# tendencia en los residuos
