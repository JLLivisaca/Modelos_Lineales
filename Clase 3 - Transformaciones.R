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
modelo<-lm(produccion~altitud) # Y = Producción, X=Altitud
summary(modelo)
# Gráfica de dispersión
plot(altitud,produccion, pch=16,
     main="Diagrama de dispersión, Altitud vs Producción", col="blue" )
abline(modelo, col="red")
# se observa que la tendencia es no lineal y que 
# cuando x es bajo, hay mas dispersion de Y en la gráfica

# Ahora ¿cómo están los residuos?
res<-modelo$residuals
plot(res~produccion, pch=16, col="black");abline(h=0, col="red")

# vemos que el grafico de residuos presenta tendencia, como una U
# por tanto podemos intuir que no hay normalidad, 
# y la dispersion de errores no es constante

hist(altitud)# no existe normalidad
# ¿qué hacemos?

# Vamos a transformar los datos

#### DATOS TRANSFORMADOS DEL MODELO: Transformacion Log ####
x = altitud 
y = produccion
yt<-log(y) # Acá transformamos los datos de altitud

#### Modelo de regresión con Variable Altitud transformada ####
modelot<-lm(yt~x) 
summary(modelot) #mejoro el R cuadrado y el RSS
# R2 (modelo sin trandformar)=0.8816
# R2 (modelo transformado)= 0.981
par(mfrow=c(1,2))
plot(yt~x, main="Variable transformada");abline(modelot)#el modelo es ahora LINEAL
plot(y~x, main="Variable sin transformar")
abline(modelo)
par(mfrow=c(1,1))
# veamos los residuos
rest<-modelot$residuals
plot(rest~x, main="Variable transformada");abline(h=0) #vemos que ahora ya no hay tendencia en los residuos
plot(res~x, main="Variable sin transformar");abline(h=0)
#### DATOS TRANSFORMADOS DEL MODELO: Transformacion Raíz ####
#Ejemplo: e está analizando la contratación de un servicio de limpieza para ciertas oﬁcinas. 
# El coste del servicio de limpieza depende del número de personas que se contraten. Para poder valo-
# rar el número de personas, se han tomado unos datos sobre servicios realizados en el pasado, en
# los cuales consta el número de habitaciones que se han podido limpiar junto al número de personas
# que realizaron el servicio. Los datos se encuentran en el ﬁchero " cleaning.txt".

library(readr)
datos <- read_table("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/cleaning.txt")
attach(datos)

y<-Rooms # habitaciones
x<-Crews # personas
#Modelo que relaciona el número de cuartos limpiados por las personas que limpian
modelo<-lm(y~x)
summary(modelo)
#Grafica modelo
library(ggplot2)
library(gridExtra)
ggplot(datos, aes(x = x, y = y)) +
  geom_point() +  # Puntos de datos
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(title = "Modelo Regresión",
       x = "Número de personas",
       y = "Número de habitaciones") + 
  annotate("text", x = Inf, y = 20, label = "Línea de regresión", hjust = 1, vjust = 1, color = "blue")+
  theme_classic()

#X es una variable de conteo y 
# Y tiene dispersion no constante
#cuando se tienen variables de conteo, se debe transformar Y y X
# esto de conteo se ve en la grafica
yt<-sqrt(y)
xt<-sqrt(x)
modelot<-lm(yt~xt)
summary(modelot)
plot(yt~xt, main="modelo de regresión");abline(modelot, col="blue") 
# hay mejoramiento de R cuadrado y del RSS

#### Graficas para comprobar homocedasticidad
residuos <- residuals(modelo)
# Crea un dataframe con los residuos estandarizados y los valores ajustados
datos_residuos <- data.frame(residuos, valores_ajustados = predict(modelo))
# Crea el gráfico de residuos estandarizados versus valores ajustados
p1=ggplot(datos_residuos, aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Línea horizontal en y = 0
  labs(title = "Gráfico de Residuos vs. Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos")
#Modelo Transformado
residuos_t <- residuals(modelot)
# Crea un dataframe con los residuos estandarizados y los valores ajustados
datos_residuos <- data.frame(residuos_t, valores_ajustados = predict(modelot))
# Crea el gráfico de residuos estandarizados versus valores ajustados
p2= ggplot(datos_residuos, aes(x = valores_ajustados, y = residuos_t)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Línea horizontal en y = 0
  labs(title = "Gráfico de Residuos vs. Valores Ajustados",
       x = "Valores Ajustados transformados",
       y = "Residuos transformados")
grid.arrange(p1, p2, ncol = 2)

#### DATOS TRANSFORMADOS DEL MODELO: Transformacion Box-Cox ####

# Instala y carga el paquete MASS si aún no está instalado
library(MASS)
y<-Rooms # habitaciones
x<-Crews # personas
# Aplica la transformación Box-Cox, se encuentra el valor óptimo de lambda
transformacion_boxcox <- boxcox(y ~ x, data = datos)
# Se puede ver en la gráfica cuál sería el lambda óptimo
# Muestra el valor óptimo de lambda
lambda_optimo <- transformacion_boxcox$x[which.max(transformacion_boxcox$y)]
print(lambda_optimo)
# lambda óptimo = 0.6666667
# Aplica la transformación Box-Cox con el lambda óptimo
#datos_transformados <- predict(transformacion_boxcox, lambda = lambda_optimo)
datos_transformados <- (datos$Rooms^lambda_optimo - 1) / lambda_optimo
yt<-datos_transformados
xt<-datos$Crews
modelo_boxcox<-lm(yt~xt)
summary(modelo_boxcox)


