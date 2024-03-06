#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Marzo 2024
#### Iniciamos ####

#### Ejmemplo regresión múltiple: vinos ####
library(readr)
vinos <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/Vinos.csv")
attach(vinos)

# Modelo de Regresión Múltiple Sin interacción
mfull <- lm(Calidad ~ Cosecha+Fin_cosecha + Lluvia)
summary(mfull)
# Modelo regresión considerando interacción
mfull.interaccion <- lm(Calidad ~ Fin_cosecha + Lluvia+ Lluvia:Fin_cosecha)
summary(mfull.interaccion)

# Si deseo graficar los datos relizando segregación por si llovió o no.
# Grafico
vinos$Lluvia <- factor(vinos$Lluvia)
# Crear el gráfico de dispersión con ggplot y añadir líneas de regresión
ggplot(vinos, aes(x = Fin_cosecha, y = Calidad, color = Lluvia, shape = Lluvia)) +
  geom_point(size = 3) +  # Puntos de dispersión
  scale_shape_manual(values = c(16, 17)) +  # Define los símbolos
  labs(title = "Gráfico de dispersión de Calidad vs. Fin_cosecha",
       x = "Fin de cosecha",
       y = "Calidad") +
  theme_minimal() +
  geom_smooth(data = vinos[vinos$Lluvia == 1,], aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", color = "black", size = 1) +  # Línea de regresión para modelo1
  geom_smooth(data = vinos[vinos$Lluvia == 0,], aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, linetype = "solid", color = "blue", size = 1) +  # Línea de regresión para modelo2
  geom_text(aes(label = Lluvia), hjust = 1, vjust = -1) +  # Etiqueta de datos para identificar el color del punto
  geom_text(aes(x = 15, y = 5, label = "Línea de Regresión"), color = "black", size = 3)  # Etiqueta de datos para identificar el color de la línea de regresión

# ¿existirán outliers en el modelo?

# Para conocer esto, se realiza la prueba de Bonferroni
# Prueba de Bonferroni para detectar outliers
library(car)
outlierTest(mfull, cutoff=Inf, n.max=4) #n.max son las interaciones máximas que se visualizarán
influenceIndexPlot(mfull, vars="Bonf", las=1) # Grafica el pvalor de los puntos de la prueba
# Hipótesis nula (H0): No hay outliers en los datos. En otras palabras, todos los valores son consistentes con el modelo de regresión lineal 
# Hipótesis alternativa (H1): Hay al menos un outlier presente en los datos. Esto significa que al menos uno de los puntos de datos está demasiado alejado de la tendencia general del modelo de regresión lineal y puede afectar negativamente la precisión del modelo.
#  La observación ubicada en la línea 15 es la única con un valor-P muy pequeño 
#  y por lo tanto hay evidencias para considerar esa observación como un posible outlier

# Modelo con interacción
# Prueba de bonferroni
outlierTest(mfull.interaccion, cutoff=Inf, n.max=4)
influenceIndexPlot(mfull.interaccion, vars="Bonf", las=1)
#  La observación ubicada en la línea 17, 15 y 32 tienen un valor-P muy pequeño 
#  y por lo tanto hay evidencias para considerar esas observaciones como posibles outliers 
# Considerar el valor 17 como dato inicial para cambiar.

# Distancias de cook 
# Son puntos influyentes las observaciones que presenten Di=4/ n−p−2. 
# n = total de datos, p = variables
# Si una distancia de Cook es mucho mayor que 1, se considera que 
# la observación correspondiente tiene una influencia sustancial en el modelo
cooks.distance(mfull)
cooks.distance(mfull.interaccion)
#Forma grafica
cutoff <- 4/(44-3-2)  # Cota
plot(mfull, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="blue")

# Distancia de cook para modelo con interacción 
cutoff <- 4/(44-4-2)  # Cota
plot(mfull.interaccion, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="blue")

#### Regresión Polinómica ####
# Vamos a considerar la relación entre la concentración y la resistencia de cemento
conc <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
resis <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1, 
           52, 52.5, 48, 42.8, 27.8, 21.9)
datos <- data.frame(Concentración=conc,Resistencia=resis)
#Grafica
ggplot(datos, aes(x=Concentración, y=Resistencia)) + 
  geom_point() + theme_light()

# De este diagrama se ve claramente que hay una relación de tipo no lineal entre las variables. 
# ¿Será mejor un modelo de grado 2 que un modelo de grado 1?
library(car)


library(readr)
datos1 <- read_table2("C:/Users/Juan Llivisaca/OneDrive/Escritorio/Maestria Estadística/3 Modelos Lineales/Bases de datos/acaros.txt")
datos1 <- read_table("Maestria Estadística/3 Modelos Lineales/Bases de datos/acaros.txt")
attach(datos1)
datos1=as.data.frame(datos1)
#Diagrama de dispersion
plot(datos1)

#Polinomio de grado cero
modelo0<-lm(datos1$acaros~1)
abline(modelo0,lwd=2,lty=2)

#Polinomio de grado uno
modelo1<-lm(datos1$acaros~humedad)
summary(modelo1)
abline(modelo1,lwd=2,col="blue")
#EL AJUSTE NO ES BUENO, EL BETA CERO SE ANULA

#Polinomio de grado dos
humedad2=humedad*humedad # es la variable al cuadrado
modelo2<-lm(datos1$acaros~humedad+humedad2)
modelo2<-lm(datos1$acaros~humedad+I(humedad^2)) # es la mejor forma esta 
# el colocar el valor de I y poner al cuadrado I(humedad^2)
summary(modelo2)
beta=coef(modelo2)
curve(beta[1]+beta[2]*x+beta[3]*x^2,add=TRUE,lwd=2,col="red")
#LOS BETAS SON SIGNIFICATIVOS, EL AJUSTE ES MEJOR
#OJO- LA FUNCION CURVE ES PARA GRAFICAR FUNCIONES GENERICAS

#PREDICCION PARA UNA HUMEDAD DEL 80%
#hay que fijarse en las escalas de las variables (% o escala de 1)
Ypred0<-beta[1]+beta[2]*80+beta[3]*(80)^2 # cuando uso la variable ficticia de la linea 21
Ypred0
predict(modelo2,data.frame(humedad=80)) # OJO:solo funciona porque usamos el comando I en la regresion

#Polinomio de grado tres
modelo3<-lm(datos1$acaros~humedad+I(humedad^2)+I(humedad^3))
summary(modelo3)
beta=coef(modelo3)
curve(beta[1]+beta[2]*x+beta[3]*x^2+beta[4]*x^3,add=TRUE,lwd=2,lty=2,col="green")
#CUANDO SE SOBREESTIMAN LOS PARAMETROS SE PIERDEN SIGNIFICACION DE LOS BETAS.
#OBSERVE QUE EL VALOR DE F ES SIGNIFICATIVO (el pe valor del modelo es menor a alfa), LO QUE INDICA QUE GLOBALMENTE ESTA BIEN
#PERO INDIVIDUALMENTE LOS BETAS SON NULOS
# esto es un sobreajuste

# pimer paso fijar el k=3
summary(modelo3) #sobreajuste lo elimino
summary(modelo2)# funciona mejor
#podría buscar algún modelo que se ajuste mejor, bajo el k
summary(modelo1) 
# como se ve en el modelo uno, se elimina un betha, y reduce la 
#bondad de ajuste

#conclusión se queda con el modelo 2, porque me conviene por ajuste y significancia
# Encontrar el mejor modelo con AIC
step(modelo3)
# Encontrar el VIF para la colinealidad


vif(modelo)

