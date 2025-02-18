# Cargar datos
#### ¿Existe correlación? ####
# Ejemplo del libro A Modern Approach to Regression with R, los derechos de autor son reconocidos a Simon Sheather
library(readr)
Datos <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/FieldGoals2003to2006.csv")
attach(Datos)
library(corrplot)
library(dplyr)
Datos1 <-  Datos[,c(5,7)]
(M= cor(Datos1, method="pearson")) # -0.05300749
 
#Grafico de la correlación
plot(Datos$FGtM1,Datos$FGt, 
     main="Gráfico de correlación = -0.504",
     xlab="Field Goal Percentage in Year t-1",
     ylab="Field Goal Percentage in Year t")
#¿ existe una correlación?

# Modelo de regresión sin considerar pateadores
modelo.sin.pateadores <-  lm(FGt ~ FGAtM1) 
anova(modelo.sin.pateadores)
summary(modelo.sin.pateadores)

# Modelo de regresión considerando pateadores
modelo.con.pateadores <- lm(FGt~FGtM1 +Name +FGtM1:Name,data=Datos)
anova(modelo.con.pateadores)
summary(modelo.con.pateadores)
# Solo con nombres de pateadores
fit.2 <- lm(FGt ~ Name + FGtM1,data=Datos)
fit.2
# Grafica real 
plot(Datos$FGtM1,Datos$FGt,
     main="Correlación total = -0.504",
     xlab="Field Goal Percentage in Year t-1",
     ylab="Field Goal Percentage in Year t")
tt <- seq(60,100,length=1001)
slope.piece <- summary(fit.2)$coef[20]*tt
lines(tt,summary(fit.2)$coef[1]+slope.piece,lty=2)
for (i in 2:19)
{lines(tt,summary(fit.2)$coef[1]+summary(fit.2)$coef[i]+slope.piece,lty=2)}
# Hay dos aspectos destacables en las líneas de regresión, 
# la pendiente común de cada línea es negativa. 
# Esto significa que si un lanzador tuvo un alto porcentaje de goles de campo en el año anterior, 
# entonces se predice que tendrá un menor porcentaje de goles de campo en el año en curso. 
# En segundo lugar, la diferencia en las alturas de las líneas (es decir, en las intercepciones) llega al 20%, 
# lo que indica una gran variedad en el rendimiento de los 19 pateadores.

# Otra forma de ver (Se utiliza chatGPT)

# Instala y carga los paquetes necesarios si aún no están instalados
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Ajustar un modelo de regresión lineal para cada categoría
modelos <- Datos %>%
  group_by(Name) %>%
  do(modelo = lm(FGt ~ FGAtM1:Name, data =Datos))

# Extraer los coeficientes de pendiente e intercepto de cada modelo
coeficientes <- modelos %>%
  summarise(intercepto = coef(modelo)[1],
            pendiente = coef(modelo)[2])
# Grafica para un solo modelo de regresión general 
ggplot(Datos, aes(x = FGAtM1, y = FGt, color = Name)) +
  geom_point() +
  geom_abline(data = coeficientes, aes(intercept = intercepto, slope = pendiente), 
              linetype = "dashed", size = 1) +
  labs(title = "Regresión lineal sin categoría Nombres",
       x = "Field Goal Percentage in Year t-1",
       y = "Field Goal Percentage in Year t") +
  theme_minimal()

# Crear un gráfico de dispersión con líneas de regresión separadas por categoría
ggplot(Datos, aes(x = FGAtM1, y = FGt, color = Name)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +  # Se utiliza geom_smooth con método de regresión lineal
  labs(title = "Regresión lineal por categoría Nombres",
       x = "Field Goal Percentage in Year t-1",
       y = "Field Goal Percentage in Year t") +
  theme_minimal()


#### Mi primer modelo de regresión ####
# Los datos originales son el tiempo necesario (en minutos) para una producción, Y, 
# y el número de artículos producidos, X, para 20 pedidos seleccionados al azar y supervisados por tres directivos.
library(readr)
production <- read_delim("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/production.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
attach(production)

#Figura del diagrama de dispersión, importante para saber si hay una relación lineal
plot(RunTime~RunSize,xlab="Run Size", ylab="Run Time",
     pch=16,main="Diagrama de dispersión, correlación = 0.8545", col="blue",)
abline(lsfit(production$RunSize,production$RunTime), col="red")
cor(RunTime,RunSize)
# Modelo lineal
m1 <- lm(RunTime~RunSize)
# Informe del modelo m1
summary(m1)
anova(m1)
# Grafica del modelo m1
plot(RunTime~RunSize,xlab="Run Size", ylab="Run Time",
     pch=16,main="Diagrama de dispersion", col="blue",)
abline(m1, col="green")

#95% intervalos de confianza
round(confint(m1,level=0.95),3)

# Valores predichos 
predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="confidence",level=0.95)
predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="prediction",level=0.95)

# ¿Cómo está la variabilidad?
media_y <- mean(RunTime) # Vector de medias
proy_y <- m1$fitted.values
resid <- m1$residuals
n <- length(RunTime)

plot(RunTime ~ RunSize,xlab="Run Size", ylab="Run Time",pch=16)
abline(m1,col="red")
abline(h = media_y,col="blue")

SCT = sum((RunTime - media_y)^2) # suma cuadrada total
SCReg = sum((proy_y - media_y)^2) # suma cuadrada regresion
SCE = sum(resid^2) # suma cuadrada del error (residual)

SCReg
SCE
SCT
SCReg + SCE

F.test <- (SCReg/1) / (SCE/(n-2))
F.test

P.valor <- 1 - pf(F.test,1,n-2) # cola derecha de la distrb F
P.valor

# usando la funcion "anova"
anova(m1)

summary(m1)

#### Supuestos del modelo ####

residuos<- m1$residuals

# Normalidad 

shapiro.test(residuos)
# se puede decir que los residuos siguen una distribución normal

# homocedasticidad
library(car)
leveneTest(residuos, production$RunTime) 
## el test de levene mide la homogeneidad de varianzas de una serie de observaciones a lo largo de un factor (que agrupa)
 # la prueba de Levene se utiliza para comprobar la hipótesis nula de que las muestras que se van a comparar proceden de una población con la misma varianza
 # H0: Los grupos tienen varianzas iguales
 # H1: Los grupos tienen varianzas diferentes

# No se tiene homocedasticidad en los residuos

