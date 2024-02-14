# Cargar datos
# Ejemplo del libro A Modern Approach to Regression with R, los derechos de autor son reconocidos a Simon Sheather
library(readr)
Datos <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/FieldGoals2003to2006.csv")
attach(Datos)
library(corrplot)
library(dplyr)
Datos1 <-  Datos[,c(5,7)]
M= cor(Datos1)
corrplot(M,method= "number",
         main="Gráfico de correlación = -0.504"
         )

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

