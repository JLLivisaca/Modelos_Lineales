#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Marzo 2024
#### Iniciamos ####

# Cargar datos ==============================================================================
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
datos <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/meatspec.csv")
attach(datos)
# Realizar la partición de los datos
library(caret)  # Instala y carga la biblioteca 'caret'
# Para este modelo se va a dividir los datos en train y test
set.seed(1234)  # Establece una semilla para reproducibilidad
train_index <- createDataPartition(datos$fat, p = 0.7, list = FALSE) # Realiza una partición tomando cel 70% de los datos.
datos_train <- datos[train_index, ]
datos_test  <- datos[-train_index, ]
# Primer modelo ==============================================================================
# Crea un objeto de modelo de regresión lineal
modelo <- lm(fat ~ ., data = datos_train) # El . significa que hará el modelo con todas las variables
summary(modelo); glance(modelo)
paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))
# "Número de predictores incluidos en el modelo: 102"
# Este modelo es muy amplio, se ven muchos predictores que no son significativos. Hay que mejorarlo.

# Valores de los coeficientes ==============================================================================
library(dplyr) # instalar libreria en caso de no tener
library(tibble) # instalar libreria en caso de no tener
df_coeficientes <- modelo$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")
# Gráfica de los coeficientes
df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

# Predicciones de entrenamiento. En este caso se usa el modelo, se carga el dataset train y se evalúa al modelo
# ==============================================================================
predicciones_train <- predict(modelo, newdata = datos_train)
# MSE de entrenamiento, se obtiene el error cuadrado medio
# ==============================================================================
training_mse <- round(mean((predicciones_train - datos_train$fat)^2),4)
paste("Error (mse) de entrenamiento:", training_mse)
# Predicciones de test, se carga el dataset test y se evalúa al modelo
# ==============================================================================
predicciones_test <- predict(modelo, newdata = datos_test)

# MSE de test
# ==============================================================================
test_mse_ols <- round(mean((predicciones_test - datos_test$fat)^2),4)
paste("Error (mse) de test:", test_mse_ols)
# ==============================================================================
 # El modelo tiene un MSE muy bajo (0.3706) cuando predice las mismas observaciones con las que se ha entrenado, 
#  pero mucho más alto (18.5229) al predecir nuevas observaciones. Esto significa que el modelo no es útil, 
# ya que el objetivo es aplicarlo para predecir el contenido en grasa de futuras muestras de carne. 
# Esto se conoce como overfitting (sobre ajuste), es decir el modelo se ajusta demasiado

# Por lo tanto es necesario la selección de variables para evitar este problema

# Creación y entrenamiento del modelo contando con la selección stepwise
# Segundo modelo ==============================================================================
modelo <- step(
  object    = lm(formula = fat ~ ., data = datos_train),
  direction = "backward",
  scope     = list(upper = ~., lower = ~1),
  trace     = FALSE
)
summary(modelo); glance(modelo)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- modelo$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Stepwise") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))
# "Número de predictores incluidos en el modelo: 68"

# Predicciones de entrenamiento. En este caso se usa el modelo, se carga el dataset train y se evalúa al modelo
# ==============================================================================
predicciones_train <- predict(modelo, newdata = datos_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- round(mean((predicciones_train - datos_train$fat)^2),4)
paste("Error (mse) de entrenamiento:", training_mse)

# Predicciones de test, se carga el dataset test y se evalúa al modelo
# ==============================================================================
predicciones_test <- predict(modelo, newdata = datos_test)

# MSE de test
# ==============================================================================
test_mse_step <- round(mean((predicciones_test - datos_test$fat)^2), 4)
paste("Error (mse) de test:", test_mse_step)

# El modelo tiene un MSE muy bajo (0.3963) , 
  #  pero mucho más alto (10.3473) que es alto.
  # Se tiene el mismo error.
# Regresión Ridge==============================================================================
# Regresión Ridge
# Matrices de entrenamiento y test
# ==============================================================================
x_train <- model.matrix(fat~., data = datos_train)[, -1]
y_train <- datos_train$fat
x_test <- model.matrix(fat~., data = datos_test)[, -1]
y_test <- datos_test$fat

# Creación y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Ridge se indica argumento alpha=0. Para Lasso = 1
# Si no se especifica valor de lambda, se selecciona un rango automático el programa.
library(glmnet)
library(tidyr)
library(scales)
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
) 
# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")
# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

#El gráfico muestra el Mean Square Error obtenido por validación cruzada para cada valor de lambda
# junto con la barra de error correspondiente. con el que se consigue el modelo más sencillo que se 
# aleja menos de 1 desviación estándar del error mínimo encontrado.

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)

# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = x_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- round(mean((predicciones_train - y_train)^2),4)
paste("Error (mse) de entrenamiento:", training_mse)

# Predicciones de test
# ==============================================================================
predicciones_test <- predict(modelo, newx = x_test)

# MSE de test
# ==============================================================================
test_mse_ridge <- round(mean((predicciones_test - y_test)^2),4)
paste("Error (mse) de test:", test_mse_ridge)

# "Error (mse) de entrenamiento: 110.797"
# "Error (mse) de test: 89.4984"

# Extrae los coeficientes
# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

#Regresión lasso

# Regresión lasso==============================================================================
# Matrices de entrenamiento y test
# ==============================================================================
x_train <- model.matrix(fat~., data = datos_train)[, -1]
y_train <- datos_train$fat
x_test <- model.matrix(fat~., data = datos_test)[, -1]
y_test <- datos_test$fat

# Creación y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)
# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)

# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)
# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
) 
# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))
df_coeficientes %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 
# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = x_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- round(mean((predicciones_train - y_train)^2),4)
paste("Error (mse) de entrenamiento:", training_mse)
# Predicciones de test
# ==============================================================================
predicciones_test <- predict(modelo, newx = x_test)

# MSE de test
# ==============================================================================
test_mse_lasso <- round(mean((predicciones_test - y_test)^2),4)
paste("Error (mse) de test:", test_mse_lasso)

# "Error (mse) de entrenamiento: 10.2237"
# "Error (mse) de test: 10.2861"
# Predicciones del modelo 
predicciones <- predict(modelo, newx = x_test, s = cv_error$lambda.1se)
# Calcular el número de observaciones
n_obs <- length(y_test)
# Calcular el log-likelihood, que es la función de máxima verosimilitud
log_likelihood <- -n_obs/2 * log(sum((y_test - predicciones)^2))
# Calcular el número de parámetros
n_parametros <- sum(coef(modelo, s = cv_error$lambda.1se) != 0)
# Calcular AIC, BIC y AICc
(AIC <- -2 * log_likelihood + 2 * n_parametros)
(BIC <- -2 * log_likelihood + log(n_obs) * n_parametros)
(AICc <- AIC + 2 * n_parametros * (n_parametros + 1) / (n_obs - n_parametros - 1))
