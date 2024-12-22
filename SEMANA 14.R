####### TEMA: MODELO DE ANÁLISIS DE VARIANZA DE DOS FACTORES CON INTERACCIÓN #######
####### EJEMPLO #######

# Definir los datos

Sexo <- c("M", "H", "H", "M", "H", "H", "H", "M", 
          "M", "H", "H", "H", "H", "M", "M", "M", 
          "H", "M", "M", "H", "H", "M", "H", "H", 
          "H", "H", "H", "H", "M", "H")
Edad <- c("A", "A", "A", "A", "A", "A", "J", "J", 
          "A", "J", "J", "A", "J", "J", "J", "A", 
          "J", "A", "J", "J", "J", "J", "A", "J", 
          "J", "J", "J", "J", "J", "A")
Eficacia <- c(7.1, 11.0, 5.8, 8.8, 8.6, 8.0, 3.0, 
              5.2, 3.4, 4.0, 5.3, 11.3, 4.6, 6.4, 
              13.5, 4.7, 5.1, 7.3, 9.5, 5.4, 3.7, 
              6.2, 10.0, 1.7, 2.9, 3.2, 4.7, 4.9, 
              9.8, 9.4)

datos <- data.frame(Sexo = as.factor(Sexo), 
                    Edad = as.factor(Edad), 
                    Eficacia = Eficacia)
print(datos)

## AED - ANÁLISIS EXPLORATORIO DE DATOS

library(ggplot2)
ggplot(data = datos,
       aes(x = Sexo, y = Eficacia, color = Sexo)) +
  geom_boxplot() +
  theme_bw()

ggplot(data = datos,
       aes(x = Edad, y = Eficacia, color = Edad)) +
  geom_boxplot() +
  theme_bw()     

## Gráficas de Interacción

ggplot(data = datos, aes(x = Sexo, y = Eficacia, colour = Edad,
                         group = Edad)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Eficacia)') +
  theme_bw()

ggplot(data = datos, aes(x = Edad, y = Eficacia, colour = Sexo,
                         group = Sexo)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Eficacia)') +
  theme_bw()

## Aplicar el modelo de dos factores con interacción
## modelo: anova de dos factores

anova <- aov(datos$Eficacia ~ datos$Sexo + datos$Edad)
summary(anova)

# Adecuación del Modelo
par(mfrow = c(2,2))
plot(anova, which = 1:4)

par(mfrow = c(1,1))

residuos<-anova$residuals
datos<-cbind(datos,residuos)
datos

## Supuestos 
## Supuestos de normalidad

require(nortest)
by(data = datos, INDICES = datos$Sexo,FUN = function(x){shapiro.test(x$Eficacia)})
by(data = datos, INDICES = datos$Edad,FUN = function(x){shapiro.test(x$Eficacia)})

## Supuesto de varianzas constantes

require(car)
leveneTest(Eficacia ~ Sexo, datos, 
           center = "median")

leveneTest(Eficacia ~ Edad, datos, 
           center = "median")

## Supuestos de normalidad del residual

require(nortest)
by(data = datos, INDICES = datos$Sexo,FUN = function(x){shapiro.test(x$residuos)})
by(data = datos, INDICES = datos$Edad,FUN = function(x){shapiro.test(x$residuos)})

## Supuesto de varianzas constantes (Residuos)

require(car)
leveneTest(residuos ~ Sexo, datos, 
           center = "median")

leveneTest(residuos ~ Edad, datos, 
           center = "median")

leveneTest(residuos ~ Sexo * Edad,datos,center = "median")


