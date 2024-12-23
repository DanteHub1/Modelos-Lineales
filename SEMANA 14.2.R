####### TEMA: MODELO DE ANÁLISIS DE VARIANZA DE DOS FACTORES CON INTERACCIÓN #######
####### EJEMPLO #######

# Factor A: tipo de bateria

Tipo <- rep(c(1,2,1,2),each=4)
Tipo <- as.factor(Tipo)

# Factor B: temperatura

Temperatura <- rep(c(1,2),each=8)
Temperatura <- as.factor(Temperatura)

# Variable respuesta

Tiempo <- c(130,155,174,180,
            150,188,159,126,
            20,70,82,58,
            25,70,58,45)

# base de datos

datos <- data.frame(Tipo, Temperatura, Tiempo)
datos

## AED

library(ggplot2)
ggplot(data = datos,
       aes(x = Tipo, y = Tiempo, color = Tipo)) +
  geom_boxplot() +
  theme_bw()

library(ggplot2)
ggplot(data = datos,
       aes(x = Edad, y = Tiempo, color = Edad)) +
  geom_boxplot() +
  theme_bw()     

## Modelo: Anova de dos factores

anova <- aov(datos$Tiempo ~ datos$Tipo + datos$Temperatura)
summary(anova)

## Supuestos de normalidad

require(nortest)
by(data = datos, INDICES = datos$Tipo,FUN = function(x){shapiro.test(x$Tiempo)})
by(data = datos, INDICES = datos$Temperatura,FUN = function(x){shapiro.test(x$Tiempo)})

## Supuestos de normalidad del residual

residual <- anova$residuals
datos <- data.frame(datos,residual)

require(nortest)
by(data = datos, INDICES = datos$Tipo,FUN = function(x){shapiro.test(x$residual)})
by(data = datos, INDICES = datos$Temperatura,FUN = function(x){shapiro.test(x$residual)})

## Supuesto de varianzas constantes

require(car)
leveneTest(Tiempo ~ Tipo, datos, 
           center = "median")

leveneTest(Tiempo ~ Temperatura, datos, 
           center = "median")


### ANOVA de dos factores con interacción ###
# Factor A: Tipo de Bateria

Tipo<-rep(c(1,2,1,2),each=4)
Tipo<-as.factor(Tipo)

# Factor B: Temperatura

Temperatura<-rep(c(1,2),each=8)
Temperatura<-as.factor(Temperatura)

# Variable Respuesta

Tiempo<-c(130,155,174,180,150,188,159,126,20,70,82,58,25,70,58,45)

datos <- data.frame(Tipo, Temperatura, Tiempo)
print(datos)

# ANALISIS EXPLORATORIO DE DATOS

library(ggplot2)
ggplot(data = datos,
       aes(x = Tipo, y = Tiempo, color = Tipo)) +
  geom_boxplot() +
  theme_bw()

library(ggplot2)
ggplot(data = datos,
       aes(x = Temperatura, y = Tiempo, color = Temperatura)) +
  geom_boxplot() +
  theme_bw()     

## Interaccion

ggplot(data = datos, aes(x = Tipo, y = Tiempo, colour = Temperatura,
                         group = Temperatura)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Tiempo)') +
  theme_bw()

ggplot(data = datos, aes(x = Temperatura, y = Tiempo, colour = Tipo,
                         group = Tipo)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Tiempo)') +
  theme_bw()

## Modelo: Anova de dos factores

anova <- aov(datos$Tiempo ~ datos$Tipo + datos$Temperatura)
summary(anova)

# Adecuación del Modelo

par(mfrow = c(2,2))
plot(anova, which = 1:4)

par(mfrow = c(1,1))

residuos<-anova$residuals
datos<-cbind(datos,residuos)
datos

## Supuestos de normalidad

require(nortest)
by(data = datos, INDICES = datos$Tipo,FUN = function(x){shapiro.test(x$residuos)})
by(data = datos, INDICES = datos$Temperatura,FUN = function(x){shapiro.test(x$residuos)})

## Supuesto de varianzas constantes

require(car)
leveneTest(residuos ~ Tipo, datos, 
           center = "median")

leveneTest(residuos ~ Temperatura, datos, 
           center = "median")

leveneTest(residuos ~ Tipo * Temperatura,datos,center = "median")

