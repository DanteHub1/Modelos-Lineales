####### TEMA: DISTRIBUCIONES MULTIVARIANTES #######
####### EJEMPLO #######


# Parámetros

mu.x <- c(1,2); mu.x
mu.y <- c(0,0); mu.y
sigma.x <- matrix(c(6,0,0,5),2); sigma.x
sigma.y <- diag(2); sigma.y

# Número de observaciones

n <- 50

# Parámetros

p <- 2

# Muestrales

m.x <- c(0.75,1.5); m.x
m.y <- c(0.5,-0.5); m.y
s.x <- matrix(c(5,0,0,4.5),2); s.x
s.y <- matrix(c(0.75,0,0,0.75),2);s.y

## Region de confianza - Elipse (99%, 95% y 90%)

install.packages("car")
library(car)

plot(x=NULL, y=NULL,
     xlab = "y1", ylab = "y2",
     xlim = c(-15,15),
     ylim = c(-15,15), las = 1)
car::ellipse(center = mu.y,
             shape = s.y,
             col = "goldenrod1",
             fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.99,p,n-p)))
car::ellipse(center = mu.y, shape = s.y,
             col = "tomato", fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.95,p,n-p)))
car::ellipse(center = mu.y, shape = s.y,
             col = "blue", fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.90,p,n-p)))
legend("topleft", col = c("blue", "tomato", "goldenrod1"),
       lwd = 2, bty = "n",
       legend = c("90%","95%","99%"))

## Número de observaciones

n <- 60


plot(x=NULL, y=NULL,
     xlab = "x1", ylab = "x2",
     xlim = c(-30,30),
     ylim = c(-30,30), las = 1)
car::ellipse(center = mu.x,
             shape = s.x,
             col = "goldenrod1",
             fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.99,p,n-p)))
car::ellipse(center = mu.x, shape = s.x,
             col = "tomato", fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.95,p,n-p)))
car::ellipse(center = mu.x, shape = s.x,
             col = "blue", fill = TRUE,
             radius = (p*(n-1)/(n-p)*qf(0.90,p,n-p)))
legend("topleft", col = c("blue", "tomato", "goldenrod1"),
       lwd = 2, bty = "n",
       legend = c("90%","95%","99%"))

