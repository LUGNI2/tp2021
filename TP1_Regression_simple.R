cath <- read.table('cathedral.txt',header = TRUE)
#cath=read.table(file.choose(),sep=";",header=T)
names(cath)

library(tidyverse)
library(MASS)


ggplot(cath) + geom_point(aes(x = Length, y = Height))
plot(cath$Length,cath$Height)
cor(cath$Height,cath$Length)

reg.simple <- lm(Height~Length, data = cath)
summary(reg.simple)
names(reg.simple)
names(summary(reg.simple))
reg.simple$coeff
coef(reg.simple)

#Affichage de la droite de regression

ggplot(cath,aes(x = Length, y = Height)) + geom_point() +
geom_smooth(method = lm, fullrange = TRUE, se = FALSE)

plot(cath$Length,cath$Height)
abline(reg.simple)

#Rajout des points ajustés
#Affichage de la droite de regression
ggplot(cath,aes(x = Length, y = Height)) + geom_point() +
  geom_abline(
               intercept = coef(reg.simple)[1],
               slope = coef(reg.simple)[2]
              ) +
geom_point(aes(x = Length, y = reg.simple$fitted.values),
           size = 3,color = 'Red')

plot(cath$Height~cath$Length)
abline(reg.simple,col = 'red')
points(cath$Length,reg.simple$fitted.values,pch = 15,col = 'blue')


#modele sans constante
reg.ss.simple <- lm(Height~Length-1, data = cath)
summary(reg.ss.simple)


#Analyse de residus
par(mfrow = c(2, 2))
plot(reg.simple, 1:4)

library(ggfortify)
autoplot(reg.simple,1:4)

#residus
reg.simple$residuals
residu.student <- rstudent(reg.simple)
residu.stantard <- rstandard(reg.simple)
n <- length(cath$Height)

#Quand l'échantillon ne contient pas de valeurs aberrantes, 
#95% des résidus studientisés se trouvent dans l'intervalle  
#et les autres 5% pas loin de ces limites. C'est le cas dans notre exemple puisque un résidu 
#seulement (sur 25) se trouvent à l'extérieur de l'intervalle

plot(1:n,residu.student,col = 'blue',xlab = 'Index',ylab = 'Résidus studentisés')
abline(-2,0)
abline(2,0)

ggplot(cath,aes(x = 1:n, y  = residu.student)) + 
  geom_point(color = "blue") +
  geom_abline(intercept = -2,slope = 0) +
  geom_abline(intercept = 2,slope = 0)

# Point Levier
#Afin d'identifier des éventuelles observations trop influentes 
#dans le jeu de données, on analyse les poids  
#des observations que l'on obtient par l'instruction suivante :

#levier
levier <- hatvalues(reg.simple)
p <- reg.simple$rank
seuil1 <- 2*p/n
seuil2 <- 3*p/n
ID <- (1:n)[levier > seuil1]
plot(1:n, levier,xlab = 'Index',ylab = 'Poids h_ii',
text(ID,levier[ID],ID,col = 'red',pos = 1))
abline(seuil1,0,lty = 2)
abline(seuil2,0,lty = 3)

#Distance de Cook
#La distance de Cook est une autre mesure 
#pour l'influence d'un individu sur l'estimation
#distance de Cook
cook <- cooks.distance(reg.simple)
s1 <- qf(0.5,p,n - p)
s2 <- qf(0.1,p,n - p)
ID <- (1:n)[levier > s2]
plot(1:n, cook, xlab = 'Index',ylab = 'Distance de Cook'
     ,text(ID,levier[ID],ID,col = 'red',pos = 1))
abline(s2,0,lty = 2)
abline(s1,0,lty = 3)

#prediction
xnew <- 312
val.pred <- coef(reg.simple)[1] + coef(reg.simple)[2]*xnew
val.pred
# par la fonction predict :
xnew <- data.frame(Length = xnew)
predict(reg.simple,xnew,interval = "pred")
