# REGRESSION LINEAIRE MULTIPLE ET SELECTION DE VARIABLE

### base ozone

base.ozone <- read.table(file.choose(), header = TRUE, sep = ";")

str(base.ozone)
head(base.ozone)
edit(base.ozone)

# prends toutes les covariables
modele.max <- lm(ozone ~. , data = base.ozone)	
summary(modele.max)

modele1 <- lm(ozone~T6+T9+T12+T15+T18+N6+N9+N15+N18+VVENT,
         data = base.ozone)
summary(modele1)
AIC(modele1)

modele2 <- lm(ozone~T6+T9+T15+T18+N6+N15+N18+VVENT, data = base.ozone)
summary(modele2)

modele3 <- lm(ozone~T6+T9+T18+N6+N15+N18+VVENT,data = base.ozone)
summary(modele3)

modele4 <- lm(ozone~T6+T18+N6+N15+N18+VVENT, data = base.ozone)
summary(modele4)

modele5 <- lm(ozone~T18+N6+N15+N18+VVENT, data = base.ozone)
summary(modele5)

modele6 <- lm(ozone~T18+N6+N15+VVENT, data = base.ozone)
summary(modele6)

modele7 <- lm(ozone~T18+N6+VVENT, data = base.ozone)
summary(modele7)

anova(modele7)


# Pour d?terminer un sous-mod?le qui ne contient 
#pas de variable inutile (selection de modele)

modele.finale <- step(modele.max)
summary(modele.finale)		
# renvoie le meilleur modele (critere AIC)

library(MASS)

n <- dim(base.ozone)[1]
modele.bic <- stepAIC(modele.max,k = log(n),trace = FALSE)	# BIC k=log(n)
summary(modele.bic)

modele.null <- lm(ozone ~ 1, base.ozone)
modele.aic <- stepAIC(modele.null, k = 2, trace = TRUE,
                      direction = c("forward"),
                      scope = list(lower = modele.null, 
                                   upper = modele.max))	# AIC k=2 par defaut
summary(modele.aic)

modeleb.aic <- stepAIC(modele.max, k = 2, trace = TRUE,
                       direction = c("backward"),
                       scope = list(lower = modele.null, 
                                    upper = modele.max))	# AIC k=2 par defaut
summary(modeleb.aic)


library(FactoMineR)

x <- subset(base.ozone, select = -ozone)
RegBest(y = base.ozone[,1], x = base.ozone[,-1], nbest = 1)

############################################

dim(base.ozone)
b <- base.ozone[-15,]
dim(b)
base.ozone[15,]
base.ozone[16,]
b[15,]

b84 <- base.ozone[-84,]
dim(b84)

base.ozone[85,]
b84[84,]


m7 <- lm(ozone~T18+N6+VVENT, data = base.ozone)
summary(m7)

residus <- m7$residuals
par(mfrow <- c(2,2))
plot(m7,las = 1)

nbase = base.ozone[-c(7,65,84),]
dim(base.ozone)
dim(nbase)

m7bis <- lm(ozone~T18+N6+VVENT, data = nbase)
summary(m7bis)



m9 <- lm(ozone~T18+N6+VVENT, data = b84)
summary(m9)

m7
m8

par(mfrow = c(2,2))
plot(m7,las = 1)

########################################



########## Verification graphique

par(mfrow = c(2,2))
plot(m7, las = 1)

x11()
plot(cooks.distance(m7),type = "h",ylab = "Distance de Cook")

rstud = rstudent(m7)    # residus studentis?s
rstud

# les points dont les résidus studentisés sont supérieures à 2 en valeur absolue ou 
#Distance de Cook supérieur à 1 représentent les point influents sur le modèle

########## Verification par test d'hypoth?ses

# test de nullit? de la moyenne
# H0: la moyenne des r?sidus est nulle

t.test(residus,mu = 0)

# lin?arit? de la relation, normalit? et homosc?dasticit? / h?t?rosc?dasticit?
# H0: les r?sidus suivent une loi normale de m?me variance

library(lmtest)
hmctest(m7)


# Non autocorr?lation des r?sidus
# Durbin-Watson test 
# H0: les residus sont non autocorr?l?s

dwtest(m7)

# test de normalit? 
# H0: les r?sidus suivent une loi normale

shapiro.test(residus)

par(mfrow = c(1,2))
plot(density(residus))
hist(residus)

# Colin?arit? des variables explicatives

library(car)
vif1 = vif(m7)
vif1

############################################################################################
