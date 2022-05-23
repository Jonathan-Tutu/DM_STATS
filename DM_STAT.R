assur <- read.csv("Data.csv", sep = ";", row.names = 1)
head(assur)


test = table(assur$CSP, assur$Sexe)
test

test2 = prop.table(test, margin=2) * 100 #permet d'y ramener sur un pourcentage
test2

barplot(test2, col =c("green4","red1","red2","red3","red4") ,
        main="Niveau de satisfaction par test", xlim=c(0,5), legend=TRUE)

prop.test(test2,alternative="greater")


#PARTIE B STUDENT
dh <- subset(assur, assur$CSP == "Cadre") #On sépare en sous ensemble
lala = table(dh$Salaire.annuel.net, dh$Sexe)

iris4 = subset(assur, Sexe == "Femme" & CSP == "Cadre")
mean(iris4$Salaire.annuel.net)
sd(iris4$Salaire.annuel.net)
length(iris4$Salaire.annuel.net)

iris5 = subset(assur, Sexe == "Homme" & CSP == "Cadre")
mean(iris5$Salaire.annuel.net)
sd(iris5$Salaire.annuel.net)
length(iris5$Salaire.annuel.net)


#PARTIE C ANOVA



#PARTIE D
install.packages("car")
library("car")

plot(assur$Salaire.annuel.net, assur$Age, pch=16)
regmodel = lm(assur$Salaire.annuel.net ~ assur$Age)
abline(regmodel)

plot(assur$Salaire.annuel.net, residuals(regmodel),pch=16,col="red")
abline(h = 0)

car.qqPlot(residuals(regmodel),pch=16)

summary(regmodel)