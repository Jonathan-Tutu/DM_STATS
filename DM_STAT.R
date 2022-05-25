assur <- read.csv("Data.csv",sep=";",dec=".",header=TRUE, row.names=1, stringsAsFactors=TRUE)
head(assur)

#Table de contigence
test = table(assur$CSP, assur$Sexe)
test

test2 = prop.table(test, margin=2) * 100 #permet d'y ramener sur un pourcentage
test2

barplot(test2, col =c("green4","red1","red2","red3","red4") ,
        main="Niveau de satisfaction par test", xlim=c(0,5), legend=TRUE)

#Test du chi2
chisq.test(test)


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

boxplot(iris4$Salaire.annuel.net, iris5$Salaire.annuel.net, whisklty = 1, boxfill=c("mistyrose2","cadetblue3", main="Re ́partition des clients suivant leur temps d’attente"))

var.test(iris4$Salaire.annuel.net, iris5$Salaire.annuel.net)

#p-value > alpha Conserve H0 donc on dit que les variances sont égaux

t.test(iris5$Salaire.annuel.net, iris4$Salaire.annuel.net,  alternative = "greater", var.equal=TRUE) #A vérifier
#p-value < alpha donc on rejete H0 et oui machin est supérieur.


#PARTIE C ANOVA
iris6 = subset(assur, Sexe == "Femme")
boxplot(iris6$Salaire.annuel.net~iris6$CSP, data=iris4, whisklty = 2, boxfill=c("mistyrose2","cadetblue3","bisque3","aquamarine3", "mistyrose3"), main="Répartition des clients suivant leur temps d’attente", legend=TRUE)

femmeOuv = subset(assur, Sexe == "Femme" & CSP == "Ouvrier")
shapiro.test(femmeOuv$Salaire.annuel.net)

femmeCadre = subset(assur, Sexe == "Femme" & CSP == "Cadre")
shapiro.test(femmeCadre$Salaire.annuel.net)

femmeEmploy = subset(assur, Sexe == "Femme" & CSP == "Employé")
shapiro.test(femmeEmploy$Salaire.annuel.net)

femmeInterm = subset(assur, Sexe == "Femme" & CSP == "Prof Interm")
shapiro.test(femmeInterm$Salaire.annuel.net)

femmeInterm = subset(assur, Sexe == "Femme" & CSP == "Technicien")
shapiro.test(femmeInterm$Salaire.annuel.net)


bartlett.test(iris6$Salaire.annuel.net~iris6$CSP)

#Avec correction de welch car hétérogénéité des variances 
oneway.test(iris6$Salaire.annuel.net ~ iris6$CSP, data=iris6, var.equal = FALSE)

#CSP -> Donc il y a un impact sur le salaire des femmes

pairwise.t.test(iris6$Salaire.annuel.net, iris6$CSP, p.adjust.method = "fdr")

#PARTIE D
install.packages("car")
library("car")

plot(Salaire.annuel.net ~ Age, data=iris4, pch=16)
regmodel<-lm( Salaire.annuel.net ~ Age, data=iris4)
abline(regmodel)

plot(iris4$Age, residuals(regmodel),pch=16,col="red")
abline(h = 0)

qqPlot(residuals(regmodel),pch=16)

cor.test(iris4$Age, iris4$Salaire.annuel.net, alternative="greater", method="pearson")
summary(regmodel)


