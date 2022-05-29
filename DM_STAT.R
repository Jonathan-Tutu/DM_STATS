assur <- read.csv("Data.csv",sep=";",dec=".",header=TRUE, row.names=1, stringsAsFactors=TRUE)
head(assur)

#Table de contingence
TabCSPSEXE = table(assur$CSP, assur$Sexe)
TabCSPSEXE

propTable = prop.table(test, margin=2) * 100 #permet d'y ramener sur un pourcentage
propTable

bp <- barplot(propTable, col =c("mistyrose2","cadetblue3","bisque3","aquamarine3", "bisque1") ,
        main="Répartition par CSP chez les hommes et les femmes", xlim=c(0,5), legend=TRUE)

#Test du chi2
chisq.test(TabCSPSEXE)


#PARTIE B STUDENT
subsetCadre <- subset(assur, assur$CSP == "Cadre") #On sépare en sous ensemble
tabSalaireCadre = table(subsetCadre$Salaire.annuel.net, subsetCadre$Sexe)

FemmeCadre = subset(assur, Sexe == "Femme" & CSP == "Cadre")
mean(FemmeCadre$Salaire.annuel.net)
sd(FemmeCadre$Salaire.annuel.net)
length(FemmeCadre$Salaire.annuel.net)

HommeCadre = subset(assur, Sexe == "Homme" & CSP == "Cadre")
mean(HommeCadre$Salaire.annuel.net)
sd(HommeCadre$Salaire.annuel.net)
length(HommeCadre$Salaire.annuel.net)

boxplot(FemmeCadre$Salaire.annuel.net, HommeCadre$Salaire.annuel.net, whisklty = 1, boxfill=c("mistyrose2","cadetblue3"), main="Distribution des salaires", ylab="Salaires en euros", legend=TRUE)
       
var.test(FemmeCadre$Salaire.annuel.net, HommeCadre$Salaire.annuel.net)

#p-value > alpha Conserve H0 donc on dit que les variances sont égaux

t.test(HommeCadre$Salaire.annuel.net, FemmeCadre$Salaire.annuel.net,  alternative = "greater", var.equal=TRUE)
#p-value < alpha donc on rejete H0 et oui machin est supérieur.


#PARTIE C ANOVA
SubsetFemme = subset(assur, Sexe == "Femme")
boxplot(SubsetFemme$Salaire.annuel.net~SubsetFemme$CSP, data=SubsetFemme, whisklty = 2, boxfill=c("mistyrose2","cadetblue3","bisque3","aquamarine3", "bisque1"), main="Répartition des clients suivant leur temps d’attente", legend=TRUE)

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

plot(Salaire.annuel.net ~ Age, data=FemmeCadre, pch=16)
regmodel<-lm( Salaire.annuel.net ~ Age, data=FemmeCadre)
abline(regmodel)

plot(FemmeCadre$Age, residuals(regmodel),pch=16,col="red")
abline(h = 0)

qqPlot(residuals(regmodel),pch=16)

cor.test(FemmeCadre$Age, FemmeCadre$Salaire.annuel.net, alternative="greater", method="pearson")
summary(regmodel)


