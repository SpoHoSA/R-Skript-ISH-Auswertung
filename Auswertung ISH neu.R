#AUSWERTUNG ISH

setwd("C:/Users/berrn/Documents/Datensätze/Auswertung")
evalges <- read.csv("evalges_long.csv", header = T, sep =";", dec = ",")

> install.packages ("psych")
> library(psych)
> evalges$Zeitpunkt <- factor(evalges$Zeitpunkt)  #als Faktor kodieren, da bei der Erstellung von ggplots kommt es zu Probleme
> evalges$Geschlecht <- factor(evalges$Geschlecht, levels = 1:2) 


##Geschlecht##

>describeBy(evalges$BMI, evalges$Geschlecht)
>describeBy(evalges$I, evalges$Geschlecht)
>describeBy(evalges$R, evalges$Geschlecht)
>describeBy(evalges$SKH, evalges$Geschlecht)
>describeBy(evalges$KK, evalges$Geschlecht)
>describeBy(evalges$SI, evalges$Geschlecht)
>describeBy(evalges$FEESS, evalges$Geschlecht)  

# Modalwert - !!! WICHTIG: die Funktion liegt im Paket "lsr"

> install.packages("lsr") 
> library(lsr)
> modeOf( evalges$Geschlecht) 
> aggregate(x = Geschlecht ~ Zeitpunkt,data = evalges, FUN = modeOf)
> aggregate(x = Geschlecht ~ Zeitpunkt + Kürzel,data = evalges, FUN = modeOf)


> library(ggplot2)


### BMI ###

# Modalwert# - 3 Vorschläge - man kann den Modalwert in Abhängigkeit von mehreren Variablen betrachten
> aggregate(x = Zeitpunkt ~ BMI , data = evalges, FUN = modeOf)
> aggregate(x = BMI ~ Zeitpunkt, data = evalges, FUN = modeOf)
> aggregate(x = BMI ~ Zeitpunkt + Geschlecht,data = evalges, FUN = modeOf)

# Deskriptive Statistik

> describeBy(evalges$BMI, evalges$Zeitpunkt)


# Boxplot - BMI 
> ?geom_boxplot #Hilfe
> vignette("ggplot2-specs") # Aesthetic specifications 

> ggplot(data = evalges, aes(x= Zeitpunkt,y = BMI)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "BMI") + theme_classic() 


## Identifikation (I) ##

# Modalwert

> aggregate(x = I ~ Zeitpunkt,data = evalges, FUN = modeOf) 
> aggregate(x = I ~ Zeitpunkt + Kürzel,data = evalges, FUN = modeOf) 
> aggregate(x = I ~ Zeitpunkt + Geschlecht,data = evalges, FUN = modeOf) 

# deskriptive Statistik

> describeBy(evalges$I, evalges$Zeitpunkt)


# Boxplot I

> ggplot(data = evalges, aes(x= Zeitpunkt,y = I)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "Identifikation") + theme_classic()

### Regulation ###

# Modalwert zu jedem Zeitpunkt

> aggregate(x = R ~ Zeitpunkt,data = evalges, FUN = modeOf)
> aggregate(x = R ~ Zeitpunkt + Geschlecht,data = evalges, FUN = modeOf)

# deskriptive Statistik 

> describeBy(evalges$R, evalges$Zeitpunkt)

# Boxplot Regulation (zu jedem Zeitpunkt) 

> ggplot(data = evalges, aes(x= Zeitpunkt,y = R)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "Regulation") + theme_classic()


### SKH ###

#Modalwert zu jedem Zeitpunkt 

> aggregate(x = SKH ~ Zeitpunkt,data = evalges, FUN = modeOf)

# deskriptive Statistik

> describeBy(evalges$SKH, evalges$Zeitpunkt)

# Boxplot

>  ggplot(data = evalges, aes(x= Zeitpunkt,y = SKH)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "Sozial Kompetent Handeln")+ylim(0,18) + theme_classic() 


### KK ###

# Modalwert zu jedem Zeitpunkt:
> aggregate(x = KK ~ Zeitpunkt,data = evalges, FUN = modeOf)

# deskriptive Statistik

> describeBy(evalges$KK,evalges$Zeitpunkt)

#Boxplot#

> ggplot(data = evalges, aes(x= Zeitpunkt,y = KK)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "Klassenklima")+ ylim(0,15) + theme_classic()


### SI ###

> aggregate(x = SI ~ Zeitpunkt,data = evalges, FUN = modeOf) # Modalwert
> describeBy(evalges$SI,evalges$Zeitpunkt)

# Boxplot #

> ggplot(data = evalges, aes(x= Zeitpunkt,y = SI)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "SI")+ ylim (0,15) + theme_classic()


### FEESS ###

>  #Modalwert
  > describeBy(evalges$FEES, evalges$Zeitpunkt)

# Boxplot #

> ggplot(data = evalges, aes(x= Zeitpunkt,y = FEES)) + geom_boxplot(colour = "black",fill = "floralwhite",outlier.color = "red", outlier.fill = "red") +  labs(x = "Zeitpunkt", y = "FEES ")+ ylim(0,25) + theme_classic()






###############################Inferenzstatistik###############################

evalges <- read.csv("ISH_Daten.csv", header = T, sep = ";", dec = ",")



#######Vergleiche EG & KG####### (1_2 noch nicht drin, da in Kontrolle nicht vorliegend)


##BMI## t-Test für unabhängige Stichproben

#mit Levene-Test auf Varianzhomogenität prüfen -> liegt vor wenn H1 verworfen wird

#deskriptiver Ersteindruck
describeBy(evalges$BMI_1_1, evalges$Gruppe)
describeBy(evalges$BMI_2, evalges$Gruppe)
describeBy(evalges$BMI_3, evalges$Gruppe)
describeBy(evalges$BMI_4, evalges$Gruppe)

#Levene-Test durchführen
install.packages("car")
library(car)
leveneTest(evalges$BMI_1_1, evalges$Gruppe)
leveneTest(evalges$BMI_2, evalges$Gruppe)
leveneTest(evalges$BMI_3, evalges$Gruppe)
leveneTest(evalges$BMI_4, evalges$Gruppe)

#t-Test durchführen (t.test(x~y, var.equal, alternative))
#x: Testvariable, y: Gruppenvariable, var.equal = TRUE
#wenn var.equal = FALSE wird automatisch Welch-Test gerechnet
#alternative: "two.sided", "greater"(1 größer 2), "less"(1 kleiner 2)
#greater/ less: Subdatensatz mit beiden Gruppen bilden (Gruppenzugehörigkeit 1 & 2)
t.test(evalges$BMI_1_1 ~ evalges$Gruppe, var.equal = T, alternative = "two.sided")
t.test(evalges$BMI_2 ~ evalges$Gruppe, var.equal = T, alternative = "less")
t.test(evalges$BMI_3 ~ evalges$Gruppe, var.equal = T, alternative = "less")
t.test(evalges$BMI_4 ~ evalges$Gruppe, var.equal = T, alternative = "less")

#bei signifikanten Tests: Effektstärken berechnen
cohensD(evalges$BMI_1_1 ~ evalges$Gruppe)
cohensD(evalges$BMI_2 ~ evalges$Gruppe)
cohensD(evalges$BMI_3 ~ evalges$Gruppe)
cohensD(evalges$BMI_4 ~ evalges$Gruppe)
#ab 0,2 (klein)
#ab 0,5 (mittel)
#ab 0,8 (groß)


##Stand## t-Test für unabhängige Stichproben

#links
describeBy(evalges$Stand_links_1_1, evalges$Gruppe)
describeBy(evalges$Stand_links_2, evalges$Gruppe)
describeBy(evalges$Stand_links_3, evalges$Gruppe)
describeBy(evalges$Stand_links_4, evalges$Gruppe)

leveneTest(evalges$Stand_links_1_1, evalges$Gruppe)
leveneTest(evalges$Stand_links_2, evalges$Gruppe)
leveneTest(evalges$Stand_links_3, evalges$Gruppe)
leveneTest(evalges$Stand_links_4, evalges$Gruppe)

t.test(evalges$Stand_links_1_1 ~ evalges$Gruppe, var.equal = T, alternative = "two.sided")
t.test(evalges$Stand_links_2 ~ evalges$Gruppe, var.equal = T, alternative = "greater")
t.test(evalges$Stand_links_3 ~ evalges$Gruppe, var.equal = T, alternative = "greater")
t.test(evalges$Stand_links_4 ~ evalges$Gruppe, var.equal = T, alternative = "greater")

cohensD(evalges$Stand_links_1_1 ~ evalges$Gruppe)
cohensD(evalges$Stand_links_2 ~ evalges$Gruppe)
cohensD(evalges$Stand_links_3 ~ evalges$Gruppe)
cohensD(evalges$Stand_links_4 ~ evalges$Gruppe)

#rechts
describeBy(evalges$Stand_rechts_1_1, evalges$Gruppe)
describeBy(evalges$Stand_rechts_2, evalges$Gruppe)
describeBy(evalges$Stand_rechts_3, evalges$Gruppe)
describeBy(evalges$Stand_rechts_4, evalges$Gruppe)

leveneTest(evalges$Stand_rechts_1_1, evalges$Gruppe)
leveneTest(evalges$Stand_rechts_2, evalges$Gruppe)
leveneTest(evalges$Stand_rechts_3, evalges$Gruppe)
leveneTest(evalges$Stand_rechts_4, evalges$Gruppe)

t.test(evalges$Stand_rechts_1_1 ~ evalges$Gruppe, var.equal = T, alternative = "two.sided")
t.test(evalges$Stand_rechts_2 ~ evalges$Gruppe, var.equal = T, alternative = "greater")
t.test(evalges$Stand_rechts_3 ~ evalges$Gruppe, var.equal = T, alternative = "greater")
t.test(evalges$Stand_rechts_4 ~ evalges$Gruppe, var.equal = T, alternative = "greater")

cohensD(evalges$Stand_rechts_1_1 ~ evalges$Gruppe)
cohensD(evalges$Stand_rechts_2 ~ evalges$Gruppe)
cohensD(evalges$Stand_rechts_3 ~ evalges$Gruppe)
cohensD(evalges$Stand_rechts_4 ~ evalges$Gruppe)


##Identifikation## Mann-Whitney-U-Test/ Wilcoxon-Test

#deskriptiver Ersteindruck
describeBy(evalges$I_1_1, evalges$Gruppe)
describeBy(evalges$I_2, evalges$Gruppe)
describeBy(evalges$I_3, evalges$Gruppe)
describeBy(evalges$I_4, evalges$Gruppe)

#bei weniger als 40-50 Gesamtbeobachtungen: exact = TRUE
wilcox.test(evalges$I_1_1 ~ evalges$Gruppe, alternative = "two.sided", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$I_2 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$I_3 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$I_4 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)

#Effektstärken
wilcoxonR(x = evalges$I_1_1, g = evalges$Gruppe)
wilcoxonR(x = evalges$I_2, g = evalges$Gruppe)
wilcoxonR(x = evalges$I_3, g = evalges$Gruppe)
wilcoxonR(x = evalges$I_4, g = evalges$Gruppe)
#ab 0,1 (schwach)
#ab 0,3 (mittel)
#ab 0,5 (stark)


##Regulation## Mann-Whitney-U-Test/ Wilcoxon-Test

describeBy(evalges$R_1_1, evalges$Gruppe)
describeBy(evalges$R_2, evalges$Gruppe)
describeBy(evalges$R_3, evalges$Gruppe)
describeBy(evalges$R_4, evalges$Gruppe)

wilcox.test(evalges$R_1_1 ~ evalges$Gruppe, alternative = "two.sided", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$R_2 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$R_3 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$R_4 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)

wilcoxonR(x = evalges$R_1_1, g = evalges$Gruppe)
wilcoxonR(x = evalges$R_2, g = evalges$Gruppe)
wilcoxonR(x = evalges$R_3, g = evalges$Gruppe)
wilcoxonR(x = evalges$R_4, g = evalges$Gruppe)


##Sozialkompetentes Handeln##

describeBy(evalges$SKH_1_1, evalges$Gruppe)
describeBy(evalges$SKH_2, evalges$Gruppe)
describeBy(evalges$SKH_3, evalges$Gruppe)
describeBy(evalges$SKH_4, evalges$Gruppe)

wilcox.test(evalges$SKH_1_1 ~ evalges$Gruppe, alternative = "two.sided", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$SKH_2 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$SKH_3 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$SKH_4 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)

wilcoxonR(x = evalges$SKH_1_1, g = evalges$Gruppe)
wilcoxonR(x = evalges$SKH_2, g = evalges$Gruppe)
wilcoxonR(x = evalges$SKH_3, g = evalges$Gruppe)
wilcoxonR(x = evalges$SKH_4, g = evalges$Gruppe)


#Achtung: gelb markierte VP (s. Excel): falsche Werte für FEESS


##FEESS##

describeBy(evalges$FEESS_1_1, evalges$Gruppe)
describeBy(evalges$FEESS_2, evalges$Gruppe)
describeBy(evalges$FEESS_3, evalges$Gruppe)
describeBy(evalges$FEESS_4, evalges$Gruppe)

wilcox.test(evalges$FEESS_1_1 ~ evalges$Gruppe, alternative = "two.sided", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$FEESS_2 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$FEESS_3 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)
wilcox.test(evalges$FEESS_4 ~ evalges$Gruppe, alternative = "greater", exact = FALSE, correct = FALSE, conf.int = TRUE)

wilcoxonR(x = evalges$FEESS_1_1, g = evalges$Gruppe)
wilcoxonR(x = evalges$FEESS_2, g = evalges$Gruppe)
wilcoxonR(x = evalges$FEESS_3, g = evalges$Gruppe)
wilcoxonR(x = evalges$FEESS_4, g = evalges$Gruppe)