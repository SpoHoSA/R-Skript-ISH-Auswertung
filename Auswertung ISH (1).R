####Poweranalyse####
library(pwr)
library (effsize, psych, powerAnalysis)
install.packages("powerAnalysis")

library (powerAnalysis)

cohen.ES(test="anov", size="medium")

pwr.anova.test(k=5, f=.4, sig.level=.05, power=.9)

pwr.anova.test(k=5, f=.35, sig.level=.05, power=.8)

wp.rmanova(n=NULL, ng=1, nm=6, f=.4, power=.8, nscor=0.7)

pwr.t.test(d=0.5, sig.level=.05, power=.8, type="paired")

####Auswertung Evaluation####
setwd("C:/Users/Stefan Ackermann/Documents/ISH")

install.packages("Rmisc")
library (Rmisc)

evalges <- read.csv("2022_06.csv", header = T, sep=";", dec=",")
evalges <- read.csv("2022_1_Entwicklung_AK3.csv", header = T, sep=";", dec=",")
evalges <- read.csv("2022_1_Entwicklung_V3.csv", header = T, sep=";", dec=",")
evalges <- read.csv("2022_1_Entwicklung_AK4c.csv", header = T, sep=";", dec=",")
evalges <- read.csv("2022_1_Entwicklung_V2.csv", header = T, sep=";", dec=",")


str(evalges)

library(psych)

describe(data.frame(evalges$BMI.2019, evalges$BMI.2021, evalges$I.2019, evalges$I.2021, evalges$R.2019, evalges$R.2021, evalges$SKH.2019, evalges$SKH.2021, evalges$KK.2019, evalges$KK.2021, evalges$SI.2019, evalges$SI.2021, evalges$FEESS.2019, evalges$FEESS.2021))
describe(data.frame(evalges$BMI, evalges$I_Gesamt, evalges$R_Gesamt, evalges$SKH_Gesamt, evalges$KK_Gesamt, evalges$SI_Gesamt, evalges$FEESS_Gesamt))

####Sprachen####

table(evalges$Sprache1)
table(evalges$Sprache2)
round(100*prop.table(table(evalges$Sprache1)), 2)
round(100*prop.table(table(evalges$Sprache2)), 2)

table(eval1$Sprache1)
table(eval1$Sprache2)

table(eval2$Sprache1)
table(eval2$Sprache2)

table(eval3$Sprache1)
table(eval3$Sprache2)

table(eval3$Sprache1)
table(eval4$Sprache2)


####Konfidenzintervalle####
install.packages("Rmisc")
library(Rmisc)




#Erklärung kruscal-Wallis-Test: https://www.youtube.com/watch?v=AESs_KStlYY 
#https://bjoernwalther.com/kruskal-wallis-test-in-r-rechnen/ 

kruskal.test(evalges$BMI ~ evalges$Klasse)
pairwise.wilcox.test(evalges$BMI, evalges$Klasse, paired=FALSE, p.adjust="bonferroni")

kruskal.test(evalges$I_Gesamt ~ evalges$Klasse)
pairwise.wilcox.test(evalges$I_Gesamt, evalges$Klasse, paired=F, p.adjust="bonferroni")

kruskal.test(evalges$R_Gesamt ~ evalges$Klasse)
pairwise.wilcox.test(evalges$R_Gesamt, evalges$Klasse, paired=F, p.adjust="bonferroni")

kruskal.test(evalges$SKH_Gesamt ~ evalges$Klasse)
pairwise.wilcox.test(evalges$SKH_Gesamt, evalges$Klasse, paired=F, p.adjust="bonferroni")

kruskal.test(evalges$FEESS_Ges ~ evalges$Klasse)
pairwise.wilcox.test(evalges$FEESS_Ges, evalges$Klasse, paired=F, p.adjust="bonferroni")

kruskal.test(evalges$KK_Ges ~ evalges$Klasse)
pairwise.wilcox.test(evalges$KK_Ges, evalges$Klasse, paired=F, p.adjust="bonferroni")

kruskal.test(evalges$SI_Ges ~ evalges$Klasse)
pairwise.wilcox.test(evalges$SI_Ges, evalges$Klasse, paired=F, p.adjust="bonferroni")


#Effektstärke f
#1. Eta² = (H-k + 1)/(n-k); H = Testwert, der im KWT vor den df angegeben wird
#k = Anzahl der Gruppen; n = Anzahl der getesteten Kinder
#2. f= Wurzel(Eta²/1-Eta²)

eta_squared <- (15.345 - 4 + 1)/(108-4)
sqrt(eta_squared/(1-eta_squared))

####Entwicklung der einzelnen Jahrgangsstufen#### 
#Entwicklung der einzelnen Jahrgangsstufen 

setwd("C:/Users/Stefan Ackermann/Documents/ISH")

evalges <- read.csv("2022_1_Entwicklung_JGS3.csv", header = T, sep=";", dec=",")


library(car)

#JGS 1
evalges$KKT.2021 <- recode(evalges$KK.2021, "0=20; 1=23; 2=27; 3=30; 4=32; 5=35; 6=38; 7=42; 8=46; 9=51; 10=57; 11=65")
evalges$KKT.2022 <- recode(evalges$KK.2022_Post, "0=20; 1=23; 2=27; 3=30; 4=32; 5=35; 6=38; 7=42; 8=46; 9=51; 10=57; 11=65") 
evalges$SIT.2021 <- recode(evalges$SI.2021, "0=20; 1=24; 2=27; 3=30; 4=33; 5=36; 6=39; 7=42; 8=46; 9=50; 10=56; 11=64")
evalges$SIT.2022 <- recode(evalges$SI.2022_Post, "0=20; 1=24; 2=27; 3=30; 4=33; 5=36; 6=39; 7=42; 8=46; 9=50; 10=56; 11=64")

describe(data.frame(evalges$KKT.2019, evalges$KKT.2021, evalges$KKT.2022, evalges$SIT.2019, evalges$SIT.2021, evalges$SIT.2022))

#JGS 2
evalges$KKT.2021 <- recode(evalges$KK.2021, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$KKT.2022 <- recode(evalges$KK.2022_Post, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$SIT.2021 <- recode(evalges$SI.2021, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")
evalges$SIT.2022 <- recode(evalges$SI.2022_Post, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")


wilcox.test (evalges$BMI.2019, evalges$BMI.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$BMI.2021, evalges$BMI.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$BMI.2019, evalges$BMI.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$I.2019, evalges$I.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$I.2021, evalges$I.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$I.2019, evalges$I.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$R.2019, evalges$R.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$R.2021, evalges$R.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$R.2019, evalges$R.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$SKH.2019, evalges$SKH.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$SKH.2021, evalges$SKH.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$SKH.2019, evalges$SKH.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$KK.2019, evalges$KK.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$KKT.2021, evalges$KKT.2022, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$KK.2021, evalges$KK.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$KK.2019, evalges$KK.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$SI.2019, evalges$SI.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$SI.2021, evalges$SI.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$SI.2019, evalges$SI.2022_Post, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$SIT.2021, evalges$SIT.2022, paired = TRUE, correct = TRUE, conf.int = TRUE)

wilcox.test (evalges$FEESS.2019, evalges$FEESS.2021, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$FEESS.2021, evalges$FEESS.2022, paired = TRUE, correct = TRUE, conf.int = TRUE)
wilcox.test (evalges$FEESS.2019, evalges$FEESS.2022, paired = TRUE, correct = TRUE, conf.int = TRUE)

install.packages(("tidyr"))
library(tidyr)

install.packages("rcompanion") 
library(rcompanion) 

evalges_long <- gather (evalges, t, v, BMI.2021:BMI.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, I.2021:I.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, R.2021:R.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, SKH.2021:SKH.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, KK.2021:KK.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, SI.2021:SI.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

evalges_long <- gather (evalges, t, v, FEESS.2021:FEESS.2022_Post)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#JGS 3
evalges$KKT.2019 <- recode(evalges$KK.2021, "0=20; 1=23; 2=27; 3=30; 4=32; 5=35; 6=38; 7=42; 8=46; 9=51; 10=57; 11=65")
evalges$KKT.2021 <- recode(evalges$KK.2021, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$KKT.2022 <- recode(evalges$KK.2022_Post, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$SIT.2019 <- recode(evalges$SI.2021, "0=20; 1=24; 2=27; 3=30; 4=33; 5=36; 6=39; 7=42; 8=46; 9=50; 10=56; 11=64")
evalges$SIT.2021 <- recode(evalges$SI.2021, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")
evalges$SIT.2022 <- recode(evalges$SI.2022_Post, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")

#JGS 4
evalges$KKT.2019 <- recode(evalges$KK.2019, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$KKT.2021 <- recode(evalges$KK.2021, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$KKT.2022 <- recode(evalges$KK.2022_Post, "0=20; 1=24; 2=30; 3=34; 4=37; 5=41; 6=44; 7=48; 8=52; 9=55; 10=60; 11=67")
evalges$SIT.2019 <- recode(evalges$SI.2019, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")
evalges$SIT.2021 <- recode(evalges$SI.2021, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")
evalges$SIT.2022 <- recode(evalges$SI.2022_Post, "0=22; 1=26; 2=28; 3=31; 4=33; 5=37; 6=40; 7=44; 8=46; 9=58; 10=55; 11=63")

#JGS 1 
describe(data.frame(evalges$BMI.2021, evalges$BMI.2022_Post,evalges$I.2021, evalges$I.2022_Post, evalges$R.2021, evalges$R.2022_Post, evalges$SKH.2021, evalges$SKH.2022_Post, evalges$KK.2021, evalges$KK.2022_Post, evalges$SI.2021, evalges$SI.2022_Post, evalges$FEESS.2021, evalges$FEESS.2022_Post))

#JGS 2
describe(data.frame(evalges$BMI.2021, evalges$BMI.2022_Post, evalges$I.2021, evalges$I.2022_Post, evalges$R.2021, evalges$R.2022_Post, evalges$SKH.2021, evalges$SKH.2022_Post, evalges$KK.2021, evalges$KK.2022_Post, evalges$SI.2021, evalges$SI.2022_Post, evalges$FEESS.2021, evalges$FEESS.2022))
describe(data.frame(evalges$KKT.2021, evalges$KKT.2022, evalges$SIT.2021, evalges$SIT.2022))
#JGS 3 & 4
describe(data.frame(evalges$BMI.2019, evalges$BMI.2021, evalges$BMI.2022_Post, evalges$I.2019, evalges$I.2021, evalges$I.2022_Post, evalges$R.2019, evalges$R.2021, evalges$R.2022_Post, evalges$SKH.2019, evalges$SKH.2021, evalges$SKH.2022_Post, evalges$KK.2019, evalges$KK.2021, evalges$KK.2022_Post, evalges$SI.2019, evalges$SI.2021, evalges$SI.2022_Post, evalges$FEESS.2019, evalges$FEESS.2021, evalges$FEESS.2022_Post))
describe(data.frame(evalges$KKT.2019, evalges$KKT.2021, evalges$KKT.2022, evalges$SIT.2019, evalges$SIT.2021, evalges$SIT.2022))



library(psych)

library(ggplot2)

evalges$I.2019 <- as.numeric(evalges$I.2019)
evalges$I.2021 <- as.numeric(evalges$I.2021)
evalges$I.2022_Post <- as.numeric(evalges$I.2022_Post)
evalges$R.2019 <- as.numeric(evalges$R.2019)
evalges$R.2021 <- as.numeric(evalges$R.2021)
evalges$R.2022_Post <- as.numeric(evalges$R.2022_Post)
evalges$SKH.2019 <- as.numeric(evalges$SKH.2019)
evalges$SKH.2021 <- as.numeric(evalges$SKH.2021)
evalges$SKH.2022_Post <- as.numeric(evalges$SKH.2022_Post)
evalges$FEESS.2022 <- as.numeric(evalges$FEESS.2022)
evalges$FEESS.2021 <- as.numeric(evalges$FEESS.2021)
evalges$FEESS.2022_Post <- as.numeric(evalges$FEESS.2022_Post)
evalges$KK.2019 <- as.numeric(evalges$KK.2019)
evalges$KK.2021 <- as.numeric(evalges$KK.2021)
evalges$KK.2022_Post <- as.numeric(evalges$KK.2022_Post)
evalges$KKT.2022 <- as.numeric(evalges$KKT.2022)
evalges$KKT.2022.1 <- as.numeric(evalges$KKT.2022.1)
evalges$SI.2019 <- as.numeric(evalges$SI.2019)
evalges$SI.2021 <- as.numeric(evalges$SI.2021)
evalges$SI.2022_Post <- as.numeric(evalges$SI.2022_Post)
evalges$SIT.2021 <- as.numeric(evalges$SIT.2021)
evalges$SIT.2022 <- as.numeric(evalges$SIT.2022)
evalges$BMI.2019 <- as.numeric(evalges$BMI.2019)
evalges$BMI.2021 <- as.numeric(evalges$BMI.2021)
evalges$BMI.2022_Post <- as.numeric(evalges$BMI.2022_Post)


####Geschlecht####

library(psych)

describeBy(evalges$BMI, evalges$Geschlecht)
describeBy(evalges$I_Gesamt, evalges$Geschlecht)
describeBy(evalges$R_Gesamt, evalges$Geschlecht)
describeBy(evalges$SKH_Gesamt, evalges$Geschlecht)
describeBy(evalges$KK_Gesamt, evalges$Geschlecht)
describeBy(evalges$SI_Gesamt, evalges$Geschlecht)
describeBy(evalges$FEESS_Gesamt, evalges$Geschlecht)

####Konfidenzintervalle####
library(Rmisc)

#Auswahl MÄnnlich
evalm <- evalges[evalges$Geschlecht=="1",]

CI(evalm$BMI)
CI(evalm$I_Gesamt)
CI(evalm$R_Gesamt)
CI(evalm$SKH_Gesamt)
CI(evalm$KK_Gesamt)
CI(evalm$SI_Gesamt)
CI(evalm$FEESS_Gesamt)

#Auswahl weiblich
evalw <- evalges[evalges$Geschlecht=="2",]

CI(evalw$BMI)
CI(evalw$I_Gesamt)
CI(evalw$R_Gesamt)
CI(evalw$SKH_Gesamt)
CI(evalw$KK_Gesamt)
CI(evalw$SI_Gesamt)
CI(evalw$FEESS_Gesamt)

#Auswahl Sprache1 = Deutsch 
evalD <- evalges[evalges$sprache_kat=="1",]

CI(evalD$BMI)
CI(evalD$I_Gesamt)
CI(evalD$R_Gesamt)
CI(evalD$SKH_Gesamt)
CI(evalD$KK_Gesamt)
CI(evalD$SI_Gesamt)
CI(evalD$FEESS_Gesamt)

#Auswahl Sprache1 = Sonstige 
evalS <- evalges[evalges$sprache_kat=="2",]

CI(evalS$BMI)
CI(evalS$I_Gesamt)
CI(evalS$R_Gesamt)
CI(evalS$SKH_Gesamt)
CI(evalS$KK_Gesamt)
CI(evalS$SI_Gesamt)
CI(evalS$FEESS_Gesamt)


####Sprache####
library(car)
evalges$sprache_kat <- recode(evalges$Sprache1, "1=1; 2:hi=2")

library(psych)
describeBy(evalges$BMI, evalges$sprache_kat)
describeBy(evalges$I_Gesamt, evalges$sprache_kat)
describeBy(evalges$R_Gesamt, evalges$sprache_kat)
describeBy(evalges$SKH_Gesamt, evalges$sprache_kat)
describeBy(evalges$KK_Gesamt, evalges$sprache_kat)
describeBy(evalges$SI_Gesamt, evalges$sprache_kat)
describeBy(evalges$FEESS_Gesamt, evalges$sprache_kat)

####Friedman's ANOVA - Entwicklung >2 Zeitpunkte####
#https://youtu.be/Qg7_Y9LKnYQ
#https://bjoernwalther.com/friedman-test-in-r-rechnen/ 

data1 <- subset(evalges, select = c(BMI.2019, BMI.2021, BMI.2022_Post))
data1 <- subset(evalges, select = c(I.2019, I.2021, I.2022_Post))
data1 <- subset(evalges, select = c(R.2019, R.2021, R.2022_Post))
data1 <- subset(evalges, select = c(SKH.2019, SKH.2021, SKH.2022_Post))
data1 <- subset(evalges, select = c(KK.2019, KK.2021, KK.2022_Post))
data1 <- subset(evalges, select = c(SI.2019, SI.2021, SI.2022_Post))
data1 <- subset(evalges, select = c(FEESS.2019, FEESS.2021, FEESS.2022_Post))

#Subdatensatz in Matrix umwandeln
datam <- as.matrix(data1)

#deskriptive Überprüfung vor dem eigentlichen Test
library(psych)

describe(data1)

#der Friedman Test = standardprogramm in R --> Kein zusätzliches Package nötig
#H0 = gleichheit der drei Messzeitpunkte (vgl. Chi²-Test)
friedman.test(datam)

#posthoc
install.packages("pgirmess")
library(pgirmess)

#Multiple comparisons between groups after Friedman test
#es wird auf das alpha Niveau von 0,05 getestet & es werden intern direkt Korrekturen vorgenommen (Grund = alpha Komulierung)
#obs.dif = w-Wert = empirischer Unterschied zw. den Zeitpunkten
#critical.dif = Kritischer Wert = Wert, der vom obs.dif überschritten werden muss, damit H0 verworfen wird
friedmanmc(datam)

#Signifikanzniveau anpassen
#strengeres Niveau --> critical.dif steigt, weil dieser Wert anhand des Sig.N. ermittelt wird
friedmanmc(datam, probs = 0.01)

####Daten Einlesen####
#Fun fact: Excel einlesen: 
data <- read_excel("Dateiname.xlsx", na="NA")

#Für die Berechnung der Effektstärke ist der Datensatz im Long Format nötig
#D.h. es muss eine Excel/CSV Datei mit 2 Spalten vorbereitet werden
#Spalte 1 = Wert der jeweiligen Variable; Spalte 2 = Zeitpunkt (1, 2, 3 ... oder x)

datalong <- read.csv("JGS3_BMI_Long.csv", header = T, sep=";", dec=",")
datalong <- read.csv("V3_Long.csv", header = T, sep=";", dec=",")

####Effektstärke Friedman Test####
#https://www.youtube.com/watch?v=Hbi6Injrtw4

#Datensatz im Long Format eingelesen? (falls nicht, siehe Daten Einlesen)

#Subdatensätze bilden
data1_2 <- subset(datalong, Zeitpunkt == 1 | Zeitpunkt == 2)
data1_3 <- subset(datalong, Zeitpunkt_BMI == 1 | Zeitpunkt_BMI == 3)
data2_3 <- subset(datalong, Zeitpunkt == 2 | Zeitpunkt == 3)

data19_22 <- subset(datalong, Zeitpunkt_I == 2019 | Zeitpunkt_I == 2022)

wilcox.test(data19_22$I ~ data19_22$Zeitpunkt_I, paired = TRUE)

library(rcompanion)

wilcoxonPairedR(x = data1_2$BMI, g = data1_2$Zeitpunkt)

wilcoxonPairedR(x = data1_3$BMI, g = data1_3$Zeitpunkt_BMI)
wilcoxonPairedR(x = data1_3$I, g = data1_3$Zeitpunkt_I)
wilcoxonPairedR(x = data1_3$R, g = data1_3$Zeitpunkt_R)
wilcoxonPairedR(x = data1_3$SKH, g = data1_3$Zeitpunkt_SKH)
wilcoxonPairedR(x = data1_3$KK, g = data1_3$Zeitpunkt_KK)
wilcoxonPairedR(x = data1_3$SI, g = data1_3$Zeitpunkt_SI)
wilcoxonPairedR(x = data1_3$FEESS, g = data1_3$Zeitpunkt_FEESS)


wilcoxonPairedR(x = data19_22$I, g = data19_22$Zeitpunkt_I)
wilcoxonPairedR(x = data19_22$R, g = data19_22$Zeitpunkt_R)
wilcoxonPairedR(x = data19_22$SKH, g = data19_22$Zeitpunkt_SKH)
wilcoxonPairedR(x = data19_22$KK, g = data19_22$Zeitpunkt_KK)
wilcoxonPairedR(x = data19_22$SI, g = data19_22$Zeitpunkt_SI)
wilcoxonPairedR(x = data19_22$FEESS, g = data19_22$Zeitpunkt_FEESS)

####Expression####

evalges <- read.csv("Expression4.csv", header = T, sep=";", dec=",")

str(evalges)

#Angst  
wilcox.test (evalges$X2021_AA, evalges$X2022_AA, paired = TRUE, correct = TRUE, conf.int = TRUE)

install.packages(("tidyr"))
library(tidyr)

install.packages("rcompanion") 
library(rcompanion) 

evalges_long <- gather (evalges, t, v, X2021_AA:X2022_AA)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#Ekel
wilcox.test (evalges$X2021_EE, evalges$X2022_EE, paired = TRUE, correct = TRUE, conf.int = TRUE)

evalges_long <- gather (evalges, t, v, X2021_EE:X2022_EE)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#Freude
wilcox.test (evalges$X2021_FF, evalges$X2022_FF, paired = TRUE, correct = TRUE, conf.int = TRUE)

evalges_long <- gather (evalges, t, v, X2021_FF:X2022_FF)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#Trauer
wilcox.test (evalges$X2021_TT, evalges$X2022_TT, paired = TRUE, correct = TRUE, conf.int = TRUE)

evalges_long <- gather (evalges, t, v, X2021_TT:X2022_TT)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#Überraschung
wilcox.test (evalges$X2021_ÜÜ, evalges$X2022_ÜÜ, paired = TRUE, correct = TRUE, conf.int = TRUE)

evalges_long <- gather (evalges, t, v, X2021_ÜÜ:X2022_ÜÜ)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)

#Wut
wilcox.test (evalges$X2021_WW, evalges$X2022_WW, paired = TRUE, correct = TRUE, conf.int = TRUE)

evalges_long <- gather (evalges, t, v, X2021_WW:X2022_WW)
wilcoxonPairedR (x=evalges_long$v, g = evalges_long$t)