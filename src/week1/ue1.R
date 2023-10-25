# AUFGABE 1
# Datensatz erzeugen
mensch <- data.frame(
  lnr       = c(seq(1,10)),
  Sex       = c('w','w','w','m','m','w','w','w','m','w'),
  Alter     = c(15,39,101,23,30,1,34,5,89,67),
  Gewicht   = c(50,70,55,80,85,3,90,10,55,71),
  Einkommen = c(500,3000,1500,900,2000,NA,1200,NA,2500,1100)
)

# Berechnen Sie die Mittelwerte, Standardabweichungen, Mediane und die
#25% und 75% Perzentile der Variablen Alter und Einkommen. Beachten
#Sie, dass in Einkommen fehlende Werte vorkommen.

avgAlter <- mean(mensch$Alter)
avgEinkommen <- mean(mensch$Einkommen, na.rm = TRUE)
sdAlter <- sd(mensch$Alter)
sdEinkommen <- sd(mensch$Einkommen, na.rm = TRUE)
medianAlter <- median(mensch$Alter)
medianEinkommen <- median(mensch$Einkommen, na.rm = TRUE)
percAlter <- quantile(mensch$Alter, c(.25,.75))
percEinkommen <- quantile(mensch$Einkommen, c(.25,.75), na.rm = TRUE)


#Berechnen Sie Fallzahlen, Mittelwerte, Standardabweichungen, Mediane
#und die 25% und 75% Perzentile für das Einkommen getrennt für Männer
#und Frauen (Sex).


menschW <- subset(mensch, Sex == 'w')
menschM <- subset(mensch, Sex == 'm')

fallW <- nrow(menschW)
fallM <- nrow(menschM)

avgAlterW <- mean(menschW$Alter)
avgAlterM <- mean(menschM$Alter)

# Analog für restliche Werte...


# AUFGABE 2
#Berechne manuell 70% und 90% Perzentile mit der mathematischen Formel aus der VL
v <- c(4, 2, 7, 8, 1, 0, 3, 5, 6, 10)

v <- sort(v)
k7 <- ((length(v) + 1) * .7)
k9 <- ((length(v) + 1) * .9)
#1. Erst 70% Perzentil
if (k7 %% 1 == 0) {
  p7 <- v[k7]
} else {
  ganz7 <- floor(k7)
  gamma7 <- k7 - ganz7
  p7 <- (v[ganz7] * (1-gamma7)) + (v[(ganz7+1)] * gamma7)
}
#2. Dann 90%
if (k9 %% 1 == 0) {
  p9 <- v[k9]
} else {
  ganz9 <- floor(k9)
  gamma9 <- k9 - ganz9
  p9 <- (v[ganz9] * (1-gamma9)) + (v[(ganz9+1)] * gamma9)
}

# Check
print(p7)
print(p9)
print(quantile(v, c(0.7, 0.9), type = 6))
# Type 6 gibt das gleiche Ergebnis zurück

# AUFGABE 3

schueler <- read.table(file = "~/Uni/Statistics1/src/week1/Schueler.txt", header=TRUE)

hist(schueler$Groesse,
     main="Verteilung der Größe",
     xlab="cm",
     col="red")

hist(schueler$Gewicht,
     main="Verteilung des Gewichts",
     xlab="kg",
     col="blue",
     breaks=14)
# Bereits ab 14 verschlechtert sich die Darstellung

schuelerW <- subset(schueler, Gesch =='w')
schuelerM <- subset(schueler, Gesch =='m')

boxplot(schuelerW$Groesse ~ schuelerW$Gewicht)


plot(schuelerM$Groesse, schuelerM$Gewicht, pch=19, cex=0.3, col='blue')
points(schuelerW$Groesse, schuelerW$Gewicht, pch=19, cex=0.3, col='red')
legend("topleft", legend = c("Männlich", "Weiblich"), col = c("blue", "red"), pch = c(19,19), title="Geschlecht")

schuelerW9 <- subset(schueler, Klasse == 9 & Gesch == 'w')
schuelerM10 <- subset(schueler, Klasse == 10 & Gesch == 'm')

plot(schuelerW9$Groesse, schuelerW9$Gewicht, pch=19, cex=0.3, col="red", main="Größen-/Gewichtsverteilung")
points(schuelerM10$Groesse, schuelerM10$Gewicht, pch=19, cex=0.3, col="blue")
