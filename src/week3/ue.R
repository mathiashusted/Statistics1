#Aufgabe 11: Berechnen Sie mit R für einen diagnostischen Test mit sensitivityitivität
#80% und Spezifität 70% die prädiktiven Werte für folgende Prävalenzen:
#a: 10%, b: 5%, c: 1%, d: 0,1%
#Interpretieren Sie das Ergebnis. Welche Relevanz könnte das Ergebnis für die
#medizinische Praxis haben?

sensitivity <- 0.8
specifity <- 0.7

prevalence <- c(0.1, 0.5, 0.01, 0.001) # a, b, c, d

posPred <- (sensitivity * prevalence)/((sensitivity*prevalence) + (1-specifity)*(1-prevalence))
negPred <- (specifity*(1-prevalence))/((specifity * (1-prevalence)) + (1-sensitivity) * prevalence)



# Aufgabe 12: Plotten Sie mit R in einem Liniendiagramm die Abhängigkeit des
# positiven prädiktiven Werts von der Prävalenz im Bereich von 0% bis 100% für

# A: Sens = 0.8, Spez = 0.9
# B: Sens = 0.8, Spez = 0.99
# C: Sens = 0.99, Spez = 0.8

diaSensitivity <- c(0.8, 0.8, 0.99)
diaSpecifity <- c(0.9, 0.99, 0.8)


for (x in 1:3) {
  plot((diaSensitivity[x] * 1:100)/((diaSensitivity[x]*1:100) + (1-diaSpecifity[x])*(1-1:100)), type="l", col="red", ylab="pos. prädiktiver Wert")
}
