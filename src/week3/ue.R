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

