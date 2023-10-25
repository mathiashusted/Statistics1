# This is a test of the histogram function, using the real results from my biochemistry 1 exam

results <- read.table("~/Uni/Statistics1/src/test/bc/result.txt", header=TRUE, dec=",")
hist(results$Note, main="Notenverteilung Biochemie I Nachklausur", breaks = c(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 4.3, 4.7, 5), freq=TRUE, xlab="Note")
print(median(results$Note))
abline(v= median(results$Note),
       col="red",
       lwd=3)
text(x = median(results$Note) * 0.5,
     y = median(results$Note) * 4,
     paste("Median =", median(results$Note)),
     col = "red",
     cex = 2)

hist(results$Gesamtpunktzahl, main="Notenverteilung Biochemie I Nachklausur", freq=TRUE, xlab="Punktzahl", breaks = seq(0,66, by=3))
abline(v= median(results$Gesamtpunktzahl),
       col="green",
       lwd=3)
text(x = median(results$Gesamtpunktzahl) * 0.6,
     y = median(results$Gesamtpunktzahl) * 0.2,
     paste("50%"),
     col = "green",
     cex = 2)
