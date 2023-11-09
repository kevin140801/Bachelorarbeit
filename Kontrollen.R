#Kevin Bauer
#Bachelorarbeit
#Parametrische und nicht-parametrische Methoden zur Überlebenszeit-Analyse am Beispiel von Adenokarzinomen des Magens

#Kontrollen importieren und Klassen ändern
Kontrollen <- read.csv2("/Users/kevinbauer/Library/Mobile Documents/com~apple~CloudDocs/Studium/6. Semester/Bachelorarbeit/Datensatz/eigene Nummerierung/Kontrollen.csv", na="NA")
Kontrollen <- head(Kontrollen, 48)
Kontrollen[, 3 ] <- as.numeric(unlist(Kontrollen[,3 ]))
Kontrollen[, 4 ] <- as.numeric(unlist(Kontrollen[,4 ]))
Kontrollen[, 10 ] <- as.numeric(unlist(Kontrollen[,10 ]))
Kontrollen[, 11] <- as.numeric(unlist(Kontrollen[,11 ]))
summary(Kontrollen)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Zugriff auf das Geschlechet der Kontrollen
Ko_man <- Kontrollen[Kontrollen$Gender == 1,]
Ko_fem <- Kontrollen[Kontrollen$Gender == 2,]

#Zugriff auf low/high Serum Level der Kontrollen
Ko_high <- Kontrollen[Kontrollen$Serum.MMP.14.High.Low == 1,]
Ko_low <- Kontrollen[Kontrollen$Serum.MMP.14.High.Low == 0,]

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Deskreptive Statistik

#Aufteilung des Geschlechts
h1 <- hist(Kontrollen$Gender, col = c("lightblue", "lightpink"),
           border = c("black", "black"), breaks = c(0,1,2) ,
           ylim = c(0,30), xlab = "Geschlecht", ylab = "Anzahl der Patienten", 
           main = "Geschlechteranteil", xaxt = "n")
text(h1$mids, h1$counts, labels = h1$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.5, 1.5), labels = c("Männer", "Frauen"))

#Altersverteilung der Kontrollen
boxplot(Kontrollen$Age, main = "Altersverteilung der Kontrollen", ylab = "Alter in Jahren", ylim = c(30,95), 
        col = "grey80")
text(1, median(Kontrollen$Age), labels = as.character(median(Kontrollen$Age)), 
     pos = 3, col = "black")
Median_Alter <- median(Kontrollen$Age)
abline(h = Median_Alter, col = "red", lwd = 2,lty = 2)
legend("topright", legend = "Median", col = "red", lty = 2)

#Altersverteilung der Frauen und Männer
boxplot(Kontrollen$Age ~ Kontrollen$Gender, main = "Altersverteilung nach Geschlecht", 
        xlab = "Geschlecht", ylab = "Alter in Jahren", xaxt = "n", ylim = c(30,95), 
        col = c("lightblue", "lightpink"))
axis(1, at = c(1, 2), labels = c("Männer", "Frauen"))
text(1, median(Ko_man$Age), labels = as.character(median(Ko_man$Age)), pos = 3, col = "black")
text(2, median(Ko_fem$Age), labels = as.character(median(Ko_fem$Age)), pos = 3, col = "black")


#Boxplot Survival Years
boxplot(Kontrollen$Survival.years, main = "Überlebensjahre der Kontrollen", 
        ylab = "Alter in Jahren", ylim = c(0,20), 
        col = "grey80")
text(1, median(Kontrollen$Survival.years), labels = as.character(median(Kontrollen$Survival.years)), 
     pos = 3, col = "black")
Median_Survival <- median(Kontrollen$Survival.years)
abline(h = Median_Survival, col = "red", lwd = 2,lty = 2)
legend("topright", legend = "Median", col = "red", lty = 2)

#Anzahl low und high Serum
h7 <- hist(Kontrollen$Serum.MMP.14.High.Low, breaks = 2, col = c("lightcyan", "lightslateblue"),
           main = "MMP-14 Gehalt der Kontrollen", xlab = "MMP-14 Level",
           ylab = "Anzahl der Patienten", ylim = c(0,40), xaxt = "n")
text(h7$mids, h7$counts, labels = h7$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.25, 0.75), labels = c("niedriges Level", "hohes Level"))