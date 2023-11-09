#Kevin Bauer
#Bachelorarbeit
#Parametrische und nicht-parametrische Methoden zur Überlebenszeit-Analyse am Beispiel von Adenokarzinomen des Magens

#Krebspatienten importieren und Klassen ändern
Krebspatienten <- read.csv2("/Users/kevinbauer/Library/Mobile Documents/com~apple~CloudDocs/Studium/6. Semester/Bachelorarbeit/Datensatz/eigene Nummerierung/Krebspatienten.csv", na = "NA")
Krebspatienten <- head(Krebspatienten, 240)

Krebspatienten[, 3 ] <- as.numeric(unlist(Krebspatienten[,3 ]))
Krebspatienten[, 4 ] <- as.numeric(Krebspatienten[,4 ])
Krebspatienten[, 5 ] <- as.factor(Krebspatienten[,5 ])
Krebspatienten[, 11 ] <- as.numeric(Krebspatienten[,11 ])

summary(Krebspatienten)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Zugriff auf das Geschlecht der Patienten
K_man <- Krebspatienten[Krebspatienten$Gender == 1,]
K_fem <- Krebspatienten[Krebspatienten$Gender == 2,]

#Zugriff auf die intestinalen/diffusen Magenkrebse
K_Darm_Intestinal <- Krebspatienten[Krebspatienten$Lauren.classification == 1,]
K_Darm_Diffuse <- Krebspatienten[Krebspatienten$Lauren.classification == 2,]

#Zugriff auf die Zensierungen
K_zensiert <- Krebspatienten[Krebspatienten$Zensur == 1,]
K_unzensiert <- Krebspatienten[Krebspatienten$Zensur == 0,]

#Zugriff auf pM
K_pM0 <- Krebspatienten[Krebspatienten$pM == 0,]
K_pM1 <- Krebspatienten[Krebspatienten$pM == 1,]

#Zugriff auf pN
K_pN0 <- na.omit(Krebspatienten[Krebspatienten$pN == 0,])
K_pN123 <- na.omit(Krebspatienten[Krebspatienten$pN != 0,])

#Zugriff auf pT
K_pT12 <- Krebspatienten[Krebspatienten$pT == 1 | Krebspatienten$pT == 2,]
K_pT34 <- Krebspatienten[Krebspatienten$pT == 3 | Krebspatienten$pT == 4,]

#Zugriff auf das Krebsstadium
K_Stat1 <- Krebspatienten[Krebspatienten$stage == 1 | Krebspatienten$stage == 2,]
K_Stat2 <- Krebspatienten[Krebspatienten$stage == 3 | Krebspatienten$stage == 4,]
K_Stat3 <- Krebspatienten[Krebspatienten$stage == 5 | Krebspatienten$stage == 6 | Krebspatienten$stage == 7,]
K_Stat4 <- Krebspatienten[Krebspatienten$stage == 8,]

#Zugriff auf low/high Serum Level
K_high <- Krebspatienten[Krebspatienten$Serum.MMP.14.Status == 1,]
K_low <- Krebspatienten[Krebspatienten$Serum.MMP.14.Status == 0,]

#Unter 5 Jahre
K_lower5 <- Krebspatienten[Krebspatienten$Survival.years <= 5,]
K_lower5_Z0 <- K_lower5[K_lower5$Zensur == 0,]

#Einteilung der Krebsstadien
K_Stadium <- data.frame(stage = Krebspatienten$stage, Stadium = ifelse(Krebspatienten$stage %in% c(1,2), 1,
                                                                       ifelse(Krebspatienten$stage %in% c(3,4), 2,
                                                                              ifelse(Krebspatienten$stage %in% c(5,6,7), 3, 4))))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Deskreptive Statistik

#Aufteilung in Frauen und Männer
prozent <- round((table(Krebspatienten$Gender)/sum(table(Krebspatienten$Gender)))*100,2)
beschriftung <- paste(prozent, "%", sep=" ")
pie(table(Krebspatienten$Gender), labels = beschriftung, main = "Geschlechteranteil", 
    col = c("lightblue", "lightpink"))
text(0,0.35, labels = 118, col = "black")
text(0, -0.35, labels = 122, col = "black")
legend("topright", c("Männer", "Frauen"),
       fill = c("lightblue", "lightpink"))

#Altersverteilung
boxplot(Krebspatienten$Age, main = "Altersverteilung aller Patienten", ylab = "Alter in Jahren", ylim = c(30,95), 
        col = "grey80")
text(1, median(Krebspatienten$Age), labels = as.character(median(Krebspatienten$Age)), 
     pos = 3, col = "black")
Median_Alter <- median(Krebspatienten$Age)
abline(h = Median_Alter, col = "red", lwd = 2,lty = 2)
legend("topright", legend = "Median", col = "red", lty = 2)

#Altersverteilung nach Geschlecht
boxplot(Krebspatienten$Age ~ Krebspatienten$Gender, main = "Altersverteilung nach Geschlecht", 
        xlab = "Geschlecht", ylab = "Alter in Jahren", xaxt = "n", ylim = c(30,95), 
        col = c("lightblue", "lightpink"))
axis(1, at = c(1, 2), labels = c("Männer", "Frauen"))
text(1, median(K_man$Age), labels = as.character(median(K_man$Age)), pos = 3, col = "black")
text(2, median(K_fem$Age), labels = as.character(median(K_fem$Age)), pos = 3, col = "black")

#Lauren Klassifikation
h2 <- hist(Krebspatienten$Lauren.classification, col = c("salmon", "salmon4"), 
           border = c("black", "black"), breaks = c(0,1,2) ,ylim = c(0,200), 
           xlab = "Magenkrebs Typ", ylab = "Anzahl an Patienten", main = "Laurén Klassifikation", 
           xaxt = "n")
text(h2$mids, h2$counts, labels = h2$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.5, 1.5), labels = c("Diffuser", "Intestinaler"))

#Krebsstadium
h3 <- hist(K_Stadium$Stadium, breaks = c(0,1,2,3,4), ylim = c(0,100), 
           col = c("cyan", "cyan4", "cornflowerblue", "dodgerblue4"),
           border = "black", main = "Verteilung der Krebsstadien", 
           xlab = "Krebsstadien", ylab = "Anzahl der Patienten", xaxt = "n")
text(h3$mids, h3$counts, labels = h3$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.5, 1.5, 2.5, 3.5), 
     labels = c("1. Stadium", "2. Stadium", "3. Stadium", "4.Stadium"))

#Distant Metastasis
h4 <- hist(Krebspatienten$pM, breaks = 2, col = c("#33CC66", "#006633"),
           main = "Klassifikation der Fernmetastasen", xlab = "Klassifikation",
           ylab = "Anzahl der Patienten", ylim = c(0,250), xaxt = "n")
text(h4$mids, h4$counts, labels = h4$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.25, 0.75), labels = c("pM0", "pM1"))

#Lymph node Metastasis
h5 <- hist(K_pN$Kategorie, breaks = 2, col = c("#66C9FF", "lightslateblue"),
           main = "Lymphknotenmetastasen", xlab = "Klassifikation",
           ylab = "Anzahl der Patienten", ylim = c(0,250), xaxt = "n")
text(h5$mids, h5$counts, labels = h5$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.25, 0.75), labels = c("pN0", "pN1-3"))

#pT Klassifikation
h6 <- hist(K_pT$Kategorie, breaks = 2, col = c("#FFCC33", "#FFFF66"),
           main = "Tumorklassifikation", xlab = "Klassifikation",
           ylab = "Anzahl der Patienten", ylim = c(0,250), xaxt = "n")
text(h6$mids, h6$counts, labels = h6$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.25, 0.75), labels = c("pT1-2", "pT3-4"))

#Boxplot Survival Years
boxplot(Krebspatienten$Survival.years, main = "Überlebensjahre der Patienten", 
        ylab = "Alter in Jahren", ylim = c(0,17), 
        col = "grey80")
text(1, median(Krebspatienten$Survival.years), 
     labels = as.character(median(Krebspatienten$Survival.years)), 
     pos = 3, col = "black")
Median_Survival <- median(Krebspatienten$Survival.years)
abline(h = Median_Survival, col = "red", lwd = 2,lty = 2)
legend("topright", legend = "Median", col = "red", lty = 2)

#Anzahl low und high Serum
h7 <- hist(Krebspatienten$Serum.MMP.14.Status, breaks = 2, col = c("#FF3366", "#660066"),
           main = "MMP-14 Gehalt der Krebspatienten", xlab = "MMP-14 Level",
           ylab = "Anzahl der Patienten", ylim = c(0,250), xaxt = "n")
text(h7$mids, h7$counts, labels = h7$counts, adj=c(0.5, -0.5))
axis(1, at = c(0.25, 0.75), labels = c("niedriges Level", "hohes Level"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Mann-Whitney U-Test

#Alter, wobei wir eine Grenze bei 67 Jahren setzen und quasi jünger/äter als 67 testen
K_Age67 <- data.frame(Age = Krebspatienten$Age, Alter = ifelse(Krebspatienten$Age < 67, 0, 1))
wilcox.test(Krebspatienten$Serum.MMP.14 ~ K_Age67$Alter, exact = FALSE, correct = FALSE)


#Geschlecht
wilcox.test(Krebspatienten$Serum.MMP.14 ~ Krebspatienten$Gender, correct = FALSE)

#pT
K_pT <- data.frame(pT = Krebspatienten$pT, Kategorie = ifelse(Krebspatienten$pT < 3, 0, 1))
wilcox.test(Krebspatienten$Serum.MMP.14 ~ K_pT$Kategorie, correct = FALSE)

#pN
K_pN <- data.frame(pN = Krebspatienten$pN, Kategorie = ifelse(Krebspatienten$pN < 1, 0, 1))
wilcox.test(Krebspatienten$Serum.MMP.14 ~ K_pN$Kategorie, correct = FALSE)

#pM
wilcox.test(Krebspatienten$Serum.MMP.14 ~ Krebspatienten$pM, correct = FALSE)

#Lauren Klassifikation
wilcox.test(Krebspatienten$Serum.MMP.14 ~ Krebspatienten$Lauren.classification, correct = FALSE)

#Krebsstadium
K_Stat <- data.frame(Stat = Krebspatienten$stage, Kategorie = ifelse(Krebspatienten$stage <= 4 , 0, 1))
kruskal.test(Krebspatienten$Serum.MMP.14 ~ K_Stat$Kategorie)
wilcox.test(Krebspatienten$Serum.MMP.14 ~ K_Stat$Kategorie)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survival analysis

library(survival)
library(survminer)

#Kaplan-Meier-Methode aller Krebspatienten
surv_Krebspatienten <- Surv(Krebspatienten$Survival.years, Krebspatienten$Zensur == 0) #Zensur = 0 ?????
fit1 <- survfit(surv_Krebspatienten ~ Krebspatienten$Serum.MMP.14.Status, data = Krebspatienten)
fit1
summary(fit1, times = 12)

ggsurvplot(fit1, xlim = c(0,20), xlab = "Jahre", break.x.by = 5, 
           ylab = "Überlebensrate", pval = TRUE, pval.coord = c(6,0.75),
           risk.table = TRUE, 
           legend.labs = c("niedriger MMP-14 Gehalt", "hoher MMP-14 Gehalt"),
           font.legend = list(size = 14, color = "black"),
           legend.title = "", palette = c("blue", "red"),risk.table.height = .23, risk.table.y.text = FALSE, risk.table.title = "Patienten im Risiko")

#logrank-Test
survdiff(surv_Krebspatienten ~ Krebspatienten$Zensur == 0)

#multivariates Cox
Cox_multi <- coxph(surv_Krebspatienten ~ K_Age67$Alter + as.factor(K_Stadium$Stadium) + Krebspatienten$Lauren.classification + Krebspatienten$Serum.MMP.14.Status)
summary(Cox_multi)

#univariates Cox
Cox_uniAlter <- coxph(surv_Krebspatienten ~ K_Age67$Alter)
summary(Cox_uniAlter)

Cox_uniLauren <- coxph(surv_Krebspatienten ~ Krebspatienten$Lauren.classification)
summary(Cox_uniLauren)

Cox_uniMMP14 <- coxph(surv_Krebspatienten ~ as.factor(K_Stadium$Stadium))
summary(Cox_uniMMP14)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Die Kaplan Meier Kurven der Untergruppen

#Grafik Fig2A
surv_pT34 <- Surv(K_pT34$Survival.years, K_pT34$Zensur ==0) #Zensur = 0 ?????
fit2A <- survfit(surv_pT34 ~ K_pT34$Serum.MMP.14.Status, data = K_pT34)
fit2A
summary(fit2A, times = 14.6)
sum(fit2A$time >= 15)

ggsurvplot(fit2A, xlim = c(0,20), xlab = "Jahre", break.x.by = 5, 
           ylab = "Überlebensrate", pval = TRUE, pval.coord = c(6,0.75),
           risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", 
           legend.labs = c("niedriger MMP-14 Gehalt", "hoher MMP-14 Gehalt"),
           legend.title = "", palette = c("blue", "red"), 
           risk.table.height = .23, risk.table.y.text = FALSE,
           title = "pT3-4 Tumor Klassifikation",
           font.legend = list(size = 14, color = "black"))

#Fig2B
surv_pN123 <- Surv(K_pN123$Survival.years, K_pN123$Zensur ==0) #Zensur = 0 ?????
fit2B <- survfit(surv_pN123 ~ K_pN123$Serum.MMP.14.Status, data = K_pN123)
fit2B

summary(fit2B, times = 10)
sum(fit2B$time >= 15)

ggsurvplot(fit2B, xlim = c(0,20), xlab = "Jahre", break.x.by = 5, 
           ylab = "Überlebensrate", pval = TRUE, pval.coord = c(6,0.75),
           risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", 
           legend.labs = c("niedriger MMP-14 Gehalt", "hoher MMP-14 Gehalt"),
           legend.title = "", palette = c("blue", "red"), 
           risk.table.height = .23, risk.table.y.text = FALSE,
           title = "Lymphknotenmetastasen",
           font.legend = list(size = 14, color = "black"))


#Fig2C
surv_Intestinal <- Surv(K_Darm_Intestinal$Survival.years, K_Darm_Intestinal$Zensur ==0) #Zensur = 0 ?????
fit2C <- survfit(surv_Intestinal ~ K_Darm_Intestinal$Serum.MMP.14.Status, data = K_Darm_Intestinal)
fit2C
sum(fit2C$time > 15)
summary(fit2C, times = 5)

ggsurvplot(fit2C, xlim = c(0,20), xlab = "Jahre", break.x.by = 5, 
           ylab = "Überlebensrate", pval = TRUE, pval.coord = c(6,0.75),
           risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", 
           legend.labs = c("niedriger MMP-14 Gehalt", "hoher MMP-14 Gehalt"),
           legend.title = "", palette = c("blue", "red"), 
           risk.table.height = .23, risk.table.y.text = FALSE,
           title = "Intestinaler Magenkrebs",
           font.legend = list(size = 14, color = "black"))
#Fig2D
surv_diffuse <- Surv(K_Darm_Diffuse$Survival.years, K_Darm_Diffuse$Zensur == 0) #Zensur = 0 ?????
fit2D <- survfit(surv_diffuse ~ K_Darm_Diffuse$Serum.MMP.14.Status, data = K_Darm_Diffuse)
fit2D

sum(fit2D$time > 15)
summary(fit2D, times = 15)

ggsurvplot(fit2D, xlim = c(0,20), xlab = "Jahre", break.x.by = 5, 
           ylab = "Überlebensrate", pval = TRUE, pval.coord = c(6,0.75),
           risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", 
           legend.labs = c("niedriger MMP-14 Gehalt", "hoher MMP-14 Gehalt"),
           legend.title = "", palette = c("blue", "red"), 
           risk.table.height = .23, risk.table.y.text = FALSE,
           title = "Diffuser Magenkrebs",
           font.legend = list(size = 14, color = "black"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#univariates Cox-Modell der Patienten mit hohem Gehalt

#Gender 
#Männer
cox_man <- coxph(Surv(K_man$Survival.years, K_man$Zensur == 0) ~ K_man$Serum.MMP.14.Status)
summary(cox_man)
#Frauen
cox_fem <- coxph(Surv(K_fem$Survival.years, K_fem$Zensur == 0) ~ K_fem$Serum.MMP.14.Status)
summary(cox_fem)

#TumorKlassifikation pT
#pT12
cox_pT12 <- coxph(Surv(K_pT12$Survival.years, K_pT12$Zensur == 0) ~ K_pT12$Serum.MMP.14.Status)
summary(cox_pT12)
#pT34
cox_pT34 <- coxph(Surv(K_pT34$Survival.years, K_pT34$Zensur == 0) ~ K_pT34$Serum.MMP.14.Status)
summary(cox_pT34)

#Lymphknotenmetastasen pN
#pN0
cox_pN0 <- coxph(Surv(K_pN0$Survival.years, K_pN0$Zensur == 0) ~ K_pN0$Serum.MMP.14.Status)
summary(cox_pN0)
#pn123
cox_pN123 <- coxph(Surv(K_pN123$Survival.years, K_pN123$Zensur == 0) ~ K_pN123$Serum.MMP.14.Status)
summary(cox_pN123)

#Fernmetastasen pM
#pM0
cox_pM0 <- coxph(Surv(K_pM0$Survival.years, K_pM0$Zensur == 0) ~ K_pM0$Serum.MMP.14.Status)
summary(cox_pM0)
#pM1
cox_pM1 <- coxph(Surv(K_pM1$Survival.years, K_pM1$Zensur == 0) ~ K_pM1$Serum.MMP.14.Status)
summary(cox_pM1)

#Lauren Klassifikation
#Intestinal
cox_intes <- coxph(Surv(K_Darm_Intestinal$Survival.years, K_Darm_Intestinal$Zensur == 0) ~ K_Darm_Intestinal$Serum.MMP.14.Status)
summary(cox_intes)
#Diffus
cox_diffus <- coxph(Surv(K_Darm_Diffuse$Survival.years, K_Darm_Diffuse$Zensur == 0) ~ K_Darm_Diffuse$Serum.MMP.14.Status)
summary(cox_diffus)

#Alter
unter67 <- Krebspatienten[Krebspatienten$Age < 67,]
über67 <- Krebspatienten[Krebspatienten$Age >= 67,]

#unter 67
cox_unter67 <- coxph(Surv(unter67$Survival.years, unter67$Zensur == 0) ~ unter67$Serum.MMP.14.Status)
summary(cox_unter67)
#über67
cox_über67 <- coxph(Surv(über67$Survival.years, über67$Zensur == 0) ~ über67$Serum.MMP.14.Status)
summary(cox_über67)

#Krebsstadium
#2
cox_Stat2 <- coxph(Surv(K_Stat2$Survival.years, K_Stat2$Zensur == 0) ~ K_Stat2$Serum.MMP.14.Status)
summary(cox_Stat2)
#3
cox_Stat3 <- coxph(Surv(K_Stat3$Survival.years, K_Stat3$Zensur == 0) ~ K_Stat3$Serum.MMP.14.Status)
summary(cox_Stat3)
#4
cox_Stat4 <- coxph(Surv(K_Stat4$Survival.years, K_Stat4$Zensur == 0) ~ K_Stat4$Serum.MMP.14.Status)
summary(cox_Stat4)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#parametrische Methode

library(flexsurv)
library(survminer)

#Exponential-, Weibull- und Log-Normal-Verteilung aller Krebspatienten

surv_Krebspatienten <- Surv(Krebspatienten$Survival.years, Krebspatienten$Zensur == 0) #Zensur = 0 ?????
fit1 <- survfit(surv_Krebspatienten ~ Krebspatienten$Serum.MMP.14.Status, data = Krebspatienten)
fit1

ex <- flexsurvreg(surv_Krebspatienten ~ Krebspatienten$Serum.MMP.14.Status, 
                  data = Krebspatienten, dist = "exp")
ex$AIC
ex
weib <- flexsurvreg(surv_Krebspatienten ~ Krebspatienten$Serum.MMP.14.Status, 
                  data = Krebspatienten, dist = "weibull")
weib$AIC
lnorm <- flexsurvreg(surv_Krebspatienten ~ Krebspatienten$Serum.MMP.14.Status, 
                  data = Krebspatienten, dist = "lnorm")
lnorm
lnorm$AIC

ggsurvplot(ex, data = Krebspatienten, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23, 
           pval = "AIC: 845.32", pval.coord = c(7,0.7), 
           title = "Exponentialverteiluing", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

ggsurvplot(weib, data = Krebspatienten, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23,
           pval = "AIC: 811.53", pval.coord = c(7,0.7),
           title = "Weibull-Verteilung", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

ggsurvplot(lnorm, data = Krebspatienten, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23,
           pval = "AIC: 777.63", pval.coord = c(7,0.7),
           title = "Log-Normal-Verteilung", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#für den fall D der diffuse Magenkrebspatienten

surv_diffuse <- Surv(K_Darm_Diffuse$Survival.years, K_Darm_Diffuse$Zensur == 0) #Zensur = 0 ?????

ex_diff <- flexsurvreg(surv_diffuse ~ K_Darm_Diffuse$Serum.MMP.14.Status, 
                  data = K_Darm_Diffuse, dist = "exp")
ex_diff$AIC

weib_diff <- flexsurvreg(surv_diffuse ~ K_Darm_Diffuse$Serum.MMP.14.Status, 
                    data = K_Darm_Diffuse, dist = "weibull")
weib_diff$AIC

lnorm_diff <- flexsurvreg(surv_diffuse ~ K_Darm_Diffuse$Serum.MMP.14.Status, 
                     data = K_Darm_Diffuse, dist = "lnorm")

lnorm_diff$AIC

ggsurvplot(ex_diff, data = K_Darm_Diffuse, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23, 
           pval = "AIC: 593.81", pval.coord = c(7,0.7), 
           title = "Exponentialverteiluing", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

ggsurvplot(weib_diff, data = K_Darm_Diffuse, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23,
           pval = "AIC: 564.83", pval.coord = c(7,0.7),
           title = "Weibull-Verteilung", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

ggsurvplot(lnorm_diff, data = K_Darm_Diffuse, conf.int = TRUE,
           xlab = "Jahre", xlim = c(0,20), break.x.by = 5,
           ylab = "Überlebensrate", risk.table = TRUE,
           risk.table.title = "Patienten im Risiko", risk.table.height = .23,
           pval = "AIC: 535.83", pval.coord = c(7,0.7),
           title = "Log-Normal-Verteilung", legend.title = "", font.legend = list(size = 14, color = "black", face = "bold"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Simulation

set.seed(123)
diffus_low <- K_Darm_Diffuse[K_Darm_Diffuse$Serum.MMP.14.Status == 0,]
diffus_high <- K_Darm_Diffuse[K_Darm_Diffuse$Serum.MMP.14.Status == 1,]

#Parameter herausfinden
surv_lnorm_low <- Surv(diffus_low$Survival.years, diffus_low$Zensur == 0)
lnorm_model_low <- survreg(surv_lnorm_low ~ diffus_low$Serum.MMP.14.Status,
                       data = diffus_low,
                       dist = "lognormal")
summary(lnorm_model_low)
#meanlog 1.3734
#sdlog 1.73

surv_lnorm_high <- Surv(diffus_high$Survival.years, diffus_high$Zensur == 0)
lnorm_model_high <- survreg(surv_lnorm_high ~ diffus_high$Serum.MMP.14.Status,
                           data = diffus_high,
                           dist = "lognormal")
summary(lnorm_model_high)
#meanlog 0.680
#sdlog 1.64

#damit haben wir unsere Parameter für die Simulation der Überlebenszeiten auf Grundlage der Log-Normal-Verteilung

#Simulation

#Erstellen einer leeren Matrix für die Ergebnisse
ergebnisse <- matrix(0, nrow = 1000, ncol = length(seq(100, 2000, by = 50)))

#Schleife über j-Werte (seq(100, 2000, by = 50))
for (j_idx in 1:length(seq(100, 2000, by = 50))) {
  j <- seq(100, 2000, by = 50)[j_idx]
  
  #Wiederholung für 1000 Durchläufe
  for (i in 1:1000) {
    set.seed(123 + i)
    
    #Überlebenszeiten erzeugen
    survival_times <- c(rlnorm(j * 0.8, meanlog = 1.37, sdlog = 1.73), rlnorm(j * 0.2, meanlog = 0.680, sdlog = 1.64))
    survival_times[survival_times >= 17.5] <- 17.5 #end of time
    MMP14 <- c(rep(0, j * 0.8), rep(1, j * 0.2)) #80% haben den MMP-14 Status 1
    
    zensur <- rep(0, length(survival_times))
    zensur[survival_times == 17.5] <- 1 
    
    survival_daten <- data.frame(Zeit = survival_times, Zensur = zensur, MMP14_Status = MMP14)
    anzahlZensiert <- survival_daten[survival_daten$Zensur == 1,]
    
    zusätzliche_positionen <- sample(which(survival_daten$Zensur == 0), (j * 0.4) - length(anzahlZensiert$Zensur))
    survival_daten$Zensur[zusätzliche_positionen] <- 1 #insg immer 40% der Zeiten sind zensiert
    
    survival_times_zensiert <- survival_daten[survival_daten$Zensur == 1,]
    
    #Log-Rank Test
    surv <- Surv(time = survival_daten$Zeit, event = survival_daten$Zensur)
    logrank <- survdiff(surv ~ survival_daten$MMP14_Status)
    
    #p-Wert berechnen und in der Ergebnismatrix speichern
    p_wert <- pchisq(logrank$chisq, 1, lower.tail = FALSE)
    ergebnisse[i, j_idx] <- p_wert
  }
}

#Mittelwerte über die 1000 Durchläufe für jeden j-Wert berechnen
mittelwerte <- colMeans(ergebnisse)
mittelwerte

#x_Achse entspricht den j-Werten
library(ggplot2)
plot_data <- data.frame(x_Achse, mittelwerte)

ggplot(data = plot_data, aes(x = x_Achse, y = mittelwerte)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_vline(xintercept = x_Achse[which.min(mittelwerte > 0.05)]-25, linetype = "dashed", color = "blue") +
  labs(x = "Anzahl der Patienten", y = "Mittelwerte der p-Werte", title = "Simulation der Stichprobengröße") +
  theme(
    plot.title = element_text(face = "bold", size = 14),  # Anpassen von Titelattributen
    axis.title.x = element_text(face = "bold", size = 12),  # Anpassen von x-Achsentitelattributen
    axis.title.y = element_text(face = "bold", size = 12)  # Anpassen von y-Achsentitelattributen
  )
