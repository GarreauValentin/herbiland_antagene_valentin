library(dplyr)
library(readxl)
library(ggplot2)
library(purrr)
library(vegan)

# Chargement des donnees occurences seuil 
table12s <- read.csv(file = "output/12s_table.csv", row.names = 1L, check.names = FALSE)
tableaste <- read.csv(file = "output/aste_table.csv", row.names = 1L, check.names = FALSE)
tabletnrl <- read.csv(file = "output/tnrl_table.csv", row.names = 1L, check.names = FALSE)
tablecype <- read.csv(file = "output/cype_table.csv", row.names = 1L, check.names = FALSE)
tablepoac <- read.csv(file = "output/poac_table.csv", row.names = 1L, check.names = FALSE)
tableassign <- read.csv(file = "output/assignation_nom_niveau.csv", row.names = 1L, check.names = FALSE)
doublons <- read_xlsx("data/Doublons_crottes_total.xlsx", col_names = TRUE)

#chargement des données taxon occurences seuil
taxontableaste <- read.csv(file = "output/aste_table_taxon.csv", row.names = 1L, check.names = FALSE)
taxontabletnrl <- read.csv(file = "output/tnrl_table_taxon.csv", row.names = 1L, check.names = FALSE)
taxontablecype <- read.csv(file = "output/cype_table_taxon.csv", row.names = 1L, check.names = FALSE)
taxontablepoac <- read.csv(file = "output/poac_table_taxon.csv", row.names = 1L, check.names = FALSE)

# Suppression des doublons
colnames(tabledoublons) <- c("id_doublon", "N_Antagene")
doublons <- tabledoublons %>% dplyr::distinct(id_doublon, .keep_all = TRUE)

fichier_list <- list(table12s,
                     tableaste,
                     tabletnrl,
                     tablecype,
                     tablepoac)

fichiers_clean <- c("table12s_clean",
                    "tableaste_clean",
                    "tabletnrl_clean",
                    "tablecype_clean",
                    "tablepoac_clean")

for (i in 1:length(fichier_list)) {
  
  df <- fichier_list[[i]]
  df <- df[!df$N_Antagene %in% tabledoublons$N_Antagene,]
  
  assign(fichiers_clean[i], df)
  write.csv(file = paste0("output/", fichiers_clean[i], ".csv"), df)
    
}

# Création des graphs sur jeu de données trnl avec colonne espèces genetiques
###Nombre de crottes/espèces###
pdf('graphes/graphe1.pdf')
barplot(table(tabletnrl_clean$espece_gen),
        xlab = "Espèces",
        ylab = "Nombre d'échantillons",
        main = "Nombres d'échantillons pour chaque Espèce",
        cex.lab = 1.5, # Taille de la police pour les étiquettes des axes
        cex.main = 1.8, # Taille de la police pour le titre
        space = 0.5, 
        col = "skyblue") 
dev.off()
###Nombre de crottes/espèces/sites###
pdf('graphes/graphe2.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$massif)), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  labs(x = "Massif", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par massif") +
  theme_minimal() +
  scale_fill_discrete(name = "Espèces")
dev.off()
###Nombre d'espèces/moyens de conservations###
pdf('graphes/graphe3.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$conservation)), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par moyens de conservation") +
  theme_minimal() +
  scale_fill_discrete(name = "Espèces")
dev.off()

###Nombre de crottes/mois###
tabletnrl_clean$date <- gsub("-", "/", tabletnrl_clean$date)
tabletnrl_clean$month<-as.numeric(strftime(tabletnrl_clean$date,format="%m"))
pdf('graphes/graphe4.pdf')
barplot(table(tabletnrl_clean$month), xlab = "Mois", ylab = "Nombre d'échantillons", main = "Nombre d'échantillons par mois")
dev.off()
###Nombre de crottes/mois/sites###
pdf('graphes/graphe5.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$month, tabletnrl_clean$massif)),aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par mois") +
  theme_minimal()+
  scale_fill_discrete(name = "Mois")
dev.off()
###Nombre d'echantillons/Fraicheur###
sum(is.na(tabletnrl_clean$fraicheur))
pdf('graphes/graphe6.pdf')
barplot(table(tabletnrl_clean$fraicheur, useNA = "ifany"), xlab = "État de fraicheur", ylab = "Nombres d'échantillons", main = "Nombres d'échantillons par état de fraicheur")
dev.off()
###Liste expèces/altitude###
tabletnrl_clean$Tranche_altitude <- cut(as.numeric(tabletnrl_clean$altitude), breaks = seq(1500, 2500, by = 200), labels = FALSE)
for (i in 1:length(seq(1500, 2500, by = 200))) {
  cat("Tranche", i, ":", seq(1500, 2500, by = 200)[i], "-", seq(1500, 2500, by = 200)[i+1], "m\n")
}
pdf('graphes/graphe7.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$Tranche_altitude)), aes(x = Var2, y = Freq, color = Var1)) +
  geom_line() +
  geom_point() +
  labs(x = "Altitude (m)", y = "Nombre d'espèces", title = "Nombre d'espèces par altitude") +
  theme_minimal()
dev.off()
###Espèces supposées vs Espèces séquencées###
data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen))
pdf('graphes/graphe8.pdf')
ggplot(data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Espèces supposées", y = "Espèces séquencées", title = "Comparaison des espèces") +
  theme_minimal()
dev.off()

pdf('graphes/graphe8.pdf')
ggplot(data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Espèces supposées", y = "Occurrences", title = "Comparaison des occurrences par espèce supposée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name = "Espèces séquencées") 
dev.off()
###Etat de fraicheur vs occurence###
pdf('graphes/graphe9.pdf')
plots_list <- ggplot(tabletnrl_clean, aes(x = fraicheur, y = occurrence_cibles, fill = espece_gen)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x = "État de fraîcheur", y = "Nombre d'occurrences (échelle logarithmique)", title = "Nombre d'occurrences par espèces et par état de fraîcheur") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_discrete(name = "Espèces")
dev.off()
###comparaison doublons###
doublons_12s <- read.csv(file = "output/12s_doublons.csv", check.names = FALSE)
doublons_tnrl <- read.csv(file = "output/tnrl_doublons.csv", check.names = FALSE)
doublons_poac <- read.csv(file = "output/poac_doublons.csv", check.names = FALSE)
doublons_cype <- read.csv(file = "output/cype_doublons.csv", check.names = FALSE)
doublons_aste <- read.csv(file = "output/aste_doublons.csv", check.names = FALSE)

# Calculer la distance de Bray-Curtis entre chaque paire d'échantillons
bray_12s <- doublons_12s[, c(25:30, 32)]
bray_12s <- bray_12s[order(bray_12s$doublons), ]
distances_long <- vegdist(as.matrix(bray_12s[, c(1:6)]), method = "bray", zerodist = "add")
distances_long <- as.matrix(distances_long)

#distribution des doublons
colonnes <- doublons_poac[, 26:43]

par(mfrow = c(12, 12))
for (i in 1:ncol(colonnes)) {
  hist(colonnes[, i], main = paste("Colonne", i), xlab = "Valeur", ylab = "Fréquence")
}

###Collecteur vs occurence###
#regrouper les collecteur "agéris" vs les "non ageris", demander à Anne
pdf('graphes/graphe11.pdf')
ggplot(tabletnrl_clean, aes(x = observateur, y = occurrence_cibles, fill = espece_gen)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x = "État de fraîcheur", y = "Nombre d'occurrences (échelle logarithmique)", title = "Nombre d'occurrences par espèces et par état de fraîcheur") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_discrete(name = "Espèces")
dev.off()

###Puissance sequençage/famille de plantes/amorces###
families <- assignation_nom_niveau[assignation_nom_niveau[, 1] == "family", "nom"]
fam_aste <- data.frame(tableaste_clean[, colnames(tableaste_clean) %in% families], tableaste_clean$espece_gen)
fam_tnrl <- data.frame(tabletnrl_clean[, colnames(tabletnrl_clean) %in% families], tabletnrl_clean$espece_gen)
fam_poac <- data.frame(tablepoac_clean[, colnames(tablepoac_clean) %in% families], tablepoac_clean$espece_gen)
fam_cype <- data.frame(tablecype_clean[, colnames(tablecype_clean) %in% families], tablecype_clean$espece_gen)

fam_tot <- merge(fam_aste, fam_cype, by = 1) +
           merge(fam_tot, fam_poac, by = 1)+ 
           merge(fam_tot, fam_tnrl, by = 1)
#Asteraceae
#tnrl
pdf('graphes/graphe12.pdf')
ggplot(fam_tnrl, aes(x = tableaste_clean$espece_gen, y = Asteraceae)) +
  geom_boxplot(aes(fill = tableaste_clean$espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Asteracea (trnl)") +
  xlab("Espèces") +
  ylab("Occurence") +
  scale_y_log10()
dev.off()
#Aste
pdf('graphes/graphe13.pdf')
plots_list <- ggplot(fam_aste, aes(x = tableaste_clean$espece_gen, y = Asteraceae)) +
  geom_boxplot(aes(fill = tableaste_clean$espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Asteracea (Aste)") +
  xlab("Espèces") +
  ylab("Occurence")+
  scale_y_log10()
dev.off()

###Vaccinium###
#trnl
# Selectionne les colonumes avec Vaccinium
vaccinium <- tabletnrl_clean %>%
  select(espece_gen, `Vaccinium gaultherioides`, `Vaccinium ovalifolium`, `Vaccinium uliginosum`, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium$Vaccinium <- rowSums(vaccinium[, 2:4])
vaccinium <- vaccinium[, c(1, 5)]
#outliers
boxplot.stats(vaccinium$Vaccinium)$out
which(vaccinium$Vaccinium %in% c(boxplot.stats(vaccinium$Vaccinium)$out))
vaccinium_clean <- vaccinium[!(vaccinium$Vaccinium %in% boxplot.stats(vaccinium$Vaccinium)$out), ]

pdf('graphes/graphe14.pdf')
ggplot(as.data.frame(vaccinium_clean), aes(x = espece_gen, y = Vaccinium)) +
  geom_boxplot(aes(fill = espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Vaccinium sp (trnl)") +
  xlab("Espèces") +
  ylab("Occurence")+
  scale_y_log10()
dev.off()
#visualiser les effets différement 
vaccinium_clean$espece_gen <- as.factor(vaccinium_clean$espece_gen)
pdf('graphes/graphe15.pdf')
plot.design(Vaccinium ~ espece_gen, data = vaccinium_clean, ylab = "mean of occurence")
dev.off()
#Aste
# Selectionne les colonumes avec Vaccinium
vaccinium1 <- tableaste_clean %>%
  select(espece_gen, `Vaccinium vitis-idaea`, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium1$Vaccinium <- rowSums(vaccinium1[, 2:3])
vaccinium1 <- vaccinium1[, c(1, 3)]
#outliers
boxplot.stats(vaccinium1$Vaccinium)$out
which(vaccinium1$Vaccinium %in% c(boxplot.stats(vaccinium1$Vaccinium)$out))
vaccinium_clean1 <- vaccinium1[!(vaccinium1$Vaccinium %in% boxplot.stats(vaccinium1$Vaccinium)$out), ]

pdf('graphes/graphe16.pdf')
ggplot(as.data.frame(vaccinium1), aes(x = espece_gen, y = Vaccinium)) +
  geom_boxplot(aes(fill = espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Vaccinium sp (Aste)") +
  xlab("Espèces") +
  ylab("Occurence")+
  scale_y_log10()
dev.off()
#Cype
# Selectionne les colonumes avec Vaccinium
vaccinium2 <- tablecype_clean %>%
  select(espece_gen, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium2$Vaccinium <- rowSums(vaccinium1[, 2])
vaccinium2 <- vaccinium1[, c(1, 3)]
#outliers
boxplot.stats(vaccinium2$Vaccinium)$out
which(vaccinium2$Vaccinium %in% c(boxplot.stats(vaccinium2$Vaccinium)$out))
vaccinium_clean2 <- vaccinium2[!(vaccinium2$Vaccinium %in% boxplot.stats(vaccinium2$Vaccinium)$out), ]

pdf('graphes/graphe17.pdf')
ggplot(as.data.frame(vaccinium_clean2), aes(x = espece_gen, y = Vaccinium)) +
  geom_boxplot(aes(fill = espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Vaccinium sp (Cype)") +
  xlab("Espèces") +
  ylab("Occurence")
dev.off()

###diversité spécifique###
div2 <- read.csv("data/crotte_landscape.csv")
# Ajouter colonnes à la fin du tableau
tabletnrl_clean$diversite <- rowSums(tabletnrl_clean[, 25:dim(tabletnrl_clean)[2]] > 0)
div <- merge(div2, tabletnrl_clean[, c("N_Antagene", "diversite","espece_gen")], by = "N_Antagene", all.x = TRUE)
div <- subset(diversite, !(espece_gen %in% c("vache", "Non analysable", "mouton/mouflon", "chevreuil")))

#shdi250
ggplot(div, aes(x = shdi250, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "shdi250", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#shdi500
ggplot(div, aes(x = shdi500, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "shdi500", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#shdi1000
ggplot(div, aes(x = shdi1000, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "shdi1000", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#shdi1500
ggplot(div, aes(x = shdi1500, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "shdi1500", y = "Somme d'espèces de plantes par echantillon") +
  theme_minimal()
str(div$ed250)

#ed250
ggplot(div, aes(x = ed250, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "ed250", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#ed500
ggplot(div, aes(x = ed500, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "ed500", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#ed1000
ggplot(div, aes(x = ed1000, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "shdi1000", y = "Nombre d'espèces de plantes") +
  theme_minimal()
#ed1500
ggplot(div, aes(x = ed1500, y = diversite, color = espece_gen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "ed1500", y = "Somme d'espèces de plantes par echantillon") +
  theme_minimal()

###REGIME ALIMENTAIRE PAR ESPÈCES###
#Mise en forme données
familytnrl <- filter(taxontabletnrl, grepl("family", rang))
familyaste <- filter(taxontableaste, grepl("family", rang))
familycype <- filter(taxontablecype, grepl("family", rang))
familypoac <- filter(taxontablepoac, grepl("family", rang))
genretnrl <- filter(taxontabletnrl, grepl("genus", rang))
genreaste <- filter(taxontableaste, grepl("genus", rang))
genrecype <- filter(taxontablecype, grepl("genus", rang))
genrepoac <- filter(taxontablepoac, grepl("genus", rang))
especetnrl <- filter(taxontabletnrl, grepl("species", rang))
especeaste <- filter(taxontableaste, grepl("species", rang))
espececype <- filter(taxontablecype, grepl("species", rang))
especepoac <- filter(taxontablepoac, grepl("species", rang))

#CERF
#famille - tnrl
pdf('graphes/graphe20.pdf')
ggplot(data.frame(familytnrl), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Cerf (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - tnrl
pdf('graphes/graphe21.pdf')
ggplot(data.frame(genretnrl), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genres plantes consommées", y = "Nombre d'occurrences", title = "Cerf (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèces - tnrl
pdf('graphes/graphe22.pdf')
ggplot(data.frame(especetnrl), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Cerf (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Aste
pdf('graphes/graphe23.pdf')
ggplot(data.frame(familyaste), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Aste
pdf('graphes/graphe23.pdf')
ggplot(data.frame(genreaste), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genres plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Aste
pdf('graphes/graphe24.pdf')
ggplot(data.frame(especeaste), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Cype
pdf('graphes/graphe25.pdf')
ggplot(data.frame(familycype), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Cype
pdf('graphes/graphe26.pdf')
ggplot(data.frame(genrecype), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genres plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Cype
pdf('graphes/graphe27.pdf')
ggplot(data.frame(espececype), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Poac
pdf('graphes/graphe28.pdf')
ggplot(data.frame(familypoac), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Poac
pdf('graphes/graphe29.pdf')
ggplot(data.frame(genrepoac), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genres plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Poac
pdf('graphes/graphe30.pdf')
ggplot(data.frame(especepoac), aes(x = nom_scientifique, y = cerf)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Cerf (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#CHAMOIS
#famille - tnrl
pdf('graphes/graphe31.pdf')
ggplot(data.frame(familytnrl), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Chamois (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - tnrl
pdf('graphes/graphe32.pdf')
ggplot(data.frame(genretnrl), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Chamois (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - tnrl
pdf('graphes/graphe33.pdf')
ggplot(data.frame(especetnrl), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Chamois (tnrl)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Aste
pdf('graphes/graphe34.pdf')
ggplot(data.frame(familyaste), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Aste
pdf('graphes/graphe35.pdf')
ggplot(data.frame(genreaste), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genres plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Aste
pdf('graphes/graphe36.pdf')
ggplot(data.frame(especeaste), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèces plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Aste)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Cype
pdf('graphes/graphe37.pdf')
ggplot(data.frame(familycype), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Cype
pdf('graphes/graphe38.pdf')
ggplot(data.frame(genrecype), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genre plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Cype
pdf('graphes/graphe39.pdf')
ggplot(data.frame(espececype), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèce plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Cype)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#famille - Poac
pdf('graphes/graphe40.pdf')
ggplot(data.frame(familypoac), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Familles plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#genre - Poac
pdf('graphes/graphe41.pdf')
ggplot(data.frame(genrepoac), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Genre plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
#espèce - Poac
pdf('graphes/graphe42.pdf')
ggplot(data.frame(especepoac), aes(x = nom_scientifique, y = chamois)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 25000)) +
  labs(x = "Espèce plantes consommées", y = "Nombre d'occurrences", title = "Chamois (Poac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#HEAT MAP 
#chargement des données
freq12s <- read.csv(file = "output/12s_frequence.csv", check.names = FALSE)
freqtrnl <- read.csv(file = "output/trnl_frequence.csv", check.names = FALSE)
freqaste <- read.csv(file = "output/aste_frequence.csv", check.names = FALSE)
freqcype <- read.csv(file = "output/cype_frequence.csv", check.names = FALSE)
freqpoac <- read.csv(file = "output/poac_frequence.csv", check.names = FALSE)

#Création tableaux
trnl_freq <- tabletnrl_clean[, c(24:158, 2)]
# Diviser chaque valeur des colonnes par la colonne "occurrence_cibles"
trnl_freq[, 2:135] <- apply(trnl_freq[, 2:135], 1, function(x) x / trnl_freq$occurrence_cibles)
trnl_freq <- aggregate(trnl_freq[, 1:135], by=list(trnl_freq$espece_gen), FUN=sum)
names(trnl_freq)[names(trnl_freq) == "Group.1"] <- "espece_gen"
trnl_freq <- trnl_freq[, !colnames(trnl_freq) %in% c("occurrence_cibles")]
trnl_freq_long <- tidyr::pivot_longer(trnl_freq, cols = colnames(trnl_freq)[-1], names_to = "Plantes", values_to = "Valeur")
trnl_freq_long <- merge(trnl_freq_long, freqtrnl$rang)

#heatmap
ggplot(trnl_freq_long, aes(x = Plantes, y = espece_gen, fill = Valeur)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Plantes", y = "Espèce animal", fill = "Valeur") +
  ggtitle("Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data.frame(especepoac), aes(x = nom_scientifique, y = cerf, fill = cerf)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Plantes", y = "Espèces animal", fill = "Valeur") +
  ggtitle("Heatmap de familypoac") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###TABLEAU RECP AMORCES/ESPECES###
chamoistnrl <- filter(taxontabletnrl, chamois > 0)
chamoistnrl <- select(chamoistnrl, chamois, nom_scientifique)

files <- c(taxontableaste,
           taxontablecype,
           taxontablepoac,
           taxontabletnrl)
nom <- c("chamois_tnrl", "vache_tnrl", "cerf_tnrl", "mouton_tnrl", "bouquetin_tnrl",
         "chamois_aste", "vache_aste", "cerf_aste", "mouton_aste", "bouquetin_aste",
         "chamois_cype", "vache_cype", "cerf_cype", "mouton_cype", "bouquetin_cype",
         "chamois_poac", "vache_poac", "cerf_poac", "mouton_poac", "bouquetin_poac")

# Liste des colonnes à filtrer
columns_to_filter <- c("chamois", "cerf", "vache", "mouton", "bouquetin")
for (i in seq_along(files)) {
  df3 <- files[[i]]
  df3 <- df3 %>% filter(.data[[columns_to_filter]] > 0)
  df3 <- df3[, c(col_name, "nom_scientifique")]
  df_name <- paste0(nom[i], "_", col_name, "_aliments")
  assign(df_name, df_filtered)
  write.csv(file = paste0("output/", nom[i], "_table_taxon.csv"), df3)
}




