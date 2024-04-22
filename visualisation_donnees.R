library(dplyr)
library(readxl)
library(ggplot2)

# Chargement des donnees
table12s <- read.csv(file = "output/12s_table.csv", row.names = 1L, check.names = FALSE)
tableaste <- read.csv(file = "output/aste_table.csv", row.names = 1L, check.names = FALSE)
tabletnrl <- read.csv(file = "output/tnrl_table.csv", row.names = 1L, check.names = FALSE)
tablecype <- read.csv(file = "output/cype_table.csv", row.names = 1L, check.names = FALSE)
tablepoac <- read.csv(file = "output/poac_table.csv", row.names = 1L, check.names = FALSE)
tableassign <- read.csv(file = "output/assignation_nom_niveau.csv", row.names = 1L, check.names = FALSE)
doublons <- read_xlsx("data/Doublons_crottes_total.xlsx", col_names = TRUE)

# Suppression des doublons
colnames(doublons) <- c("id_doublon", "id_interne")
doublons <- doublons %>% dplyr::distinct(id_doublon, .keep_all = TRUE)

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
  df <- df[!df$id_interne %in% doublons$id_interne,]
  
  assign(fichiers_clean[i], df)
  write.csv(file = paste0("output/", fichiers_clean[i], ".csv"), df)
    
}


# Création des graphs sur jeu de données trnl avec colonne espèces genetiques
###Nombre de crottes/espèces###
barplot(table(tabletnrl_clean$espece_gen), xlab = "Espèces", ylab =  "Nombre d'échantillons", main = "Nombres d'échantillons pour chaque Espèces")  

###Nombre de crottes/espèces/sites###
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$massif)), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  labs(x = "Massif", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par massif") +
  theme_minimal() +
  scale_fill_discrete(name = "Espèces")

###Nombre d'espèces/moyens de conservations###
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$conservation)),aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par moyens de conservation") +
  theme_minimal()+
  scale_fill_discrete(name = "Espèces")

###Nombre de crottes/mois###
tabletnrl_clean$date <- gsub("-", "/", tabletnrl_clean$date)
tabletnrl_clean$month<-as.numeric(strftime(tabletnrl_clean$date,format="%m"))
barplot(table(tabletnrl_clean$month), xlab = "Mois", ylab = "Nombre d'échantillons", main = "Nombre d'échantillons par mois")

###Nombre de crottes/mois/sites###
ggplot(as.data.frame(table(tabletnrl_clean$month, tabletnrl_clean$massif)),aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par mois") +
  theme_minimal()+
  scale_fill_discrete(name = "Mois")

###Nombre d'echantillons/Fraicheur###
sum(is.na(tabletnrl_clean$fraicheur))
barplot(table(tabletnrl_clean$fraicheur, useNA = "ifany"), xlab = "État de fraicheur", ylab = "Nombres d'échantillons", main = "Nombres d'échantillons par état de fraicheur")

###Liste expèces/altitude###
tabletnrl_clean$Tranche_altitude <- cut(as.numeric(tabletnrl_clean$altitude), breaks = seq(1500, 2500, by = 200), labels = FALSE)
for (i in 1:length(seq(1500, 2500, by = 200))) {
  cat("Tranche", i, ":", seq(1500, 2500, by = 200)[i], "-", seq(1500, 2500, by = 200)[i+1], "m\n")
}
ggplot(as.data.frame(table(tabletnrl_clean$espece_gen, tabletnrl_clean$Tranche_altitude)), aes(x = Var2, y = Freq, color = Var1)) +
  geom_line() +
  geom_point() +
  labs(x = "Altitude (m)", y = "Nombre d'espèces", title = "Nombre d'espèces par altitude") +
  theme_minimal()

###Nombre d'echantillons/mois/moyens de conservation/espèces###
ggplot(as.data.frame (table(tabletnrl_clean$month, tabletnrl_clean$espece_gen ,tabletnrl_clean$conservation)), aes(x = Var1, Var2, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyen de conservation", y = "Nombre d'échantillons", title = "Nombres d'échantillons par espèces et par moyen de conservation secteur Chamonix") +
  theme_minimal() +
  theme(axis.text.x = element_text (hjust = 1)) +
  scale_fill_discrete(name = "Espèces")

###Espèces supposées vs Espèces séquencées###
as.data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen))
ggplot(as.data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Espèces supposées", y = "Espèces séquencées", title = "Comparaison des espèces") +
  theme_minimal()
ggplot(as.data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Espèces supposées", y = "Occurrences", title = "Comparaison des occurrences par espèce supposée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name = "Espèces séquencées") 

###Etat de fraicheur vs occurence###
ggplot(tabletnrl_clean, aes(x = fraicheur, y = occurrence_cibles, fill = espece_gen)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x = "État de fraîcheur", y = "Nombre d'occurrences (échelle logarithmique)", title = "Nombre d'occurrences par espèces et par état de fraîcheur") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_discrete(name = "Espèces")

###Collecteur vs occurence###
#regrouper les collecteur "agéris" vs les "non ageris", demander à Anne
ggplot(tabletnrl_clean, aes(x = observateur, y = occurrence_cibles, fill = espece_gen)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x = "État de fraîcheur", y = "Nombre d'occurrences (échelle logarithmique)", title = "Nombre d'occurrences par espèces et par état de fraîcheur") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_discrete(name = "Espèces")

###Puissance sequençage/famille de plantes/amorces###
families <- assignation_nom_niveau[assignation_nom_niveau[, 1] == "family", "nom"]
data <- as.data.frame(tableaste_clean[, colnames(tableaste_clean) %in% families], tableaste_clean$espece_gen)

ggplot(data, aes(x = tableaste_clean$espece_gen, y = Asteraceae)) +
  geom_boxplot(aes(fill = tableaste_clean$espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Asteracea") +
  xlab("Espèces") +
  ylab("Occurence")


###Vaccinium###
#trnl
# Selectionne les colonumes avec Vaccinium
vaccinium <- tabletnrl_clean %>%
  select(espece_gen, `Vaccinium gaultherioides`, `Vaccinium ovalifolium`, `Vaccinium uliginosum`, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium$Vaccinium <- rowSums(vaccinium[, 2:4])
vaccinium <- vaccinium[, c(1, 5)]

ggplot(as.data.frame(vaccinium), aes(x = espece_gen, y = Vaccinium)) +
  geom_boxplot(aes(fill = espece_gen), color = "black", size = 1) +
  geom_jitter(color = "black", size = 2, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  ggtitle("Vaccinium sp (trnl)") +
  xlab("Espèces") +
  ylab("Occurence")

#Aste
# Selectionne les colonumes avec Vaccinium
vaccinium1 <- tableaste_clean %>%
  select(espece_gen, `Vaccinium vitis-idaea`, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium1$Vaccinium <- rowSums(vaccinium1[, 2:3])
vaccinium1 <- vaccinium1[, c(1, 3)]

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
  ylab("Occurence")

#Cype
# Selectionne les colonumes avec Vaccinium
vaccinium2 <- tablecype_clean %>%
  select(espece_gen, Vaccinium)
# Création d'une nouvelle colonne "Vaccinium" contenant la somme des colonnes 2, 3 et 4
vaccinium2$Vaccinium <- rowSums(vaccinium1[, 2])
vaccinium2 <- vaccinium1[, c(1, 3)]

ggplot(as.data.frame(vaccinium2), aes(x = espece_gen, y = Vaccinium)) +
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

###Espèces suavages vs espèces sequencées###

