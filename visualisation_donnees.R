library(dplyr)
library(readxl)
library(ggplot2)
library(purrr)
library(vegan)
library(tidyr)
library(reshape2)

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
###Nombre de crottes/mois/sites###
pdf('graphes/graphe4.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$month, tabletnrl_clean$massif)),aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par mois") +
  theme_minimal()+
  scale_fill_discrete(name = "Mois")
dev.off()
###Nombre de crottes/mois/Eespèces###
pdf('graphes/graphe5.pdf')
ggplot(as.data.frame(table(tabletnrl_clean$month, tabletnrl_clean$espece_gen)),aes(x = Var2, y = Freq, fill = Var1)) +
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
pdf('graphes/graphe8.pdf')
ggplot(data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Espèces supposées", y = "Espèces séquencées", title = "Comparaison des espèces") +
  theme_minimal()
dev.off()

pdf('graphes/graphe8.2.pdf')
ggplot(data.frame(table(tabletnrl_clean$espece_sup, tabletnrl_clean$espece_gen)), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Espèces supposées", y = "Occurrences", title = "Comparaison des occurrences par espèce supposée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_discrete(name = "Espèces séquencées") 
dev.off()
###Etat de fraicheur vs occurence###
pdf('graphes/graphe9.pdf')
ggplot(tabletnrl_clean, aes(x = fraicheur, y = occurrence_cibles, fill = espece_gen)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(x = "État de fraîcheur", y = "Nombre d'occurrences (échelle logarithmique)", title = "Nombre d'occurrences par espèces et par état de fraîcheur") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_discrete(name = "Espèces")
dev.off()

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
       
#RAREFACTION 
#TRNL
new_table <- freqtrnl %>%
  select(nom_scientifique, rang) 
#Création tableaux
trnl_rarefy <- tabletnrl_clean[, c(2, 24:158)] %>%
  pivot_longer(-c(espece_gen, occurrence_cibles), names_to = "nom_scientifique", values_to = "valeur")
trnl_rarefy <- trnl_rarefy[, -2]
#joind les tableaux avec valeurs et rangs
trnl_rarefy_df <- left_join(trnl_rarefy, new_table, by = "nom_scientifique")
#supprime colonne avec titre vide
trnl_rarefy_df <- trnl_rarefy_df %>% select(-matches("^$"))
#change valeaur en 1 ou 0 
trnl_rarefy_df$valeur <- ifelse(trnl_rarefy_df$valeur > 0, 1, 0)
#tri par espèce 
trnl_rarefy_df_sp <- subset(trnl_rarefy_df, rang == "species")
trnl_rarefy_df_gen <- subset(trnl_rarefy_df, rang == "genus")
#tri
trnl_rarefy_df_gen <- trnl_rarefy_df_gen[, c("espece_gen", "nom_scientifique", "valeur")]
trnl_rarefy_df_sp <- trnl_rarefy_df_sp[, c("espece_gen", "nom_scientifique", "valeur")]

df_gen <- trnl_rarefy_df_gen %>%
  pivot_wider(espece_gen) %>%
  as.data.frame()

t1 <- specaccum(trnl_rarefy_df_gen)

rownames(trnl_rarefy_df_gen) <- trnl_rarefy_df_gen$espece_gen
trnl_rarefy_df_gen <- trnl_rarefy_df_gen[, -1]
min_n_seqs <- trnl_rarefy_df %>%
  group_by(espece_gen) %>%
  summarize(n_seqs = sum(valeur)) %>%
  summarize(min = min(n_seqs)) %>%
  pull(min)

rarefy(trnl_rarefy_df_gen, min_n_seqs)

ggplot(trnl_rarefy_df_gen, aes(x = espece_gen, y = nom_scientifique, fill = valeur)) +
  geom_() +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("Absence", "Présence")) +
  labs(x = "Espèce animale", y = "Fréquence", title = "Présence/Absence des plantes par espèce animale")

#mettre en format large 
data_matrix <- as.data.frame(trnl_rarefy_df)
rownames(data_matrix) <- data_matrix$espece_gen
colnames(data_matrix) <- data_matrix$
raréfaction_curve <- rarecurve(data_matrix, step = 1, sample = nrow(data_matrix), col = "blue")
plot(raréfaction_curve, xlab = "Nombre d'individus échantillonnés", ylab = "Nombre d'espèces uniques observées", main = "Courbe de raréfaction aléatoire")


t1 <- specaccum(trnl_rarefy_df)
summary(t1)
plot(data_matrix, ylab = "Nb echantillons", xlab = "Nb nouvelles espèces")

trnl_rarefy <- tabletnrl_clean[, c(2, 25:158)] %>%
  filter() %>%
  pivot_longer(espece_gen, names_to = "nom_scientifique", values_to = "valeur") %>%
  as.data.frame() 

# Convertir la colonne "valeur" en numérique si nécessaire
trnl_rarefy_df_sp$valeur <- as.numeric(trnl_rarefy_df_sp$valeur)

# Calculer l'accumulation des espèces
t1 <- specaccum(trnl_rarefy_df_gen)

plot(t1, ylab = "Nb echantillons", xlab = "Nb nouvelles espèces")



