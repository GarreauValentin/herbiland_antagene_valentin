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
###TRNL###
#Création tableaux
trnl_freq <- tabletnrl_clean[, c(24:158, 2)] %>%
  filter() %>%
  pivot_longer(-c(espece_gen, occurrence_cibles), names_to = "nom_scientifique", values_to = "valeur")
#supprimer les occurence_cible = 0
trnl_freq <- trnl_freq %>% filter(occurrence_cibles != 0)
#joind les tableaux avec valeurs et rangs
heatmap_trnl <- left_join(trnl_freq, freqtrnl, by = "nom_scientifique")
#supprime colonne avec titre vide
heatmap_trnl <- heatmap_trnl %>% select(-matches("^$"))
##supprime chevreuil et non analysable 
heatmap_trnl <- heatmap_trnl %>% filter(!espece_gen %in% c("chevreuil", "Non analysable"))
#supprime les lignes =0 dans la colonnes valeur
heatmap_trnl <- heatmap_trnl %>% filter(valeur != 0)
#sommer par espèces les occurences des plantes
heatmap_trnl_sum <- aggregate(cbind(occurrence_cibles, valeur) ~ espece_gen + nom_scientifique + rang, data = heatmap_trnl, FUN = sum)
#divise valeur par occurence cible
heatmap_trnl_sum$freq <- heatmap_trnl_sum$valeur / heatmap_trnl_sum$occurrence_cibles
#tri les espèces <0,005
heatmap_trnl_sum <- heatmap_trnl_sum %>%  filter(valeur >= 0.005)
#tri par espèce 
heatmap_trnl_espece <- subset(heatmap_trnl_sum, rang == "species")
#tri par genre 
heatmap_trnl_genre <- subset(heatmap_trnl_sum, rang == "genus")
# Filtrer les lignes où nom_scientifique n'est pas "Ceratodon purpureus"
heatmap_trnl_espece_light <- heatmap_trnl_espece %>%
  filter(nom_scientifique != "Ceratodon purpureus")
#tri par espèce pour cerf
heatmap_trnl_espece_cerf <- subset(heatmap_trnl_espece, espece_gen == "cerf")
#tri par espèce pour chamois
heatmap_trnl_espece_chamois <- subset(heatmap_trnl_espece, espece_gen == "chamois")

pdf('graphes/HeatMap_espèce_trnl.pdf')
ggplot(data = heatmap_trnl_espece, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  labs(x = "Espèces animal", y = "Plantes (espèce)", title = "Heatmap trnl")
dev.off()

pdf('graphes/HeatMap_genre _trnl.pdf')
ggplot(data = heatmap_trnl_genre, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  labs(x = "Espèces animal", y = "Plantes (genre)", title = "Heatmap trnl")
dev.off()

ggplot(data = heatmap_trnl_espece_cerf, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  labs(x = "Cerf", y = "Plantes (genre)", title = "Heatmap trnl")

ggplot(data = heatmap_trnl_espece_light, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  labs(x = "Cerf", y = "Plantes (genre)", title = "Heatmap trnl")

###ASTE###
#Création tableaux
aste_freq <- tableaste_clean[, c(24:42, 2)] %>%
  filter() %>%
  pivot_longer(-c(espece_gen, occurrence_cibles), names_to = "nom_scientifique", values_to = "valeur")
#supprimer les occurence_cible = 0
aste_freq <- aste_freq %>% filter(occurrence_cibles != 0)
#joind les tableaux avec valeurs et rangs
heatmap_aste <- left_join(aste_freq, freqtrnl, by = "nom_scientifique")
#supprime colonne avec titre vide
heatmap_aste <- heatmap_aste %>% select(-matches("^$"))
#supprime chevreuil et non analysable 
heatmap_aste <- heatmap_aste %>% filter(!espece_gen %in% c("chevreuil", "Non analysable"))
#supprime les lignes =0 dans la colonnes valeur
heatmap_aste <- heatmap_aste %>% filter(valeur != 0)
#sommer par espèces les occurences des plantes
heatmap_aste_sum <- aggregate(cbind(occurrence_cibles, valeur) ~ espece_gen + nom_scientifique + rang, data = heatmap_aste, FUN = sum)
#divise valeur par occurence cible
heatmap_aste_sum$freq <- heatmap_aste_sum$valeur / heatmap_aste_sum$occurrence_cibles
#tri les espèces <0,005
#heatmap_aste_sum <- heatmap_aste_sum %>%  filter(valeur >= 0.005)
#tri par espèce 
heatmap_aste_espece <- subset(heatmap_aste_sum, rang == "species")
#tri par espèce 
heatmap_aste_genre <- subset(heatmap_aste_sum, rang == "genus")

ggplot(data = heatmap_aste_espece, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Espèces animal", y = "Plantes (genre)", title = "Heatmap aste")

ggplot(data = heatmap_aste_genre, aes(x = espece_gen, y = nom_scientifique, fill = freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Espèces animal", y = "Plantes (espèce)", title = "Heatmap aste")
