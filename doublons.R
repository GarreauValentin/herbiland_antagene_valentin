library(dplyr)
library(tidyr)
library(vegan)
library(tibble)
library(ggplot2)


###comparaison doublons###
doublons_12s <- read.csv(file = "output/12s_doublons.csv", check.names = FALSE)
doublons_trnl <- read.csv(file = "output/tnrl_doublons.csv", check.names = FALSE)
doublons_poac <- read.csv(file = "output/poac_doublons.csv", check.names = FALSE)
doublons_cype <- read.csv(file = "output/cype_doublons.csv", check.names = FALSE)
doublons_aste <- read.csv(file = "output/aste_doublons.csv", check.names = FALSE)

#12s
# Distance de Bray-Curtis entre chaque paire d'échantillons
bray_12s <- doublons_12s[, c(25:30, 32)] %>%
  group_by(doublons) %>%
  mutate(doublons = as.numeric(paste0(doublons, ".", rep(1:2, length.out = n())))) %>%
  ungroup() %>%
  pivot_longer(-doublons) %>%
  group_by(doublons) %>%
  mutate(total = sum(value)) %>%
  group_by(name) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)
# Séparer en deux colonnes A et B
bray_12s <- bray_12s %>%
  mutate(A = ifelse(grepl("\\.1$", doublons), doublons, NA),
         B = ifelse(grepl("\\.2$", doublons), doublons, NA)) %>%
  mutate(A = as.numeric(gsub("\\.1$", "", A)),
         B = as.numeric(gsub("\\.2$", "", B))) %>%
  select(-doublons)
# Initialiser le vecteur pour stocker les distances
distances_12s <- numeric()
# Itérer sur les numéros de colonnes (de 1 à 25)
for (i in 1:25) {
  # Sélectionner les données de la colonne A et de la colonne B correspondantes
  data_A <- bray_12s %>%
    filter(A == i) %>%
    select(-A, -B)  
  data_B <- bray_12s %>%
    filter(B == i) %>%
    select(-A, -B)  
  # Calculer la distance de Bray-Curtis entre les deux ensembles de données
  dist_12s <- vegdist(rbind(data_A, data_B), method = "bray")
  # Ajouter la distance à la liste des résultats
  distances_12s <- c(distances_12s, dist_12s[1])  # On ne récupère que la distance entre les premières lignes de A et B
}
print(distances)
# Créer un diagramme en barres des distances de Bray-Curtis
barplot(distances_12s, 
        names.arg = 1:25,  # Noms des échantillons
        xlab = "Échantillons", 
        ylab = "Distance de Bray-Curtis",
        main = "Distances de Bray-Curtis entre échantillons")
# Créer un tableau
distance_df_12s <- as.data.frame(t(distances_12s))
colnames(distance_df_12s) <- paste0("Doublon ", 1:25)
write.csv(file = paste0("output/bray_curtis_doublons.csv"), distance_df_12s)


#trnl
# Distance de Bray-Curtis entre chaque paire d'échantillons
bray_trnl <- doublons_trnl[, c(24:42, 44)] %>%
  group_by(doublons) %>%
  mutate(doublons = as.numeric(paste0(doublons, ".", rep(1:2, length.out = n())))) %>%
  ungroup() %>%
  pivot_longer(-doublons) %>%
  group_by(doublons) %>%
  mutate(total = sum(value)) %>%
  group_by(name) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)
# Séparer en deux colonnes A et B
bray_trnl <- bray_trnl %>%
  mutate(A = ifelse(grepl("\\.1$", doublons), doublons, NA),
         B = ifelse(grepl("\\.2$", doublons), doublons, NA)) %>%
  mutate(A = as.numeric(gsub("\\.1$", "", A)),
         B = as.numeric(gsub("\\.2$", "", B))) %>%
  select(-doublons)
# Initialiser le vecteur pour stocker les distances
distances_trnl <- numeric()
# Itérer sur les numéros de colonnes (de 1 à 25)
for (i in 1:25) {
  # Sélectionner les données de la colonne A et de la colonne B correspondantes
  data_A <- bray_trnl %>%
    filter(A == i) %>%
    select(-A, -B)  
  data_B <- bray_trnl %>%
    filter(B == i) %>%
    select(-A, -B)  
  # Calculer la distance de Bray-Curtis entre les deux ensembles de données
  dist_trnl <- vegdist(rbind(data_A, data_B), method = "bray")
  # Ajouter la distance à la liste des résultats
  distances_trnl <- c(distances_trnl, dist_trnl[1])  # On ne récupère que la distance entre les premières lignes de A et B
}
print(distances_trnl)
# Créer un diagramme en barres des distances de Bray-Curtis
barplot(distances_trnl, 
        names.arg = 1:25,  # Noms des échantillons
        xlab = "Échantillons", 
        ylab = "Distance de Bray-Curtis",
        main = "Distances de Bray-Curtis entre échantillons")
# Créer un tableau
distance_df_trnl <- as.data.frame(t(distances_trnl))
colnames(distance_df_trnl) <- paste0("Doublon ", 1:25)
write.csv(file = paste0("output/bray_curtis_doublons.csv"), distance_df_trnl)

#aste
# Remplacer le nom de la colonne 'doublons.x' par 'doublons'
doublons_aste <- doublons_aste %>%
  rename(doublons = doublons.x) %>%
  filter(!is.na(occurrence_cibles)) 
# Supprimer les 3 dernières colonnes
cols_to_keep <- names(doublons_aste)[1:(ncol(doublons_aste) - 3)]
doublons_aste <- doublons_aste %>%
  select(all_of(cols_to_keep))
# Distance de Bray-Curtis entre chaque paire d'échantillons
bray_aste <- doublons_aste[, c(24:42, 44)] %>%
  group_by(doublons) %>%
  mutate(doublons = as.numeric(paste0(doublons, ".", rep(1:2, length.out = n())))) %>%
  ungroup() %>%
  pivot_longer(-doublons) %>%
  group_by(doublons) %>%
  mutate(total = sum(value)) %>%
  group_by(name) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)
# Séparer en deux colonnes A et B
bray_aste <- bray_aste %>%
  mutate(A = ifelse(grepl("\\.1$", doublons), doublons, NA),
         B = ifelse(grepl("\\.2$", doublons), doublons, NA)) %>%
  mutate(A = as.numeric(gsub("\\.1$", "", A)),
         B = as.numeric(gsub("\\.2$", "", B))) %>%
  select(-doublons)
# Initialiser le vecteur pour stocker les distances
distances_aste <- numeric()
# Itérer sur les numéros de colonnes (de 1 à 25)
for (i in 1:25) {
  # Sélectionner les données de la colonne A et de la colonne B correspondantes
  data_A <- bray_aste %>%
    filter(A == i) %>%
    select(-A, -B)  
  data_B <- bray_aste %>%
    filter(B == i) %>%
    select(-A, -B)  
  # Calculer la distance de Bray-Curtis entre les deux ensembles de données
  dist_aste <- vegdist(rbind(data_A, data_B), method = "bray")
  # Ajouter la distance à la liste des résultats
  distances_aste <- c(distances_aste, dist_aste[1])  # On ne récupère que la distance entre les premières lignes de A et B
}
print(distances_aste)
# Créer un diagramme en barres des distances de Bray-Curtis
barplot(distances_aste, 
        names.arg = 1:25,  # Noms des échantillons
        xlab = "Échantillons", 
        ylab = "Distance de Bray-Curtis",
        main = "Distances de Bray-Curtis entre échantillons")
# Créer un tableau
distance_df_aste <- as.data.frame(t(distances_aste))
colnames(distance_df_aste) <- paste0("Doublon ", 1:25)
write.csv(file = paste0("output/bray_curtis_doublons.csv"), distance_df_aste)


#cype
# Remplacer le nom de la colonne 'doublons.x' par 'doublons'
doublons_cype <- doublons_cype %>%
  rename(doublons = doublons.x) %>%
  filter(!is.na(occurrence_cibles)) 
# Supprimer les 3 dernières colonnes
cols_to_keep <- names(doublons_cype)[1:(ncol(doublons_cype) - 1)]
doublons_cype <- doublons_cype %>%
  select(all_of(cols_to_keep))
# Distance de Bray-Curtis entre chaque paire d'échantillons
bray_cype <- doublons_cype[, c(24:42, 44)] %>%
  group_by(doublons) %>%
  mutate(doublons = as.numeric(paste0(doublons, ".", rep(1:2, length.out = n())))) %>%
  ungroup() %>%
  pivot_longer(-doublons) %>%
  group_by(doublons) %>%
  mutate(total = sum(value)) %>%
  group_by(name) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)
# Séparer en deux colonnes A et B
bray_cype <- bray_cype %>%
  mutate(A = ifelse(grepl("\\.1$", doublons), doublons, NA),
         B = ifelse(grepl("\\.2$", doublons), doublons, NA)) %>%
  mutate(A = as.numeric(gsub("\\.1$", "", A)),
         B = as.numeric(gsub("\\.2$", "", B))) %>%
  select(-doublons)
# Initialiser le vecteur pour stocker les distances
distances_cype <- numeric()
# Itérer sur les numéros de colonnes (de 1 à 25)
for (i in 1:25) {
  # Sélectionner les données de la colonne A et de la colonne B correspondantes
  data_A <- bray_cype %>%
    filter(A == i) %>%
    select(-A, -B)  
  data_B <- bray_cype %>%
    filter(B == i) %>%
    select(-A, -B)  
  # Calculer la distance de Bray-Curtis entre les deux ensembles de données
  dist_cype <- vegdist(rbind(data_A, data_B), method = "bray")
  # Ajouter la distance à la liste des résultats
  distances_cype <- c(distances_cype, dist_cype[1])  # On ne récupère que la distance entre les premières lignes de A et B
}
print(distances_cype)
# Créer un diagramme en barres des distances de Bray-Curtis
barplot(distances_cype, 
        names.arg = 1:25,  # Noms des échantillons
        xlab = "Échantillons", 
        ylab = "Distance de Bray-Curtis",
        main = "Distances de Bray-Curtis entre échantillons")
# Créer un tableau
distance_df_cype <- as.data.frame(t(distances_cype))
colnames(distance_df_cype) <- paste0("Doublon ", 1:25)
write.csv(file = paste0("output/bray_curtis_doublons.csv"), distance_df_cype)

#poac
# Distance de Bray-Curtis entre chaque paire d'échantillons
bray_poac <- doublons_poac[, c(24:42, 46)] %>%
  group_by(doublons) %>%
  mutate(doublons = as.numeric(paste0(doublons, ".", rep(1:2, length.out = n())))) %>%
  ungroup() %>%
  pivot_longer(-doublons) %>%
  group_by(doublons) %>%
  mutate(total = sum(value)) %>%
  group_by(name) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)
# Séparer en deux colonnes A et B
bray_poac <- bray_poac %>%
  mutate(A = ifelse(grepl("\\.1$", doublons), doublons, NA),
         B = ifelse(grepl("\\.2$", doublons), doublons, NA)) %>%
  mutate(A = as.numeric(gsub("\\.1$", "", A)),
         B = as.numeric(gsub("\\.2$", "", B))) %>%
  select(-doublons)
# Initialiser le vecteur pour stocker les distances
distances_poac <- numeric()
# Itérer sur les numéros de colonnes (de 1 à 25)
for (i in 1:25) {
  # Sélectionner les données de la colonne A et de la colonne B correspondantes
  data_A <- bray_poac %>%
    filter(A == i) %>%
    select(-A, -B)  
  data_B <- bray_poac %>%
    filter(B == i) %>%
    select(-A, -B)  
  # Calculer la distance de Bray-Curtis entre les deux ensembles de données
  dist_poac <- vegdist(rbind(data_A, data_B), method = "bray")
  # Ajouter la distance à la liste des résultats
  distances_poac <- c(distances_poac, dist_poac[1])  # On ne récupère que la distance entre les premières lignes de A et B
}
print(distances_poac)
# Créer un diagramme en barres des distances de Bray-Curtis
barplot(distances_poac, 
        names.arg = 1:25,  # Noms des échantillons
        xlab = "Échantillons", 
        ylab = "Distance de Bray-Curtis",
        main = "Distances de Bray-Curtis entre échantillons")
# Créer un tableau
distance_df_poac <- as.data.frame(t(distances_poac))
colnames(distance_df_poac) <- paste0("Doublon ", 1:25)
write.csv(file = paste0("output/bray_curtis_doublons.csv"), distance_df_poac)

###TESTS STATISTIQUES###
#TRNL
#Supprimer les NaN
distances_trnl <- distances_trnl[!is.nan(distances_trnl)]

# Tester la significativité des distances de Bray-Curtis par rapport à 0
wilcox_testtrnl <- wilcox.test(distances_trnl, mu = 0)

# Afficher les résultats du test
print(wilcox_testtrnl)

#12s
#Supprimer les NaN
distances_12s <- distances_12s[!is.nan(distances_12s)]

# Tester la significativité des distances de Bray-Curtis par rapport à 0
wilcox_test12s <- wilcox.test(distances_12s, mu = 0)

# Afficher les résultats du test
print(wilcox_test12s)
