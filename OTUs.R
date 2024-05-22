###Tableau longuers barcodes/amorces###
#chargement des données 
OTUtrnl <- read.csv("data/OTUs/trnL/F0764-CREA_Regime_Ongules-trnL_OTUs_repartition.csv" )
OTUpoac <- read.csv("data/OTUs/poac/F0764-CREA_Regime_Ongules-Poac_OTUs_repartition.csv")
OTUcype <- read.csv("data/OTUs/cype/F0764-CREA_Regime_Ongules-Cype_OTUs_repartition.csv")
OTUaste <- read.csv("data/OTUs/aste/F0764-CREA_Regime_Ongules-Aste_OTUs_repartition.csv")
OTU12s <- read.csv("data/OTUs/12s/F0764-CREA_Regime_Ongules-12S_OTUs_repartition.csv")

# Fonction pour extraire la première valeur après le premier point-virgule
extract_first_value <- function(otu_string) {
  parts <- strsplit(otu_string, ";")[[1]]
  return(as.numeric(parts[2]))
}
# Appliquer la fonction à chaque élément de la colonne 'OTU'
OTU_tot_12s <- data.frame('12s' = sapply(OTU12s$OTU, extract_first_value))
OTU_tot_trnl <- data.frame('trnl' = sapply(OTUtrnl$OTU, extract_first_value))
OTU_tot_aste <- data.frame('aste' = sapply(OTUaste$OTU, extract_first_value))
OTU_tot_poac <- data.frame('poac' = sapply(OTUpoac$OTU, extract_first_value))
OTU_tot_cype <- data.frame('cype' = sapply(OTUcype$OTU, extract_first_value))

# Création du tableau OTUtot avec les colonnes spécifiées et les lignes pour les statistiques
OTUtot <- data.frame(
  Statistique = c("Minimum", "Maximum", "Moyenne", "Médiane"),
  OTU_tot_12s = numeric(4),
  OTU_tot_trnl = numeric(4),
  OTU_tot_aste = numeric(4),
  OTU_tot_poac = numeric(4),
  OTU_tot_cype = numeric(4)
)

# Fonction pour calculer la médiane 
calculate_median <- function(x) {
  sorted_x <- sort(x)
  n <- length(sorted_x)
  if (n %% 2 == 0) {
    return((sorted_x[n/2] + sorted_x[n/2 + 1]) / 2)
  } else {
    return(sorted_x[(n + 1) / 2])
  }
}

# Calculer la médiane, la moyenne, la valeur maximale et la valeur minimale
OTUtot$OTU_tot_12s <- c(
  min(OTU_tot_12s$X12s),
  max(OTU_tot_12s$X12s),
  mean(OTU_tot_12s$X12s),
  calculate_median(OTU_tot_12s$X12s)
)

OTUtot$OTU_tot_trnl <- c(
  min(OTU_tot_trnl$trnl),
  max(OTU_tot_trnl$trnl),
  mean(OTU_tot_trnl$trnl),
  calculate_median(OTU_tot_trnl$trnl)
)

OTUtot$OTU_tot_aste <- c(
  min(OTU_tot_aste$aste),
  max(OTU_tot_aste$aste),
  mean(OTU_tot_aste$aste),
  calculate_median(OTU_tot_aste$aste)
)

OTUtot$OTU_tot_poac <- c(
  min(OTU_tot_poac$poac),
  max(OTU_tot_poac$poac),
  mean(OTU_tot_poac$poac),
  calculate_median(OTU_tot_poac$poac)
)

OTUtot$OTU_tot_cype <- c(
  min(OTU_tot_cype$cype),
  max(OTU_tot_cype$cype),
  mean(OTU_tot_cype$cype),
  calculate_median(OTU_tot_cype$cype)
)
write.csv(file = paste0("output/resum_OTU.csv"), OTUtot)

#représentation graphique 
OTUtot_melted <- melt(OTUtot, id.vars = "Statistique", variable.name = "OTU", value.name = "Valeur")

ggplot(OTUtot_melted, aes(x = OTU, y = Valeur, fill = OTU)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Statistiques des OTUs", x = "OTU", y = "Valeur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Statistique, scales = "free_y")

