library(readxl)
library(dplyr)
library(ggplot2)

table12s <- read_xlsx("data/01_F0764_CREA_MontBlanc_Ongulés_resultats_12S/F0764-CREA_Regime_Ongules-12S-plq01et02_Metadata_seuil_100-100-100.xlsx")
tabletrnl <- read_xlsx("data/02_F0764_CREA_MontBlanc_Ongulés_resultats_trnL/F0764-CREA_Regime_Ongules-trnL-plq01et02_Metadata_seuil_100-100-100.xlsx")
tablecype <- read_xlsx("data/04_F0764_CREA_MontBlanc_Ongulés_resultats_Cype/F0764-CREA_Regime_Ongules-Cype-plq01et02_Metadata_seuil_100-100-100.xlsx")
tableaste <- read_xlsx("data/03_F0764_CREA_MontBlanc_Ongulés_resultats_Aste/F0764-CREA_Regime_Ongules-Aste-plq01et02_Metadata_seuil_bruts.xlsx")
tablepoac <- read_xlsx("data/05_F0764_CREA_MontBlanc_Ongulés_resultats_Poac/F0764-CREA_Regime_Ongules-Poac-plq01et02_Metadata_seuil_100-100-100.xlsx")


### Formattage table 12S - Identification espece 
# Creation nouveaux noms de colonne
table12s <- table12s[-c(1,2),]
colnames(table12s) <- c("espece", "prelevement", "id_interne", "date", "observateur",
                        "commune", "site", "longitude", "latitude", "sexe", "conservation", "altitude",
                        "fraicheur", "milieu", "remarques", "id_antagene", "qualite",
                        "abondance_micro", "abondance_taxons_nc", "abondance_cibles",
                        "occurrence_micro", "occurrence_taxons_nc", "occurrence_cibles",
                        "genus_bos", "species_ibex", "species_ovis", "species_rupircapra", "species_capreolus", "species_elaphus")


for (j in 1:dim(table12s)[1]) {
  if (table12s[j, "commune"] %in% c("St Colomban", "Allemond")) {
    table12s[j, "massif"] <- "belledonne"
  } else {
    table12s[j, "massif"] <- "mont_blanc"
  }
}

table12s[table12s$massif == "belledonne",]$conservation <- "silicagel"
table12s[table12s$massif == "belledonne",]$fraicheur <- "oui"
table12s[table12s$conservation == "Alcool 90 denaturee",]$conservation <- "denature"

write.csv(file = "output/12s_table.csv", table12s)

### Formattage tables tnrl, aste, cype et poac 
# Remplacements noms de colonnes communes a tous les fichiers

fichier_list <- list(tabletrnl,
                     tablecype,
                     tablepoac,
                     tableaste)

base_nom <- c("tnrl",
              "cype",
              "poac",
              "aste")


for (i in 1:length(fichier_list)) {
  
  df <- fichier_list[[i]]
  fichier_niveaux <- as.data.frame(matrix(nrow = length(c(25:dim(df)[2])), ncol = 2))
  fichier_niveaux[, 1] <- t(df[1, c(25:dim(df)[2])])
  fichier_niveaux[, 2] <- t(df[2, c(25:dim(df)[2])])
  fichier_niveaux <- as.data.frame(as.matrix((fichier_niveaux)))
  colnames(fichier_niveaux) <- c("niveau", "nom")
  
  colnames(df)[c(1:24)] <- c("espece_sup", "espece_gen", "prelevement", "id_interne", "date", "observateur",
                             "commune", "site", "longitude", "latitude", "sexe", "conservation", "altitude",
                             "fraicheur", "milieu", "remarques", "id_antagene", "qualite",
                             "abondance_micro", "abondance_taxons_nc", "abondance_cibles",
                             "occurrence_micro", "occurrence_taxons_nc", "occurrence_cibles")
  
  colnames(df)[c(25:dim(df)[2])] <- df[2,c(25:dim(df)[2])]
  df <- df[-c(1:2),]
  
  for (j in 1:dim(df)[1]) {
    if (df[j, "commune"] %in% c("St Colomban", "Allemond")) {
      df[j, "massif"] <- "belledonne"
    } else {
      df[j, "massif"] <- "mont_blanc"
    }
  }
  
  df[df$massif == "belledonne",]$conservation <- "silicagel"
  df[df$massif == "belledonne",]$fraicheur <- "oui"
  df[df$conservation == "Alcool 90 denaturee",]$conservation <- "denature"
  
  assign(paste0(base_nom[i], "_table"), df)
  assign(paste0(base_nom[i], "_niveau"), fichier_niveaux)
  
  write.csv(file = paste0("output/", base_nom[i], "_table.csv"), df)

}  

assignation_nom_niveau <- rbind(tnrl_niveau, poac_niveau, aste_niveau, cype_niveau)  
assignation_nom_niveau <- assignation_nom_niveau %>% dplyr::distinct(niveau, nom)
write.csv(file = "output/assignation_nom_niveau.csv", assignation_nom_niveau)

