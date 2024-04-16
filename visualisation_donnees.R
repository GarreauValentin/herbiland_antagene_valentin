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


# Création des graphs 
###Nombre de crottes/espèces###
table(table12s_clean$espece)
barplot(table(table12s_clean$espece), xlab = "Espèces", ylab =  "Nombre d'échantillons", main = "Nombres d'échantillons pour chaque Espèces")  

###Nombre de crottes/espèces/sites###
ggplot(as.data.frame(table(table12s_clean$espece, table12s_clean$massif)), aes(x = Var2, y = Freq, fill = Var1) + 
         geom_bar(stat = "identity") +
         labs(x = "Massif", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par massif") +
         theme_minimal()+
         scale_fill_discrete(name = "Espèces"))

###Nombre d'espèces/moyens de conservations###
ggplot(as.data.frame(table(table12s_clean$espece, table12s_clean$conservation)),aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(x = "Moyens de conservation", y = "Nombre d'espèces échantillonnées", title = "Nombre d'espèces échantillonnées par moyens de conservation") +
  theme_minimal()+
  scale_fill_discrete(name = "Espèces")

###Nombre de crottes/mois###
