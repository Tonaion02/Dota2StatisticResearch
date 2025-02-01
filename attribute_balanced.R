library(corrplot)

getwd() 
setwd("ProgettiR\\ProggettoStatistica")  



# data <- read.table("dataset_dota2_augumented.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
data <- read.table("dataset_dota2_augumented_sintetico.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")

data <- data[data$gametype == 2, ]


# FREQUENZE RELATIVE PICKRATE E WINRATRE DI OGNI ATTRIBUTO(START)

# frequenze assolute di pick di ogni attributo
# voglio sapere se ce un attributo giocato piu degli altri


# calcolo del numero totale di istanze per ogni attributo
totale_istanze <- nrow(data)*2

# calcolo delle frequenze relative per ogni attributo
frequenze_relative <- c(
  strength = (sum(data$attribute_strength_1 > 0) + sum(data$attribute_strength_2 > 0)) / totale_istanze,
  intelligence = (sum(data$attribute_intelligence_1 > 0) + sum(data$attribute_intelligence_2 > 0)) / totale_istanze,
  agility = (sum(data$attribute_agility_1 > 0) + sum(data$attribute_agility_2 > 0)) / totale_istanze,
  universal = (sum(data$attribute_universal_1 > 0) + sum(data$attribute_universal_2 > 0)) / totale_istanze
)
frequenze_relative


# barplot Ranked: PickRate degli attributi
bp <- barplot(
  frequenze_relative,
  col = c("skyblue", "lightgreen", "lightcoral", "lightseagreen"),
  border = "black",
  main = "Ranked: PickRate degli attributi ",
  xlab = "Attributi",
  ylab = "Frequenza relativa",
  ylim = c(0, max(frequenze_relative) + 0.1)
)

# percentuali sopra le barre 
text(bp, 
     y = frequenze_relative + 0.05, 
     labels = paste0(round(frequenze_relative * 100, 1), "%"), 
     col = "black", 
     cex = 1)  

print(frequenze_relative)

#------------------------------------------------------------------------------------------------------------

frequenze = frequenze_relative*nrow(data)*2
frequenze

# nomi attributi
nomi_attributi <- c("Strength", "Intelligence", "Agility", "Universal")

# calcolo delle frequenze relative globali
frequenze_relative_globali <- c(
  strength = (sum(data$attribute_strength_1[data$win == 1] > 0) +
                sum(data$attribute_strength_2[data$win == -1] > 0)) / frequenze["strength"],
  intelligence = (sum(data$attribute_intelligence_1[data$win == 1] > 0) +
                    sum(data$attribute_intelligence_2[data$win == -1] > 0)) / frequenze["intelligence"],
  agility = (sum(data$attribute_agility_1[data$win == 1] > 0) +
               sum(data$attribute_agility_2[data$win == -1] > 0)) / frequenze["agility"],
  universal = (sum(data$attribute_universal_1[data$win == 1] > 0) +
                 sum(data$attribute_universal_2[data$win == -1] > 0)) / frequenze["universal"]
)

# numero totale di istanze nel dataset
total_instances <- nrow(data)*2

# barplot Ranked: WinRate attributi
bp <- barplot(
  frequenze_relative_globali,
  col = c("skyblue", "lightgreen", "lightcoral", "lightseagreen"),
  border = "black",
  main = "Ranked: WinRate attributi",
  xlab = "Attributi",
  ylab = "Frequenza relativa",
  names.arg = nomi_attributi,
  ylim = c(0, max(frequenze_relative_globali) + 0.1)
)

# valori delle frequenze relative sopra le barre
text(bp, 
     y = frequenze_relative_globali + 0.02,  
     labels = round(frequenze_relative_globali, 3), 
     col = "black", 
     cex = 1)  




# FREQUENZE RELATIVE PICKRATE E WINRATRE DI OGNI ATTRIBUTO(END)



# Grafico 2 riche 2 colonne
par(mfrow=c(2,2))


# FREQUENZE ASSOLUTE DI PICKRATE DI OGNI ATTRIBUTO (START)


# combina le colonne in un unico vettore per l'attributo strength
combined_values <- c(data$attribute_strength_1, data$attribute_strength_2)

# calcolo delle frequenze assolute totali
frequenze_totali <- table(combined_values)

# barplot Ranked: PickRate attributo Strength
bar_positions <- barplot(
  frequenze_totali/length(combined_values),
  col = "skyblue",
  border = "black",
  #main = "Ranked: PickRate attributo Strength",
  xlab = "Valori attributo Strength",
  ylab = "Frequenza",
  ylim = c(0, 0.5)
)

# valori sopra le colonne
text(
  x = bar_positions,                                    
  y = frequenze_totali / length(combined_values),        
  labels = round(frequenze_totali / length(combined_values), 2),  
  cex = 1,                                               
  col = "black",                                        
  pos = 3                                               
)

#------------------------------------------------------------------------------------------------------------


# combino le colonne in un unico vettore per l'attributo intelligence
combined_values <- c(data$attribute_intelligence_1, data$attribute_intelligence_2)

# calcolo delle frequenze assolute
frequenze_totali <- table(combined_values)
frequenze_totali

# barplot Unranked: Pickrate attributo Intelligence
bar_positions <- barplot(
  frequenze_totali/length(combined_values),
  col = "lightgreen",
  border = "black",
  #main = "Unranked: Pickrate attributo Intelligence",
  xlab = "Valori attributo Intelligence",
  ylab = "Frequenza",
  ylim = c(0, 0.54)
)

# valori sopra le colonne
text(
  x = bar_positions,                                  
  y = frequenze_totali / length(combined_values),        
  labels = round(frequenze_totali / length(combined_values), 2),  
  cex = 1,                                                  
  col = "black",                                            
  pos = 3                                                  
)


#------------------------------------------------------------------------------------------------------------


# combino le colonne in un unico vettore per agility
combined_values <- c(data$attribute_agility_1, data$attribute_agility_2)

# cCalcolo delle frequenze assolute 
frequenze_totali <- table(combined_values)
frequenze_totali

# barplot Unranked: Pickrate attributo Agility
bar_positions <- barplot(
  frequenze_totali/length(combined_values),
  col = "lightcoral",
  border = "black",
  #main = "Unranked: Pickrate attributo Agility",
  xlab = "Valori attributo Agility",
  ylab = "Frequenza",
  ylim = c(0, 0.48)
)

# valor sopra le colonne
text(
  x = bar_positions,                                      
  y = frequenze_totali / length(combined_values),          
  labels = round(frequenze_totali / length(combined_values), 2), 
  cex = 1,                                                 
  col = "black",                                         
  pos = 3                                                  
)


#------------------------------------------------------------------------------------------------------------


# combino le colonne in un unico vettore per universal
combined_values <- c(data$attribute_universal_1, data$attribute_universal_2)

# calcolo delle frequenze assolute 
frequenze_totali <- table(combined_values)
frequenze_totali

# barplot Unranked: Pickrate attributo Universal
bar_positions <- barplot(
  frequenze_totali/length(combined_values),
  col = "lightseagreen",
  border = "black",
  #main = "Unranked: Pickrate attributo Universal",
  xlab = "Valori attributo Universal",
  ylab = "Frequenza",
  ylim = c(0, 0.5)
)

# valori sopra le colonne
text(
  x = bar_positions,                                       
  y = frequenze_totali / length(combined_values),           
  labels = round(frequenze_totali / length(combined_values), 2), 
  cex = 1,                                                
  col = "black",                                           
  pos = 3                                                 
)



# FREQUENZE ASSOLUTE DI GIOCO DI OGNI ATTRIBUTO (END)


# Grafico con due riche e due colonne
par(mfrow=c(2,2))


# FREQUENZE ASSOLUTE DI PICKRATE DI OGNI ATTRIBUTO IN RAPPORTO CON IL WINRATE(START)



# filtro i valori per attribute_strength_1 quando win == 1
filtered_strength_1 <- data$attribute_strength_1[data$win == 1]

# filtro i valori per attribute_strength_2 quando win == -1
filtered_strength_2 <- data$attribute_strength_2[data$win == -1]

# unione dei valori filtrati in un unico vettore
combined_values <- c(filtered_strength_1, filtered_strength_2)

# calcolo delle frequenze assolute totali
frequenze_totali <- table(combined_values)
frequenze_totali_strength = frequenze_totali
frequenze_totali_strength

# calcolo delle frequenze relative globali per ogni valore di "strength"
frequenze_relative_globali <- c(
  strength_0 = sum(combined_values == 0) / sum(data$attribute_strength_1 == 0 | data$attribute_strength_2 == 0),
  strength_1 = sum(combined_values == 1) / sum(data$attribute_strength_1 == 1 | data$attribute_strength_2 == 1),
  strength_2 = sum(combined_values == 2) / sum(data$attribute_strength_1 == 2 | data$attribute_strength_2 == 2),
  strength_3 = sum(combined_values == 3) / sum(data$attribute_strength_1 == 3 | data$attribute_strength_2 == 3),
  strength_4 = sum(combined_values == 4) / sum(data$attribute_strength_1 == 4 | data$attribute_strength_2 == 4),
  strength_5 = sum(combined_values == 5) / sum(data$attribute_strength_1 == 5 | data$attribute_strength_2 == 5)
)

frequenze_relative_globali <- frequenze_relative_globali[0 : 4]

# barplot Ranked: WinRate per i valori di Strength
bp <- barplot(
  frequenze_relative_globali,
  col = "skyblue",
  border = "black",
  main = "Ranked: WinRate per i valori di Strength",
  xlab = "Valori attributo Strength",
  ylab = "Frequenza relativa",
  names.arg = c("0","1","2","3"),
  ylim = c(0, max(frequenze_relative_globali) + 0.1)
)

# valori delle frequenze relative sopra le barre
text(bp, 
     y = frequenze_relative_globali + 0.04, 
     labels = round(frequenze_relative_globali, 3),  
     col = "black",  
     cex = 1)  

print(frequenze_relative_globali)

frequenze_relative_globali_strength = frequenze_relative_globali
frequenze_relative_globali_strength

#------------------------------------------------------------------------------------------------------------


# filtro i valori per attribute_intelligence_1 quando win == 1
filtered_intelligence_1 <- data$attribute_intelligence_1[data$win == 1]

# filtro i valori per attribute_intelligence_2 quando win == -1
filtered_intelligence_2 <- data$attribute_intelligence_2[data$win == -1]

# unione dei valori filtrati in un unico vettore
combined_values_intelligence <- c(filtered_intelligence_1, filtered_intelligence_2)

# calcolo delle frequenze assolute totali per intelligence
frequenze_totali_intelligence <- table(combined_values_intelligence)
frequenze_totali_intelligence

# calcolo delle frequenze relative globali per ogni valore di "intelligence"
frequenze_relative_globali_intelligence <- c(
  intelligence_0 = sum(combined_values_intelligence == 0) / sum(data$attribute_intelligence_1 == 0 | data$attribute_intelligence_2 == 0),
  intelligence_1 = sum(combined_values_intelligence == 1) / sum(data$attribute_intelligence_1 == 1 | data$attribute_intelligence_2 == 1),
  intelligence_2 = sum(combined_values_intelligence == 2) / sum(data$attribute_intelligence_1 == 2 | data$attribute_intelligence_2 == 2),
  intelligence_3 = sum(combined_values_intelligence == 3) / sum(data$attribute_intelligence_1 == 3 | data$attribute_intelligence_2 == 3),
  intelligence_4 = sum(combined_values_intelligence == 4) / sum(data$attribute_intelligence_1 == 4 | data$attribute_intelligence_2 == 4),
  intelligence_5 = 0
)

frequenze_relative_globali_intelligence <- frequenze_relative_globali_intelligence[0 : 4]

# barplot Ranked: WinRate per i valori di Intelligence
bp_intelligence <- barplot(
  frequenze_relative_globali_intelligence,
  col = "lightgreen",
  border = "black",
  main = "Ranked: WinRate per i valori di Intelligence",
  xlab = "Valori attributo Intelligence",
  ylab = "Frequenza relativa",
  names.arg = c("0", "1", "2", "3"),
  ylim = c(0, max(frequenze_relative_globali_intelligence) + 0.1)
)

# valori delle frequenze relative sopra le barre
text(bp_intelligence, 
     y = frequenze_relative_globali_intelligence + 0.04, 
     labels = round(frequenze_relative_globali_intelligence, 3), 
     col = "black", 
     cex = 1) 


print(frequenze_relative_globali_intelligence)

#------------------------------------------------------------------------------------------------------------


# filtro i valori per attribute_agility_1 quando win == 1
filtered_agility_1 <- data$attribute_agility_1[data$win == 1]

# filtro i valori per attribute_agility_2 quando win == -1
filtered_agility_2 <- data$attribute_agility_2[data$win == -1]

# unione dei valori filtrati in un unico vettore
combined_values_agility <- c(filtered_agility_1, filtered_agility_2)

# calcolo delle frequenze assolute totali per agility
frequenze_totali_agility <- table(combined_values_agility)

# calcolo delle frequenze relative globali per ogni valore di "agility"
frequenze_relative_globali_agility <- c(
  agility_0 = sum(combined_values_agility == 0) / sum(data$attribute_agility_1 == 0 | data$attribute_agility_2 == 0),
  agility_1 = sum(combined_values_agility == 1) / sum(data$attribute_agility_1 == 1 | data$attribute_agility_2 == 1),
  agility_2 = sum(combined_values_agility == 2) / sum(data$attribute_agility_1 == 2 | data$attribute_agility_2 == 2),
  agility_3 = sum(combined_values_agility == 3) / sum(data$attribute_agility_1 == 3 | data$attribute_agility_2 == 3),
  agility_4 = sum(combined_values_agility == 4) / sum(data$attribute_agility_1 == 4 | data$attribute_agility_2 == 4),
  agility_5 = sum(combined_values_agility == 5) / sum(data$attribute_agility_1 == 5 | data$attribute_agility_2 == 5)
)

frequenze_relative_globali_agility <- frequenze_relative_globali_agility[0 : 4]

# barplot Ranked: WinRate per i valori di Agility
bp_agility <- barplot(
  frequenze_relative_globali_agility,
  col = "lightcoral",
  border = "black",
  main = "Ranked: WinRate per i valori di Agility",
  xlab = "Valori attributo Agility",
  ylab = "Frequenza relativa",
  names.arg = c("0", "1", "2", "3"),
  ylim = c(0, max(frequenze_relative_globali_agility) + 0.1)
)

# valori delle frequenze relative sopra le barre
text(bp_agility, 
     y = frequenze_relative_globali_agility + 0.04,  
     labels = round(frequenze_relative_globali_agility, 3), 
     col = "black", 
     cex = 1) 

print(frequenze_relative_globali_agility)


#------------------------------------------------------------------------------------------------------------


# filtro i valori per attribute_universal_1 quando win == 1
filtered_universal_1 <- data$attribute_universal_1[data$win == 1]

# filtro i valori per attribute_universal_2 quando win == -1
filtered_universal_2 <- data$attribute_universal_2[data$win == -1]

# unione dei valori filtrati in un unico vettore
combined_values_universal <- c(filtered_universal_1, filtered_universal_2)

# calcolo delle frequenze assolute totali per universal
frequenze_totali_universal <- table(combined_values_universal)
frequenze_totali_universal

# calcolo delle frequenze relative globali per ogni valore di "universal"
frequenze_relative_globali_universal <- c(
  universal_0 = sum(combined_values_universal == 0) / sum(data$attribute_universal_1 == 0 | data$attribute_universal_2 == 0),
  universal_1 = sum(combined_values_universal == 1) / sum(data$attribute_universal_1 == 1 | data$attribute_universal_2 == 1),
  universal_2 = sum(combined_values_universal == 2) / sum(data$attribute_universal_1 == 2 | data$attribute_universal_2 == 2),
  universal_3 = sum(combined_values_universal == 3) / sum(data$attribute_universal_1 == 3 | data$attribute_universal_2 == 3),
  universal_4 = sum(combined_values_universal == 4) / sum(data$attribute_universal_1 == 4 | data$attribute_universal_2 == 4),
  universal_5 = 0
)

frequenze_relative_globali_universal <- frequenze_relative_globali_universal[0 : 4]


# barplot Ranked: WinRate per i valori di Universal
bp_universal <- barplot(
  frequenze_relative_globali_universal,
  col = "lightseagreen",
  border = "black",
  main = "Ranked: WinRate per i valori di Universal",
  xlab = "Valori attributo Universal",
  ylab = "Frequenza relativa",
  names.arg = c("0", "1", "2", "3"),
  ylim = c(0, max(frequenze_relative_globali_universal) + 0.1)
)

# valori delle frequenze relative sopra le barre
text(bp_universal, 
     y = frequenze_relative_globali_universal + 0.04, 
     labels = round(frequenze_relative_globali_universal, 3), 
     col = "black",  
     cex = 1) 


print(frequenze_relative_globali_universal)
frequenze_relative_globali_universal


# FREQUENZE ASSOLUTE DI PICKRATE DI OGNI ATTRIBUTO IN RAPPORTO CON IL WINRATE(END)






# FORMAZIONI CON WINRATE MAGGIORE(START)



risultati_completi <- list()

for (i in 0:3) {
  for (j in 0:3) {
    for (k in 0:3) {
      for (l in 0:3) {
        
        if ((i + j + k + l) == 5) {
          
          # filtro per gli attributi che terminano con 1
          result_1 <- data[data$attribute_strength_1 == i & data$attribute_intelligence_1 == j &
                             data$attribute_agility_1 == k & data$attribute_universal_1 == l, ]
          
          # filtro per gli attributi che terminano con 2
          result_2 <- data[data$attribute_strength_2 == i & data$attribute_intelligence_2 == j &
                             data$attribute_agility_2 == k & data$attribute_universal_2 == l, ]
          
          # unione dei risultati di result_1 e result_2
          combined_result <- rbind(result_1, result_2)
          
          # considero solo le fomrazioni di team per le quali ho un numero maggiore uguale di 200 partite
          if (nrow(combined_result) >= 200) {
            
            # calcolo il numero di partite vinte per result_1 (win == 1)
            partite_vinte_1 <- sum(result_1$win == 1)
            
            # calcolo il numero di partite vinte per result_2 (win == -1)
            partite_vinte_2 <- sum(result_2$win == -1)
            
            # somma delle partite vinte dalla squadra 1 con la squadra 2
            partite_vinte_totali <- partite_vinte_1 + partite_vinte_2
            
            # calcolo il rapporto tra partite vinte e partite giocate di quel determinato team
            rapporto_vittorie <- partite_vinte_totali / nrow(combined_result)
            
            # memorizzo tutti i risultati in una lista
            risultati_completi[[paste(i, j, k, l, sep = "_")]] <- list(
              numero_istanze = nrow(combined_result),
              partite_vinte = partite_vinte_totali,
              rapporto_vittorie = rapporto_vittorie
            )
          }
        }
      }
    }
  }
}

# ordino i risultati in ordine decrescente del winrate 
risultati_ordinati <- risultati_completi[order(sapply(risultati_completi, function(x) x$rapporto_vittorie), decreasing = TRUE)]

# stampa dei risultati
for (key in names(risultati_ordinati)) {
  cat("\nCombinazione:", key, 
      "- Numero di istanze:", risultati_ordinati[[key]]$numero_istanze, 
      "- Partite vinte:", risultati_ordinati[[key]]$partite_vinte, 
      "- Rapporto vittorie/partite giocate:", round(risultati_ordinati[[key]]$rapporto_vittorie, 3), "\n")
}



# FORMAZIONI CON WINRATE MAGGIORE(END)





