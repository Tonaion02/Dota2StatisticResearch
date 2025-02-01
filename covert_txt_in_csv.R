library(dplyr)
library(corrplot)
library(splines)
library(kernlab)
library(GauPro)


getwd()  
setwd("ProgettiR\\ProggettoStatistica")  

# nome del file contenente le istanze generate da chat-gpt
file_path <- "istanze_dataset_sintetico.txt"

# lettura del file
righe <- readLines(file_path)

# stampa le righe
print(righe)



# generazione delle colonne
gamemode <- integer(0)
gametype <- integer(0)
hero_columns <- as.data.frame(matrix(ncol = 113, nrow = 0))
colnames(hero_columns) <- paste0('hero', 1:113)
win <- integer(0)


# creazione del dataset sintetico
dataset_cleaned <- data.frame(
  gamemode = gamemode,
  gametype = gametype,
  hero_columns,
  win = win
)


# creazione delle colonne con il suffisso "_1"
dataset_cleaned$attribute_strength_1 = integer(0)
dataset_cleaned$attribute_agility_1 = integer(0)
dataset_cleaned$attribute_intelligence_1 = integer(0)
dataset_cleaned$attribute_universal_1 = integer(0)
dataset_cleaned$role_carry_1 = integer(0)
dataset_cleaned$role_sup_1 = integer(0)
dataset_cleaned$role_nuker_1 = integer(0)
dataset_cleaned$role_disabler_1= integer(0)
dataset_cleaned$role_jungler_1 = integer(0)
dataset_cleaned$role_durable_1= integer(0)
dataset_cleaned$role_escape_1 = integer(0)
dataset_cleaned$role_pusher_1 = integer(0)
dataset_cleaned$role_initiator_1 = integer(0)

# creazione delle colonne con il suffisso "_2"
dataset_cleaned$attribute_strength_2= integer(0)
dataset_cleaned$attribute_agility_2 = integer(0)
dataset_cleaned$attribute_intelligence_2 = integer(0)
dataset_cleaned$attribute_universal_2 = integer(0)
dataset_cleaned$role_carry_2 = integer(0)
dataset_cleaned$role_sup_2 = integer(0)
dataset_cleaned$role_nuker_2 = integer(0)
dataset_cleaned$role_disabler_2 = integer(0)
dataset_cleaned$role_jungler_2 = integer(0)
dataset_cleaned$role_durable_2 = integer(0)
dataset_cleaned$role_escape_2= integer(0)
dataset_cleaned$role_pusher_2 = integer(0)
dataset_cleaned$role_initiator_2 = integer(0)



for(i in 1 : length(righe)){
  riga = righe[i]
  lista_colonne <- strsplit(riga, split = ",")[[1]]
  lista_colonne
  for(j in 2 : 6){
    hero_colum= paste0("hero",lista_colonne[j])
    dataset_cleaned[i,hero_colum] = 1
  }
  for(j in 7 : 11){
    hero_colum= paste0("hero",lista_colonne[j])
    print(hero_colum)
    dataset_cleaned[i,hero_colum] = -1
  }
  dataset_cleaned[i,"gamemode"] = 2
  dataset_cleaned[i,"gametype"] = lista_colonne[1]
  
  if(lista_colonne[length(lista_colonne)] == 1){
    dataset_cleaned[i,"win"] = 1
  }else{
    dataset_cleaned[i,"win"] = -1
  }
}

dataset_cleaned[is.na(dataset_cleaned)] <- 0

write.table(dataset_cleaned, "sitetic_dataset_dota2_2.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = TRUE, dec = ".")


# my_data <- read.table("dataset_dota2_augumented_2.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
# my_data <- my_data[, -1]
# write.table(my_data, "dataset_dota2_augumented_2.csv", row.names = FALSE, col.names = TRUE, sep = ",", quote = TRUE, dec = ".")


predictions <- scan("predizioni_win.txt", what = numeric())
table(my_data$win)

for(i in 1 :length(predictions)){
  my_data$win[i] = predictions[i]
  
}



