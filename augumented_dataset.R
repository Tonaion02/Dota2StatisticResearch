
library(jsonlite)

# leggo il file json
data <- fromJSON("heroes.json")

str(data)


heroes <- data$heroes
  
triples <- list()
  
for(i in 1:113){
  vn = c(heroes[i,"id"],heroes[i,"attribute"],heroes[i,"role"],heroes[i,"complexity"])
  triples <- c(triples, list(vn))
}
 


----------------------------------------------------------------------------------

library(dplyr)
library(corrplot)
library(splines)
library(kernlab)
library(GauPro)


getwd()  
setwd("ProgettiR\\ProggettoStatistica") 


# selezione del dataset
# my_data <- read.table("dota2_2.csv", header = TRUE, sep = ";", na.strings = "NA", dec = ",")
 my_data <- read.table("sitetic_dataset_dota2_2.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
 
# trasformazione delle colonne stringe in int
columInt = paste0("hero", 22:113)
for (name in columInt) {
  my_data[,name] = as.integer(as.numeric(my_data[,name]))
}
my_data$win = as.integer(as.numeric(my_data$win))

dataset_cleaned = filter(my_data, gamemode==2 & (gametype==2 | gametype==3))


dataset_cleaned <- dataset_cleaned[complete.cases(dataset_cleaned), ]

# creazione delle colonne con il suffisso "_1"
dataset_cleaned$attribute_strength_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_agility_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_intelligence_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_universal_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_carry_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_sup_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_nuker_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_disabler_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_jungler_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_durable_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_escape_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_pusher_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_initiator_1 <- integer(nrow(dataset_cleaned))

# creazione delle colonne con il suffisso "_2"
dataset_cleaned$attribute_strength_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_agility_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_intelligence_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$attribute_universal_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_carry_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_sup_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_nuker_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_disabler_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_jungler_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_durable_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_escape_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_pusher_2 <- integer(nrow(dataset_cleaned))
dataset_cleaned$role_initiator_2 <- integer(nrow(dataset_cleaned))

dataset_cleaned$complexity_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$complexity_2 <- integer(nrow(dataset_cleaned))



for(i in 1:113){
  name_colonna_hero = paste0("hero", i)
  colonna_hero = dataset_cleaned[[name_colonna_hero]]
  
  for(j in 1 : length(colonna_hero)){
    if(colonna_hero[j] == 1){
      # aumento il valore degli attributi per tutte le istanze in cui compare il campione heroi
      
      if(triples[[i]][2]== 0){
        dataset_cleaned[j, "attribute_strength_1"] <- dataset_cleaned[j, "attribute_strength_1"] + 1
      }else if(triples[[i]][2]== 1){
        dataset_cleaned[j, "attribute_agility_1"] <- dataset_cleaned[j, "attribute_agility_1"] + 1
      }else if(triples[[i]][2]== 2){
        dataset_cleaned[j, "attribute_intelligence_1"] <- dataset_cleaned[j, "attribute_intelligence_1"] + 1
      }else if(triples[[i]][2]== 3){
        dataset_cleaned[j, "attribute_universal_1"] <- dataset_cleaned[j, "attribute_universal_1"] + 1
      }
      
      if(triples[[i]][3]== 0){
        dataset_cleaned[j, "role_carry_1"] <- dataset_cleaned[j, "role_carry_1"] + 1
      }else if(triples[[i]][3]== 1){
        dataset_cleaned[j, "role_sup_1"] <- dataset_cleaned[j, "role_sup_1"] + 1
      }else if(triples[[i]][3]== 2){
        dataset_cleaned[j, "role_nuker_1"] <- dataset_cleaned[j, "role_nuker_1"] + 1
      }else if(triples[[i]][3]== 3){
        dataset_cleaned[j, "role_disabler_1"] <- dataset_cleaned[j, "role_disabler_1"] + 1
      }else if(triples[[i]][3]== 4){
        dataset_cleaned[j, "role_jungler_1"] <- dataset_cleaned[j, "role_jungler_1"] + 1
      }else if(triples[[i]][3]== 5){
        dataset_cleaned[j, "role_durable_1"] <- dataset_cleaned[j, "role_durable_1"] + 1
      }else if(triples[[i]][3]== 6){
        dataset_cleaned[j, "role_escape_1"] <- dataset_cleaned[j, "role_escape_1"] + 1
      }else if(triples[[i]][3]== 7){
        dataset_cleaned[j, "role_pusher_1"] <- dataset_cleaned[j, "role_pusher_1"] + 1
      }else if(triples[[i]][3]== 8){
        dataset_cleaned[j, "role_initiator_1"] <- dataset_cleaned[j, "role_initiator_1"] + 1
      }
      
    }else if(colonna_hero[j] == -1){
      
      if(triples[[i]][2]== 0){
        dataset_cleaned[j, "attribute_strength_2"] <- dataset_cleaned[j, "attribute_strength_2"] + 1
      }else if(triples[[i]][2]== 1){
        dataset_cleaned[j, "attribute_agility_2"] <- dataset_cleaned[j, "attribute_agility_2"] + 1
      }else if(triples[[i]][2]== 2){
        dataset_cleaned[j, "attribute_intelligence_2"] <- dataset_cleaned[j, "attribute_intelligence_2"] + 1
      }else if(triples[[i]][2]== 3){
        dataset_cleaned[j, "attribute_universal_2"] <- dataset_cleaned[j, "attribute_universal_2"] + 1
      }
      
      if(triples[[i]][3]== 0){
        dataset_cleaned[j, "role_carry_2"] <- dataset_cleaned[j, "role_carry_2"] + 1
      }else if(triples[[i]][3]== 1){
        dataset_cleaned[j, "role_sup_2"] <- dataset_cleaned[j, "role_sup_2"] + 1
      }else if(triples[[i]][3]== 2){
        dataset_cleaned[j, "role_nuker_2"] <- dataset_cleaned[j, "role_nuker_2"] + 1
      }else if(triples[[i]][3]== 3){
        dataset_cleaned[j, "role_disabler_2"] <- dataset_cleaned[j, "role_disabler_2"] + 1
      }else if(triples[[i]][3]== 4){
        dataset_cleaned[j, "role_jungler_2"] <- dataset_cleaned[j, "role_jungler_2"] + 1
      }else if(triples[[i]][3]== 5){
        dataset_cleaned[j, "role_durable_2"] <- dataset_cleaned[j, "role_durable_2"] + 1
      }else if(triples[[i]][3]== 6){
        dataset_cleaned[j, "role_escape_2"] <- dataset_cleaned[j, "role_escape_2"] + 1
      }else if(triples[[i]][3]== 7){
        dataset_cleaned[j, "role_pusher_2"] <- dataset_cleaned[j, "role_pusher_2"] + 1
      }else if(triples[[i]][3]== 8){
        dataset_cleaned[j, "role_initiator_2"] <- dataset_cleaned[j, "role_initiator_2"] + 1
      }
    }
  
    # Calcolo complessita totale di un team
    if(colonna_hero[j] == 1){
      dataset_cleaned[j, "complexity_1"] <- dataset_cleaned[j, "complexity_1"] + triples[[i]][4]
    } else if(colonna_hero[j] == -1){
      dataset_cleaned[j, "complexity_2"] <- dataset_cleaned[j, "complexity_2"] + triples[[i]][4]
    }
  }
}

dataset_cleaned





# nome del file che contiene gli i counter di ogni hero
file_path <- "heroes_counters.txt"

# leggo il file e lo trasformo inun vewttore di liste
righe_a_liste <- function(file_path) {

  righe <- readLines(file_path, warn = FALSE)
  
  # converto ogni riga in una lista di numeri, ignorando righe non valide
  liste <- lapply(righe, function(riga) {
    numeri <- suppressWarnings(as.numeric(unlist(strsplit(riga, ",[ \t]*"))))
    if (all(is.na(numeri))) {
      warning(paste("Riga non valida trovata e ignorata:", riga))
      return(NULL)
    }
    return(numeri)
  })
  
  # rimizione elmenti nall
  liste <- Filter(Negate(is.null), liste)
  
  return(liste)
}


vettore_di_liste <- righe_a_liste(file_path)





# aggiungo le colonne del numoero di counterPick per squadra 
dataset_cleaned$counter_pick_1 <- integer(nrow(dataset_cleaned))
dataset_cleaned$counter_pick_2 <- integer(nrow(dataset_cleaned))


# aggiungo i valori ai campi del nuovo dataset

for(i in 1:113){
  name_colonna_hero = paste0("hero", i)
  colonna_hero = dataset_cleaned[[name_colonna_hero]]
  
  # per ogni riga del dataset
  for(j in 1 : length(colonna_hero)){
    if(colonna_hero[j] == 1){
       
      # per ogni elemento nella lista dei counterPick  del hero j
      for(elemento in vettore_di_liste[[i]]){
        name_colonna_hero_countered = paste0("hero", elemento)
        if(dataset_cleaned[j, name_colonna_hero_countered] == -1){
           
           dataset_cleaned[j, "counter_pick_1"] <- dataset_cleaned[j, "counter_pick_1"] + 1
         }
       }
      
      
    } else if(colonna_hero[j] == -1){
      
      # per ogni elemento nella lista dei counterPick  del hero j
      for(elemento in vettore_di_liste[[i]]){
        name_colonna_hero_countered = paste0("hero", elemento)
        if(dataset_cleaned[j, name_colonna_hero_countered] == 1){
          
          dataset_cleaned[j, "counter_pick_2"] <- dataset_cleaned[j, "counter_pick_2"] + 1
        }
      }
      
    }
  }
}



# aggiungo la colonna elo
dataset_cleaned$elo_game <- numeric(nrow(dataset_cleaned))

# funzione per calcolare l'elo
calculate_z <- function(x, y, alpha = 0.5, beta = 0.5) {
  # Controlla che i valori di x e y siano nei range validi
  if (x < 10 || x > 30) {
    print(x)
    print(y)
    stop("Il parametro 'x' deve essere compreso tra 10 e 30.")
  }
  if (y < 0 || y > 25) {
    stop("Il parametro 'y' deve essere compreso tra 0 e 25.")
  }
  
  # normalizzazione di x e y
  x_normalized <- (x - 10) / 20  # Da 10-30 a 0-1
  y_normalized <- y / 25         # Da 0-25 a 0-1
  
  # calcolo di z
  z <- 500 + ((alpha * x_normalized + beta * y_normalized) * (5500 - 500))
  
  return(z)
}


# parametri
alpha <- 0.85
beta <- 0.15


# ciclo per calcolare l'elo di ogni partita
for (i in 1:nrow(dataset_cleaned)) {
  # somma dei parametri 'complexity_1' e 'complexity_2' per il parametro x
  x <- dataset_cleaned$complexity_1[i] + dataset_cleaned$complexity_2[i]
  
  # somma dei parametri 'counter_pick_1' e 'counter_pick_2' per il parametro y
  y <- dataset_cleaned$counter_pick_1[i] + dataset_cleaned$counter_pick_2[i]
  
  # calcolo del valore elo_game utilizzando la funzione calculate_z
  dataset_cleaned$elo_game[i] <- calculate_z(x, y, alpha, beta)
}




mean(dataset_cleaned$elo_game)

hist(dataset_cleaned$elo_game)

write.csv(dataset_cleaned, "dataset_dota2_augumented_2.csv", row.names = TRUE)










