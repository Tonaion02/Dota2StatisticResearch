library(dplyr)

getwd()  # Mostra la directory corrente
# setwd("ProgettiR\\ProggettoStatistica")  # Imposta una nuova directory di lavoro


# Separazione delle colonne
my_data <- read.table("dota2_2.csv", header = TRUE, sep = ";", na.strings = "NA", dec = ",")

# Trasformazione delle colonne stringe in int
columInt = paste0("hero", 22:113)
for (name in columInt) {
  my_data[,name] = as.integer(as.numeric(my_data[,name]))
}
my_data$win = as.integer(as.numeric(my_data$win))



# Stampa frequenze gamemode
freq <- table(my_data$gamemode)
print(freq)
barplot(freq)

# Stampa frequenze gametype
freq <- table(my_data$gametype)
print(freq)
barplot(freq)

# Stampa frequenze gamemode e gametype
freq <- table(my_data$gametype,my_data$gamemode)
print(freq)
barplot(freq, ylab="Frequenze", xlab="Gamemode",legend.title="Gametype", main="Frequenze coppie Gamemode Gametype", col= 1:3)
# Aggiunta della legenda
legend(
  "topright", 
  legend = sort(unique(my_data$gametype)), 
  col = 1:3, 
  pch = 15, 
  title = "gametype"
)


# Valori null
any_na <- any(is.na(my_data))
print(any_na)


# Frequenze di pick degli Hero con gamemode=2 e gametype=2|3
#install pakages dplyr
my_data2 = filter(my_data, gamemode==2 & (gametype==2 | gametype==3))

hero_columns <- my_data2[, paste0("hero", 1:113)]  # Per esempio, da hero1 a hero3

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(count_values)



# Grafico combinato con gametype=2 
par(mfrow=c(2,1))

# Frequenze di pick degli Hero con gamemode=2 e gametype=2
my_data3 = filter(my_data, gamemode==2 & gametype==2)

hero_columns <- my_data3[, paste0("hero", 1:113)]  # Per esempio, da hero1 a hero3

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(sapply(count_values, function(x) x/nrow(my_data2)))


# Frequenze di pick degli Hero con gamemode=2 e gametype=3
my_data4 = filter(my_data, gamemode==2 & gametype==3)

hero_columns <- my_data4[, paste0("hero", 1:113)]  # Per esempio, da hero1 a hero3

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(sapply(count_values, function(x) x/nrow(my_data2)))




# WinRate degli Hero

# Definizione della funzione
compare_hero_to_win <- function(data, hero_columns, win_column) {
  # Crea un vettore per memorizzare i risultati
  results <- numeric(length(hero_columns))
  
  # Loop su ogni colonna hero
  for (i in seq_along(hero_columns)) {
    hero_col <- hero_columns[i]
    # Conta le righe dove il valore di hero è uguale al valore di win
    results[i] <- sum(data[[hero_col]] == data[[win_column]], na.rm = TRUE)
  }
  
  # Ritorna i risultati come un named vector
  names(results) <- hero_columns
  return(results)
}

# Utilizzo della funzione sul dataset hero_columns
hero_column_names <- paste0("hero", 1:113)
win_column_name <- "win"

# Calcola il numero di istanze per ogni colonna hero
result <- compare_hero_to_win(my_data2, hero_column_names, win_column_name)

for(i in 1:113){
  result[i]= result[i]/count_values[i]
}
  

# Mostra i risultati
barplot(result)


plot(count_values,result)