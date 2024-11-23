library(dplyr)
library(corrplot)

getwd()  # Mostra la directory corrente
setwd("ProgettiR\\ProggettoStatistica")  # Imposta una nuova directory di lavoro


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
barplot(freq, ylab="Frequenze", xlab="Gamemode", main="Frequenze coppie Gamemode Gametype", col= 1:3)
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

hero_columns <- my_data2[, paste0("hero", 1:113)]  

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
# Numero di volte che ogni campione è stato selezionato
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(count_values)



# Grafico combinato con gametype=2 
par(mfrow=c(2,1))

# Frequenze relative di pick degli Hero con gamemode=2 e gametype=2
my_data3 = filter(my_data, gamemode==2 & gametype==2)

hero_columns <- my_data3[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values3 <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(sapply(count_values3, function(x) x/nrow(my_data2)))

# Frequenze relative di pick degli Hero con gamemode=2 e gametype=3
my_data4 = filter(my_data, gamemode==2 & gametype==3)

hero_columns <- my_data4[, paste0("hero", 1:113)] 

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values4 <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

barplot(sapply(count_values4, function(x) x/nrow(my_data2)))



# Differenza tra pickRate con (gamemode=2 e gametype=2) e (gamemode=2 e gametype=3)

barplot(sapply(count_values3, function(x) x/nrow(my_data2)) - sapply(count_values4, function(x) x/nrow(my_data2)), ylim = c(-0.1,0.2))





# WinRate degli Hero

# Definizione della funzione
compare_hero_to_win <- function(data, hero_columns, win_column) {
  # Crea un vettore per memorizzare i risultati
  results <- numeric(length(hero_columns))
  
  win_col_value= data$win
  
  # Loop su ogni colonna hero
  for (i in seq_along(hero_columns)) {
    hero_col <- hero_columns[i]
    # Conta le righe dove il valore di hero è uguale al valore di win
    
    hero_col_val = data[,hero_col]
    
    for(j in seq_along(hero_col_val)){
      if(hero_col_val[j] == win_col_value[j]){
        results[i] = results[i]+1;
      }
    }
    
    #results[i] <- sum(data[[hero_col]] == data[[win_column]], na.rm = TRUE)
  }
  
  print(results)
  
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



# Rapporto WinRate e PickRate
plot(count_values,result, xlab="PickRate", ylab="WinRate")

# Correlazione tra WinRate e PickRate

# Creazione del dataframe con colonne PickRate e WinRate
df_cor = data.frame(
  PickRate = count_values,
  WinRate = result
  )

cor(df_cor, use = "complete.obs")

corrplot.mixed(cor(df_cor, use = "complete.obs"),upper ="circle", lower = "number")



# Istogramma del WinRate
hist(result, freq=TRUE, xlab= "WinRate", ylab="frequenze")

result <- result[!is.nan(result)]

# Kernel density plot WinRate
n = length(result)
h_sturges = (max(result) - min(result)) / sqrt(n)

density_sturges = density(result, bw=h_sturges)
plot(density_sturges, main="WinRate")


# Istogramma del PickRate
result = sapply(count_values, function(x) x/nrow(my_data2))

hist(result, freq=TRUE, xlab= "PickRate", ylab="freqienze")

result <- result[!is.nan(result)]

# Kernel density plot PickRate
n = length(result)
h_sturges = (max(result) - min(result)) / sqrt(n)

density_sturges = density(result, bw=h_sturges)
plot(density_sturges, main="PickRate")




# BoxPlot ad intaglio del WinRate con gamemode=2 e gametype=2 o gametype=3

# WinRate con gamemode=2 e gametype=2
WinRate1 <- compare_hero_to_win(my_data3, hero_column_names, win_column_name)

hero_columns <- my_data3[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

for(i in seq_along(WinRate1)){
  WinRate1[i]= WinRate1[i]/count_values[i]
}


# WinRate con gamemode=2 e gametype=3
WinRate2 <- compare_hero_to_win(my_data4, hero_column_names, win_column_name)

hero_columns <- my_data4[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  # Conta quante volte appaiono 1 o -1
  sum(col == 1) + sum(col == -1) 
})

for(i in seq_along(WinRate2)){
  WinRate2[i]= WinRate2[i]/count_values[i]
}

boxplot(WinRate1, WinRate2, notch = TRUE)

summary(WinRate1)
summary(WinRate2)

WinRate1 = WinRate1[!is.nan(WinRate1)]
WinRate2 = WinRate2[!is.nan(WinRate2)]

IQR_1 = quantile(WinRate1,0.75)-quantile(WinRate1,0.25)
M1_1 = quantile(WinRate1,0.5) - 1.57*IQR_1 / sqrt(length(WinRate1))
M2_1 = quantile(WinRate1,0.5) + 1.57*IQR_1 / sqrt(length(WinRate1))

IQR_2 = quantile(WinRate2,0.75)-quantile(WinRate2,0.25)
M1_2 = quantile(WinRate2,0.5) - 1.57*IQR_2 / sqrt(length(WinRate2))
M2_2 = quantile(WinRate2,0.5) + 1.57*IQR_2 / sqrt(length(WinRate2))

confidenza = c(M1_1 - M2_2, M2_1 - M1_2)
confidenza
# Chiedere al prof come calcolare la percentuale

