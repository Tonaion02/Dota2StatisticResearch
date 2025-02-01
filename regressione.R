library(dplyr)
library(corrplot)
library(splines)
library(kernlab)
library(GauPro)



getwd()  
setwd("ProgettiR\\ProggettoStatistica") 

# selezione del dataset
#my_data <- read.table("dataset_dota2_augumented.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
my_data <- read.table("dataset_dota2_augumented_sintetico.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")


# trasformazione delle colonne stringe in int
columInt = paste0("hero", 22:113)
for (name in columInt) {
  my_data[,name] = as.integer(as.numeric(my_data[,name]))
}
my_data$win = as.integer(as.numeric(my_data$win))



# costanti

hero_column_names <- paste0("hero", 1:113)
win_column_name <- "win"




# Definizione funzioni(START)


# Win degli Hero
compute_hero_win_abs <- function(data, hero_columns, win_column) {

  results <- numeric(length(hero_columns))
  
  win_col_value= data$win
  
  # Loop su ogni colonna hero
  for (i in seq_along(hero_columns)) {
    hero_col <- hero_columns[i]
    # Conto le righe dove il valore di hero è uguale al valore di win
    
    hero_col_val = data[,hero_col]
    
    for(j in seq_along(hero_col_val)){
      if(hero_col_val[j] == win_col_value[j]){
        results[i] = results[i]+1;
      }
    }
  }
  
  names(results) <- hero_columns
  return(results)
}


#Definizione funzioni(END)




# Creazione dataset utilizati(START)


# Dataset Cleaned(START)


# Dataset contenente solo le istanze valutate
dataset_cleaned = filter(my_data, gamemode==2 & (gametype==2 | gametype==3))

hero_values <- dataset_cleaned[, paste0("hero", 1:113)]  

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
# Numero di volte che ogni campione è stato selezionato
hero_pick_abs <- apply(hero_values, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})


# Calcolo il WinRate
winrate <- compute_hero_win_abs(dataset_cleaned, hero_column_names, win_column_name)

for(i in 1:113){
  winrate[i]= winrate[i]/hero_pick_abs[i]
}

# Calcolo il PickRate
pickrate = c()

for(i in 1:113){
  pickrate[i]= hero_pick_abs[i]/nrow(dataset_cleaned)
}


# Dataset Cleaned(END)




# Dataset GameType=2(START)


# Dataset contenente solo le istanze delle partite ranked
dataset_gametype2 = filter(my_data, gamemode==2 & gametype==2)

hero_values <- dataset_gametype2[, paste0("hero", 1:113)]  

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
# Numero di volte che ogni campione è stato selezionato
hero_pick_abs_gametype2 <- apply(hero_values, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})


# Calcolo il WinRate
winrate_gemetype2 <- compute_hero_win_abs(dataset_gametype2, hero_column_names, win_column_name)

for(i in 1:113){
  winrate_gemetype2[i]= winrate_gemetype2[i]/hero_pick_abs_gametype2[i]
}

# Calcolo il PickRate
pickrate_gametype2 = c()

for(i in 1:113){
  pickrate_gametype2[i]= hero_pick_abs_gametype2[i]/nrow(dataset_gametype2)
}


# Dataset GameType=2(END)




# Dataset GameType=3(START)


# Dataset contenente solo le istanze delle parite unranked
dataset_gametype3 = filter(my_data, gamemode==2 & gametype==3)

hero_values <- dataset_gametype3[, paste0("hero", 1:113)]  

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
# Numero di volte che ogni campione è stato selezionato
hero_pick_abs_gametype3 <- apply(hero_values, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})


# Calcolo il WinRate
winrate_gemetype3 <- compute_hero_win_abs(dataset_gametype3, hero_column_names, win_column_name)

for(i in 1:113){
  winrate_gemetype3[i]= winrate_gemetype3[i]/hero_pick_abs_gametype3[i]
}

# Calcolo il PickRate
pickrate_gametype3 = c()

for(i in 1:113){
  pickrate_gametype3[i]= hero_pick_abs_gametype3[i]/nrow(dataset_gametype3)
}


# Dataset GameType=3(END)


# Creazione dataset utilizati(END)





# plot Rapporto WinRate e PickRate Ranked
plot(winrate_gemetype2, pickrate_gametype2,
     main = "Rapporto WinRate e PickRate Ranked", 
     xlab = "WinRate",                    
     ylab = "PickRate",                      
     col = "black",                      
     pch = 1,                              
     cex = 1.2,                           
     cex.main = 1.5,                       
     cex.lab = 1.2,                       
     cex.axis = 1)                          

# linea orizzontale a y = 0.45
abline(v = 0.45, col = "blue", lwd = 2, lty = 2)

# linea verticale a x = 0.15
abline(h = 0.15 , col = "blue", lwd = 2, lty = 2)

grid(col = "gray", lty = "dotted")




# rapporto WinRate e PickRate Unranked
plot(winrate_gemetype3, pickrate_gametype3,
     main = "Rapporto WinRate e PickRate Unranked",  
     xlab = "WinRate",                    
     ylab = "PickRate",                     
     col = "black",                       
     pch = 1,                              
     cex = 1.2,                           
     cex.main = 1.5,                      
     cex.lab = 1.2,                       
     cex.axis = 1)                          

# inea orizzontale a y = 0.15
abline(h = 0.15, col = "blue", lwd = 2, lty = 2)

# linea verticale a x = 0.45
abline(v = 0.45 , col = "blue", lwd = 2, lty = 2)

grid(col = "gray", lty = "dotted")








# creazione del dataframe con colonne PickRate e WinRate
df_cor = data.frame(
  PickRate = pickrate,
  WinRate = winrate
)

cor(df_cor, use = "complete.obs")
corrplot.mixed(cor(df_cor, use = "complete.obs"),upper ="circle", lower = "number")




# Regressione Spline WinRate e PickRate(START)


spline_winrate_pickrate <- lm(winrate ~ bs(pickrate, knots=seq(0.0, 0.15, length.out=10)))

preds <- predict(spline_winrate_pickrate, newdata=list(x=pickrate))

# Plot Rapporto WinRate e PickRate
plot(winrate,pickrate, xlab = "WinRate", ylab = "PickRate", main="Spline")


# aggiungo il valore di ARS in basso a destra, leggermente all'interno del riquadro
text(x = par("usr")[2] - 0.17 * (par("usr")[2] - par("usr")[1]),  
     y = par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3]),
     labels = "RS: 0.2411", 
     pos = 4, 
     cex = 1, 
     col = "blue", 
     xpd = TRUE)


punti <- data.frame(pickrate = pickrate, preds = preds)


# ordino il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]
lines(punti_ordinati, col="blue")

summary(spline_winrate_pickrate)


# Regressione Spline WinRate e PickRate(END)




# Gaussian Process Regression WinRate e PickRate(START)

# Eliminazioni valore NaN
winrate = winrate[-24]
winrate = winrate[-107]

pickrate = pickrate[-24]
pickrate = pickrate[-107]


# costruzione del modello di regressione gaussiana
gausmodel_winrate_pickrate <- gausspr(x = pickrate, y = winrate, kernel = "rbfdot")

# previsione sui dati di input
predictions <- predict(gausmodel_winrate_pickrate, pickrate)

# visualizzazione dei dati e della predizione
plot(winrate, pickrate, main = "Gaussian Process Regression",
     xlab = "WinRate",ylab = "PickRate")

# aggiungo il valore di ARS in basso a destra, leggermente all'interno del riquadro
text(x = par("usr")[2] - 0.17 * (par("usr")[2] - par("usr")[1]), 
     y = par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3]), 
     labels = "RS: 0.3189", 
     pos = 4, 
     cex = 1, 
     col = "blue", 
     xpd = TRUE)

punti <- data.frame(pickrate = pickrate, preds = predictions)

# ordino il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]
lines(punti_ordinati, col ="blue")


# calcolo la somma dei quadrati totali (TSS)
tss <- sum((winrate - mean(winrate))^2)

# calcolo la somma dei quadrati residui (RSS)
rss <- sum((winrate - predictions)^2)

# calcolo R-quadro (R²)
r_squared <- 1 - (rss / tss)
print(paste("R-squared:", r_squared))

# numero di osservazioni (n) e numero di parametri del modello (p)
n <- length(winrate)  
p <- 1          # Numero di predittori. 

# calcolo R-quadro aggiustato
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
print(paste("Adjusted R-squared:", r_squared_adj))


# Gaussian Process Regression WinRate e PickRate(END)






# creazione del dataframe con colonne PickRate e WinRate
df_cor = data.frame(
  PickRateRanked = pickrate_gametype2,
  WinRateRanked = winrate_gemetype2
)

cor(df_cor, use = "complete.obs")
corrplot.mixed(cor(df_cor, use = "complete.obs"),upper ="circle", lower = "number")




# inverto per non invertire tutte le operaioni fatte su di essi
temp = winrate_gemetype2
winrate_gemetype2 = pickrate_gametype2
pickrate_gametype2 = temp


# Regressione Spline WinRate e PickRate Gamemode=2 e Gametype=2(START)


spline_winrate_pickrate_gametype2 <- lm(winrate_gemetype2 ~ bs(pickrate_gametype2, knots=seq(0.0, 0.15, length.out=10)))

preds <- predict(spline_winrate_pickrate_gametype2, newdata=list(x=pickrate_gametype2))

# Plot Rapporto WinRate e PickRate
plot(pickrate_gametype2,winrate_gemetype2, xlab = "WinRate", ylab = "PickRate", main="Spline Ranked")

text(x = par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1]),
     y = par("usr")[4] - 0.05 * (par("usr")[4] - par("usr")[3]),
     labels = "RS: 0.1205", 
     pos = 4, 
     cex = 1, 
     col = "blue", 
     xpd = TRUE)

punti <- data.frame(pickrate = pickrate_gametype2, preds = preds)

# ordinaro il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]
lines(punti_ordinati, col="blue")

summary(spline_winrate_pickrate_gametype2)


# Regressione Spline WinRate e PickRate Gamemode=2 e Gametype=2(END)




# creazione del dataframe con colonne PickRate e WinRate
df_cor = data.frame(
  PickRateUnranked = pickrate_gametype3,
  WinRateUnranked = winrate_gemetype3
)

cor(df_cor, use = "complete.obs")
corrplot.mixed(cor(df_cor, use = "complete.obs"),upper ="circle", lower = "number")






# Gaussian Process Regression WinRate e PickRate Gamemode=2 e Gametype=2(START)



# Eliminazione valore NaN
winrate_gemetype2 = winrate_gemetype2[-24]
winrate_gemetype2 = winrate_gemetype2[-107]

pickrate_gametype2 = pickrate_gametype2[-24]
pickrate_gametype2 = pickrate_gametype2[-107]


# costruzione del modello di regressione gaussiana
gausmodel_winrate_pickrate_gametype2 <- gausspr(x = pickrate_gametype2, y = winrate_gemetype2, kernel = "rbfdot")

# previsione sui dati di input
predictions <- predict(gausmodel_winrate_pickrate_gametype2, pickrate_gametype2)

# plot Gaussian Process Regression Ranked
plot(pickrate_gametype2, winrate_gemetype2, main = "Gaussian Process Regression Ranked",
     xlab = "WinRate",ylab = "PickRate",      ylim = c(0,0.4))

text(x = par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1]),  # Posizione vicino al bordo sinistro
     y = par("usr")[4] - 0.05 * (par("usr")[4] - par("usr")[3]),  # Posizione vicino al bordo superiore
     labels = "RS: 0.2412", 
     pos = 4, 
     cex = 1, 
     ylim = 0.4,
     col = "blue", 
     xpd = TRUE)

punti <- data.frame(pickrate = pickrate_gametype2, preds = predictions)

# ordinaro il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]

lines(punti_ordinati, col="blue")


# calcolo la somma dei quadrati totali (TSS)
tss <- sum((winrate_gemetype2 - mean(winrate_gemetype2))^2)

# calcolo la somma dei quadrati residui (RSS)
rss <- sum((winrate_gemetype2 - predictions)^2)

# calcolo R-quadro (R²)
r_squared <- 1 - (rss / tss)
print(paste("R-squared:", r_squared))

# numero di osservazioni (n) e numero di parametri del modello (p)
n <- length(winrate)  
p <- 1          # Numero di predittori. 

# calcola R-quadro aggiustato
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
print(paste("Adjusted R-squared:", r_squared_adj))


# Gaussian Process Regression WinRate e PickRate Gamemode=2 e Gametype=2(END)



temp = winrate_gemetype3
winrate_gemetype3 = pickrate_gametype3
pickrate_gametype3 = temp

# Regressione Spline WinRate e PickRate Gamemode=2 e Gametype=3(START)


spline_winrate_pickrate_gametype3 <- lm(winrate_gemetype3 ~ bs(pickrate_gametype3, knots=seq(0.0, 0.15, length.out=10)))

preds <- predict(spline_winrate_pickrate_gametype3, newdata=list(x=pickrate_gametype3))

# Plot Rapporto WinRate e PickRate
plot(pickrate_gametype3,winrate_gemetype3, xlab = "WinRate",ylab = "PickRate", main="Spline Unranked")

 # aggiunta valore ars
text(x = par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1]),  
     y = par("usr")[4] - 0.05 * (par("usr")[4] - par("usr")[3]),  
     labels = "RS: 0.1462", 
     pos = 4, 
     cex = 1, 
     col = "blue", 
     xpd = TRUE)

punti <- data.frame(pickrate = pickrate_gametype3, preds = preds)

# ordinare il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]
lines(punti_ordinati, col="blue")

summary(spline_winrate_pickrate_gametype3)


# Regressione Spline WinRate e PickRate Gamemode=2 e Gametype=3(END)





# Gaussian Process Regression WinRate e PickRate Gamemode=2 e Gametype=3(START)

# Eliminazione valore NaN
winrate_gemetype3 = winrate_gemetype3[-24]
winrate_gemetype3 = winrate_gemetype3[-107]

pickrate_gametype3 = pickrate_gametype3[-24]
pickrate_gametype3 = pickrate_gametype3[-107]


# costruzione del modello di regressione gaussiana
gausmodel_winrate_pickrate_gametype3 <- gausspr(x = pickrate_gametype3, y = winrate_gemetype3, kernel = "rbfdot")

# previsione sui dati di input
predictions <- predict(gausmodel_winrate_pickrate_gametype3, pickrate_gametype3)

# visualizzazione dei dati e della predizione
plot(pickrate_gametype3, winrate_gemetype3, main = "Gaussian Process Regression Unranked",
     xlab = "WinRate", ylab = "PickRate")

# aggiunta valore ars
text(x = par("usr")[1] + 0.05 * (par("usr")[2] - par("usr")[1]),
     y = par("usr")[4] - 0.05 * (par("usr")[4] - par("usr")[3]),
     labels = "RS: 0.2625", 
     pos = 4, 
     cex = 1, 
     col = "blue", 
     xpd = TRUE)

punti <- data.frame(pickrate = pickrate_gametype3, preds = predictions)

# ordinaro il data frame in base a pickrate
punti_ordinati <- punti[order(punti$pickrate), ]
lines(punti_ordinati, col = "blue")


# calcolo la somma dei quadrati totali (TSS)
tss <- sum((winrate_gemetype3 - mean(winrate_gemetype3))^2)

# calcolo la somma dei quadrati residui (RSS)
rss <- sum((winrate_gemetype3 - predictions)^2)

# calcolo R-quadro (R²)
r_squared <- 1 - (rss / tss)
print(paste("R-squared:", r_squared))

# numero di osservazioni (n) e numero di parametri del modello (p)
n <- length(winrate) 
p <- 1          # Numero di predittori.

# calcolo R-quadro aggiustato
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
print(paste("Adjusted R-squared:", r_squared_adj))


# Gaussian Process Regression WinRate e PickRate Gamemode=2 e Gametype=3(END)

