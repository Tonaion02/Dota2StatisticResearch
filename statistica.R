library(dplyr)
library(corrplot)

getwd()  
setwd("ProgettiR\\ProggettoStatistica") 


# Selezione del dataset
# my_data <- read.table("dataset_dota2_augumented.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
my_data <- read.table("dataset_dota2_augumented_sintetico.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")


# Trasformazione delle colonne stringe in int
columInt = paste0("hero", 22:113)
for (name in columInt) {
  my_data[,name] = as.integer(as.numeric(my_data[,name]))
}
my_data$win = as.integer(as.numeric(my_data$win))



# Stampa frequenze gamemode
freq <- table(my_data$gamemode)
print(freq)

# SOLO PER DATASET NON PULITO (nome= dota2_2.csv)
#---------------------------------------------------------------------------------

coord = barplot(
  freq, 
  names.arg = c("1","2","3","4","5","6","7","8","9"),                
  col = c("black", "skyblue" ,"darkblue", "darkgreen", "blue", "lightgreen", "yellow", "orange", "white"),
  border = "black",                    
  xlab = "GameMode",                 
  ylab = "Frequenze",                  
  main = "Frequenze delle GameMode",
  cex.names = 1,                      
  cex.axis = 1,                      
  ylim= c(0,6500),
  las = 1                              
)


text(
  x = coord,                            
  y = freq,                             
  labels = freq,                        
  pos = 3,                             
  cex = 1.2,                          
  col = "black"                       
)


# barplot frequenze gametype
freq <- table(my_data$gametype)
print(freq)

coord = barplot(
  freq, 
  names.arg = c("1","2","3"),                
  col = c("black","skyblue", "lightgreen"),         
  border = "black",                    
  xlab = "GameType",                  
  ylab = "Frequenze",                   
  main = "Frequenze delle GameType", 
  cex.names = 1,                     
  cex.axis = 1,                       
  ylim= c(0,5500),
  las = 1                               
)

text(
  x = coord,                           
  y = freq,                             
  labels = freq,                        
  pos = 3,                              
  cex = 1.2,                            
  col = "black"                         
)

#---------------------------------------------------------------------------------

freq <- table(my_data$gametype)
print(freq)

# Barplot frequenze gametype
coord2 = barplot(freq,
        main = "Frequenze GameType",    
        col = 1:3,       
        border = "black",                  
        ylim = c(0, max(freq) + 500),        
        xlab = "GameType",                
        ylab = "Frequenze",                
        cex.main = 1.5,                    
        cex.lab = 1.2,
        names.arg = c("Ranked", "Unranked"),
        cex.names = 1.1)                   


text(
  x = coord2,                            
  y = freq,                            
  labels = freq,                      
  pos = 3,                            
  cex = 1.2,                          
  col = "black"                     
)


# stampa frequenze gamemode e gametype
freq <- table(my_data$gametype,my_data$gamemode)
print(freq)
barplot(freq, ylab="Frequenze", names.arg = "5 vs 5" , xlab="GameMode", main="Frequenze coppie GameMode GameType", col= 1:3)

# aggiunta della legend
legend(
  "topright", 
  legend = c ("Ranked","Unranked"), 
  col = 1:3, 
  pch = 15, 
  title = "GameType"
)


# valori null
any_na <- any(is.na(my_data))
print(any_na)


# frequenze di pick degli Hero con gamemode=2 e gametype=2|3
my_data2 = filter(my_data, gamemode==2 & (gametype==2 | gametype==3))

hero_columns <- my_data2[, paste0("hero", 1:113)]  

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
# Numero di volte che ogni campione è stato selezionato
count_values <- apply(hero_columns, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})
count_values

# Barplot Pick Hero
barplot(count_values,
        main = "Pick Hero",  # Titolo
        col = "skyblue",                   
        border = "black",                  
        ylim = c(0, max(count_values) + 10), 
        xlab = "Hero",                     
        ylab = "Frequenze",                
        names.arg = names(count_values),   
        cex.names = 0.7,                   
        las = 2,                           
        cex.main = 1.5,                   
        cex.lab = 1.2,                    
        cex.axis = 1)                      




# Grafico combinato 2 colonne 1 riga
par(mfrow=c(2,1))

# Frequenze relative di pick degli Hero con gamemode=2 e gametype=2
my_data3 = filter(my_data, gamemode==2 & gametype==2)

hero_columns <- my_data3[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values3 <- apply(hero_columns, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})


temp = sapply(count_values3, function(x) x/nrow(my_data3))

# Barplot Pickrate Hero Ranked
barplot(temp,
        main = "Pickrate Hero Ranked",  
        col = "skyblue",                  
        border = "black",                
        xlab = "Hero",                    
        ylab = "Frequenze",                
        names.arg = names(count_values),   
        cex.names = 0.7,                  
        las = 2,                          
        cex.main = 1.5,                    
        cex.lab = 1.2,                    
        cex.axis = 1)                      





# Frequenze relative di pick degli Hero con gamemode=2 e gametype=3
my_data4 = filter(my_data, gamemode==2 & gametype==3)

hero_columns <- my_data4[, paste0("hero", 1:113)] 

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values4 <- apply(hero_columns, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})

temp = sapply(count_values4, function(x) x/nrow(my_data4))

# Barplot Pickrate Hero Unraked
barplot(temp,
        main = "Pickrate Hero Unraked", 
        col = "skyblue",                  
        border = "black",                 
        xlab = "Hero",                    
        ylab = "Frequenze",               
        names.arg = names(temp),  
        cex.names = 0.7,                   
        las = 2,                          
        cex.main = 1.5,                   
        cex.lab = 1.2,                    
        cex.axis = 1)                   



# Differenza tra pickRate con (gamemode=2 e gametype=2) e (gamemode=2 e gametype=3)


temp = sapply(count_values3, function(x) x/nrow(my_data2)) - sapply(count_values4, function(x) x/nrow(my_data2))

# Barplot Differenza Pickrate Hero Unraked e Ranked
barplot(temp,
        main = "Differenza Pickrate Hero Unraked e Ranked",  
        col = "skyblue",                   
        border = "black",                  
        ylim = c(-0.05,0.2),
        xlab = "Hero",                     
        ylab = "Frequenze",                
        names.arg = names(temp),  
        cex.names = 0.7,                
        las = 2,                           
        cex.main = 1.5,                 
        cex.lab = 1.2,                    
        cex.axis = 1)                     

abline(h=0.025, col="red")

# linea y=0.025
text(x = par("usr")[1], y = 0.025, labels = "0.025", pos = 2, col = "red", cex = 1, xpd = TRUE)
abline(h=-0.025, col="red")

# linea y=-0.025
text(x = par("usr")[1], y = -0.025, labels = "-0.025", pos = 2, col = "red", cex = 1, xpd = TRUE)



# WinRate degli Hero

# definizione della funzione
compare_hero_to_win <- function(data, hero_columns, win_column) {
  results <- numeric(length(hero_columns))
  
  win_col_value= data$win
  
  # ciclo su ogni colonna hero
  for (i in seq_along(hero_columns)) {
    hero_col <- hero_columns[i]
    # conto le righe dove il valore di hero è uguale al valore di win
    
    hero_col_val = data[,hero_col]
    
    for(j in seq_along(hero_col_val)){
      if(hero_col_val[j] == win_col_value[j]){
        results[i] = results[i]+1;
      }
    }
    
    #results[i] <- sum(data[[hero_col]] == data[[win_column]], na.rm = TRUE)
  }
  
  print(results)
  
  names(results) <- hero_columns
  return(results)
}

# utilizzo della funzione sul dataset hero_columns
hero_column_names <- paste0("hero", 1:113)
win_column_name <- "win"

# calcolo il numero di istanze per ogni colonna hero
result <- compare_hero_to_win(my_data2, hero_column_names, win_column_name)

for(i in 1:113){
  result[i]= result[i]/count_values[i]
}

temp = sapply(count_values, function(x) x/nrow(my_data2))

# Scatterplot Rapporto WinRate e PickRate
plot(temp, result,
     main = "Rapporto WinRate e PickRate", 
     xlab = "PickRate",                     
     ylab = "WinRate",                      
     col = "black",                       
     pch = 1,                             
     cex = 1.2,                            
     cex.main = 1.5,                      
     xlim = c(0, 0.45),
     cex.lab = 1.2,                         
     cex.axis = 1)                         

# linea orizzontale y = 0.47
abline(h = 0.47, col = "blue", lwd = 2, lty = 2)

# linea verticale a x = 1000/5488
abline(v = 1000/5488, col = "blue", lwd = 2, lty = 2)

# aggiuna griglia
grid(col = "gray", lty = "dotted")



# Correlazione tra WinRate e PickRate

# Creazione del dataframe con colonne PickRate e WinRate
df_cor = data.frame(
  PickRate = count_values,
  WinRate = result
  )

cor(df_cor, use = "complete.obs")

corrplot.mixed(cor(df_cor, use = "complete.obs"),upper ="circle", lower = "number")



# Istogramma del WinRate totale
h <- hist(result, 
          freq = TRUE,                        
          col = "skyblue",                   
          border = "black",                  
          xlab = "WinRate",                   
          ylab = "Frequenze",               
          cex.lab = 1.2,                    
          cex.axis = 1.2,                     
          ylim = c(0, max(hist(result, plot = FALSE)$counts) + 5), 
          main = "Distribuzione del WinRate") 

# aggiungo i valori sopra le colonne
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 1.2, col = "black")

result <- result[!is.nan(result)]

# Kernel density plot WinRate
n = length(result)
h_sturges = (max(result) - min(result)) / sqrt(n)

density_sturges = density(result, bw=h_sturges)
plot(density_sturges, main="WinRate")

# aggiunta griglia
grid(col = "gray", lty = "dotted")



# valori pickrate totale
result = sapply(count_values, function(x) x/nrow(my_data2))

# Istogramma del PickRate
hist(result, 
     freq = TRUE,                        
     col = "skyblue",    
     border = "black",              
     xlab = "PickRate",                  
     ylab = "Frequenze",              
     cex.lab = 1.2,                      
     cex.axis = 1.2,                    
     ylim = c(0, max(hist(result, plot = FALSE)$counts) + 5),  
     main = "Distribuzione del PickRate") 
result <- result[!is.nan(result)]

# Kernel density plot PickRate
n = length(result)
h_sturges = (max(result) - min(result)) / sqrt(n)

density_sturges = density(result, bw=h_sturges)
plot(density_sturges, main="PickRate")

grid(col = "gray", lty = "dotted")



# BoxPlot ad intaglio del WinRate con gamemode=2 e gametype=2 o gametype=3

# WinRate con gamemode=2 e gametype=2
WinRate1 <- compare_hero_to_win(my_data3, hero_column_names, win_column_name)

hero_columns <- my_data3[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
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
  sum(col == 1) + sum(col == -1) 
})

for(i in seq_along(WinRate2)){
  WinRate2[i]= WinRate2[i]/count_values[i]
}


# Boxplot Confronto WinRate: Ranked vs Unranked
boxplot(WinRate1, WinRate2, 
        notch = TRUE,               
        names = c("Ranked", "Unranked"), 
        col = c("skyblue", "lightgreen"), 
        main = "Confronto WinRate: Ranked vs Unranked",  
        xlab = "Gametype", 
        ylab = "WinRate",           
        cex.main = 1.5,              
        cex.lab = 1.2,               
        cex.axis = 1.2,           
        horizontal = FALSE,         
        whisklty = 1,                
        whiskcol = "darkgray",     
        boxlwd = 0.5)   

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
library(moments)

summary(WinRate1)
var(WinRate1)
sd(WinRate1)
skewness(WinRate1)
kurtosis(WinRate1)

summary(WinRate2)
var(WinRate2)
sd(WinRate2)
skewness(WinRate2)
kurtosis(WinRate2)







temp = WinRate1 - WinRate2
# Barplot Differenza WinRate Hero Ranked e Unranked
barplot(temp,
        main = "Differenza WinRate Hero Ranked e Unranked", 
        col = "skyblue",                  
        border = "black",                
        ylim = c(-0.3,0.3),
        xlab = "Hero",                    
        ylab = "Frequenze",               
        names.arg = names(temp),  
        cex.names = 0.7,                  
        las = 2,                          
        cex.main = 1.5,               
        cex.lab = 1.2,                   
        cex.axis = 1)                 


abline(h=0.05, col="red")

# etichetta y=0.05
text(x = par("usr")[1], y = 0.05, labels = "0.05", pos = 2, col = "red", cex = 1, xpd = TRUE)

abline(h=-0.05, col="red")

# etichetta y=-0.05
text(x = par("usr")[1], y = -0.05, labels = "-0.05", pos = 2, col = "red", cex = 1, xpd = TRUE)




WinRate <- compare_hero_to_win(my_data2, hero_column_names, win_column_name)

hero_columns <- my_data2[, paste0("hero", 1:113)]

# Funzione per contare 1 e -1 separatamente e incrementare un contatore di 1
count_values <- apply(hero_columns, 2, function(col) {
  sum(col == 1) + sum(col == -1) 
})

for(i in seq_along(WinRate)){
  WinRate[i]= WinRate[i]/count_values[i]
}



# Barplot frequenze WinRate totale
barplot(WinRate,
        main = "WinRate Hero",  
        col = "skyblue",                
        border = "black",              
        xlab = "Hero",                 
        ylab = "WinRate",            
        names.arg = names(WinRate),  
        cex.names = 0.7,                
        las = 2,                         
        cex.main = 1.5,                   
        cex.lab = 1.2,                   
        cex.axis = 1)                   
mean(WinRate[!is.nan(WinRate)])
var(WinRate[!is.nan(WinRate)])
sd(WinRate[!is.nan(WinRate)])



# Barplot frequenze WinRate ranked
barplot(WinRate1,
        main = "WinRate Hero Ranked", 
        col = "skyblue",                  
        border = "black",                 
        xlab = "Hero",                    
        ylab = "WinRate",             
        names.arg = names(WinRate1),   
        cex.names = 0.7,                  
        las = 2,                           
        cex.main = 1.5,                
        cex.lab = 1.2,                     
        cex.axis = 1)                      
mean(WinRate1[!is.nan(WinRate1)])
var(WinRate1[!is.nan(WinRate1)])
sd(WinRate1[!is.nan(WinRate1)])



# Grafico combinato 1 colonna 2 righe
par(mfrow=c(1,2))

# Distribuzione del WinRate Ranked
h <- hist(WinRate1, 
          breaks =  seq(0.25, 0.75, by = 0.05),  
          freq = TRUE,                        
          col = "skyblue",                   
          border = "black",                   
          xlab = "WinRate",               
          ylab = "Frequenze",                
          ylim = c(0, 60),
          cex.lab = 1.2,                     
          cex.axis = 1.2,
          xaxt = "n",                        
          main = "Distribuzione del WinRate Ranked")  


axis(1, at = seq(0.25, 0.75, by = 0.05)) 

text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 1.2, col = "black")



# Barplot frequenze WinRate unranked
barplot(WinRate2,
        main = "WinRate Hero Unranked", 
        col = "skyblue",                   
        border = "black",                  
        xlab = "Hero",                   
        ylab = "WinRate",               
        names.arg = names(WinRate2),   
        cex.names = 0.7,                  
        las = 2,                        
        cex.main = 1.5,                 
        cex.lab = 1.2,                   
        cex.axis = 1)                  

mean(WinRate2[!is.nan(WinRate2)])
var(WinRate2[!is.nan(WinRate2)])
sd(WinRate2[!is.nan(WinRate2)])



# Distribuzione del WinRate Unranked
h <- hist(WinRate2, 
          freq = TRUE,                        
          col = "skyblue",               
          border = "black",               
          xlab = "WinRate",                 
          ylab = "Frequenze",               
          ylim = c(0, 45),
          cex.lab = 1.2,                    
          cex.axis = 1.2,
          xaxt = "n",                        
          main = "Distribuzione del WinRate Unranked")

axis(1, at = seq(0.2, 0.65, by = 0.05))  # Intervalli di 0.5

text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 1.2, col = "black")



# BoxPlot ad intaglio del PickRate con gamemode=2 e gametype=2 o gametype=3


temp_pickrate_gt2 = sapply(count_values3, function(x) x/nrow(my_data3))

temp_pickrate_gt3 = sapply(count_values4, function(x) x/nrow(my_data4))

library(moments)
summary(temp_pickrate_gt2)
var(temp_pickrate_gt2)
sd(temp_pickrate_gt2)
skewness(temp_pickrate_gt2)
kurtosis(temp_pickrate_gt2)

# Grafico combinato 1 riga 2 colonne
par(mfrow=c(1,2))

# Distribuzione del PickRate Ranked 
hist(temp_pickrate_gt2, 
     freq = TRUE,                     
     col = "skyblue",    
     border = "black",             
     xlab = "PickRate",                  
     ylab = "Frequenze",              
     cex.lab = 1.2,                     
     cex.axis = 1.2,                    
     ylim = c(0, max(hist(temp_pickrate_gt2, plot = FALSE)$counts) + 5),  
     main = "Distribuzione del PickRate Ranked")  

# Distribuzione del PickRate Unranked 
hist(temp_pickrate_gt3, 
     freq = TRUE,                     
     col = "skyblue",     
     border = "black",               
     xlab = "PickRate",                  
     ylab = "Frequenze",              
     cex.lab = 1.2,                    
     cex.axis = 1.2,                   
     ylim = c(0, max(hist(temp_pickrate_gt2, plot = FALSE)$counts) + 5),  
     main = "Distribuzione del PickRate Unranked") 

summary(temp_pickrate_gt3)
var(temp_pickrate_gt3)
sd(temp_pickrate_gt3)
skewness(temp_pickrate_gt3)
kurtosis(temp_pickrate_gt3)



# Boxplot Confronto PickRate: Ranked vs Unranked
boxplot(temp_pickrate_gt2, temp_pickrate_gt3, 
        notch = TRUE,               
        names = c("Ranked", "Unranked"), 
        col = c("skyblue", "lightgreen"),
        main = "Confronto PickRate: Ranked vs Unranked",
        xlab = "Gametype",  
        ylab = "PickRate",           
        cex.main = 1.5,              
        cex.lab = 1.2,               
        cex.axis = 1.2,              
        horizontal = FALSE,         
        whisklty = 1,                
        whiskcol = "darkgray",       
        boxlwd = 0.5)                


summary(temp_pickrate_gt2)
summary(temp_pickrate_gt3)

temp_pickrate_gt2 = temp_pickrate_gt2[!is.nan(temp_pickrate_gt2)]
temp_pickrate_gt3 = temp_pickrate_gt3[!is.nan(temp_pickrate_gt3)]

IQR_1 = quantile(temp_pickrate_gt2,0.75)-quantile(temp_pickrate_gt2,0.25)
M1_1 = quantile(temp_pickrate_gt2,0.5) - 1.57*IQR_1 / sqrt(length(temp_pickrate_gt2))
M2_1 = quantile(temp_pickrate_gt2,0.5) + 1.57*IQR_1 / sqrt(length(temp_pickrate_gt2))

IQR_2 = quantile(temp_pickrate_gt3,0.75)-quantile(temp_pickrate_gt3,0.25)
M1_2 = quantile(temp_pickrate_gt3,0.5) - 1.57*IQR_2 / sqrt(length(temp_pickrate_gt3))
M2_2 = quantile(temp_pickrate_gt3,0.5) + 1.57*IQR_2 / sqrt(length(temp_pickrate_gt3))

confidenza = c(M1_1 - M2_2, M2_1 - M1_2)
confidenza







temp = temp_pickrate_gt2 - temp_pickrate_gt3

# Barplot Differenza Pickrate Hero Ranked e Unranked
barplot(temp,
        main = "Differenza Pickrate Hero Ranked e Unranked", 
        col = "skyblue",                   
        border = "black",                  
        ylim = c(-0.3,0.3),
        xlab = "Hero",                     
        ylab = "Frequenze",               
        names.arg = names(temp),  
        cex.names = 0.7,                  
        las = 2,                           
        cex.main = 1.5,                   
        cex.lab = 1.2,                     
        cex.axis = 1)                  


abline(h=0.025, col="red")

text(x = par("usr")[1], y = 0.025, labels = "0.025", pos = 2, col = "red", cex = 1, xpd = TRUE)

abline(h=-0.025, col="red")

text(x = par("usr")[1], y = -0.025, labels = "-0.025", pos = 2, col = "red", cex = 1, xpd = TRUE)









# VARI TEST - ELEMENTI NON UTILIZZATI----------------------------------------------------------------------------------------

plot(my_data2$complexity_1 + my_data2$complexity_2)


# Seleziona le istanze in cui gametype è uguale a 3
filtered_data <- my_data2[my_data2$gametype == 3, ]

# Calcola la complessità totale solo per queste righe
total_complexity <- filtered_data$complexity_1 + filtered_data$complexity_2

# Istogramma Distribuzione della Complessità delle Partite
hist(total_complexity, 
     freq = TRUE,                       
     col = rgb(0.2, 0.6, 0.8, 0.7),     
     border = "black",                
     xlab = "Complesso Totale delle Partite",  
     ylab = "Frequenze",               
     main = "Distribuzione della Complessità delle Partite", 
     cex.main = 1.5,                  
     cex.lab = 1.2,                   
     cex.axis = 1.2)                     
# Questo istrogramma ci fa supporre che il dataset rispecchi effettivamente la distribuzione dei player nei vari Rank(elo) dato che la complessità dei campioni giocati e molto correlata al livello del giocatore come si puo determinare analizzando i valori di pickRate degli Hero piu utilizzati nel sito di principale di analisi di Dota 2



if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Aggrega i dati per contare le occorrenze di ogni combinazione (x, y)
agg_data <- my_data2 %>%
  dplyr::group_by(complexity_1, complexity_2) %>%
  dplyr::summarise(count = n(), .groups = "drop")

# Scatter plot con dimensione proporzionale alla frequenza
ggplot(agg_data, aes(x = complexity_1, y = complexity_2, size = count)) +
  geom_point(alpha = 0.6) +  
  scale_size_area(max_size = 10) +  
  labs(
    x = "Complexity 1",
    y = "Complexity 2",
    size = "Frequency",
    title = "Scatter plot con dimensione proporzionale alla frequenza"
  ) +
  theme_minimal()

# Histogramma della comlessita totale
hist(my_data2$complexity_1+my_data2$complexity_2)





# confronto complessita e counterPick
plot(my_data2$counter_pick_1+ my_data2$counter_pick_2, my_data2$complexity_1+ my_data2$complexity_2)



# Confronto tra complexity_1 e Counter_pick_1 
agg_data <- my_data2 %>%
  dplyr::group_by(counter_pick_1, complexity_1) %>%
  dplyr::summarise(count = n(), .groups = "drop")

# Scatter plot con dimensione proporzionale alla frequenza
ggplot(agg_data, aes(y = complexity_1, x = counter_pick_1, size = count)) +
  geom_point(alpha = 0.6) + 
  scale_size_area(max_size = 10) + 
  labs(
    x = "Counter Pick 1",
    y = "Complexity 1",
    size = "Frequency",
    title = "Scatter plot con dimensione proporzionale alla frequenza"
  ) +
  theme_minimal()



# Confronto tra complexity_2 e Counter_pick_2 
agg_data <- my_data2 %>%
  dplyr::group_by(counter_pick_2, complexity_2) %>%
  dplyr::summarise(count = n(), .groups = "drop")

# Scatter plot con dimensione proporzionale alla frequenza
ggplot(agg_data, aes(y = complexity_2, x = counter_pick_2, size = count)) +
  geom_point(alpha = 0.6) +  
  scale_size_area(max_size = 10) +  
  labs(
    x = "Counter Pick 2",
    y = "Complexity 2",
    size = "Frequency",
    title = "Scatter plot con dimensione proporzionale alla frequenza"
  ) +
  theme_minimal()



# Confronto tra complexity_totale e Counter_pick_totale 
my_data_total <- my_data2 %>%
  mutate(
    counter_pick_total = counter_pick_1 + counter_pick_2,
    complexity_total = complexity_1 + complexity_2
  )

# Aggrego i dati per contare le occorrenze di ogni combinazione
agg_data <- my_data_total %>%
  group_by(counter_pick_total, complexity_total) %>%
  summarise(count = n(), .groups = "drop")

# Scatter plot con dimensione proporzionale alla frequenza
ggplot(agg_data, aes(x = complexity_total, y = counter_pick_total, size = count)) +
  geom_point(alpha = 0.6) +  
  scale_size_area(max_size = 10) +  
  labs(
    x = "Counter Pick Total",
    y = "Complexity Total",
    size = "Frequency",
    title = "Scatter plot con dimensione proporzionale alla frequenza"
  ) +
  theme_minimal()


# Histogramma del counter pick totale
hist(my_data2$counter_pick_1+my_data2$counter_pick_2)



# istanze in cui gametype è uguale a 3
filtered_data <- my_data2[my_data2$gametype == 3, ]

# Calcola il counter pick totale unranked
total_counter_pick <- filtered_data$counter_pick_1 + filtered_data$counter_pick_2

# Istogramma Distribuzione del Counter Pick Totale
hist(total_counter_pick, 
     freq = TRUE,                      
     col = "skyblue",    
     border = "black",                 
     xlab = "Counter Pick Totale",      
     ylab = "Frequenze",                
     main = "Distribuzione del Counter Pick Totale",
     cex.main = 1.5,                     
     cex.lab = 1.2,                      
     cex.axis = 1.2)  
# stesso concetto che vale per la complessita vale anche per il counterpick



# Calcola la media delle due colonne specificate
media_counter_pick_1 <- mean(my_data2$counter_pick_1, na.rm = TRUE)
media_counter_pick_2 <- mean(my_data2$counter_pick_2, na.rm = TRUE)
media_complexity_2 <- mean(my_data2$complexity_2, na.rm = TRUE)

# Stampa i risultati
cat("Media della counter_pick_1:", media_counter_pick_1, "\n")
cat("Media della counter_pick_2:", media_counter_pick_2, "\n")
cat("Media della complexity_2:", media_complexity_2, "\n")

# VARI TEST - ELEMENTI NON UTILIZZATI------------------------------------------------------------------------------------


# Q2. Blueside e Redside

frequenze <- table(my_data3$win)

barplot(frequenze)


percentuali <- prop.table(frequenze) * 100
print(percentuali)
#47.61   52.39 (Ranked)


frequenze <- table(my_data4$win)

barplot(frequenze)


percentuali <- prop.table(frequenze) * 100
print(percentuali)
#48.43   51.57- (Unranked)




