
getwd()  
setwd("ProgettiR\\ProggettoStatistica") 



data <- read.table("dataset_dota2_augumented.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")

# trasformo la colonna in numeric
data$elo_game <- as.numeric(data$elo_game)

data = filter(data, gamemode==2 & gametype==2)

# variabile aleatoria dell'elo
elo_game <- data$elo_game



#calcolo degli intervalli di confidenza (calcolo valore medio di elo-game con varianza incognita) 


# stima dell'elo attraverso l'utilizzo della massima verosomiglianza
elo <- mean(elo_game)
print(paste("Stima di elo:", elo))


# grado di significatività
alpha <- 0.05  

# quantile della normale
z_alpha <- qnorm(1 - alpha / 2)

# deviazione standard campionaria
sigma_hat <- sd(elo_game)

# ampiezza dataset
n <- length(elo_game)  

# intervallo di confidenza dell elo
mu_hat <- mean(elo_game)
lower_elo <- mu_hat - z_alpha * (sigma_hat / sqrt(n))
upper_elo <- mu_hat + z_alpha * (sigma_hat / sqrt(n))


print(paste("Intervallo di confidenza per elo: [", lower_elo, ", ", upper_elo, "]"))


if (elo >= lower_elo && elo <= upper_elo) {
  print(paste("Non possiamo rifiutare l'ipotesi nulla: l'elo medio è pari a", elo))
} else {
  print(paste("Rifiutiamo l'ipotesi nulla: l'elo medio è diverso da", elo))
}




# verifica delle ipotesi tramite un test bilaterale

# valore ipotizzato della media elo della popolazione
mu_0 <- mean(elo_game)  

media_campionaria <- mean(elo_game)
dev_std_campionaria <- sd(elo_game)
n <- length(elo_game)
alpha <- 0.05

# limiti range del valore di Tos
ta_p = qt(1- alpha/2, df= n-1)
ta_p

Tos <- (media_campionaria - mu_0) / (dev_std_campionaria / sqrt(n))
Tos

p_value <- 2 * (1-pt(Tos, df = n - 1))
p_value
alpha <- 0.01
if (p_value > alpha) {
  print(paste("Non possiamo rifiutare l'ipotesi nulla: l'elo medio è pari a", mu_0))
} else {
  print(paste("Rifiutiamo l'ipotesi nulla: l'elo medio è diverso da", mu_0))
}



