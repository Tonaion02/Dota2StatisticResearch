library(tidyverse)

getwd()  
setwd("ProgettiR\\ProggettoStatistica") 


#data <- read.table("dataset_dota2_augumented.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")
data <- read.table("dataset_dota2_augumented_sintetico.csv", header = TRUE, sep = ",", na.strings = "NA", dec = ",")

# trasformo la colonna in numeric
data$elo_game <- as.numeric(data$elo_game)
colnames(data)


data <- data[data$gamemode == 2 & data$gametype == 2, ]





# test chi-quadro slide (normale)


# variabile aleatoriadell'elo
elo_game <- data$elo_game


# Istogramma ELO Ranked/Unranked
hist(elo_game, 
     col = "lightblue",               
     border = "white",                 
     main = "Distribuzione ELO Ranked",  
     xlab = "ELO",                     
     ylab = "Frequenza",               
     ylim = c(0, 500),
     las = 1,                          
     freq = TRUE)                      

grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")




# media
m = mean(elo_game)
m

# deviazione standard
d = sd(elo_game)
d

# numero di elementi
n = length(elo_game)
n

a = numeric(4)
for(i in 1:4){
  a[i] = qnorm(0.2*i, mean=m, sd=d)
}
a

r =5
nint = numeric(r)
nint[1] = length(which(elo_game<a[1]))
nint[2] = length(which(elo_game>=a[1] & (elo_game<a[2])))
nint[3] = length(which(elo_game>=a[2] & (elo_game<a[3])))
nint[4] = length(which(elo_game>=a[3] & (elo_game<a[4])))
nint[5] = length(which(elo_game>=a[4]))
nint

chi2 = sum(((nint-n*0.2)/ sqrt(n*0.2))^2)
chi2

r = 5
k = 2
alpha = 0.05

qchisq(alpha/2, df=r-k-1)

chi2

qchisq(1-alpha/2, df=r-k-1)

if(qchisq(alpha/2, df=r-k-1) < chi2 && chi2 < qchisq(1-alpha/2, df=r-k-1) ){
  print("La variabile aletaria ha una distribuzione normale");
}else{
  print("La variabile aletaria NON ha una distribuzione normale");
}






# test chi-quadro slide (poisson)

# numero di elementi
n = length(elo_game)
n

freq = table(elo_game)

# media
stimalambda = mean(elo_game)
stimalambda

nint = numeric(r)
nint[1] = length(which(elo_game<a[1]))
nint[2] = length(which(elo_game>=a[1] & (elo_game<a[2])))
nint[3] = length(which(elo_game>=a[2] & (elo_game<a[3])))
nint[4] = length(which(elo_game>=a[3]))
nint

r =4
p = numeric(r)
p[1] = dpois(0, stimalambda)
p[2] = dpois(1, stimalambda)
p[3] = dpois(2, stimalambda)
p[4] = 1-p[1]-p[2]-p[3]
p

chi2 = sum(((nint-n*0.2)/ sqrt(n*0.2))^2)
chi2



r =4
k = 1
alpha = 0.05

qchisq(alpha/2, df=r-k-1)

chi2

qchisq(1-alpha/2, df=r-k-1)

if(qchisq(alpha/2, df=r-k-1) < chi2 && chi2 < qchisq(1-alpha/2, df=r-k-1) ){
  print("La variabile aletaria ha una distribuzione di poisson");
}else{
  print("La variabile aletaria NON ha una distribuzione di poisson");
}




