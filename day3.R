##part 1
library(stringr)

input <- as.data.frame(readLines('day3.txt'))

sum <- c()

for (i in 1:max(nchar(input[,1]))) {
  input[,i+1] <- str_sub(input[,1], i, i)
  input[,i+1] <- as.numeric(input[,i+1])
  sum[i] <- sum(input[,i+1])
}

# Summe größer als Mittelwert der Beobachtungen? -> 1
gamma <- as.numeric(sum > (nrow(input)/2))
epsilon <- as.numeric(!gamma)

gamma <- paste0(gamma, collapse = "") 
epsilon <- paste0(epsilon, collapse = "") 
strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
#2743844


##part 2
#oxygen generator
input_oxy <- input
sum <- c()
oxy <- c()
rows <- c()

for (i in 1:ncol(input_oxy)) {
  sum[i] <- sum(input_oxy[,i+1])
  rows[i] <- nrow(input_oxy)
  oxy[i] <- as.numeric(sum[i] >= (rows[i]/2))
  input_oxy <- subset(input_oxy, input_oxy[,i+1]==oxy[i])
  if(nrow(input_oxy) == 1) break
}

oxy <- input_oxy[1,1] 
oxy #011110111101

#co2
input_co2 <- input
sum <- c()
co2 <- c()
rows <- c()

for (i in 1:ncol(input_co2)) {
  sum[i] <- sum(input_co2[,i+1])
  rows[i] <- nrow(input_co2)
  co2[i] <- as.numeric(!(sum[i] >= (rows[i]/2)))
  input_co2 <- subset(input_co2, input_co2[,i+1]==co2[i])
  if(nrow(input_co2) == 1) break
}

co2 <- input_co2[1,1]
co2 #110100101011


strtoi(oxy, base = 2) * strtoi(co2, base = 2) #6677951
