#part 1
fishies <- scan('day6.txt', sep=",", nlines = 1)
fishies <- as.numeric(fishies)

growth <- function(x) {ifelse(x == -1, 6, x)}
days = 80
for (i in 1:days) {
  new_fish <- sum(fishies == 0)
  fishies <- fishies - 1
  fishies <- unlist(sapply(fishies, growth))
  fishies <- append(fishies, c(rep(8, new_fish)))
}

length(fishies) #379114


#part 2
#reset
fishies <- scan('day6.txt', sep=",", nlines = 1)
fishies <- as.numeric(fishies)
#256 days

#number of fishes per state
states <- c(rep(0,9))
for (i in 0:8) {
  states[i+1]  <- sum(sapply(fishies, '==', i))
}

#number of fishes per state per day, each row representing a new day
df <- data.frame(matrix(ncol = length(states), nrow = 1))
names(df) <- c(0:8)
df[1,] <- states #row 1 = day 0

 
days <- 257
for (j in 2:days){
  new_fish <- df[j-1, 1]
  df[j,1:8] <- df[j-1,2:9]
  df[j, 7] <- df[j-1,8]+new_fish
  df[j,9] <- new_fish
}

options(scipen = 999)
sum(df[nrow(df),]) #1702631502303
