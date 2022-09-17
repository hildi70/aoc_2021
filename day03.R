library(stringr)

input <- read.table("input/day03.txt", colClasses = "character", col.names = "bits_complete")
for (i in 1:nchar(input[1,1])) {
  input[,i+1] <- as.numeric(str_sub(input[,1], i, i))
}
input <- input[, 2:length(input)]

##### part 1 #####
# We want to keep the most frequent bit in each position.
# colSums of each position are larger than half of the nrows if 1 is more often.
# Statement returns boolean values -> we keep that as 0 or 1

gamma <- as.numeric(colSums(input[1:length(input)]) > (nrow(input)/2))
epsilon <- as.numeric(!gamma)

gamma <- paste0(gamma, collapse = "") 
epsilon <- paste0(epsilon, collapse = "")

# binary to decimal
strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)   #2743844

##### part 2 #####
# oxygen generator
input_oxy <- input
oxy <- c()

for (i in 1:ncol(input_oxy)) {
  if(nrow(input_oxy) == 1) break
  oxy[i] <- as.numeric(colSums(input_oxy[i]) >= (nrow(input_oxy)/2))
  input_oxy <- subset(input_oxy, input_oxy[,i]==oxy[i])
}

oxy <- paste0(input_oxy, collapse = "")   #011110111101


# co2
input_co2 <- input
co2 <- c()

for (i in 1:ncol(input_co2)) {
  if(nrow(input_co2) == 1) break
  co2[i] <- as.numeric(!(colSums(input_co2[i]) >= (nrow(input_co2)/2)))
  input_co2 <- subset(input_co2, input_co2[,i]==co2[i])
}

co2 <- paste0(input_co2, collapse = "")   #110100101011

# binary to decimal
strtoi(oxy, base = 2) * strtoi(co2, base = 2) #6677951