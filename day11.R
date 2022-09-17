#part 1
day11 <- read.table("input/day11.txt",sep="")
day11[,1] <- as.character(day11[,1])
matrix <- matrix(as.numeric(unlist(strsplit(day11[,1],""))), byrow=TRUE, nrow=nrow(day11))

# "absolute" address as index - found this one on stack overflow
w <-  which(matrix==matrix, arr.ind=TRUE)
# distance matrix
d <- as.matrix(dist(w, "maximum", diag=TRUE, upper=TRUE))


counter = 0 # counter for flashes (whenever one octopus hits 10)
steps = 1
flash_neighbours <- c()

while (steps < 101) {
  matrix[] <- matrix[] + 1 #1. all octopuses increase by 1
  while (any(matrix == 10)) {
    flash <- which(matrix==10) #2. which octopuses reached 10?
    counter = counter + length(flash)
    for (i in 1:length(flash)) {
      flash_neighbours <- append(flash_neighbours, c(which(d[flash[i],]==1))) #what are the neighbours of the flashing octopuses?
      n <- table(flash_neighbours) #how often do they get energized by their flashing neighbours?
      flash_neighbours_sorted <- sort(unique(flash_neighbours)) 
    }
    flash_neighbours <- c()
    for (j in 1:length(flash_neighbours_sorted)) {
      if (matrix[flash_neighbours_sorted[j]]<10) {
        if (matrix[flash_neighbours_sorted[j]] + n[j] > 10){
          matrix[flash_neighbours_sorted[j]] <- 10
          } else {
          matrix[flash_neighbours_sorted[j]] <- matrix[flash_neighbours_sorted[j]] + n[j]
        }
      }
    }
    matrix[flash] = matrix[flash] + 1 
  }
  matrix[which(matrix>10)] <- 0 #reset every octpus >10 to 0
  steps <- steps + 1
}

counter #1634

#part 2
#reset
matrix <- matrix(as.numeric(unlist(strsplit(day11[,1],""))), byrow=TRUE, nrow=nrow(day11))
steps = 1
flash_neighbours <- c()

#seems way too easy...
while (sum(matrix) != 0) {
  matrix[] <- matrix[] + 1 #1. all octopuses increase by 1
  while (any(matrix == 10)) {
    flash <- which(matrix==10) #2. which octopuses reached 10?
    counter = counter + length(flash)
    for (i in 1:length(flash)) {
      flash_neighbours <- append(flash_neighbours, c(which(d[flash[i],]==1))) #what are the neighbours of the flashing octopuses?
      n <- table(flash_neighbours) #how often do they get energized by their neighbours?
      flash_neighbours_sorted <- sort(unique(flash_neighbours)) 
    }
    flash_neighbours <- c()
    for (j in 1:length(flash_neighbours_sorted)) {
      if (matrix[flash_neighbours_sorted[j]]<10) {
        if (matrix[flash_neighbours_sorted[j]] + n[j] > 10){
          matrix[flash_neighbours_sorted[j]] <- 10
        } else {
          matrix[flash_neighbours_sorted[j]] <- matrix[flash_neighbours_sorted[j]] + n[j]
        }
      }
    }
    matrix[flash] = matrix[flash] + 1 
  }
  matrix[which(matrix>10)] <- 0 #reset every octpus >10 to 0
  steps <- steps + 1
}
steps-1
