library(tidyverse)
#Not the most beautiful code. Not at all... :D

#part 1
day13 <- read.table("day13.txt", nrows = 782)
instructions <- read.table("day13.txt", skip = 782)

day13 <- day13 %>% 
  separate(V1, c("x", "y"))

day13$x <- as.numeric(day13$x)
day13$y <- as.numeric(day13$y)

mat <- matrix(nrow = max(day13$y), ncol = max(day13$x))
mat[] <- 0

for (i in 1:nrow(day13)){
  mat[day13[i,"y"], day13[i,"x"]] <- 1
}

mat <- rbind(0, cbind(0, mat))

first_fold <- instructions[1,]
print(first_fold) #x=655

mat1 <- mat[,1:655] 
mat2 <- mat[,657:ncol(mat)] #"cut"
mat2 <- mat2[,ncol(mat2):1] #"fold"

mat3 <- mat1 + mat2

length(which(mat3 == 1)) + length(which(mat3 == 2)) #661

# part 2
print(instructions)
mat <- mat3

# 2  fold along y=447
mat1 <- mat[1:447,] 
mat2 <- mat[449:nrow(mat),] 
nrow(mat1)-nrow(mat2) #difference of 5 rows?
mat2 <- rbind(mat2,0,0,0,0,0)
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

# 3  fold along x=327
mat1 <- mat[,1:327]
mat2 <- mat[,329:ncol(mat)]
mat2 <- mat2[,ncol(mat2):1]

mat <- mat1 + mat2

# 4  fold along y=223
mat1 <- mat[1:223,] 
mat2 <- mat[225:nrow(mat),] 
mat2 <- mat2[nrow(mat2):1,] 
mat <- mat1 + mat2 

# 5  fold along x=163
mat1 <- mat[,1:163] 
mat2 <- mat[,165:ncol(mat6)] 
mat2 <- mat2[,ncol(mat2):1] 

mat <- mat1 + mat2

# 6  fold along y=111
mat1 <- mat[1:111,]
mat2 <- mat[113:nrow(mat),]
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

# 7  fold along  x=81
mat1 <- mat[,1:81] 
mat2 <- mat[,83:ncol(mat)]
mat2 <- mat2[,ncol(mat2):1]

mat <- mat1 + mat2

# 8  fold along  y=55
mat1 <- mat[1:55,]
mat2 <- mat[57:nrow(mat),]
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

# 9  fold along  x=40
mat1 <- mat[,1:40]
mat2 <- mat[,42:ncol(mat)]
mat2 <- mat2[,ncol(mat2):1]

mat <- mat1 + mat2

# 10 fold along  y=27
mat1 <- mat[1:27,] 
mat2 <- mat[29:nrow(mat),] 
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

# 11 fold along  y=13
mat1 <- mat[1:13,]
mat2 <- mat[15:nrow(mat),]
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

# 12 fold along   y=6
mat1 <- mat[1:6,]
mat2 <- mat[8:nrow(mat),]
mat2 <- mat2[nrow(mat2):1,]
mat <- mat1 + mat2 

View(mat)
