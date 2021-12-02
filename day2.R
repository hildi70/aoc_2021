#part 1
input <- as.data.frame(read.table('day2.txt'))
colnames(input) <- c('V1', 'V2')
View(input)
forward <- subset(input, V1=='forward')
sum(forward[,2]) #2024

down <- subset(input, V1=='down') 
up <- subset(input, V1=='up')
depth <- sum(down[,2]) - sum(up[,2])

#forward * depth
2024*717 #1451208


#part 2
input <- as.data.frame(read.table('day2.txt'))
colnames(input) <- c('V1', 'V2')
input$forward <- rep(0, nrow(input))
input$depth <- rep(0, nrow(input))
input$aim <- rep(0, nrow(input))

input$forward[1] <- input$V2[1]

for (i in 2:nrow(input)) {
  if (input$V1[i] == 'down') {
    input$aim[i] <- input$aim[i-1]+input$V2[i];
  } else if (input$V1[i] == 'up') {
    input$aim[i] <- input$aim[i-1]-input$V2[i];
  } else {
    input$aim[i] <- input$aim[i-1]
  }
}

ssforward <- subset(input, V1=='forward')
View(ssforward)

for (i in 2:nrow(ssforward)) {
  ssforward$depth[i] <- ssforward$V2[i] * ssforward$aim[i]+ssforward$depth[i-1]
}


ssforward$depth[nrow(ssforward)]*sum(ssforward$V2) #1620141160
