#part 1
input <- as.data.frame(read.table('day2.txt'))

forward <- subset(input, V1=='forward')
down <- subset(input, V1=='down') 
up <- subset(input, V1=='up')
depth <- sum(down[,2]) - sum(up[,2])

#forward * depth
2024*717 #1451208


#part 2
input <- as.data.frame(read.table('day2.txt'))

input$aim <- rep(0, nrow(input))
input$depth <- rep(0, nrow(input))

for (i in 2:nrow(input)) {
  if (input$V1[i] == 'down') {
    input$aim[i] <- input$aim[i-1]+input$V2[i];
  } else if (input$V1[i] == 'up') {
    input$aim[i] <- input$aim[i-1]-input$V2[i];
  } else {
    input$aim[i] <- input$aim[i-1]
  }
}

input_forward <- subset(input, V1=='forward')

for (i in 2:nrow(input_forward)) {
  input_forward$depth[i] <- input_forward$V2[i] * input_forward$aim[i]+input_forward$depth[i-1]
}

input_forward$depth[nrow(input_forward)]*sum(input_forward$V2) #1620141160
