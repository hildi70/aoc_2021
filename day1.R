#part 1
input <- as.data.frame(read.table('day1.txt'))
input$increase <- c(rep(0, length(input)))
for (i in 2:nrow(input)) {
  if (input[i-1,1] < input[i,1])
  {input[i,2] <- 1} 
}

sum(input$increase) #1655

#part 2
install.packages('runner')
library('runner')
input$increase2 <- c(rep(0, length(input)))
input$sums <- runner(input$depth[1:nrow(input)], f = sum, k = 3)
for (i in 4:nrow(input)) {
  if (input[i-1,4] < input[i,4])
  {input$increase2[i] <- 1} 
}

sum(input$increase2) #1683
