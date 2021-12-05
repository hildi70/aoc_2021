library(tidyverse)
#part1
input <- read.table("day5.txt")
#getting data ready
input <- input %>% 
  separate(V1, c("x1", "y1")) %>%
  separate(V3, c("x2", "y2")) %>%
  select(-3)


#only horizontal or vertical lines
input <- subset(input, y1==y2|x1==x2)
input$x1 <- as.numeric(input$x1)
input$x2 <- as.numeric(input$x2)
input$y1 <- as.numeric(input$y1)
input$y2 <- as.numeric(input$y2)

#Build coordinate system
max(input) #990
min(input) #10

x_axis <- c(1:1000)
y_axis <- c(1:1000)

df <- data.frame(matrix(ncol = length(x_axis), nrow = length(y_axis)))
df[,] <- 0

#Adding the lines
for (i in 1:nrow(input)) {
  df[input$y1[i]:input$y2[i], input$x1[i]:input$x2[i]] = df[input$y1[i]:input$y2[i], input$x1[i]:input$x2[i]] + 1
}

#How often do at least 2 lines overlap?
sum(df[,]>=2) #6572

#part2
#Now also diagonal lines
#Reset input
input <- read.table("day5.txt")
input <- input %>% separate(V1, c("x1", "y1"))
input <- input %>% separate(V3, c("x2", "y2"))
input <- input[-3]
input$x1 <- as.numeric(input$x1)
input$x2 <- as.numeric(input$x2)
input$y1 <- as.numeric(input$y1)
input$y2 <- as.numeric(input$y2)

#Reset df
df[,] <- 0

#Adding the lines in dependency of their gradient
for (i in 1:nrow(input)) {
  if (input$y1[i]==input$y2[i]|input$x1[i]==input$x2[i]) {
    df[input$y1[i]:input$y2[i], input$x1[i]:input$x2[i]] = df[input$y1[i]:input$y2[i], input$x1[i]:input$x2[i]] + 1
  }
  else {
    if ((input$y1[i]<input$y2[i] && input$x1[i]<input$x2[i])||(input$y1[i]>input$y2[i]&& input$x1[i]>input$x2[i])) {
      m = +1 #gradient
    } else {
      m = -1
    }
    diff <- input$y2[i] - input$y1[i]
    for (j in 0:diff) {
      y <- input$y1[i]+j 
      x <- input$x1[i]+(m*j) 
      df[y, x] = df[y, x] + 1
    }
  }
}

sum(df[,]>=2) #21466