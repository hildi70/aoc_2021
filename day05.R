library(tidyverse)

input <- read.table("input/day05.txt")%>% 
  separate(V1, c("x1", "y1")) %>%
  separate(V3, c("x2", "y2")) %>%
  select(-3) %>%
  mutate(across(1:length(input), function(x) as.numeric(as.character(x))))

##### part1 #####
# keep only horizontal or vertical lines
straight_lines <- subset(input, y1==y2|x1==x2)

# build coordinate system
max(straight_lines) #990
min(straight_lines) #10

x_axis <- c(1:1000)
y_axis <- c(1:1000)

df <- data.frame(matrix(ncol = length(x_axis), nrow = length(y_axis)))
df[,] <- 0

# adding lines
for (i in 1:nrow(straight_lines)) {
  df[straight_lines$y1[i]:straight_lines$y2[i], straight_lines$x1[i]:straight_lines$x2[i]] = df[straight_lines$y1[i]:straight_lines$y2[i], straight_lines$x1[i]:straight_lines$x2[i]] + 1
}

# how often do at least 2 lines overlap?
sum(df[,]>=2) #6572

##### part2 #####
# keep also diagonal lines
df[,] <- 0

# adding lines according to their gradient
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
