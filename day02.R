library(dplyr)
input <- read.table("input/day02.txt", col.names = c("direction", "units"))%>%
  mutate(units = ifelse(direction == "up", units*(-1), units),
         direction = ifelse(direction == "up", "down", direction)
  )

##### part 1 #####
# base R
horizontal <- sum(input$units[input$direction == "forward"])
depth <- sum(input$units[input$direction == "down"])
horizontal * depth   #1451208


##### part 2 #####

aim <- 0
depth <- 0
horizontal <- 0

for (i in 1:nrow(input)) {
  ifelse (input$direction[i] == "down",
    aim <- aim + input$units[i],
    (depth <- depth + aim * input$units[i]) &
    (horizontal <- horizontal + input$units[i])
  )
} 
  
horizontal * depth #1620141160
