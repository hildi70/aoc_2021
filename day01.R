##### setup #####
#install.packages('runner')
library(runner)
library(dplyr)

input <- read.table("input/day01.txt")

increasing_steps <- function(x, start){
  counter = 0
  for (i in start:length(x)) {
    if (x[i] > x[i-1]) {
      counter <- counter + 1
    }
  }
  print(counter)
}

##### part 1 #####
measurements <- input[,1]
increasing_steps(measurements, 2) #1655

##### part 2 #####

measurement_windows <- runner(measurements, f = sum, k = 3)

increasing_steps(measurement_windows, 4) #1683
