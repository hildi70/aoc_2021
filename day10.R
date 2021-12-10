#part 1
day10 <- as.data.frame(readLines("day10.txt"))
wrong_closing <- c()

for (i in 1:nrow(day10)) {
  while (nchar(day10[i,1])>1) {
    x <- nchar(day10[i,1])
    day10[i,1] <- gsub("\\()","", day10[i,1])
    day10[i,1] <- gsub("<>","", day10[i,1])
    day10[i,1] <- gsub("\\[]","", day10[i,1])
    day10[i,1] <- gsub("\\{}","", day10[i,1])
    y <- nchar(day10[i,1])
    if (x == y) break;
  }
  day10[i,1] <- sub("\\]","1",day10[i,1]) #grepl und ] werden bei mir keine Freunde
  day10_temp <- unlist(strsplit(day10[i,1],""))
  wrong_closing[i] <- day10_temp[min(grep("[>\\Q)}\\E1]", day10_temp))]
}

corruptors <- wrong_closing[!is.na(wrong_closing)]
corruptors <- sub("1","57",corruptors) #] 
corruptors <- sub(")","3",corruptors) 
corruptors <- sub("}","1197",corruptors) 
corruptors <- sub(">","25137",corruptors)
sum(as.numeric(corruptors)) #389589

#part 2
#discard corrupted lines
day10 <- as.data.frame(day10[which(is.na(wrong_closing)),1])

completion_scores <- c(rep(0, times=nrow(day10)))

for (i in 1:nrow(day10)){
  day10_temp <- unlist(strsplit(day10[i,1],""))
  day10_temp <- gsub("\\[","2",day10_temp)  
  day10_temp <- gsub("\\(","1",day10_temp) 
  day10_temp <- gsub("\\{","3",day10_temp) 
  day10_temp <- gsub("<","4",day10_temp)
  day10_temp <- as.numeric(rev(day10_temp))
  for (j in 1:length(day10_temp)) {
    completion_scores[i] <- completion_scores[i]*5+day10_temp[j]
  }
}
completion_scores <- sort(completion_scores)
completion_scores[round(length(completion_scores)/2)] #1190420163
