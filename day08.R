#part 1 
#count only unique digits 1,4,7 and 8
day8 <- read.table("input/day08.txt", sep = "|")
output <- nchar(unlist(strsplit(day8[,2], " +")))

n_segments <- c(6,2,5,5,4,5,6,3,7,6) #number of segements per digit
names(n_segments) <- c(0:9) #digits 0 to 9

one <- output == n_segments["1"]
four <- output == n_segments["4"]
seven <- output == n_segments["7"]
eight <- output == n_segments["8"]
sum(one, four, seven, eight) #445

#part 2
library(stringi)
#reset
day8 <- read.table("day8.txt", sep = "")
output <- day8[,12:15]
digits <- day8[,1:10]
segments <- data.frame(matrix(ncol=7, nrow=nrow(digits)))
names(segments) <- c("a", "b", "c", "d", "e","f","g")
n_segments <- c(6,2,5,5,4,5,6,3,7,6) #number of segements per digit
names(n_segments) <- c(0:9) #digits 0 to 9


for (i in 1:nrow(digits)) {
  #temp
  digits_list <- strsplit(as.character(digits[i,]), "")
  eins <- unlist(digits_list[lapply(digits_list, length) == n_segments["1"]])
  vier <- unlist(digits_list[lapply(digits_list, length) ==n_segments["4"]])
  sieben <- unlist(digits_list[lapply(digits_list, length) ==n_segments["7"]])
  acht <- unlist(digits_list[lapply(digits_list, length) ==n_segments["8"]])


  # berechnen, wie oft die "falschen" segmente in digits vorkommen
  a <- sum(lengths(regmatches(digits[i,], gregexpr("a", digits[i,]))))
  b <- sum(lengths(regmatches(digits[i,], gregexpr("b", digits[i,]))))
  c <- sum(lengths(regmatches(digits[i,], gregexpr("c", digits[i,]))))
  d <- sum(lengths(regmatches(digits[i,], gregexpr("d", digits[i,]))))
  e <- sum(lengths(regmatches(digits[i,], gregexpr("e", digits[i,]))))
  f <- sum(lengths(regmatches(digits[i,], gregexpr("f", digits[i,]))))
  g <- sum(lengths(regmatches(digits[i,], gregexpr("g", digits[i,]))))
  wrong_seg <- c(a,b,c,d,e,f,g)
  names(wrong_seg) <- c("a", "b", "c", "d", "e","f","g")
  
  # zuordnung zu den richtigen segmenten
  segments$e[i] <- names(which(wrong_seg == 4)) # nur e kommt 4mal vor
  segments$b[i] <- names(which(wrong_seg == 6)) # nur b kommt 6mal vor
  segments$f[i] <- names(which(wrong_seg == 9)) # nur f kommt 9mal vor
  
  # c kommt in 1 vor und != f
  segments$c[i] <- eins[which(eins!=segments$f[i])] 
  
  # a gibt es 8mal und != c
  temp_a <- names(which(wrong_seg == 8))!=segments$c[i]
  segments$a[i] <- names(which(wrong_seg == 8))[temp_a]
  
  # d kommt in 4 vor und != b oder c oder f
  temp_d <- vier[vier!=segments$b[i]] #ohne b
  temp_d <- temp_d[temp_d!=segments$c[i]] #ohne c
  segments$d[i] <- temp_d[temp_d!=segments$f[i]] #ohne f
  
  # g gibt es 7mal und != d
  temp_g <- names(which(wrong_seg == 7))!=segments$d[i]
  segments$g[i] <- names(which(wrong_seg == 7))[temp_g]
}


for (i in 1:nrow(segments)) {
  #welche "falschen" segmente ergeben welche zahl? alphabetisch sortiert
    null <- paste(sort(c(segments$a[i], segments$b[i], segments$c[i], segments$e[i], segments$f[i], segments$g[i])), collapse="")
    eins <- paste(sort(c(segments$c[i], segments$f[i])), collapse="")
    zwei <- paste(sort(c(segments$a[i], segments$c[i], segments$d[i], segments$e[i], segments$g[i])), collapse="")
    drei <- paste(sort(c(segments$a[i], segments$c[i], segments$d[i], segments$f[i], segments$g[i])), collapse="")
    vier <- paste(sort(c(segments$b[i], segments$c[i], segments$d[i], segments$f[i])), collapse="")
    fünf <- paste(sort(c(segments$a[i], segments$b[i], segments$d[i], segments$f[i], segments$g[i])), collapse="")
    sechs <- paste(sort(c(segments$a[i], segments$b[i], segments$d[i], segments$e[i], segments$f[i], segments$g[i])), collapse="")
    sieben <- paste(sort(c(segments$a[i], segments$c[i], segments$f[i])), collapse="")
    acht <- paste(sort(c(segments$a[i], segments$b[i], segments$c[i], segments$d[i], segments$e[i], segments$f[i], segments$g[i])), collapse="")
    neun <- paste(sort(c(segments$a[i], segments$b[i], segments$c[i], segments$d[i], segments$f[i], segments$g[i])), collapse="")
    
    #digits alphabetisch sortieren und wieder zusammenfügen
    digits_list <- strsplit(as.character(output[i,]), "")
    digits_list <- sapply(digits_list, sort)
    for (j in 1:4) {
      digits_list[[j]] <- paste(digits_list[[j]][1:length(digits_list[[j]])], collapse="")
    }
    
    #strings durch zahlen ersetzen
    temp <- sub(acht,8, digits_list)
    temp <- sub(null,0, temp)
    temp <- sub(sechs,6, temp)
    temp <- sub(neun,9, temp)
    temp <- sub(zwei, 2, temp)
    temp <- sub(drei, 3, temp)
    temp <- sub(fünf ,5, temp)
    temp <- sub(vier,4, temp)
    temp <- sub(sieben,7, temp)
    temp <- sub(eins,1, temp)
    
    output[i,] <- temp
    output$total[i] <- paste(output[i,1:4],collapse="")
    
}

sum(as.numeric(output$total)) #1043101
