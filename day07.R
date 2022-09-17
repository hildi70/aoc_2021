#part 1
crabs <- as.numeric(scan("input/day07.txt", sep=",", nlines = 1))

fuel <- data.frame((matrix(nrow=1, ncol=length(crabs))))
for (i in 0:max(crabs)) {
  fuel[i,] <- unlist(sapply(crabs, function(x) {ifelse(x >=i, x-i, i-x)}))
}

min(rowSums(fuel)) # 347449

#part 2
fuel_new <- data.frame((matrix(nrow=1, ncol=length(fuel))))
for (i in 1:nrow(fuel)) {
  fuel_new[i,] <- unlist(sapply(fuel[i,], function(x) {max(cumsum(0:x))}))
}
min(rowSums(fuel_new)) #98039527
