#part 1
#Vector for drawn numbers
numb <- scan('day4.txt', sep=",", nlines = 1)
#Boards as list
board <- read.table('day4.txt', skip =1)
n_boards <- nrow(board)/5
n_boards
board$number <- rep(1:n_boards, each=5)
boardlist <- split(board, board$number)
boardlist <- lapply(boardlist, function(x) { x["number"] <- NULL; x })
#Boards as logical (Danke Juli ;))
boardlist_logical <- lapply(boardlist, `>`, -1)


#Work in progress: I would like to express the second loop as lapply
for (i in 1: length(numb)) {
  for (j in 1: length(boardlist)) {
    x <- boardlist[[j]][]!=numb[i]
    boardlist_logical[[j]][] <- x*boardlist_logical[[j]][]
    if (sum(colSums(boardlist_logical[[j]]) == 0)>=1 || sum(rowSums(boardlist_logical[[j]]) == 0)>=1) break;
  }
  if (sum(colSums(boardlist_logical[[j]]) == 0)>=1 || sum(rowSums(boardlist_logical[[j]]) == 0)>=1) break;
}


sum(boardlist[[j]]*boardlist_logical[[j]])*numb[i] #34506

#part 2
