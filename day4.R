#day4
numb <- scan('day4.txt', sep=",", nlines = 1)
board <- read.table('day4.txt', skip =1)
n_boards <- nrow(board)/5
n_boards
board$number <- rep(1:n_boards, each=5)
boardlist <- split(board, board$number)
boardlist <- lapply(boardlist, function(x) { x["number"] <- NULL; x })

for (i in 1: length(numb)) {
  for (j in 1: length(boardlist)) {
    x <- boardlist[[j]][]!=numb[i]
    boardlist[[j]][] <- boardlist[[j]][]*x
    if (sum(colSums(boardlist[[j]]) == 0)>=1 || sum(rowSums(boardlist[[j]]) == 0)>=1) break;
  }
  if (sum(colSums(boardlist[[j]]) == 0)>=1 || sum(rowSums(boardlist[[j]]) == 0)>=1) break;
}

sum(boardlist[[j]])*numb[i] #54810 too high

