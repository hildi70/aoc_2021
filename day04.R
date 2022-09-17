drawn_numbers <- scan("input/day04.txt", sep=",", nlines = 1)
bingo_boards <- read.table("input/day04.txt", skip =1)

n_boards <- nrow(bingo_boards)/5
bingo_boards$number <- rep(1:n_boards, each=5)
bingo_boards <- split(bingo_boards, bingo_boards$number)
bingo_boards <- lapply(bingo_boards, function(x) { x["number"] <- NULL; x })

##### part 1 #####
bingo_boards_logical <- lapply(bingo_boards, `>`, -1)


for (number in seq_along(drawn_numbers)) {
  for (board in seq_along(bingo_boards)) {
    temp <- bingo_boards[[board]][]!= drawn_numbers[number]
    bingo_boards_logical[[board]][] <- temp*bingo_boards_logical[[board]][]
    if (sum(colSums(bingo_boards_logical[[board]]) == 0)>=1 || sum(rowSums(bingo_boards_logical[[board]]) == 0)>=1) break;
  }
  if (sum(colSums(bingo_boards_logical[[board]]) == 0)>=1 || sum(rowSums(bingo_boards_logical[[board]]) == 0)>=1) break;
}

sum(bingo_boards[[board]]*bingo_boards_logical[[board]])*drawn_numbers[number]  #34506

##### part 2 #####
bingo_boards_logical <- lapply(bingo_boards, `>`, -1)  #reset
bingo_boards_2 <- bingo_boards

board = 1

for (number in seq_along(drawn_numbers)) {
  while (board <= length(bingo_boards_logical)) {
    x <- bingo_boards_2[[board]][]!=drawn_numbers[number]
    bingo_boards_logical[[board]][] <- x*bingo_boards_logical[[board]][]

    if (sum(colSums(bingo_boards_logical[[board]]) == 0)>=1 || sum(rowSums(bingo_boards_logical[[board]]) == 0)>=1) {
        bingo_boards_logical <- bingo_boards_logical[-board] 
        bingo_boards_2 <- bingo_boards_2[-board]
      } else {
      board <- board +1
      }
  }
  board = 1
  if (length(bingo_boards_logical) == 1) break;
}

for (number in seq_along(drawn_numbers)) {
  for (board in seq_along(bingo_boards_2)) {
    x <- bingo_boards_2[[board]][]!=drawn_numbers[number]
    bingo_boards_logical[[board]][] <- x*bingo_boards_logical[[board]][]
    if (sum(colSums(bingo_boards_logical[[board]]) == 0)>=1 || sum(rowSums(bingo_boards_logical[[board]]) == 0)>=1) break;
  }
  if (sum(colSums(bingo_boards_logical[[board]]) == 0)>=1 || sum(rowSums(bingo_boards_logical[[board]]) == 0)>=1) break;
}

sum(bingo_boards_2[[board]]*bingo_boards_logical[[board]])*drawn_numbers[number] #7686


