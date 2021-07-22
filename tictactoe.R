
##### TIC TAC TOE - KOPRIK #####################################################

# one_move reads input from the console and checks its validity (repeating this
# until a valid input is given). It returns a valid cell. Arguments:
# cells_possible: a vector containing the values of the cells that can be chosen
#   at this point in the game
one_move <- function(cells_possible) {
  cell_valid <- FALSE
  while (!cell_valid) {
    cell <- scan(what = numeric(), n = 1, quiet = TRUE)
    # Check validity of input
    if (!(cell %in% cells_possible)) {
      cat("Cell not valid. Again:")
    } else{
      cell_valid <- TRUE
    }
  }
  return(cell)
}

# check_vector checks if all elements of a vector are identical, but unequal to 0. 
# It is used to check whether one player won the game. It returns TRUE or FALSE.
# Arguments:
# vector: the vector to be checked
check_vector <- function(vector) {
  if (sum(vector) == 0) { # vector contains only 0s
    return(FALSE)
  } else if (any(vector != vector[1])) { # not all elements identical
    return(FALSE)
  } else { # all elements identical
    return(TRUE)
  }
}

# check_win checks if one player won the game. It returns TRUE or FALSE. Arguments:
# board: A 3x3 matrix corresponding to the current playing field. 0s correspond
#   to empty cells, 1s to cells marked by Player 1, 2s to cells marked by Player 2.
check_win <- function(board) {
  win <- FALSE
  if (any(apply(board, 1, check_vector)) | # checks for win in rows
      any(apply(board, 2, check_vector)) | # checks for win in columns
      check_vector(diag(board)) | # checks for win in diagonal
      check_vector(diag(board[, rev(seq_len(ncol(board)))]))) { # checks for win in opposite diagonal
    win <- TRUE
  }
  return(win)
}

# tic_tac_toe enables 2 persons to play Tic-tac-toe via the console. It has no
# arguments.
tic_tac_toe <- function() {
  # keep user's par settings 
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  
  # plot empty playing field
  par(xaxs = "i", yaxs = "i") # These arguments prevent the adding of extra space at the axis intervals
  plot.new()
  plot.window(xlim = c(0.5, 3.5), ylim = c(0.5, 3.5))
  grid(nx = 3, ny = 3, col = "black")
  box(lwd = 2)
  text(rep(1:3, 3), rep(1:3, each = 3), labels = 1:9, col = "lightgray", cex = 4)
  
  # initialization
  cells_possible <- 1:9
  board <- matrix(0, nrow = 3, ncol = 3)
  move_count <- 1
  which_player  <- rep(c(1, 2), length = 9)
  
  cat(paste0("In each move you have to choose one cell 1-9.\n"))
  
  while (length(cells_possible) > 0) { 
    # execute one move
    cat(paste0("Player ", which_player[move_count], ":"))
    cell <- one_move(cells_possible)
    # get coordinates for plot and mark cell in plot
    column <- ifelse(cell %% 3 == 0, 3, cell %% 3)
    row <- ceiling(cell / 3)
    points(column, row, cex = 8, pch = c(1, 4)[which_player[move_count]], 
           col = c("red", "green")[which_player[move_count]], adj = 1)
    # update board matrix
    board[row, column] <- which_player[move_count]
    # check if the current player won (we check for a win after every move, i.e.,
    # if win == TRUE the player who performed the last move is the winner)
    win <- check_win(board) 
    if (win) { # terminate function with corresponding message if player won
      message <- paste0("Player ", which_player[move_count], " wins!")
      return(message)
    } else { # update the valid cells and the move count, if the player didn't win
      cells_possible <- cells_possible[-which(cells_possible == cell)]
      move_count <- move_count + 1
    }
  }
  return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
}

tic_tac_toe()
