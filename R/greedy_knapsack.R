greedy_knapsack <- function(x, W) {
  if(missing(x) || missing(W)) {
    stop("Param missing")
  }
  if (!is.data.frame(x)) {
    stop("Invalid param type. 'x' should be a data frame.")
  }
  if (!is.numeric(W)) {
    stop("Invalid param type. 'init_node' should be a number")
  }

  if (W < 1) {
    stop("Invalid type of parameter")
  }

  if (ncol(x) != 2) {
    stop("Invalid number of column in data frame")
  }

  if (!(colnames(x)[1] == 'w' && colnames(x)[2] == 'v')) {
    stop("Invalid columns in data frame")
  }

  temp <- as.vector(x[,2]/x[,1])
  temp <- order(temp, decreasing = TRUE)
  x <- x[temp,]

  currWeight <- 0

  count <- 1
  while (currWeight <= W) {
    currWeight <- sum(x$w[1:count])
    count = count + 1
    if (is.na(currWeight)) {
      break
    }
  }
  myValue <- sum(x$v[1:(count-2)])
  elements <- as.numeric(rownames(x[1:(count-2),]))



  returnValue <- list(myValue, elements)
  names(returnValue) <- c("value", "elements")
  return(returnValue)

}
