greedy_knapsack <- function(x, W) {

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

  print(myValue)
  print(elements)

}
