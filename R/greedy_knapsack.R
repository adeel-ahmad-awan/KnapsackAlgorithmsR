#' greedy_knapsack
#' @description This function solve knapsack using greedy approach.
#' @param x data frame.
#' @param W A number.
#' @param fast A Boolean
#' @return result containing value and elements in `list`.
#' @export
greedy_knapsack <- function(x, W, fast = FALSE) {
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


  if (fast) {

    src <-
      "int getCount(NumericVector myarr, int W)
    {
    int currWeight = 0;
    int count = 1;
    while (currWeight <= W)
    {
    int start = 0;
    int end = count - 1;
    int tempVal = 0;
    for (int i = start; i <= end; i++)
    {
      tempVal = tempVal + myarr[i];
    }

    currWeight = tempVal;
    count = count + 1;
    if (R_isnancpp(currWeight))
    {
    break;
    }
    }

    return count;
    }
    "

    Rcpp::cppFunction(src)
    count <- getCount(as.vector(as.numeric(x$w)), W)
  } else {
    currWeight <- 0
    count <- 1
    while (currWeight <= W) {
      currWeight <- sum(x$w[1:count])
      count = count + 1
      if (is.na(currWeight)) {
        break
      }
    }
  }


  myValue <- sum(x$v[1:(count-2)])
  elements <- as.numeric(rownames(x[1:(count-2),]))

  returnValue <- list(myValue, elements)
  names(returnValue) <- c("value", "elements")
  return(returnValue)
}
