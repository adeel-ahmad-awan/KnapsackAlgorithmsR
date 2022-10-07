#' Brute force solution for knapsack problem. 
#' @description This algorithm tries all possible alternatives of items in the knapsack and optimize the solution.
#' @param x A data frame with 2 columns which include w (weight)and v (value)
#' @param W Represent the total capacity of the knapsack
#' @return A list which includes the total value of all the items and all the elements that are in the knapsack.
#' @export

brute_force_knapsack <- function(x, W) {

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



  wieghtVector <- as.numeric(x[,1])
  valueVector <- as.numeric(x[,2])
  optimumValue = 0
  optimumVactor <- c()


  # helper functions
  elementCombinations <- function(active, rest) {
    if (length(rest) == 1) {

      temp <- getValueWithinLimit(active, W)

      if (temp > optimumValue) {
        optimumValue <<- temp
        optimumVactor <<- active
      }
    }
    else {
      if (length(rest) == 1) {
        tempRest <- rest[1]
      }
      tempRest <- rest[2: length(rest)]
      elementCombinations(c(active , rest[1]), tempRest);
      elementCombinations(active, rest[2: length(rest)]);
    }

  }

  getValueWithinLimit <- function(myvector, weightLimit) {

    myvector <- as.numeric(myvector)

    # print(c("myvector",myvector))
    # print(c("weightVector",weightVector))
    if (sum(wieghtVector[myvector]) > weightLimit) {
      return(0)
    }
    return(sum(valueVector[myvector]))
  }


  active <- c()
  rest <- c(1:length(valueVector), '~')
  elementCombinations(active, rest);


  returnValue <- list(optimumValue, optimumVactor)
  names(returnValue) <- c("value", "elements")

  return(returnValue)
}


