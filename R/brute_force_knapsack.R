brute_force_knapsack <- function(x, W) {

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


