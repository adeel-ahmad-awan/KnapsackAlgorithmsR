dynamic_programming_knapsack <- function(x, W) {

  wieghtVector <- as.numeric(x[,1])
  valueVector <- as.numeric(x[,2])


  # - helper function can include return boolean
  # itemIndex - i, currentWeightCapacity = j
  canInclude <- function(itemIndex, currentWeightCapacity) {
    if (wieghtVector[itemIndex] > currentWeightCapacity) {
      return(FALSE)
    }
    return(TRUE)
  }

  # - helper function max (current item and previous item, not current item and previous item)
  maxOptimumValue <- function(myMatrix, itemIndex, currentWeightCapacity) {
    if (itemIndex == 1) {
      return(valueVector[itemIndex])
    }
    previousitemIndex <- itemIndex-1

    previousWeightIndex <- currentWeightCapacity - wieghtVector[itemIndex]
    if (previousWeightIndex < 1) {
      newValue <- valueVector[itemIndex] + 0
    } else {
      newValue <- valueVector[itemIndex] + myMatrix[previousitemIndex, previousWeightIndex]
    }
    previousitem <- myMatrix[previousitemIndex, currentWeightCapacity]

    return(max(newValue, previousitem))
  }

  # - helper function to get previous item)
  getPrevVal <- function(myMatrix, itemIndex, currentWeightCapacity) {
    previousitem <- itemIndex - 1
    if (previousitem < 1) {
      return(0)
    }
    previousitem <- myMatrix[previousitem, currentWeightCapacity]
    return(previousitem)
  }

  # create a matrix
  # rows = 0 - number of items
  # cols = 0 - weight upto given weight limit (W)

  myMatrix = matrix(NA, nrow = length(valueVector), ncol = W)
  rownames(myMatrix) <- c(1:length(valueVector))
  colnames(myMatrix) <- c(1:W)
  myMatrix[1,] <- 0


  # loop through the martix and at every cell
  # make a decision that we should or should not include the item based on prevvious result
  # If included add the value of the item
  for (i in 1:length(myMatrix[,1])) {
    matrixRow <- myMatrix[i, ]
    for (j in 1:length(matrixRow)) {
      if(canInclude(i,j)) {
        temp <- maxOptimumValue(myMatrix = myMatrix, itemIndex = i, currentWeightCapacity = j)
        myMatrix[i,j] <- temp
      } else {
        myMatrix[i,j] <- getPrevVal(myMatrix = myMatrix, itemIndex = i, currentWeightCapacity = j)
      }
    }
  }

  valueRowIndex <- length(myMatrix[,1])
  valueColIndex <- length(myMatrix[1,])

  value <- myMatrix[valueRowIndex, valueColIndex]


  includedValues <- c()

  # - helper function traceBack the items included
  # for traceBack the items included
  # 1 - compare the best value with the cell above, If same that is not included, If different that it is included
  # 2 - Add the item in the included vector
  # 3 - Go to one cell above and the column should be current weight - item weight
  # 4 - repete the process
  # 5 - end the process if we reach the column 0
  getIncludedValues <- function() {

    for (i in 1:length(valueVector)) {
      if (valueRowIndex - 1 > 0 && myMatrix[valueRowIndex - 1, valueColIndex] != myMatrix[valueRowIndex, valueColIndex]) {
        includedValues <<- append(includedValues, valueRowIndex)

        valueColIndex <<- valueColIndex - wieghtVector[valueRowIndex]
        if (valueColIndex < 1) {
          valueColIndex <<- 1
        }


      }
      valueRowIndex <<- valueRowIndex - 1
    }
  }

  # populate the includedValues vector
  getIncludedValues()


  returnValue <- list(value, includedValues)
  names(returnValue) <- c("value", "elements")

  return(returnValue)
}