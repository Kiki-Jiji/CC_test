#'@title Center
#'
#'@description A function that centers the data around a chosen midpoint
#'
#'@details This function takes a vector of data and retrurns a vector of equal length with the vlaues trnasformed to be centered around a new value. By defualt this value is 0
#'
#'@param data A vector of data to transform
#'
#'@param midpoint A value around which to center the data
#'
#'@return A vector of equal length
#'
#'@export
#'

center <- function(data, midpoint = 0) {

  new_data <- (data - mean(data)) + midpoint

  return(new_data)
}
