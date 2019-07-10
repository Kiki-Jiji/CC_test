#'@title European 2007 Data
#'
#'@description A function that converts the raw dataset into the need 2007 data.
#'
#'@details This datset is used in a pipeline which takes in the raw datset, and produces a GDP value, and gives this value in a denomination of choice, either Trillion, Billion or Million.
#'
#'@param data The raw dataset
#'
#'@param denomination The value in which to give GDP, excepts T = Trillion, B = Billion or M= Millions
#'
#'@param plot This will generate a plot of GDP vs life expectancy
#'
#'@return returns a dataset
#'
#'@export
#'@import magrittr
#'@import ggplot2


europe_07 = function(data, plot = FALSE, denomination = B) {

  if (denomination == "T") {
    convert = function(x ) {
      x / 1000000000000
    }
  }

  if (denomination == "B") {

  convert = function(x ) {
    x / 1000000000
   }
  }

  if (denomination == "M") {
    convert = function(x ) {
      x / 1000000
    }
  }

 internal_object <- data %>%

  dplyr::filter(year == 2007) %>%

  dplyr::mutate(GDP =  gdpPercap * pop) %>%

  dplyr::mutate(GDP_value  = convert(GDP))

 if(plot) {
   plot <- ggplot2::ggplot(internal_object, aes(x = GDP_value, y = lifeExp, color = continent)) + geom_point() + scale_x_log10()
 }

 return(plot)
 return(internal_object)

}

