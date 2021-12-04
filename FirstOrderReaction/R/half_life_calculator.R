#' Half-Life Calculator
#'
#' Calculate the half-life of decaying substance using reactivity rate constant(k)
#' 
#' @param k rate constant
#' 
#' @return The half-life of a decaying substance
#' 
#' @import SciViews
#' @import tidyverse
#' 
#' @examples 
#' half_life(k);
#' C1 <- 20
#' C2 <- 10
#' time <- 5
#' k_calculator(C1, C2, time) %>%
#'      half_life(k)
#' 
#' @export
#' 
half_life <- function(k){
  hl <- (ln(2)) / k;
  return(hl);
}