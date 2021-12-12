#' Reactivity Rate Constant (k) Calculator
#'
#' Calculate rate constant k from initial concentration, final concentration and time using a first-order reaction equation
#' @param C1 initial concentration of substance
#' @param C2 final concentration of substance
#' @param time time period of experiment
#' @return The rate constant(k) for the experiment
#' @import SciViews
#' @import tidyverse
#' @examples
#' C1 <- 20
#' C2 <- 10
#' time <- 5
#' k <- k_calculator(C1, C2, time)
#' @export
k_calculator <- function(C1,C2, time){
  k <- (ln(C1/C2)) / time;
  return(k);
}



