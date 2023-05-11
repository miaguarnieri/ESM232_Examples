#'  More complex population growth
#' @param time  period of growth
#' @param P initial population
#' @param parms$r - base growth rate
#' @param parms$carry_capacity - carrying capacityription
#' @return change in population
#'
dexppop_play = function(time, P, parms) {

  # compute rate of change of population
  dexpop = parms$r*P * parms$time

  # set rate of change to 0 if P is greater than carrying capacity
  dexpop = ifelse(P > parms$carry_capacity, 0, dexpop)
  return(list(dexpop))
}
