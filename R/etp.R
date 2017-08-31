
#' Split ET_p into Evaporation and Transpiration
#'
#' Based on Beer's law , this function splits potential evapotranspiration into
#' potential transpiration and potential evaporation depending on Leaf Area Index
#' (LAI) and the extinction factor k.
#'
#'
#' @param etp evapotranspiratio
#' @param lai Leaf area index
#' @param k extinction coefficient
#'
#' @return data.frame with columns evaporation and transpiration
#' @export
split_etp <- function(etp, lai, k = 0.39) {
  etp <- unlist(etp)
  lai <- unlist(lai)
  if(length(etp) != length(lai)) stop("etp and lai must be of same length")
  tibble::data_frame(
    evaporation = etp * exp(-k * lai),
    transpiration = etp - evaporation
    # scf = 1 - exp(-k * lai)
  )
}

#' Title
#'
#' @param p precipitation
#' @param evaporation evaporation
#' @param lai lai
#' @param a a
#' @param k k
#'
#' @return data.frame
#' @export
interception <- function(p, evaporation, lai, a = 0.025, k = 0.39) {
  p <- unlist(p)
  evaporation <- unlist(evaporation)
  lai <- unlist(lai)
  scf <- 1 - exp(-k * lai)
  tibble::data_frame(
    precipitation = p,
    interception = a * lai * (1 - (1 / (1 + (scf * p) / (a * lai)))),
    infiltration = p - interception,
    evaporation_red = pmax(0, evaporation - interception)
  )
}

