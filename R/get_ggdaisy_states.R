#' Get the states a phylogeny edge can be in.
#' These states are \code{\link{get_daisy_input_statuses}}
#' with an \code{invisible} state added
#' @author Richel J.C. Bilderbeek
#' @export
get_ggdaisy_states <- function() {
  c(get_daisy_input_statuses(), "invisible")
}
