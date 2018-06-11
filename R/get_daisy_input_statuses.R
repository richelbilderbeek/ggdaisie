#' DAISIE input statuses as used by \code{\link[DAISIE]{DAISIE_dataprep}}
#' @author Richel J.C. Bilderbeek
#' @export
get_daisy_input_statuses <- function() {
  c("Endemic", "Non_endemic", "Non_endemic_MaxAge", "Endemic&Non_Endemic")
}
