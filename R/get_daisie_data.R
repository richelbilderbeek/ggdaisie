#' Read a data frame from the \code{data} folder
#' @param filename the file's name, without the path
#' @return the content of the file as a data frame,
#'   if and only if the file is present. Will stop otherwise.
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     is.data.frame(
#'       get_daisie_data("Galapagos_datatable.RData")
#'     )
#'   )
#' @export
get_daisie_data <- function(filename) {

  full_path <- system.file("data", filename, package = "DAISIE")
  if (!file.exists(full_path)) {
    stop("'filename' absent. '", filename, "' not found")
  }
  name <- load(full_path)
  df <- get(name)
  rm(name)

  if ("Branching_times" %in% colnames(df)) {
    df$Branching_times <- as.character(df$Branching_times)
  }
  df
}
