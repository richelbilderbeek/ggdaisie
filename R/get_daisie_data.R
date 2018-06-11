#' Get the full path of a file in the \code{inst/extdata} folder
#' @param filename the file's name, without the path
#' @return the full path of the filename, if and only if
#'   the file is present. Will stop otherwise.
#' @author Richel J.C. Bilderbeek
#' @seealso for more files, use \code{\link{get_babette_paths}}
#' @examples
#'   testit::assert(is.character(get_babette_path("anthus_aco.fas")))
#'   testit::assert(is.character(get_babette_path("anthus_nd2.fas")))
#' @export
get_daisie_data <- function(filename) {

  full_path <- system.file("data", filename, package = "DAISIE")
  if (!file.exists(full_path)) {
    stop("'filename' absent. '", filename, "' not found")
  }
  name <- load(full_path)
  df <- get(name)
  rm(name)
  df
}
