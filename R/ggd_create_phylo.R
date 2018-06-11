#' Create a phylogeny with the correct statuses
#' @return a phylogeny of class \code{phylo} with a \code{status} attribute
ggd_create_phylo <- function(
  clade_name,
  status,
  branching_times
) {
  testit::assert(status %in% get_daisy_input_statuses())
  if (status == "Non_endemic_MaxAge") {
    ggd_create_phylo_non_endemic_max_age(
      time = branching_times,
      taxon_label = clade_name
    )
  } else {

  }
}

#' Create a phylogeny with the correct statuses
#' @return a phylogeny of class \code{phylo} with a \code{status} attribute
ggd_create_phylo_non_endemic_max_age <- function(
  time,
  taxon_label
) {
  group <- NULL; rm(group) # nolint, should fix warning: no visible binding for global variable

  phylo <- ape::read.tree(text = paste0("(", taxon_label,":", time, ",X:", time, ");"))
  attr(phylo, "group") <- as.factor(c(2, 1, 2))
  phylo
}
