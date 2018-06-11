#' Create a phylogeny with the correct statuses
ggd_create_phylo <- function(
  clade_name,
  status,
  branching_times
) {
  testit::assert(status %in% get_daisy_input_statuses())
  if (status == "Non_endemic_MaxAge") {
    create_single_taxon_tree(
      time = branching_times,
      taxon_label = clade_name
    )
  } else {

  }

}
