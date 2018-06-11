#' Create a phylogeny with the correct statuses
#' @param clade_name the clade's name
#' @param status the clade's status,
#'   must be a member of \code{\link{get_daisy_input_statuses}}
#' @param branching_times branching times
#' @param island_age island age
#' @return a phylogeny of class \code{phylo} with a \code{status} attribute
#' @author Richel J.C. Bilderbeek
ggd_create_phylo <- function(
  clade_name,
  status,
  branching_times,
  island_age = max(branching_times)
) {
  testit::assert(status %in% get_daisy_input_statuses())
  if (status == "Non_endemic_MaxAge") {
    testit::assert(length(branching_times) == 1)
    testit::assert(branching_times[1] >= island_age)
    ggd_create_phylo_non_endemic_max_age(
      time = island_age,
      taxon_label = clade_name
    )
  } else if (status == "Non_endemic") {
    testit::assert(length(branching_times) == 1)
    testit::assert(branching_times[1] <= island_age)
    ggd_create_phylo_non_endemic(
      immigration_time = branching_times,
      taxon_label = clade_name,
      island_age = island_age
    )
  } else {
    NULL
  }
}

#' Create a phylogeny with the correct statuses
#' @param time age of the clade
#' @param taxon_label name of the one species in this clade
#' @return a phylogeny of class \code{phylo} with a \code{status} attribute
#' @author Richel J.C. Bilderbeek
ggd_create_phylo_non_endemic_max_age <- function(
  time,
  taxon_label
) {
  status <- NULL; rm(status) # nolint, should fix warning: no visible binding for global variable

  phylo <- ape::read.tree(text = paste0("(", taxon_label,":", time, ",X:", time, ");"))
  attr(phylo, "status") <- factor(
    c("Non_endemic_MaxAge", "invisible", "Non_endemic_MaxAge"),
    levels = get_ggdaisy_states()
  )

  phylo
}

#' Create a phylogeny with the correct statuses
#' @param immigration_time immigration time
#' @param taxon_label name of the one species in this clade
#' @param island_age the island's age, used for padding
#' @return a phylogeny of class \code{phylo} with a \code{status} attribute
#' @author Richel J.C. Bilderbeek
ggd_create_phylo_non_endemic <- function(
  immigration_time,
  taxon_label,
  island_age = immigration_time
) {
  testit::assert(immigration_time <= island_age)

  status <- NULL; rm(status) # nolint, should fix warning: no visible binding for global variable

  newick <- paste0("(",
    taxon_label,":", immigration_time,
    ",X:", immigration_time, ");"
  )

  phylo <- ape::read.tree(text = newick)

  # Pad to the right, by adding a hidden outgroup to the stem
  phylo <- ribir::add_outgroup_to_phylogeny(
   phylogeny = phylo,
   stem_length = island_age - immigration_time,
   outgroup_name = "X"
  )

  attr(phylo, "status") <- factor(
    c("invisible", "Non_endemic", "invisible", "invisible", "invisible"),
    levels = get_ggdaisy_states()
  )


  phylo
}
