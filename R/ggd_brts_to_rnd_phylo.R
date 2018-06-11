#' Create a comb phylo from branch length
ggd_brts_to_rnd_phylo <- function(
  branching_times
) {
  testit::assert(length(branching_times) >= 1)
  branching_times <- sort(branching_times)
  # Shortest Branching Time
  sbt <- branching_times[1]
  init_newick <- paste0("(A:", sbt,",B:", sbt,");")
  phylo <- ape::read.tree(text = init_newick)
  if (length(branching_times) == 1) return(phylo)
  for (i in seq(2, length(branching_times))) {
    phylo <- ribir::add_outgroup_to_phylogeny(
      phylogeny = phylo,
      stem_length = branching_times[i] - branching_times[i-1],
      outgroup_name = LETTERS[i + 1]
    )
  }
  phylo
}
