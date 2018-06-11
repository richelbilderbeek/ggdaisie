#' Creku
#' Plot a DAISIE data table
#' @param df a data frame
#' @param island_age age of the island
#' @author Richel J.C. Bilderbeek
#' @export
ggd_create_phylos <- function(
  df,
  island_age
) {
  testit::assert("Status" %in% names(df))
  testit::assert(all(df$Status %in% get_daisy_input_statuses()))
  testit::assert(class(df$Branching_times) == "character")

  nrows <- nrow(df)

  # Initialize with a dummy phylo object,
  # so that 'c' can convert to multiPhylo
  phylos <- ape::read.tree(text = "(X:0,Y:0);")

  for (i in seq(1, nrows)) {
    brts_str <- df$Branching_times[i]
    brts <- as.numeric(strsplit(brts_str, ',')[[1]])
    phylo <- ggd_create_phylo(
      clade_name = df$Clade_name[i],
      status = df$Status[i],
      branching_times = brts,
      island_age = island_age
    )
    testit::assert(!is.null(phylo))
    phylos <- c(phylos, phylo)
    testit::assert(class(phylos) == "multiPhylo")
  }

  testit::assert(class(phylos) == "multiPhylo")
  # Remove the dummy phylo object
  phylos <- phylos[-1]

  phylos
}
