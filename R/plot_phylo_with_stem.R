#' Plot a phylogeny with a stem
#' @param phylo a phylogeny of class \code{phylo}
#' @author Richel J.C. Bilderbeek
plot_phylo_with_stem <- function(phylo) {
  plot_phylo_with_stem_ggtree(phylo)
}

#' Plot a phylogeny with a stem using ape
#' @param phylo a phylogeny of class \code{phylo}
#' @author Richel J.C. Bilderbeek
plot_phylo_with_stem_ape <- function(phylo) {
  ape::plot.phylo(phylo, root.edge = TRUE)
}

#' Plot a phylogeny with a stem using ape
#' @param phylo a phylogeny of class \code{phylo}
#' @author Richel J.C. Bilderbeek
plot_phylo_with_stem_ggtree <- function(phylo) {

  group <- NULL; rm(group) # nolint, should fix warning: no visible binding for global variable

  phylo_with_outgroup <- ribir::add_outgroup_to_phylogeny(
    phylogeny = phylo,
    stem_length = phylo$root.edge,
    outgroup_name = "invisible"
  )
  #class(phylo_with_outgroup)
  attributes(phylo_with_outgroup)
  ape::Ntip(phylo_with_outgroup)
  attr(phylo_with_outgroup, "group") <- as.factor(c(1, 2, 2, 2, 2))
  ggtree::ggtree(
    phylo_with_outgroup,
    ggplot2::aes(color = group)
  ) +
  ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
  ggtree::geom_tiplab() +
  ggtree::geom_rootpoint()
}
