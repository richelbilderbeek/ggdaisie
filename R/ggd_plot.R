#' Plot the phylogenies at the same scale
#' @author Richel J.C. Bilderbeek
#' @export
ggd_plot <- function(phylos) {

  testit::assert(class(phylos) == "multiPhylo")
  for (phylo in phylos) {
    testit::assert("status" %in% names(attributes(phylo)))
  }
  ggtree::ggtree(
    phylos
    ,ggtree::aes(color = status, linetype = status)
  ) +
    #ggplot2::scale_colour_manual(values = c("#008800", "#008800", "#FFFFFF")) +
    ggplot2::facet_wrap(~.id, scales="fixed", nrow = length(phylos)) +
    ggtree::geom_tiplab(align = FALSE) + # nolint will align by adding a hidden root later
    ggtree::theme_tree2(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank()
    ) +
    ggtree::geom_rootpoint()

}
