#' Plot the phylogenies at the same scale
#' @param phylos one or more phylogenies, of class \code{multiPhylo}
#' @author Richel J.C. Bilderbeek
#' @export
ggd_plot <- function(phylos) {

  testit::assert(class(phylos) == "multiPhylo")
  for (phylo in phylos) {
    testit::assert("status" %in% names(attributes(phylo)))
  }

  status <- NULL; rm(status) # nolint, should fix warning: no visible binding for global variable

  ggtree::ggtree(
    phylos, ggtree::aes(color = status, linetype = status)
  ) +
    ggplot2::scale_colour_manual(values = c("#0000FF", "#008800", "#008800", "#FFFFFF")) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid", "dashed", "longdash")) +
    ggplot2::facet_wrap(~.id, scales="fixed", nrow = length(phylos)) +
    ggtree::geom_tiplab(align = FALSE) + # nolint will align by adding a hidden root later
    ggtree::theme_tree2(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0, "mm") # nolint cannot make negative
    ) +
    ggtree::geom_rootpoint()

}
