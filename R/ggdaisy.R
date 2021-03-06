#install_ggtree <- function() {
#  source("https://bioconductor.org/biocLite.R")
#  # biocLite("BiocUpgrade") # you may need this
#  biocLite("ggtree")
#}

#' Plot a DAISIE data table
#' @param df a data frame
#' @param island_age age of the island
#' @author Richel J.C. Bilderbeek
#' @export
ggdaisie <- function(
  df,
  island_age
) {
  testit::assert("Status" %in% names(df))
  testit::assert(all(df$Status %in% get_daisy_input_statuses()))
  testit::assert(class(df$Branching_times) == "character")

  phylos <- ggd_create_phylos(df = df, island_age = island_age)
  testit::assert(class(phylos) == "multiPhylo")
  testit::assert(length(phylos) == nrow(df))
  ggd_plot(phylos)
}

 test_plot_two_one_taxon_trees_naive <- function(
  time_1 = 10, taxon_label_1 = "kip",
  time_2 = 5, taxon_label_2 = "koe"
) {
  group <- NULL; rm(group) # nolint, should fix warning: no visible binding for global variable

  phylo_1 <- ape::read.tree(text = paste0("(", taxon_label_1,":", time_1, ",B:", time_1, ");"))
  phylo_2 <- ape::read.tree(text = paste0("(", taxon_label_2,":", time_2, ",B:", time_2, ");"))
  attr(phylo_1, "group") <- as.factor(c(2, 1, 2))
  attr(phylo_2, "group") <- as.factor(c(2, 1, 2))
  ggtree::ggtree(
    c(phylo_1, phylo_2),
    ggtree::aes(color = group, linetype = group)
  ) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    ggplot2::facet_wrap(~.id, scales="fixed", nrow = 2) +
    ggplot2::scale_x_continuous(limits = c(0,10)) +
    ggtree::theme_tree2()
}

test_plot_two_one_taxon_trees_naive <- function(
  time_1 = 10, taxon_label_1 = "kip",
  time_2 = 5, taxon_label_2 = "koe"
) {
  group <- NULL; rm(group) # nolint, should fix warning: no visible binding for global variable

  phylo_1 <- ape::read.tree(text = paste0("(", taxon_label_1,":", time_1, ",B:", time_1, ");"))
  phylo_2 <- ape::read.tree(text = paste0("(", taxon_label_2,":", time_2, ",B:", time_2, ");"))
  attr(phylo_1, "group") <- as.factor(c(2, 1, 2))
  attr(phylo_2, "group") <- as.factor(c(2, 1, 2))
  p <- ggtree::ggtree(phylo_1, ggtree::aes(color = group, linetype = group)) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    ggtree::geom_tiplab() +
    ggtree::theme_tree2()
  q <- ggtree::ggtree(phylo_2, ggtree::aes(color = group, linetype = group)) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    ggtree::geom_tiplab() +
    ggtree::theme_tree2()
  plots <- list(p, q)
  ggtree::multiplot(plotlist = plots, ncol = 1)

}

test_plot_one_taxon_tree <- function(
  time, taxon_label) {

  group <- NULL; rm(group) # nolint, should fix warning: no visible binding for global variable

  phylo <- ape::read.tree(text = paste0("(", taxon_label,":", time, ",B:", time, ");"))
  attr(phylo, "group") <- as.factor(c(2, 1, 2))
  ggtree::ggtree(
    phylo,
    ggtree::aes(color = group, linetype = group)
  ) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    ggtree::geom_treescale() + ggtree::geom_tiplab()
}

test_plot_two_taxon_tree_with_stem <- function() {

  phylo <- ape::read.tree(text = "(A:1,B:1):20;")
  plot_phylo_with_stem(phylo)
}

test_plot_two_taxon_tree <- function() {

  phylo <- ape::read.tree(text = "(A:20,B:20);")
  ape::plot.phylo(phylo); ape::add.scale.bar()
  ggtree::ggtree(phylo) + ggtree::geom_treescale()
}
