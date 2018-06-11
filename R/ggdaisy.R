install_ggtree <- function() {
  source("https://bioconductor.org/biocLite.R")
  # biocLite("BiocUpgrade") # you may need this
  biocLite("ggtree")
}

#' Plot a DAISIE data table
#' @param df a data frame
#' @author Richel J.C. Bilderbeek
#' @export
ggdaisie <- function(
  df
) {
  nrows <- nrow(df)
  phylos <- ape::rcoal(4)

  for (i in seq(1, nrows)) {
    if (df$Status[i] == "Non_endemic_MaxAge") {

      phylos <- c(
        phylos,
        create_single_taxon_tree(
          time = df$Branching_times[i],
          taxon_label = df$Clade_name[i]
        )
      )
    }
  }
  print(class(phylos))
  phylos <- phylos[-1]
  testit::assert(class(phylos) == "multiPhylo")
  ggtree::ggtree(
    phylos,
    ggtree::aes(color = group, linetype = group)
  ) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#008800")) +
    ggplot2::facet_wrap(~.id, scales="fixed", nrow = length(phylos)) +
    ggtree::theme_tree2()
}


create_single_taxon_tree <- function(
  time, taxon_label
) {
  phylo <- ape::read.tree(text = paste0("(", taxon_label,":", time, ",B:", time, ");"))
  attr(phylo, "group") <- as.factor(c(2, 1, 2))
  phylo
}

 test_plot_two_one_taxon_trees_naive <- function(
  time_1 = 10, taxon_label_1 = "kip",
  time_2 = 5, taxon_label_2 = "koe"
) {

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

  phylo_1 <- ape::read.tree(text = paste0("(", taxon_label_1,":", time_1, ",B:", time_1, ");"))
  phylo_2 <- ape::read.tree(text = paste0("(", taxon_label_2,":", time_2, ",B:", time_2, ");"))
  attr(phylo_1, "group") <- as.factor(c(2, 1, 2))
  attr(phylo_2, "group") <- as.factor(c(2, 1, 2))
  p <- ggtree::ggtree(phylo_1, ggtree::aes(color = group, linetype = group)) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    #ggtree::geom_treescale() +
    ggtree::geom_tiplab() +
    ggtree::theme_tree2()
  q <- ggtree::ggtree(phylo_2, ggtree::aes(color = group, linetype = group)) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    #ggtree::geom_treescale() +
    ggtree::geom_tiplab() +
    ggtree::theme_tree2()
  plots <- list(p, q)
  ggtree::multiplot(plotlist = plots, ncol = 1)

}

test_plot_one_taxon_tree <- function(
  time, taxon_label) {

  phylo <- ape::read.tree(text = paste0("(", taxon_label,":", time, ",B:", time, ");"))
  attr(phylo, "group") <- as.factor(c(2, 1, 2))
  ggtree::ggtree(phylo, ggtree::aes(color = group, linetype = group)) +
    ggplot2::scale_colour_manual(values = c("#FFFFFF", "#000000")) +
    ggtree::geom_treescale() + ggtree::geom_tiplab()


}

test_plot_two_taxon_tree <- function() {

  phylo <- ape::read.tree(text = "(A:20,B:20);")
  ape::plot.phylo(phylo); ape::add.scale.bar()
  ggtree::ggtree(phylo) + ggtree::geom_treescale()
}

ggdaisie_notes <- function() {


  # beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
  # beast_tree <- read.beast(beast_file)
  # beast_tree
  # PBD::pbd_sim
  # ggtree(beast_tree, aes(color=rate)) +
  #   scale_color_continuous(low='darkgreen', high='red') +
  #   theme(legend.position="right")

  # Plot two phylos above each other
  p <- ape::rcoal(4)
  p$tip.label <- paste0("p", 1:4)
  phytools::plotSimmap(p, colors = "red")
  # s <- phytools::make.simmap(p, r
  #
  # q <- ape::rcoal(5)
  # q$tip.label <- paste0("q", 1:5)
  #
  #
  # ggtree(
  #   p,
  #   aes(color = group)
  # ) + ggtree::geom_tiplab() + geom_taxalink("p2", "p4", curvature = -0.5, color = "blue", size = 2)
  #
  # ggtree(
  #   q
  # ) + ggtree::geom_tiplab()
  #
  #
  # pp <- ggtree::ggtree(p) + ggtree::geom_tiplab()
  # pq <- ggtree::ggtree(q) + ggtree::geom_tiplab()
  # both <- list()
  # both[[1]] <- pp
  # both[[2]] <- pq
  #
  # ggtree::multiplot(
  #   both,
  #   ncol = 1
  # )
  #
  #
  # + geom_taxalink("p2", "q4")
  #
  # phylos <- c(c(p, q))
  # names(phylos[[1]])
  # ape::merge
  # ggtree::ggtree(phylos) +
  #   ggtree::geom_tiplab() +
  #   geom_taxalink("p2", "q4", curvature = -0.5, color = "blue", size = 2)


  #  geom_taxalink(
  #library(ggtree)
}
