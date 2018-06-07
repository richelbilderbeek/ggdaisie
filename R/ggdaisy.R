install_ggtree <- function() {
  source("https://bioconductor.org/biocLite.R")
  # biocLite("BiocUpgrade") # you may need this
  biocLite("ggtree")
}

ggdaisie <- function() {


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
